# load packages and function script
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(here)
library(plotly)
library(DT)
library(janitor)
library(stringr)
source(here::here('R', 'sourced', 'functions.R'))



# Server ----
server <- function(input, output, session) {
    
    # READ IN DATA -----
    dat <- reactive({
        req(input$file)
        tmp <- get_data(input$file$datapath, 
                        cover_only = FALSE,
                        keep_all_cols = TRUE)
        tmp <- tmp %>%
            mutate(across(c(Year, Month, Day), as.integer))
        
        tmp
    })
    output$preview <- DT::renderDataTable(dat()[1:10, ])
    
    mdat <- reactive({
        req(input$file)
        tmp <- get_species_info(input$file$datapath)
        tmp
    })
    
    stn_tbl <- reactive({
        req(input$file)
        tmp <- get_stn_table(input$file$datapath)
        tmp
    })
    
    # UI choice updates ----
    # Observe when the data frame changes and update the selectInput choices
    observe({
        req(dat(), mdat())  
        dat_yrs <- sort(unique(dat()$Year), decreasing = TRUE)  # Extract unique categories
        updateSelectInput(session, "yrchoice", choices = dat_yrs)
        
        dat_spps <- names(dat())[(which(names(dat()) %in% mdat()$Species))]
        default_exclusions <- mdat()$Species[mdat()$Cover_Categories == "Other layer"]
        default_unvegetateds <- mdat()$Species[mdat()$Cover_Categories == "Unvegetated category"]
        
        updateSelectInput(session, "excludespps", choices = dat_spps, selected = default_exclusions)
        updateSelectInput(session, "unvegspps", choices = dat_spps, selected = default_unvegetateds)
        
    })
    
    # GENERATE PREVIEW FOR SUMMARY ----
    dat_summ <- reactive({
        out <- list("Sites" = unique(dat()$SiteID),
                    "First Few Species" = names(dat())[(which(names(dat()) == "Total") + 1):20],
                    "Years Sampled" = unique(dat()$Year))
        out
    })
    output$summary <- renderPrint(dat_summ())
    
    
    # SAMPLING SUMMARY ----
    dat_sampling_summary <- reactive({
        req(dat())
        dat() %>% 
            group_by(SiteID, Year) %>% 
            tally() %>% 
            arrange(Year, SiteID) %>% 
            rename(Site = SiteID) %>% 
            pivot_wider(names_from = Year,
                        values_from = n,
                        values_fill = 0) %>% 
            arrange(Site) 
    })
    output$sampling_summary <- DT::renderDataTable(dat_sampling_summary())
    
    
    
    # MISSING DATA ----
    unsampds <- reactive({
        req(dat())
        
        tmp <- find_unsampleds(dat())
        
        if (nrow(tmp) == 0) {
            return(NULL)  # No missing data, return NULL
        } else {
            return(tmp)  # Return the data frame with missing data
        }
    })
    # render output, conditionally
    output$missing_samples_table <- DT::renderDataTable({
        req(unsampds())  # Ensure there is data to display
        unsampds()
    })
    output$missing_samples <- renderUI({
        if (is.null(unsampds())) {
            h5("No missing data")
        } else {
            DT::dataTableOutput("missing_samples_table")
        }
    })
    
    
    
    # PIVOT DATA ----
    dat_cdmo_measurements <- eventReactive(input$choices_made, {
        tmp <- dat()
        
        # deal with year choices
        if(!is.null(input$yrchoice)){
            tmp <- tmp %>% 
                dplyr::filter(Year %in% input$yrchoice)
        }
        
        # pivot to longer; add QAQC column if it's not there 
        # (in case it is present and used for species named for unvegetated)
        tmp_long <- pivot_to_cdmo(tmp)
        if(!("QAQC" %in% names(tmp_long))) tmp_long$QAQC <- NA
        
        
        # deal with choices of species to remove from CDMO reporting
        if(!is.null(input$excludespps)){
            tmp_long <- tmp_long %>% 
                dplyr::filter(!(Species %in% input$excludespps))
        }
        
        # deal with choices of species to lump into "Unvegetated"
        if(!is.null(input$unvegspps)){
            # if only one species, all is well
            if(length(input$unvegspps) == 1){
                tmp_long <- tmp_long %>% 
                    dplyr::mutate(Species = case_when(Species == input$unvegspps ~ "Unvegetated",
                                                      .default = Species))
            } else {
                # if multiple, need to add together Cover estimates
                # pull them out and do them separately - 
                # this will end up inserting NAs into the table for other params like
                # height and density, which should be NA
                unveg <- tmp_long %>% 
                    dplyr::filter(Species %in% input$unvegspps) %>% 
                    dplyr::mutate(Species = "Unvegetated") %>% 
                    dplyr::summarize(.by = c(Reserve:Species),
                                     Cover = sum(Cover, na.rm = TRUE),
                                     QAQC = paste(unique(QAQC), collapse = "; ")) %>% 
                    dplyr::mutate(QAQC = case_when(QAQC == "NA" ~ NA_character_,
                                                   .default = QAQC))
                
                # filter out unvegetateds
                # then bind the unveg df from above, and put in order
                tmp_long <- tmp_long %>% 
                    dplyr::filter(!(Species %in% input$unvegspps)) %>% 
                    bind_rows(unveg) 
            }
            
        }
        
        tmp_long <- tmp_long %>% 
            dplyr::mutate(Date2 = lubridate::mdy(Date)) %>% 
            dplyr::arrange(Date2, SiteID, TransectID, PlotID, Species) %>% 
            dplyr::select(-Date2)
        
        return(tmp_long)
        
    })
    output$long <- DT::renderDataTable(dat_cdmo_measurements())

        
    # PLOT TOTALS ----
    coverTotals <- reactive({
        dat_cdmo_measurements() %>% 
            summarize(.by = c(Date, SiteID, TransectID, PlotID),
                      total = sum(Cover, na.rm = TRUE))
    })
    
    output$coverSums <- DT::renderDataTable(coverTotals())
    
    
    # COMPLETE CDMO COLUMNS ----
    dat_cdmo_complete <- eventReactive(input$col.choices_made, {
        tmp <- dat_cdmo_measurements()
        stns <- stn_tbl()
        raw <- dat()
        
        
        # lat longs are only in station table
        # no choice to be made here
        latlongs <- stns  %>% 
            dplyr::select(SiteID, TransectID, PlotID, 
                   Latitude, Longitude) %>% 
            dplyr::distinct() %>% 
            dplyr::mutate(Latitude = round(Latitude, 5),
                          Longitude = round(Longitude, 5))
        
        tmp <- left_join(tmp, latlongs,
                         by = c("SiteID", "TransectID", "PlotID")) %>% 
            dplyr::rename(Lat = Latitude,
                   Long = Longitude)
        
        # distance
        # pull columns from the correct data frame
        if(input$distance.location == "Station Table"){
            distances <- stns %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                       Distance_to_Water, Distance_Along_Transect) %>% 
                dplyr::distinct()
        } else {
            # pull from the data
            # keep dates in case there were multiple measurements in a year
            distances <- raw %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                       Year, Month, Day,
                       Distance_to_Water, Distance_Along_Transect) %>% 
                dplyr::mutate(Date = lubridate::ymd(paste(Year,
                                                   Month,
                                                   Day)),
                       Date = format(Date, "%m/%d/%Y")) %>% 
                dplyr::select(-Year, -Month, -Day) %>% 
                dplyr::distinct()
        }
        
        # pull out the correct column of distances
        distances <- distances %>% 
            dplyr::mutate(Distance = case_when(input$distance.type == "distance to water" ~ Distance_to_Water,
                                               input$distance.type == "distance along transect" ~ Distance_Along_Transect,
                                               .default = NA_real_)) %>% 
            dplyr::select(-Distance_to_Water, -Distance_Along_Transect)
        # add to data frame
        tmp <- left_join(tmp, distances)
        
        
        # Ortho ht
        # pull columns from the correct data frame
        if(input$ortho.ht == "Station Table"){
            ortho.hts <- stns %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                              Orthometric_Height) %>% 
                dplyr::distinct()
        } else {
            # pull from the data
            # keep dates in case there were multiple measurements in a year
            ortho.hts <- raw %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                              Year, Month, Day,
                              Orthometric_Height) %>% 
                dplyr::mutate(Date = lubridate::ymd(paste(Year,
                                                          Month,
                                                          Day)),
                              Date = format(Date, "%m/%d/%Y")) %>% 
                dplyr::select(-Year, -Month, -Day) %>% 
                dplyr::distinct()
        }
        # add to data frame
        tmp <- left_join(tmp, ortho.hts) %>% 
            dplyr::rename(`Orthometric Height` = Orthometric_Height)
        
        # MLLW ht
        # pull columns from the correct data frame
        if(input$mllw.ht == "Station Table"){
            mllw.hts <- stns %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                              Height_Relative_to_MLLW) %>% 
                dplyr::distinct()
        } else {
            # pull from the data
            # keep dates in case there were multiple measurements in a year
            mllw.hts <- raw %>% 
                dplyr::select(SiteID, TransectID, PlotID,
                              Year, Month, Day,
                              Height_Relative_to_MLLW) %>% 
                dplyr::mutate(Date = lubridate::ymd(paste(Year,
                                                          Month,
                                                          Day)),
                              Date = format(Date, "%m/%d/%Y")) %>% 
                dplyr::select(-Year, -Month, -Day) %>% 
                dplyr::distinct()
        }
        # add to data frame
        tmp <- left_join(tmp, mllw.hts) %>% 
            dplyr::rename("Height Relative to MLLW" = Height_Relative_to_MLLW)
        
        # get other things from the station table
        stn_remainings <- stns %>% 
            dplyr::select(SiteID, TransectID, PlotID, Type, `SSAM-1`) %>% 
            dplyr::summarize(.by = c(SiteID, TransectID, PlotID),
                             Type = paste(unique(Type), collapse = ", "),
                             `SSAM-1` = paste(unique(`SSAM-1`), collapse = ", ")) %>% 
            dplyr::mutate(Type = case_when(Type == "NA" ~ NA_character_,
                                           .default = Type),
                          `SSAM-1` = case_when(`SSAM-1` == "NA" ~ NA_character_,
                                               .default = `SSAM-1`))
        
        tmp <- left_join(tmp, stn_remainings)
        
        # add empty columns - someday may build in functionality to deal with these
        tmp <- tmp %>% 
            dplyr::mutate(Subplot = NA,
                          Rep = NA,
                          Diameter = NA,
                          Height = NA)
        
        # deal with Height columns - only one will be present;
        # insert the other one
        if(!("Maximum Canopy Height") %in% names(tmp)){
            tmp$`Maximum Canopy Height` <- NA
        }
        if(!("Average Canopy Height") %in% names(tmp)){
            tmp$`Average Canopy Height` <- NA
        }
        
        # deal with Density, which may not be present if there were 
        # no density measurements in a year
        if(!("Density") %in% names(tmp)){
            tmp$Density <- NA
        }
        
        # put it all in order
        tmp <- tmp %>% 
            select(Reserve, Type, Date, 
                   SiteID, TransectID, PlotID,
                   Subplot, Rep, `SSAM-1`,
                   Lat, Long, Distance,
                   `Orthometric Height`, `Height Relative to MLLW`,
                   Species, Cover, Density,
                   `Maximum Canopy Height`, `Average Canopy Height`,
                   Diameter, Height,
                   QAQC)
        
        return(tmp)
        
    })
    output$cdmo.allCols <- DT::renderDataTable(dat_cdmo_complete())
    
    
    # DOWNLOAD DATA ---- 
    output$download_cdmo <- downloadHandler(
        filename = function() {
            paste0("download ", as.character(Sys.time()), ".xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(dat_cdmo_complete(),
                                file)
        }
    )
}