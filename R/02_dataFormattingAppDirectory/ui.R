library(shiny)

# UI ----  
ui <- fluidPage(
    
    h3("Namaste to CDMO file shapes"),
    p("Work your way through the tabs in order."),
    br(),
    # selectInput("file", "Which file would you like to use?", 
    #             choices = dir(here::here("data")),
    #             selected = NULL),
    
    fileInput("file", "Which file would you like to use?", 
              multiple = FALSE,
              accept = ".xlsx"),
    
    # Tabsets ----
    tabsetPanel(type = "tabs", id = "tabselected",
                
                tabPanel("1. Review data",
                         value = "1",
                         br(),
                         "Some key features of this file are:",
                         br(),
                         verbatimTextOutput("summary"),
                         br(),
                         h4("Sampling Summary: "),
                         br(),
                         "# samples by site/year",
                         DT::dataTableOutput("sampling_summary"),
                         br(),
                         h4("Missing samples: "),
                         br(),
                         uiOutput("missing_samples"),
                         br(),
                         h4("Preview of raw data file: "),
                         br(),
                         DT::dataTableOutput("preview"),
                         br()
                ),
                
                # CDMO layout tab ----
                tabPanel("2. Shape to CDMO layout",
                         value = "2",
                         
                         sidebarLayout(
                             sidebarPanel(
                                 "Choose options for generating CDMO-formatted file, 
            \nand click 'Reshape' button below when completed",
                                 br(),
                                 br(),
                                 selectInput("yrchoice", 
                                             label = "What year(s) to work with?",
                                             choices = NULL,
                                             multiple = TRUE),
                                 br(),
                                 selectInput("excludespps",
                                             label = HTML("What species (if any) to exclude from CDMO reporting?<br><span style='font-weight: normal;'>defaults to species labeled 'Other layer' in Species_Names</span>"),
                                             choices = NULL,
                                             multiple = TRUE),
                                 br(),
                                 selectInput("unvegspps", 
                                             label = HTML("What species should be included in 'Unvegetated'?<br><span style='font-weight: normal;'>defaults to species labeled 'Unvegetated category' in Species_Names</span>"),
                                             choices = NULL,
                                             multiple = TRUE),
                                 br(),
                                 actionButton("choices_made", "Reshape!"),
                                 br()
                                 ),
                             
                             mainPanel(
                                 h4("Preview of CDMO-shaped data"),
                                 DT::dataTableOutput("long")
                             )
                         )
                ),
                
                # verify plots total 100% cover ----
                tabPanel("3. Verify plot totals",
                         value = "3",
                         h4("Verify that plot covers add up to 100%"),
                         br(),
                         p("This table is sortable. We recommend sorting the 'total' column by clicking on the up and down arrows next to it. Then click them again. This will sort from lowest to highest, then highest to lowest. If you see any values that are far away from 100, make note of the date, site, transect, and plot, and investigate."),
                         br(),
                         DT::dataTableOutput("coverSums")
                ),
                
                # add add'l columns describing plots ----
                tabPanel("4. Add descriptive columns",
                         value = "3b",
                         
                         sidebarLayout(
                             sidebarPanel(
                                 h4("Options to generate complete set of columns"),
                                 p("Verify or change selections, then click 'Submit' button at bottom to see the output."),
                                 br(),
                                 
                                 radioButtons("distance.type", 
                                              label = "What does distance represent?",
                                              choices = c("distance to water", "distance along transect")),
                                 br(),
                                 
                                 h4("Choose workbook sheet from which to pull the following:"),
                                 p("'Station Table' recommended for parameters that are not measured repeatedly; 'Cover Sheet' recommended for values that may change between years."),
                                 br(),
                                 
                                 radioButtons("distance.location", 
                                              label = "Distance",
                                              choices = c("Station Table", "Cover Sheet")),
                                 br(),
                                 
                                 
                                 radioButtons("ortho.ht", 
                                              label = "Orthometric Height",
                                              choices = c("Station Table", "Cover Sheet"),
                                              selected = "Cover Sheet"),
                                 br(),
                                 
                                 radioButtons("mllw.ht", 
                                              label = "Height Relative to MLLW",
                                              choices = c("Station Table", "Cover Sheet"),
                                              selected = "Cover Sheet"),
                                 br(),
                                 # action button to submit choices and generate preview
                                 actionButton("col.choices_made", "Use these choices")
                             ),
                             mainPanel(
                                 
                                 # show the preview
                                 DT::dataTableOutput("cdmo.allCols")
                             )
                         )
                ),
                
                
                # download CDMO format file tab ----
                tabPanel("5. Download CDMO layout",
                         value = "4",
                         h4("Click the button below to download"),
                         downloadButton("download_cdmo", 
                                        label = "Download")
                )
    )
    
)

