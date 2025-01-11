# probably need more tests than what I'm starting with  
library(tidyverse)

# grouping ----

# want to start with this data frame:
test_df <- tribble(
    ~Cover_Categories, ~NMST_Groupings, ~Plant_Categories, ~Species, ~Cover,
    "Live vegetation", NA, "H-Halophyte", "Spartina alterniflora", 1,
    "Live vegetation", NA, "H-Halophyte", "Baccharis halimifolia", 1,
    "Live vegetation", NA, "H-Halophyte", "Salicornia maritima", 1,
    "Live vegetation", NA, "B-Brackish", "Juncus roemerianus", 1,
    "Live vegetation", NA, "B-Brackish", "Typha angustifolia", 1,
    "Live vegetation", NA, "F-Freshwater", "Acer rubrum", 1,
    "Live vegetation", NA, "F-Freshwater", "Iris versicolor", 1,
    "Live vegetation", NA, "A-Algae", "Ulva sp.", 1,
    "Unvegetated", "Dead", NA, "Deadstanding", 1,
    "Unvegetated", "Dead", NA, "Juncus roemerianus - Dead", 1,
    "Unvegetated", "Wrack", NA, "Wrack", 1,
    "Unvegetated", "Other Unvegetated", NA, "Mussels", 1,
    "Unvegetated", "Bare", NA, "Bare",  1
) 
# %>% 
#     mutate(Reserve = "TST",
#            SiteID = "Test",
#            TransectID = "1",
#            PlotID = "1",
#            SiteTrns = "Test-1",
#            StTrnsPlt = "Test-1-1",
#            Vegetation_Zone = "Zone 1",
#            Year = 2020,
#            Month = 7,
#            Day = 1,
#            Years_sinceStart = 5)   # all the grouping columns from make_spec_df function




# if somebody specifies "unvegetated" as a univariate response, would want this output:
# sum of all rows whose Cover_Categories is "Unvegetated"
specs1 <- c("Unvegetated")
out1 <- tribble(
    ~Species_or_Group, ~Included, ~Cover,
    "Unvegetated", "Bare, Deadstanding, Juncus roemerianus - Dead, Mussels, Wrack", 5
)


# what about Spalt, and Halophytes as univariate responses? Believe we told people we'd separate them out.
# So, Spalt as one, and Halophytes MINUS Spalt as another.  
specs2 <- c("Spartina alterniflora", "H-Halophyte")
out2 <- tribble(
    ~Species_or_Group, ~Included, ~Cover,
    "Spartina alterniflora", "Spartina alterniflora", 1,
    "H-Halophyte", "Baccharis halimifolia, Salicornia maritima, Spartina alterniflora", 3,
    "H-Halophyte other", "Baccharis halimifolia, Salicornia maritima", 2
) 



# if somebody specifies "H-Halophyte", "Juncus roemerianus", and "Iris versicolor" for multivariate, want this:
# sum of all rows where Plant_Categories is H-Halophyte, plus the individual rows of other species
specs3 <- c("H-Halophyte", "Juncus roemerianus", "Iris versicolor")
out3 <- tribble(
    ~Species_or_Group, ~Included, ~Cover,
    "H-Halophyte", "Baccharis halimifolia, Salicornia maritima, Spartina alterniflora", 3,
    "Juncus roemerianus", "Juncus roemerianus", 1,
    "Iris versicolor", "Iris versicolor", 1
)


# if somebody specifies "Spartina alterniflora", "Other Halophytes", "juncus roemerianus", and "Iris versicolor";
# Spartina alt. row, then sum of all rows where Plant_Categories is H-Halophyte MINUS Spalt; then individual species rows
specs4 <- c("Spartina alterniflora", "H-Halophyte", "Juncus roemerianus", "Iris versicolor")
out4 <- tribble(
    ~Species_or_Group, ~Included, ~Cover,
    "Spartina alterniflora", "Spartina alterniflora", 1,
    "H-Halophyte other", "Baccharis halimifolia, Salicornia maritima", 2,
    "Juncus roemerianus", "Juncus roemerianus", 1,
    "Iris versicolor", "Iris versicolor", 1
)


# all identifying columns involved:
# if somebody specifies "Unvegetated", "Spartina alterniflora", "Other Halophytes", "juncus roemerianus", and "Iris versicolor";
# sum of all rows whose Cover_Categories is "Unvegetated"; 
# Spartina alt. row, 
# then sum of all rows where Plant_Categories is H-Halophyte MINUS Spalt; 
# then individual species rows


# F_ columns ----
test_df <- data.frame(
    Reserve = "ABC",
    SiteID = "Site1",
    TransectID = "Transect1",
    PlotID = 1:5,
    Notes = NA,
    F_Record = NA,
    Total = 100,
    `Spartina alterniflora` = 80,
    `Juncus roemerianus` = 15,
    Unvegetated = 5,
    `F_Spartina alterniflora` = NA,
    `F_Juncus roemerianus` = NA,
    F_Unvegetated = NA
)
data = test_df
qaqc_cols_start <- which(str_starts(names(data), "F_"))[2]

min(which(names(data) != "F_Record" & str_starts(names(data), "F_")))

# or it just needs to come after 'Total'
position_Total <- which(names(data) == "Total") 
position_Fs <- which(str_starts(names(data), "F_"))
min(position_Fs[position_Fs > position_Total])
min(which(names(data) != "F_Record" & position_Fs > position_Total))
