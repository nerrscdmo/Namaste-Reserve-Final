# If you are going to use the most updated versions of packages, and avoid
# using renv, this is the script for installation. Note that packages
# do change through time, and an updated version of a package may give different
# than expected results.

# If you are committed to doing this, first deactivate renv:
renv::deactivate()

# then install the packages below, and do NOT run the code in 00_installl_packages

# file paths, file and data tasks
install.packages("here")
install.packages("janitor")
install.packages("markdown")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readxl")
install.packages("writexl")
install.packages("stringr")
install.packages("forcats")
install.packages("lubridate")
install.packages("rlang")
install.packages("xfun")


# interactive apps
install.packages("shiny")
install.packages("leaflet")


# nice graphing
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("ggiraph")  # interactive graphs
install.packages("plotly")   # interactive graphs
install.packages("khroma")   # colorblind-friendly palettes
install.packages("scico")    # colorblind-friendly palettes
install.packages("scales")
install.packages("viridis") 
install.packages("patchwork") # putting plots and tables together nicely


# nice tables
install.packages("DT")
install.packages("kableExtra")
install.packages("gt")


# statistical modeling and graphing
install.packages("broom")
install.packages("emmeans")
install.packages("lme4")
install.packages("lmerTest")
install.packages("modelbased") 
install.packages("multcomp")
install.packages("multcompView")
install.packages("MuMIn")
install.packages("pbkrtest")
install.packages("see")
install.packages("vegan")
install.packages("car")
install.packages("metR")

