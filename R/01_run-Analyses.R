# Script to generate html file of exploration and analysis of
# reserve-level vegetation data

#### INSTRUCTIONS #############################################################

# 1:
# Re-start your R session to make sure there's no interference:
# From the menu bar, select 'Session', then 'Restart R'
# Windows keyboard shortcut is Ctrl + Shift + F10

# 2: 
# Set up the file or files you want to work with
# If your reserve has multiple input files, include them both
# in line 18, in this format:
# files <- c("APA-LSM", "APA-PC")


files <- c("GRB")


# 3:
# Select this entire script. 
# Keyboard shortcut is Ctrl + a on windows or Cmd + a on Mac

# 4:
# Run it: either using the "Run" button in the upper right corner
# or the keyboard shortcut Ctrl/Cmd + Enter

###############################################################################


##############################################################################
# Do not change the code below
##############################################################################

library(rmarkdown)
library(here)
library(xfun)

# in case something doesn't work, log it:
failed_files <- c() 

# time it
strt<-Sys.time()

for(i in seq_along(files)){
    outname <- paste0(files[i], "_Veg-analyses_", Sys.Date(), ".html")
    
    # Try rendering the Rmd file
    result <- try({
        xfun::Rscript_call(
            rmarkdown::render,
            list(
                input = here::here("R", "99_Veg_analyses.Rmd"), 
                params = list("file_code" = files[i]),
                output_file = here::here("output", outname)
            )
        )
    }, silent = TRUE) 
    
    # Log it if it failed
    if (inherits(result, "try-error")) {
        cat("Error in processing file:", files[i], "\n")
        failed_files <- c(failed_files, files[i]) # Log failed files
    }
    
}


# time check
Sys.time() - strt

# Report results
if (length(failed_files) > 0) {
    cat("\nThe following files failed to process:\n")
    print(failed_files)
} else {
    cat("\nAll files processed successfully!\n")
    
}
