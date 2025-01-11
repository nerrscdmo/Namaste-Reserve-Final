# if your reserve has multiple input files, include them in the 'files' line
# e.g.:
# files <- c("APA-LSM", "APA-PC")

# select everything ( ctrl + a ) and run it ( ctrl + enter )
# then check our 'output' folder for the results!

files <- c("GRB")

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

# Report results
if (length(failed_files) > 0) {
    cat("\nThe following files failed to process:\n")
    print(failed_files)
} else {
    cat("\nAll files processed successfully!\n")
    
}

# time check
Sys.time() - strt