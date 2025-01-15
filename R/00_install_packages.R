# Script to install packages using 'renv'

#### INSTRUCTIONS #############################################################

# 1:
# Ensure your RStudio window is using the Namaste-Reserve-Final.Rproj
# by looking in the upper right corner of RStudio for a blue box
# with 'R' inside. It should say 'Namaste-Reserve-Final' next to it.
# If not, close this RStudio instance, go to the main Namaste directory,
# and double click on the 'Namaste-Reserve-Final.Rproj' file.

# 2: 
# Run the lines below one-by-one, by placing your cursor in the line
# and hitting the 'Run' button in the upper right corner of this pane
# or using the keyboard shortcut Ctrl/Cmd-Enter

###############################################################################

install.packages("renv")
renv::activate()
renv::restore()