# === Big Data Project =========================================================
# authors: David Wanjala, Nora Aydinyan, Severin Santana, Umayeer Milky

# This code analysis data on covid cases from the USA, Canada and Germany
# along with data from stock indecis from those respecive countries 

# R version
R.version.string
# "R version 4.1.2 (2021-11-01)" 
 
# NOTE: run the 1.Main.R before starting your session.
# === notes ====================================================================

# • numbering of files and folder is done to group based on same file type 
#   (folders, scripts and markdown files together in same sequence)
# • The code must be run sequentially one time through, however after that 
# • only 1.main is required to run before 3.Data.analysis or 4.Figures.

# === script index =============================================================

# 1.Main.R        
# 2.Data.management
# 3.Data.analysis.R
# 4.Figures.R

# === global variables =========================================================

#location of work directory
wk.dir <- getwd()

# === folder management ========================================================

#Folder list
folder.names <- c("a.data.raw","b.data.clean", "c.output","d.figures")

#Creating directories
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}

# ******************************************************************************

#path names to variables
p.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
p.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
p.output <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
p.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")

#___ end _______________________________________________________________________

