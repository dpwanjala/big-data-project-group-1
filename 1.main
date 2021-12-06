#location of work directory
wk.dir <- getwd()

#Folder list
folder.names <- c("a.data.raw","b.data.clean", "c.results","d.figures")

#Creating directories
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}


#path names to variables
p.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
p.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
p.results <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
p.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")
