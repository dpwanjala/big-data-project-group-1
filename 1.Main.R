### Here we gotta add a bunch of details about what our code does 
### I am not the one to do this part well lol - severin 


#location of work directory
wk.dir <- getwd()

#Folder list
folder.names <- c("a.data.raw","b.data.clean", "c.output","d.figures")

#Creating directories
for(i in 1:length(folder.names)){ 
  if(file.exists(folder.names[i]) == FALSE){
    dir.create(folder.names[i])
  } 
}


#path names to variables
p.data.raw <- paste(wk.dir, "/", folder.names[1], "/", sep = "")
p.data.clean <- paste(wk.dir, "/", folder.names[2], "/", sep = "")
p.output <- paste(wk.dir, "/", folder.names[3], "/", sep = "")
p.fig <- paste(wk.dir, "/", folder.names[4], "/", sep = "")
