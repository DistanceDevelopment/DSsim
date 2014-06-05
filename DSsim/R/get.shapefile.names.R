get.shapefile.names <- function(path){
  #get all filenames in folder
  all.filenames <- list.files(path)
  #remove file extenstions
  all.filenames.list <- strsplit(all.filenames, split="[.]")
  all.filenames <- lapply(all.filenames.list, function(x){return(x[1])})
  all.filenames <- as.character(all.filenames)
  #find unique shapefiles 
  unique.shapefiles <- unique(all.filenames)
  #remove meta.txt file
  unique.shapefiles <- unique.shapefiles[unique.shapefiles != "Meta"]
  return(unique.shapefiles)
}