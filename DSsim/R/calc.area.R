calc.area <- function(coords, gaps){
#Function to calculate the are of a study region
  list.area <- function(coords.list){
    matrix.coords <- lapply(coords.list, as.matrix)
    areas <- lapply(matrix.coords, splancs::areapl)
    areas <- unlist(areas)
    total.area <- sum(areas) 
    return(total.area)
  }
  gross.area <- unlist(lapply(coords, list.area))
  gap.area <- unlist(lapply(gaps, list.area))
  #Subtract the gaps
  net.area <- gross.area - gap.area
  return(net.area)
}