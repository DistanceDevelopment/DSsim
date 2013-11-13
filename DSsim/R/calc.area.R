calc.area <- function(coords, gaps){
  list.area <- function(coords.list){
    matrix.coords <- lapply(coords.list, as.matrix)
    areas <- lapply(matrix.coords, areapl)
    areas <- unlist(areas)
    total.area <- sum(areas) 
    return(total.area)
  }
  
  gross.area <- unlist(lapply(coords, list.area))
  gap.area <- unlist(lapply(gaps, list.area))
  
#  temp.coords <- lapply(coords, as.matrix)
#  gross.area <- lapply(temp.coords, areapl)
#  gross.area <- as.numeric(gross.area)
#  gross.area <- sum(gross.area)
#  temp.gaps <- lapply(gaps, as.matrix)
#  gap.area <- lapply(temp.gaps, areapl)
#  gap.area <- as.numeric(gap.area)
#  gap.area <- sum(gap.area)
  
  net.area <- gross.area - gap.area
  return(net.area)
}