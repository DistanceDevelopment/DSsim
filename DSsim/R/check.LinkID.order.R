check.LinkID.order <- function(shapefile){
  # Checks the order of the LinkID attribute
  # Returns NULL LinkID doesn't exist, or if the order is correct
  # Returns the new order if there is a problem
  att.table <- shapefile$dbf$dbf
  if (is.null(att.table$LinkID)){
    #If there is no LinkID don't need to worry
    return(NULL)
  }else{
    LinkID <- att.table$LinkID
    new.LinkID.order <- order(LinkID)
    compare <- new.LinkID.order == (1:length(LinkID))
    #If any do not match
    if(any(!compare)){
      #Return the new order
      return(new.LinkID.order)
    }else{
      #If they are in the right order already don't need to do anything
      return(NULL)
    }
  }
}