check.LinkID.order <- function(shapefile){
  #Checks the order of the LinkID attribute
  #Returns
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