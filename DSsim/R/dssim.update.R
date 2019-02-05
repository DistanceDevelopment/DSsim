dssim.update <- function(object, warn = FALSE){
  #Used to update simulation objects created in a previous version of DSsim for backwards compatibility
  #Checks which slots the object should have
  slot.names <- slotNames(object)
  missing.slot.names <- character()
  #Sets a flag to record if any slots are missing (created by a previous version of DSsim)
  missing.slot = FALSE
  #Looks to see if any of the slots are missing
  for (i in seq(along = slot.names)){
    if(!(.hasSlot(object, slot.names[i]))){
      missing.slot = TRUE
      missing.slot.names = c(missing.slot.names, slot.names[i])
    }
  }
  #If slots are missing then update the object and display a warning to the user
  if(missing.slot){
    if(warn){
      warning(paste("This ", class(object), " was created in a previous version of DSsim and is missing the following: ", paste(missing.slot.names, collapse = ", "), ". DSsim will assume default values.", sep = ""), call. = FALSE)  
    }
    new.object <- new(Class = class(object)) 
    for(i in seq(along = slot.names)){
      #Copy across all the existing slots
      if(.hasSlot(object, slot.names[i])){
        if(isS4(slot(object, slot.names[i]))){
          #If the slot contains an S4 object check if it needs updating
          slot(new.object, slot.names[i]) <- dssim.update(slot(object, slot.names[i]), warn = warn)  
        }else{
          slot(new.object, slot.names[i]) <- slot(object, slot.names[i])  
        }
      }  
    }
    return(new.object)
  }else{
    return(object)  
  }
}