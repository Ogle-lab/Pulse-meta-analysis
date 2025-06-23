### commonly-used functions to read-in


library(purrr)
# Create a "not in" function
`%nin%` <- negate(`%in%`)



# Organize jagsUI output as a dataframe
# Function to extract posterior means, and 2.5 and 97.5 CI quantiles from jags coda output.
# It takes a list of variable names and the coda summary.
# All variables in the list MUST have the same length posterior outputs.
#
# param jagsobj jagsUI or rjags object
# param type rjags or jagsUI. Indicates what object type jagsobj is
# param col.names Optional. Takes a list of column names you want your ID columns labeled as. Default is ID1, ID2, etc.
# return A data frame of columns for posterior means, 2.5 %, and 97.5 CI quantiles for each variable
#
dumsum <- function(jagsobj, label, type, col.names = NULL){
  
  # Create a "not in" function using negate from the purrr package
  `%nin%` <- purrr::negate(`%in%`)
  
  if(type %nin% c("rjags", "jagsUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }
  
  
  if(type == "jagsUI"){
    
    if(is.null(jagsobj$samples)){
      print("This is not a full jagsUI object. If this is just the 'samples' from jagsUI, set 'type' to rjags.")
    }
    
    jagsui <- jagsobj
    jm_coda <- jagsui$samples # convert to coda form to work with postjags functions
    
    # Organize the coda object as a dataframe
    df_sum <- coda.fast(jm_coda)
    df_sum <- tibble::rownames_to_column(df_sum, "var")
    df_sum <- df_sum %>% # make index column
      mutate(ID = sub('.*\\[(.*)\\]', '\\1', df_sum$var))
    df_sum$ID <- ifelse(grepl('[[:alpha:]]', df_sum$ID), 1, df_sum$ID) # make ID=1 if there is only 1 instance
    
    # make a lists of list of indices
    IDlist <- strsplit(df_sum$ID, ",") #temp
    
    # get number of dims in ID
    counter <- 1
    for(i in 1:length(IDlist)){
      counter <- ifelse(length(IDlist[[i]])>counter, length(IDlist[[i]]), counter)
    }
    
    # create a character vector of column names based on max dim
    new_columns <- list()
    for(i in 1:counter){
      new_columns[[i]] <- paste("ID",i, sep="")
    }
    new_columns <- as.character(new_columns)
    
    # for each dimension, create a new column with the correct ID
    df_sum <- df_sum %>%
      tidyr::separate(ID,new_columns,sep=",")
    
    df_sum <- df_sum %>%
      mutate(param = sub('(.*)\\[.*', '\\1', df_sum$var)) # get rid of numbers in var col
    
    if(exists("c")){ # remove c if overwritten
      rm(c)
    }
    df_mod <- df_sum %>%
      select("param",starts_with("ID"),"mean","pc2.5","pc97.5") %>% #reorder columns, drop ID
      mutate(overlap0 = do.call(c, jagsui$overlap0), gel = do.call(c, jagsui$Rhat), group = label)
    
    df_mod[,2:(counter+1)] <- lapply(2:(counter+1), function(x) as.numeric(df_mod[[x]])) # make appropriate columns numeric
    
    if(!is.null(col.names)){
      colnames(df_mod)[2:(counter+1)] <- col.names
    }
    
    return(df_mod)
  }
  
}



# function that finds the index of variables to remove
get_remove_index <- function(to_keep, list, type){
  
  if(type %nin% c("rjags", "jagUI")){
    paste("Please indicate whether this is a rjags or jagsUI samples object")
  }
    
  if(type == "rjags"){
    list <- list[list != "deviance"] # remove deviance
    list <- sort(list, method = "radix")
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }
  
  if(type == "jagsUI"){
    list <- list[list != "deviance"] # remove deviance
    out_list <- c()
    for(j in c(1:length(list))){
      if(list[j] %in% to_keep){
        out_list[j] = NA
      } else{
        out_list[j] = j
      }
    }
    out_list <- out_list[!is.na(out_list)]
    out_list
  }

}

# function that finds the index of each parameter in a jagsUI coda output
# will put into a single list for convenient dataframe creation
get_index <- function(jagsUI_object){
  list_out <- list()
  for(i in c(1:length(jagsUI_object$mean))){
    l <- c(1:length(jagsUI_object$mean[[i]]))
    list_out[[i]] <- l
  }
  out <- do.call(c, list_out)
  out
}

# function that will count the # of falses
count_f <- function(obj1,obj2,obj3,obj4){
  n = 0
  temp_list <- c(obj1,obj2,obj3,obj4)
  for(i in 1:4){
    if(temp_list[i] == F){
      n = n + 1
    }else{
      n = n + 0
    }
  }
  return(n)
}

###############################################################################
# Written for work on the Multicomp project
# Updated by Michael Fell 9/10/2018
#   Added an option for an arbitrary function
#   Added more informative error messages
###############################################################################
# A function to summarize output from a JAGS or OpenBUGS model.
coda.fast <- function(coda=NULL, thin=1, FUN=NULL, colname = "optfun", ...){
  
  if(is.null(coda)){
    message("No coda object provided. Summarizing nothing is too philosophical")
    message("a task for this function.")
    stop()
  }
  
  # Get the number of chains
  chains <- length(coda)
  
  codal <- length(coda[[1]][,1])
  
  # Combine chains
  Ftab <- numeric()
  for(i in 1:chains){
    Ftab <- rbind(Ftab, coda[[i]][(0:(codal-1))%%thin==0,])
  }
  
  # mean, sd, 95% CrI table
  pred <- matrix(nrow=dim(Ftab)[2], ncol=5)
  colnames(pred)<-c("mean", "median", "sd","pc2.5","pc97.5")
  
  # Fill table with stats
  pred[,1] <- colMeans(Ftab) #calculate predicted mean RW's
  pred[,2] <- apply(X=Ftab, MARGIN=2, FUN=median, na.rm=TRUE)
  pred[,3] <- apply(X=Ftab, MARGIN=2, FUN=sd,na.rm=TRUE) #calculate sd, apply column wise
  pred[,4] <- apply(X=Ftab, MARGIN=2, FUN=quantile,probs=c(0.025),na.rm=TRUE) 
  pred[,5] <- apply(X=Ftab, MARGIN=2, FUN=quantile,probs=c(0.975),na.rm=TRUE)
  
  pred <- data.frame(pred)
  if(length(rownames(pred)) == length(colnames(coda[[1]]))){
    rownames(pred) <- colnames(coda[[1]])
  }else{
    message("Could not determine row (variable) names from coda.")
  }
  
  # Optional Function
  if(!is.null(FUN))
  {
    placeholder <- tryCatch(
      {
        out <- apply(X=Ftab, MARGIN=2, FUN=FUN, na.rm=TRUE, ...)
        out <- as.matrix(out)
        if(ncol(out) == nrow(pred)){
          out <- t(out)
        }
        
        pred <- cbind(pred, out)
        colnames(pred) <- c("mean", "median", "sd","pc2.5","pc97.5", colname)
      },
      error=function(cond){
        message(paste0("A problem led to an error executing the optional function."))
        message("The result without the added function will be returned.")
        message("Here is the original error:")
        message(cond)
      },
      warning=function(cond){
        message("A warning occurred executing the optional function.")
        message("The result without the added function will be returned.")
        message("Here is the original warning:")
        message(cond)
      },
      finally={
        return(pred)
      }
    )
  }
  
  # Return the summary values
  return(pred)
}

# A function to find initial values for a JAGS or OpenBUGS model.
# Output:
# The output from this function is a list containing two elements. The first
# contains the names of the variables and their indicies. These are useful 
# when using removevars to remove variables that don't need initial values
# in JAGS. The second element contains a list of initial values (this is a 
# list of lists).


initfind <- function(coda, iteration=0, OpenBUGS=FALSE){
  mcmcin <- coda # TODO change all mcmcin to coda in the future MKF 11/27/18
  # If mcmc.list convert to mcmc
  if(is.mcmc.list(mcmcin)==TRUE){
    mcmcin <- mcmc(data=mcmcin, thin=1)
  }
  
  # Get the number of chains
  n.chains <- length(mcmcin)
  
  # get variable names from a list
  var.names <- colnames(mcmcin[[1]])
  var.dims <- dim(mcmcin[[1]])
  if(iteration==0){
    iteration <- var.dims[1]
  }
  
  if(sum(var.names=="deviance")>0){
    var.names <- var.names[-which(var.names=="deviance")]
    var.dims[2] <- var.dims[2]-1 # correct for removing deviance
  }
  
  # Get names and dimension of each variable since the output is a table
  var.names2 <- apply(X=as.matrix(var.names), MARGIN=c(1), FUN=strsplit, split="\\x5B", perl=TRUE)
  var.names2 <- lapply(X=var.names2, FUN=unlist)
  var.names2 <- lapply(X=var.names2, FUN=gsub, pattern="\\x5D", replacement="", perl=TRUE)
  
  # Create a table of names and dimensions
  # Column 1 is the variable me column 2 has the dimensions
  var.info <- matrix(nrow=length(var.names), ncol=3)
  for(i in 1:length(var.names2)){
    if(length(var.names2[[i]]) > 1){
      var.info[i,] <- c(var.names2[[i]], var.names[i])
    }else if(length(var.names2[[i]]) == 1){
      var.info[i,] <- c(var.names2[[i]], 1, var.names[i])
      #print(i)
      #print(var.names2[[i]])
    }else{
      stop("A variable name has incorrect dimensions for parsing.") 
    }
  }
  
  # Get variable names
  unique.names <- unique(var.info[,1])
  initsoutall <- list()
  
  
  for(k in 1:n.chains){
    initsout <- list()
    for(i in 1:length(unique.names)){
      sel <- which(var.info[,1]==unique.names[i])
      #sel2 <- grep(pattern=paste0("^",unique.names[i],"\\x5B"), x=var.names)
      
      # Make sure the above selections worked
      #if(length(sel) != length(sel2)){
      #  stop("Error matching unique variable names with MCMC output")  
      #}
      name.sel <- var.info[sel,3]
      index <- apply(X=as.matrix(var.info[sel,2]), MARGIN=1, FUN=strsplit, split=",", perl=TRUE)
      index <- lapply(X=index, FUN=unlist)
      index <- matrix(data=as.numeric(unlist(index)), nrow=length(index), ncol=length(index[[1]]), byrow=TRUE)
      
      # There are possibly easier ways to do this but they make more assumptions
      dims <- as.numeric(apply(X=index, MARGIN=2, FUN=max))
      variable <- array(data=NA, dim=dims)
      
      # Fill the new variable with the correct values
      for(j in 1:dim(index)[1]){
        # The output into mcmc objects lists names in the order R stacks them
        # in arrays so the single index for the variable references the 
        # correct array location.
        variable[j] <- mcmcin[[k]][iteration, which(colnames(mcmcin[[k]])==name.sel[j])]
      }
      
      # Use dims to produce a new array to store the data
      initsout[[i]] <- variable
    } # End of variable loop
    names(initsout) <- unique.names
    initsoutall[[k]] <- initsout
  } # End of chain loop
  
  listout <- list(unique.names, initsoutall)
  names(listout) <- c("variables", "initials")
  
  # Account for OpenBUGS by outputing 1 dimensional arrays as vectors.
  if(OpenBUGS==TRUE){
    for(i in 1:n.chains){
      for(j in 1:length(listout[[2]][[i]])){
        if(length(dim(listout[[2]][[i]][[j]]))==1){
          listout[[2]][[i]][[j]] <- as.vector(listout[[2]][[i]][[j]])
        }
      }
    }
  }
  
  return(listout)
} # End of function


###############################################################################
#
# Removes specific variables from the initial values
#
###############################################################################

# A function to remove variables that don't need initial values in JAGS.

removevars <- function(initsin, variables){
  n.chains <- length(initsin[[2]])
  n.vars <- 1:length(initsin[[1]])
  n.vars <- n.vars[-variables]
  
  var.names <- initsin[[1]][n.vars]
  
  new.inits <- list()
  for(i in 1:n.chains){
    chain.inits <- list()
    for(k in 1:length(n.vars)){
      chain.inits[[k]] <- initsin[[2]][[i]][[n.vars[k]]] 
    } # End of k loop
    names(chain.inits) <- var.names
    new.inits[[i]] <- chain.inits
  } # End of i loop
  
  output <- list(var.names, new.inits)
  names(output) <- c("variables", "initials")
  
  return(output)
  
} # End of function


