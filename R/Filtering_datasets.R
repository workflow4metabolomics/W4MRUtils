#' @title Filtering tools for Galaxy scripting
#'
#' @name Filtering_datasets
#'
#' @description Filtering tools to reduce a dataset,
#' based on W4M 3-tables format
#' Coded by: M.Petera,
#'
#' R functions to use in R scripts and wrappers
#' to make things easier
#'
#' V0: script structure + first function
#'
NULL


#' @title Filtering a data set following the 3-tables format
#'
#' @description generic_filter
#' This function allows to filter variables and samples according to factors or numerical values.
#' It needs 3 datasets: the data matrix, the variables' metadata, the samples' metadata.
#' It generates 3 new datasets corresponding to the 3 inputs filtered. 
#'
#' @param var.data Data.frame corresponding to the dataMatrix of your 3-tables data set
#' @param meta.samp.data Data.frame corresponding to the sampleMetadata of your 3-tables data set
#' @param meta.var.data Data.frame corresponding to the variableMetadata of your 3-tables data set
#' @param NUM Logical; indicates whether some filtering based on numeric metadata columns should be performed
#' @param ls.num List of character vectors; each vector corresponds to a numerical filter to perform; see Details for vector content
#' @param FACT Logical; indicates whether some filtering based on qualitative metadata columns should be performed
#' @param ls.fact List of character vectors; each vector corresponds to a qualitative filter to perform; see Details for vector content
#'
#' @details
#' The vectors inside \code{ls.num} and \code{ls.fact} must follow a dedicated format. 
#' For \code{ls.fact}, each vector must be constituted of exactly 3 character strings in the following order: 
#' (i) the name of a column to filter in one of the metadata table,
#' (ii) the modality inside this column to get rid off,
#' and (iii) "sample" or "variable" depending on the type of metadata you are filtering (i.e. the table that contains the column to filter).
#' For \code{ls.num}, each vector must be constituted of 4 to 5 character strings in the following order: 
#' (i) "sample" or "variable" depending on the type of metadata you are filtering (i.e. the table that contains the column to filter),
#' (ii) the name of the column to filter in the corresponding metadata table,
#' (iii) the type of numerical filtering to apply, to be chosen from "lower", "upper", "extremity" and "between",
#' (iv) the threshold to be used for filtering [in case the 'iii' argument is "extremity" or "between", it is the lower threshold],
#' and (v) in case the 'iii' argument is "extremity" or "between", the upper threshold. 
#'
#' @return a \code{list} containing the 3 tables, filtered according to the criteria input
#'
#' @author M.Petera
#'
#' @export
generic_filter <- function(var.data,
                           meta.samp.data,
                           meta.var.data,
                           NUM = FALSE,
                           ls.num = NULL,
                           FACT = FALSE,
                           ls.fact = NULL) {

  # Checking tables match regarding identifiers
  err.stock <- "\n"
  table.check <- W4MRUtils::match3(var.data, meta.samp.data, meta.var.data)
  check_err(table.check)

  # StockID
  samp.id <- W4MRUtils::stock_id(var.data, meta.samp.data, "sample")
  var.data <- samp.id$dataMatrix
  meta.samp.data <- samp.id$Metadata
  samp.id <- samp.id$id.match
  var.id <- W4MRUtils::stock_id(var.data, meta.var.data, "variable")
  var.data <- var.id$dataMatrix
  meta.var.data <- var.id$Metadata
  var.id <- var.id$id.match

  # Function 1: Filter according to numerical variables -------------------------------------
  # Allows to delete all elements corresponding to defined values of designated variables.
  if(NUM){
    
    # For each numerical variable to filter
    for(i in 1:length(ls.num)){
    
    # Which metadata table is concerned
    if(ls.num[[i]][1] == "sample"){metadata <- meta.samp.data}else{metadata <- meta.var.data}
    
    # Checking the columns and factors variables
    numcol <- which(colnames(metadata) == ls.num[[i]][2])
    if(length(numcol) == 0) {
      err.stock <- c(err.stock,"\n-------",
             "\nWarning: no '",ls.num[[i]][2],"' column detected in ",ls.num[[i]][1],
             " metadata!","\nFiltering impossible for this variable.\n-------\n") 
    }else{
      if(!is.numeric(metadata[,numcol])){
      err.stock <- c(err.stock,"\n-------",
               "\nWarning: column '",ls.num[[i]][2],"' in ",ls.num[[i]][1],
               " metadata is not a numerical variable!",
               "\nNumerical filtering impossible for this variable.\n-------\n")
      }else{
      
      # Filtering
      if(ls.num[[i]][3] == "lower"){
        toremove <- which(metadata[,numcol]<as.numeric(ls.num[[i]][4]))
        if(length(toremove)!=0){
        metadata <- metadata[-c(toremove),]
        }
      }else{if(ls.num[[i]][3] == "upper"){
        toremove <- which(metadata[,numcol]>as.numeric(ls.num[[i]][4]))
        if(length(toremove)!=0){
        metadata <- metadata[-c(toremove),]
        }
      }else{if(ls.num[[i]][3] == "between"){
        toremove <- (metadata[,numcol]>as.numeric(ls.num[[i]][4]))+(metadata[,numcol]<as.numeric(ls.num[[i]][5]))
        toremove <- which(toremove == 2)
        if(length(toremove)!=0){
        metadata <- metadata[-c(toremove),]
        }
      }else{if(ls.num[[i]][3] == "extremity"){
        toremove <- c(which(metadata[,numcol]<as.numeric(ls.num[[i]][4])),
              which(metadata[,numcol]>as.numeric(ls.num[[i]][5])))
        if(length(toremove)!=0){
        metadata <- metadata[-c(toremove),]
        }
      }}}}
      
      # Extension to the tables
      if(ls.num[[i]][1] == "sample"){
        meta.samp.data <- metadata
        var.data <- var.data[,c(1,which(colnames(var.data)%in%meta.samp.data[,1]))]
      }else{
        meta.var.data <- metadata
        var.data <- var.data[which(var.data[,1]%in%meta.var.data[,1]),]
      }
      
      }}}
    
  } # end if(NUM)

  # Function 2: Filter according to factors -------------------------------------------------
  # Allows to delete all elements corresponding to selected value of designated factor.
  if(FACT){

    # For each factor to filter
    for(i in 1:length(ls.fact)){
    
    # Which metadata table is concerned
    if(ls.fact[[i]][3]=="sample"){metadata <- meta.samp.data}else{metadata <- meta.var.data}
    
    # Checking the columns and factors variables
    numcol <- which(colnames(metadata) == ls.fact[[i]][1])
    if(length(numcol) == 0) {
    err.stock <- c(err.stock,"\n-------",
             "\nWarning: no '",ls.fact[[i]][1],"' column detected in ",ls.fact[[i]][3],
             " metadata!","\nFiltering impossible for this factor.\n-------\n") 
    }else{
    if((!(ls.fact[[i]][2]%in%levels(as.factor(metadata[,numcol]))))&((ls.fact[[i]][2]!="NA")|(length(which(is.na(metadata[,numcol])))==0))){
      err.stock <- c(err.stock,"\n-------",
             "\nWarning: no '",ls.fact[[i]][2],"' level detected in '",
             ls.fact[[i]][1],"' column (",ls.fact[[i]][3]," metadata)!\n",
             "Filtering impossible for this factor.\n-------\n")
    }else{
      
    # Filtering
    if(length(which(metadata[,numcol] == ls.fact[[i]][2]))!=0){ #if the level still exists in the data
      metadata <- metadata[-c(which(metadata[,numcol]==ls.fact[[i]][2])),]
    }else{ #to treat the special case of "NA" level
      if(ls.fact[[i]][2] == "NA"){metadata <- metadata[-c(which(is.na(metadata[,numcol]))),]}
    }
    
    # Extension to the tables
    if(ls.fact[[i]][3] == "sample"){
      meta.samp.data <- metadata
      var.data <- var.data[,c(1,which(colnames(var.data)%in%meta.samp.data[,1]))]
    }else{
      meta.var.data <- metadata
      var.data <- var.data[which(var.data[,1]%in%meta.var.data[,1]),]
    }

    }}}

  } # end if(FACT)
  
  # Check if at least one sample and one variable remain ------------------------------------

  if(nrow(meta.samp.data) == 0){
    stop("\n /!\\ Your filtering options lead to no more sample in your data matrix!\n",
       "Think about reducing your number of filter.")
  }

  if(nrow(meta.var.data) == 0){
    stop("\n /!\\ Your filtering options lead to no more variable in your data matrix!\n",
       "Think about reducing your number of filter.")
  }

  # Output ----------------------------------------------------------------------------------

  # Getting back original identifiers
  id.ori <- W4MRUtils::reproduce_id(var.data, meta.var.data, "variable", var.id)
  var.data <- id.ori$dataMatrix
  meta.var.data <- id.ori$Metadata
  id.ori <- W4MRUtils::reproduce_id(var.data, meta.samp.data, "sample", samp.id)
  var.data <- id.ori$dataMatrix
  meta.samp.data <- id.ori$Metadata

  # Error checking
  if(length(err.stock) > 1){
    stop(err.stock)
  }else{
    return(list(dataMatrix = var.data, sampleMetadata = meta.samp.data, variableMetadata = meta.var.data))
  }

}