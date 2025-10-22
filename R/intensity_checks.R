#' @title Intensity checking tools for Galaxy scripting
#'
#' @name intensity_checks
#'
#' @description Intensity checking tools to compute indices per variable,
#' based on the W4M 3-tables format
#' Original code of first function by A.Fernandes, updates by M.Petera
#'
#' R functions to use in R scripts and wrappers
#' to make things easier
#'
#' V0: script structure + first function (intens_check)
#'
NULL


#' @title Checking intensities of a data set following the W4M 3-tables format
#'
#' @description intens_check
#' This function allows to check the intensities with various statistics, number of missing values and mean fold change.
#' It needs 3 datasets: the data matrix, the variables' metadata, the samples' metadata.
#' It generates 3 new datasets corresponding to the 3 inputs filtered. 
#'
#' @param DM Data.frame corresponding to the dataMatrix of your 3-tables data set
#' @param SM Data.frame corresponding to the sampleMetadata of your 3-tables data set
#' @param VM Data.frame corresponding to the variableMetadata of your 3-tables data set
#' @param method Character string among "global", "one_class", "each_class"
#' @param chosen.stat Character string listing the chosen analysis (comma-separated) from mean, sd, median, quartile, decile and NA 
#' @param class.col Character string of the name of the sampleMetadata's column with classes (if method = one_class or each_class)
#' @param test.fold Character string among "yes" or "no" to indicate whether mean fold changes should be computed (if method = one_class or each_class)
#' @param class1 Character string corresponding to the name of the chosen class (if method = one_class)
#' @param fold.frac Character string; "Top" for class1/other or "Bottom" for other/class1 (if method = one_class) 
#' @param logarithm Character string among "log2", "log10" or "none", use for mean fold changes (if method = one_class or each_class)
#' @param graphs.output Character string for the pdf file's access for ploting the graphics
#'
#' @details
#' Three methods proposed: 
#' (i) 'global' to perform tests for each variable without distinction between samples, 
#' (ii) 'one class' to compute test for one class versus all the remaining samples in an 'other' shared class, 
#' (iii) 'each class' in case the class columns contains at least two classes and you want to test each of them individually.
#' The function produces a table with the requested indices, plus a PDF output with corresponding graphics. 
#'
#' @return a \code{dataframe} corresponding to the original variableMetadata table completed with additional columns
#'
#' @author A.Fernandes, M.Petera
#'
#' @export
intens_check <- function(DM, SM, VM, method, chosen.stat, class.col, test.fold, class1, fold.frac, 
                         logarithm, graphs.output){
  
  # Transposing the dataMatrix
  rownames(DM) <- DM[,1]
  var_names <- DM[,1]
  DM <- DM[,-1]
  DM <- data.frame(t(DM))
  
  # Re-ordering the dataMatrix to match the sampleMetadata file order
  DM <- merge(x=cbind(1:nrow(SM),SM), y=DM, by.x=2, by.y=0)
  DM <- DM[order(DM[,2]),]
  rownames(DM) <- DM[,1]
  DM <- DM[,-c(1:(ncol(SM)+1))]
  
  
  stat.list <- strsplit(chosen.stat,",")[[1]]
  
  
  # check class.col, class1 and the number of classes ---------------------------------------------------------
  
  #set 1 class for all samples in case of method = no_class
  if(method=="no_class"){
    c_class <- rep("global", length=nrow(DM))
    classnames <- "global"
    nb_class=1
    test.fold <- "No"
  }


  if(method != "no_class"){
  
    if(!(class.col %in% colnames(SM))){
      stop("\n- - - - - - - - -\n", "The ",class.col, " column is not found in the specified sample metadata file.","\n- - - - - - - - -\n")
    }
  
    c_class <- SM[,class.col]
    c_class <- as.factor(c_class)
    nb_class <- nlevels(c_class)
    classnames <- levels(c_class)
  
    if((nb_class < 2)&&(test.fold=="Yes")){
      err.1class <- c("\n The ",class.col, " column contains only one class, fold calculation could not be executed. \n")
      cat(err.1class)  
    }
  
    if((nb_class > (nrow(SM))/3)&&(method == "each_class")){
      class.err <- c("\n There are too many classes, think about reducing the number of classes and excluding those
                    with few samples. \n")
      cat(class.err)
    }

  
    if(method == "one_class"){  
      if(!(class1 %in% classnames)){
        list.class1 <- c("\n Classes:",classnames,"\n")
        cat(list.class1)
        err.class1 <- c("The ",class1, " class does not appear in the ",class.col," column.")
        stop("\n- - - - - - - - -\n", err.class1,"\n- - - - - - - - -\n")
      }
  
      #If method is "one_class", change others classes in "other"
      for(i in 1:length(c_class)){
        if(c_class[i]!=class1){
          c_class <- as.character(c_class)
          c_class[i] <- "Other"
          c_class <- as.factor(c_class)
          nb_class <- nlevels(c_class)
          classnames <- c(class1,"Other")
        }
      }
    }

  }
    
    
  # Statistics ------------------------------------------------------------------------------------------------
  
  # Check whether the dataMatrix contains non-numeric values
  if(!(is.numeric(as.matrix(DM)))){
    # findchar definition
    findchar <- function(myval){
      if(is.na(myval)){
        return("ok")
      }else{
        mytest <- as.character(myval)
        if(is.na(as.numeric(mytest[1]))){
          return("char")
        }else{
          return("ok")
        }
      }
    }
    # findchar application
    chardiag <- suppressWarnings(apply(DM,2,vapply,findchar,"character"))
    charlist <- which(chardiag == "char")
    err.stock <- paste("\n- - - - - - - - -\nYour dataMatrix contains",
                       length(charlist),
                       "non-numeric value(s). To help you check your data, please find below a short overview:\n")
    charover <- 1
    while((length(err.stock)<10)&(length(err.stock)<(length(charlist)+1))){
      charex <- paste("variable",colnames(DM)[ceiling(charlist[charover]/nrow(DM))],
                      "- sample",rownames(DM)[charlist[charover]-floor(charlist[charover]/nrow(DM))*nrow(DM)],
                      "~> value:",as.matrix(DM)[charlist[charover]],"\n")
      err.stock <- c(err.stock,charex)
      charover <- charover + 1
    }
    stop(c(err.stock,"The dataMatrix file is supposed to contain only numeric values.\n- - - - - - - - -\n"))
  }
  
  ### Initialization
  
  DM <- cbind(c_class,DM)
  
  stat.res <- t(DM[0,-1,drop=FALSE])
  names <- NULL

  mean.res <- NULL
  mean.names <- NULL
  
  sd.res <- NULL
  sd.names <- NULL
  
  med.res <- NULL
  med.names <- NULL
  
  quart.res <- NULL
  quart.names <- NULL
  
  dec.res <- NULL
  dec.names <- NULL
  
  NA.res <- NULL
  NA.names <- NULL
  pct_NA.res <- NULL
  pct_NA.names <- NULL
  
  fold.res <- NULL
  fold.names <- NULL
  
  if(("NA" %in% stat.list)||(test.fold=="Yes")){
    graphs <- 1 
  }else{
    graphs=0
    }
  
    data_bp <- data.frame() #table for NA barplot


  
  ### Computation
  
  
  for(j in 1:nb_class){
  
    # Mean ---------

    if("mean" %in% stat.list){ 
      mean.res <- cbind(mean.res, colMeans(DM[which(DM$c_class==classnames[j]),-1],na.rm=TRUE))
      mean.names <- cbind(mean.names, paste("Mean",classnames[j], sep="_"))
      if(j == nb_class){
        stat.res <- cbind(stat.res, mean.res)
        names <- cbind(names, mean.names)
      }
    }
  
    # Standard deviation -----
    
    if("sd" %in% stat.list){ 
      sd.res <- cbind(sd.res, apply(DM[which(DM$c_class==classnames[j]),-1], 2, stats::sd, na.rm = TRUE))
      sd.names <- cbind(sd.names, paste("Sd", classnames[j], sep = "_"))
      if(j == nb_class){
        stat.res <- cbind(stat.res, sd.res)
        names <- cbind(names, sd.names)
      }
    }
    
    # Median ---------
    
    if(("median" %in% stat.list)&&(!("quartile" %in% stat.list))){
      med.res <- cbind(med.res, apply(DM[which(DM$c_class==classnames[j]),-1], 2, stats::median, na.rm = TRUE))
      med.names <- cbind(med.names, paste("Median", classnames[j], sep = "_"))
      if(j == nb_class){
        stat.res <- cbind(stat.res, med.res)
        names <- cbind(names, med.names)
      }
    }
    
    # Quartiles ------
    
    if("quartile" %in% stat.list){
      quart.res <- cbind(quart.res, t(apply(DM[which(DM$c_class==classnames[j]),-1], 2, stats::quantile, na.rm = TRUE)))
      quart.names <- cbind(quart.names, paste("Min",classnames[j], sep="_"),paste("Q1",classnames[j], sep="_"),
                                        paste("Median",classnames[j],sep="_"),paste("Q3",classnames[j],sep="_"),
                                         paste("Max",classnames[j],sep="_"))
      if(j == nb_class){
        stat.res <- cbind(stat.res, quart.res)
        names <- cbind(names, quart.names)
      }
    }
  
    # Deciles ------
    
    if("decile" %in% stat.list){
      dec.res <- cbind(dec.res, t(apply(DM[which(DM$c_class==classnames[j]),-1], 2, stats::quantile, na.rm = TRUE, seq(0,1,0.1))))
      dec.names <- cbind(dec.names, t(matrix(paste((paste("D",seq(0,10,1),sep="")),classnames[j],sep="_"))))
      if(j == nb_class){
        stat.res <- cbind(stat.res, dec.res)
        names <- cbind(names, dec.names)
      }
    }

    # Missing values ------------
  
    if("NA" %in% stat.list){
      
      nb_NA <- apply(DM[which(DM$c_class==classnames[j]),-1],2,function(x) sum(is.na(x)))
      pct_NA <- round(nb_NA/nrow(DM[which(DM$c_class==classnames[j]),-1])*100,digits=4)
      NA.res <- cbind(NA.res,nb_NA)
      pct_NA.res <- cbind(pct_NA.res,pct_NA)
      NA.names <- cbind(NA.names, paste("NA",classnames[j], sep="_"))
      pct_NA.names <- cbind(pct_NA.names,paste("Pct_NA", classnames[j], sep="_"))
      if(j == nb_class){
        stat.res <- cbind(stat.res, NA.res,pct_NA.res)
        names <- cbind(names, NA.names,pct_NA.names)
      }
        
      #for barplots
      Nb_NA_0_20 <- 0
      Nb_NA_20_40 <- 0
      Nb_NA_40_60 <- 0
      Nb_NA_60_80 <- 0
      Nb_NA_80_100 <- 0
         
      for (i in 1:length(pct_NA)){
      
        if ((0<=pct_NA[i])&(pct_NA[i]<20)){
          Nb_NA_0_20=Nb_NA_0_20+1}
           
        if ((20<=pct_NA[i])&(pct_NA[i]<40)){
          Nb_NA_20_40=Nb_NA_20_40+1}
           
        if ((40<=pct_NA[i])&(pct_NA[i]<60)){
          Nb_NA_40_60=Nb_NA_40_60+1}
           
        if ((60<=pct_NA[i])&(pct_NA[i]<80)){
          Nb_NA_60_80=Nb_NA_60_80+1}   
           
        if ((80<=pct_NA[i])&(pct_NA[i]<=100)){
          Nb_NA_80_100=Nb_NA_80_100+1}   
      }
      data_bp[1,j] <- Nb_NA_0_20
      data_bp[2,j] <- Nb_NA_20_40
      data_bp[3,j] <- Nb_NA_40_60
      data_bp[4,j] <- Nb_NA_60_80
      data_bp[5,j] <- Nb_NA_80_100
      rownames(data_bp) <- c("0%-20%", "20%-40%", "40%-60%", "60%-80%", "80%-100%")
      
      if(j == nb_class){
        
        # Alert message if there is no missing value in data matrix
        sum_total <- sum(NA.res)
        alerte <- NULL
        if(sum_total==0){
          alerte <- c(alerte, "Data Matrix contains no NA.\n")
        }
        if(length(alerte) != 0){
          cat(alerte,"\n")
        }
        
        
        colnames(data_bp) <- classnames
        data_bp <- as.matrix(data_bp)
      }
    }   

    
    # Mean fold change ------------
  
    if(test.fold=="Yes"){
      if(nb_class >= 2){
        if(j!=nb_class){
          ratio1 <- NULL
          ratio2 <- NULL
          if(method=="each_class"){
            fold.frac <- "Top"
          }
          for(k in (j+1):nb_class) {
            if(fold.frac=="Bottom"){
              ratio1 <- classnames[k]
              ratio2 <- classnames[j]
            }else{
              ratio1 <- classnames[j]
              ratio2 <- classnames[k]
            }
            fold <- colMeans(DM[which(DM$c_class==ratio1),-1],na.rm=TRUE)/
                    colMeans(DM[which(DM$c_class==ratio2),-1],na.rm=TRUE)
            if(logarithm=="log2"){
              fold.res <- cbind(fold.res,log2(fold))
            }else if(logarithm=="log10"){
              fold.res <- cbind(fold.res,log10(fold))
            }else{
              fold.res <- cbind(fold.res, fold)
            }
            if(logarithm == "none"){
              fold.names <- cbind(fold.names,paste("fold",ratio1,"VS", ratio2, sep="_"))
            }else{
            fold.names <- cbind(fold.names,paste(logarithm, "fold", ratio1, "VS", ratio2, sep="_"))
            }
          }
          
        }else{
          stat.res <- cbind(stat.res,fold.res)
          names <- cbind(names, fold.names)
        }
      }
    }
  
  }
  
  ############

  # check columns names in variableMetadata
  
  VM.names <- colnames(VM)
  for (i in 1:length(VM.names)){
    for (j in 1:length(names)){
      if (VM.names[i]==names[j]){
        names[j] <- paste(names[j], "2", sep="_")
      }
    }
  }
  
  colnames(stat.res) <- names
  
  
  # Output ---------------------------------------------------------------------------------------------------
  
  VM <-cbind(VM,stat.res)
  
  ### graphics pdf
  
  if(graphs == 1){
  
  grDevices::pdf(graphs.output)

  
  #Barplots for NA
  if("NA" %in% stat.list){
  graph.colors <- c("green3","palegreen3","lightblue","orangered","red")
  graphics::par(mar = c(5.1, 4.1, 4.1, 8.1), xpd = TRUE)

  bp = graphics::barplot(data_bp, col = graph.colors, main = "Proportion of NA", xlab = "Classes", ylab = "Variables")
  graphics::legend("topright", fill = graph.colors, rownames(data_bp), inset = c(-0.3,0))

  stock = 0
  for (i in 1:nrow(data_bp)){
    graphics::text(bp, stock+data_bp[i,]/2, data_bp[i,], col = "white", cex = 0.7)
    stock <- stock + data_bp[i,]
  }
  
  }

  # Boxplots for fold test
  
  if((test.fold=="Yes")&&(nb_class >= 2)){

    clean_fold <- fold.res
    for(i in 1:nrow(clean_fold)){
      for(j in 1:ncol(clean_fold)){
        if(is.infinite(clean_fold[i,j])){
          clean_fold[i,j] <- NA
        }
      }
    }
    for (j in 1:ncol(clean_fold)){
      title <- paste(fold.names[j])
      graphics::boxplot(clean_fold[,j], main=title)
    }
  }

  grDevices::dev.off()
  
  }else{
    grDevices::pdf(graphs.output)
    graphics::plot.new()
    graphics::legend("center","You did not select any option with graphical output.")
    grDevices::dev.off()
  }
  
  ### VM output
  
  return(VM)

}
  