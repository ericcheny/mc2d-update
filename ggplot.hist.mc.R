#<<BEGIN>>
ggplot.hist.mc <- function(x, griddim = NULL, xlab = names(x),ylab = "Frequency", main = "",  ...)

{
# the function beau calculate a nice grid
	beau <- function(n){
		nc <- round(sqrt(n))
		nr <- ceiling(n/nc)
		c(nc,nr)}

    l <- length(x)
    main <- rep(main,l)
	  xlab <- rep(xlab,l)
	  ylab <- rep(ylab,l)


    loutm <- lapply(x,attr,which="outm")
    dimm <- sapply(x,dim)
    n <- sum(dimm[3,]* (loutm=="each") + (loutm!="each" & loutm!="none"))


  	if(is.null(griddim)) griddim <- beau(n)
	
	try({  #to restore par in case of error
	  plotlist <- list()
	  
    for(i in 1:l){

      if(is.null(loutm[[i]])) loutm[[i]] <- "each"
      if(loutm[[i]][1] == "none") next                                             # Pass outm == none
      
      
      for(j in loutm[[i]]){                                                     # loop on the nb of stat, j is the name of the function

        if(j == "each"){
          nvar <- dim(x[[i]])[3]
          if(nvar==1) xlab2 <- xlab[i] else xlab2 <- paste(xlab[i],1:nvar,sep="")
        }else{
          func <- get(j,mode="function")                                        # apply the function
          x[[i]] <- apply(x[[i]],c(1,2),func)
          dim(x[[i]]) <- c(dim(x[[i]]),1)
          nvar <- 1                                                             #1 dimension now for this stat
          xlab2 <- paste(j,xlab[i])                                             #change the name with the name of the stat
        }

        if(is.logical(x[[i]])) x[[i]] <- unclass(x[[i]]) * 1                        # unclass to avoid Ops.mcnode
        
        for(k in 1:nvar){
          df <- data.frame(x = as.vector(x[[i]][,,k])) # nvariates data
          #colnames(df) <- xlab2[k] 
          plotlist[[i]] <- ggplot(df,aes(x=x))+
                            geom_histogram()+
                            xlab(xlab2[k])+
                            ylab(ylab[i])
		      #hist(x[[i]][,,k],main=main[i],xlab=xlab2[k],ylab=ylab[i],...)      # loop on nvariates
		    }
      }
    }
	  gga<-ggarrange(plotlist = plotlist,ncol=griddim[2],nrow=griddim[1])
  })
	return(gga)
  }


