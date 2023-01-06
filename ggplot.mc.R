#Compare to plot.mc:
#  use ggplot()+stat_ecdf(geom="step") instead of plot.stepfun()
#  use ggplot_build() to get cdf data and then use geom_polygon() instead of ploygon() fill the envelope
#  use ggarange to typeset the plots
#  additional use ggplot2 and ggpubr packages

#Example 1:
#data(total)
#ggplot.mc(mc(xVUM3))
#ggplot.mc(mc(xVUM3),stat="mean")
#ggplot.mc(mc(xVUM3),paint=FALSE)
#ggplot.mc(mc(xVUM3),lim=c(0.025, 0.975))

#Example 2:
#conc <- mcstoc(rnorm,type="U",mean=10,sd=2)
#cook <- mcstoc(rempiricalD, type="V",values=c(1,1/5,1/50), prob=c(0.027,0.373,0.600))
#serving <- mcstoc(rgamma,type="V",shape=3.93,rate=0.0806)
#expo <- conc * cook * serving
#dose <- mcstoc(rpois,type="VU",lambda=expo)
#r <- mcstoc(runif,type="U",min=0.0005,max=0.0015)
#risk <- 1-(1-r)^dose
#EC2 <- mc(conc,cook,serving,expo,dose,r,risk)
#ggplot.mc(EC2)


ggplot.mc <- function(x, prec=0.001, 
                    stat = c("median","mean"), 
                    lim = c(0.025, 0.25, 0.75, 0.975), 
                    na.rm=TRUE, griddim = NULL, 
                    xlab = NULL, ylab = "Fn(x)", 
                    main = "", draw = TRUE, paint=TRUE, 
                    xlim=NULL,ylim=NULL,...)
{
  if(inherits(x,"mc")==TRUE) {
    x <- quantile.mc(x, probs=seq(0,1,prec),lim = lim, na.rm=na.rm, lnames=xlab)
    }

  if(draw) {
	y <- x                           # for a correct return
    stat <- match.arg(stat)


	 beau <- function(n){
		nr <- round(sqrt(n))          # plots layout
		nc <- round(sqrt(n))
		while (nr*nc<n) {
		  if ((nr-nc)==0){
		    nc <- nc+1
		  } else {
		    nr <- nr+1
		  }
		}
		c(nr,nc)}

   noms <- names(rapply(x,function(x) 1))    #moche mais efficace
   if(is.null(xlab)) xlab <- noms
   n <- length(noms)

   if(!is.null(ylim) & ((is.list(ylim) & length(ylim)!= n)|(is.vector(ylim) & length(ylim)!= 2))) stop("ylim should be NULL, a vector of 2 elements or a list of length the number of nodes") 
   if(!is.null(xlim) & ((is.list(xlim) & length(xlim)!= n)|(is.vector(xlim) & length(xlim)!= 2))) stop("xlim should be NULL, a vector of 2 elements or a list of length the number of nodes") 

	 main <- rep(main,n)
	 xlab <- rep(xlab,n)
	 ylab <- rep(ylab,n)

  if(is.null(griddim)) griddim <- beau(n)

  try({   

  i <- 1
  env <- environment()
  
  plotlist <- list()
  
  LEPLOT <- function(y,...){
      if(nrow(y) != 1) {
        if(stat=="median") y <- y[-2,,drop=FALSE]
        else y <- y[-1,,drop=FALSE]}                                              #Retrait median or mean
  		nr <- nrow(y)
      i <- get("i",envir=env)
      xlima <- if(is.null(xlim)) range(y,na.rm=TRUE) else 
		xlima <- if(is.list(xlim)) xlim[[i]] else xlim
  	  if(xlima[1]==xlima[2]) xlima <- xlima + c(-0.5,0.5)
      ylima <- if(is.null(ylim)) c(0,1) else 
		ylima <- if(is.list(ylim)) ylim[[i]] else ylim
      
      #ggplot ecdf
      ggp <- ggplot()+
        stat_ecdf(aes(unlist(y[1,])),geom = "step",na.rm = FALSE)+
        xlim(xlima)+
        ylim(ylima)+
        xlab(xlab[i])+
        ylab(ylab[i])
        
      colorlist <- list()
     
      if(nr > 1){
        rankplot <- 1 + order(-abs(lim-0.5)) 
        for(j in rankplot) {
          colorlist <- c(colorlist,grey(abs(lim[j-1]-.5)+.25))
          ggp <- ggp+
            stat_ecdf(aes_string(unlist(y[j,])),geom = "step",na.rm = FALSE)
        }
        if(paint){
          ggbuild <- ggplot_build(ggp)    #can achieve data
          ti.l <- ggbuild$data[[1]]$x[-length(ggbuild$data[[1]]$x)]  #Points for the polygon used to fill the envelope
          ti.r <- ggbuild$data[[1]]$x[-1L]
          y50 <- ggbuild$data[[1]]$y[-length(ggbuild$data[[1]]$y)]
          thex50 <- rev(as.vector(rbind(ti.l,ti.r)))
          they50 <- rev(as.vector(rbind(y50, y50)))
          for (j in 1:length(lim)){
            ti.lp <- ggbuild$data[[j+1]]$x[-length(ggbuild$data[[j+1]]$x)]
            ti.rp <- ggbuild$data[[j+1]]$x[-1L]
            yp <- ggbuild$data[[j+1]]$y[-length(ggbuild$data[[j+1]]$y)]
            thexp <- as.vector(rbind(ti.lp,ti.rp))
            theyp <- as.vector(rbind(yp, yp))
            ggp<-ggp+geom_polygon(aes_string(x=c(thexp,thex50), y=c(theyp,they50)),fill=colorlist[[j]],col="gray30")}
        }
      }
      
      #Add some aesthetics
      ggp <- ggp+
        geom_hline(aes_string(yintercept=0),linetype="dashed")+
        geom_hline(aes_string(yintercept=1),linetype="dashed")+
        theme_classic()
        
      plotlist<<- c(plotlist,list(ggp))
    assign("i",i+1,envir=env) }
  
  rapply(y,LEPLOT)
    gga<-ggarrange(plotlist = plotlist,ncol=griddim[2],nrow=griddim[1])
  })
  }
  #class(x) <- "ggplotmc"
  return(gga)}

