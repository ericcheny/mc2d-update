#### Density of Empirical Distribution ####
de <- function(dat,
               x,
               realization=NULL,
               q = c(0.025, 0.500, 0.975),
               k = 0.1,
               n_point = 10000)
#{dat}: empirical data.   
#{x}: vector
#{realization}: if have, use to define L or U
#{q}: quantile of empirical data
#{k}: overshoot
#{n_point}: precision of constructed empirical distribution
#Example
##Expert ID = 1; LabelQuestion = "AEX1"
#expdata <- read.csv("Exp1.csv")
#dat <- expdata[3:5][which(expdata["LabelQuestion"] == "AEX1"), ]
#de(dat,x=c(550,570,580,590))
{
  n.f <- length(q)
  f <- c(q[1],q[-1]-q[-n.f],1-q[n.f])
  
  L <- min(min(dat),realization)
  U <- max(max(dat),realization)
  
  L.intrinsic <- min(dat) - k * (max(dat) - min(dat))
  U.intrinsic <- max(dat) + k * (max(dat) - min(dat))
  probability.density <- c(0,f)
  xrange <- c(L.intrinsic,dat,U.intrinsic)
  
  d.save <- c()
  
  for (i in 1:length(x)){
    TF <- x[i] < xrange
    if(sum(TF)==0 | sum(TF)==length(TF)){ #x<L or x>U
      d.save[i] <- 0
      next
    }
    index <- 1
    while(TF[index]!=TRUE){
      index <- index + 1
    }
    d.save[i] <- probability.density[index]
  }
  return(d.save)
}








