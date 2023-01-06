#### Quantile Function of Empirical Distribution ####
qe <- function(dat,
              prob = c(0.025,0.5,0.975),
              q = c(0.025, 0.500, 0.975),
              realization=NULL,
              k = 0.1,
              n_point = 10000)
#{dat}: empirical data.   
#{prob}: vector of probability
#{q}: quantile of empirical data
#{realization}: if have, use to define L or U
#{k}: overshoot
#{n_point}: precision of constructed empirical distribution
#Example
##Expert ID = 1; LabelQuestion = "AEX1"
#expdata <- read.csv("Exp1.csv")
#dat <- expdata[3:5][which(expdata["LabelQuestion"] == "AEX1"), ]
#qe(dat,prob = c(0.025,0.25,0.5,0.75,0.975))
{
  #parameter
  n.f <- length(q)
  f <- c(q[1],q[-1]-q[-n.f],1-q[n.f]) # probabilities for each interval
  
  ## Function to create distribution from quantiles
  mk_dist <- function(d, k, f, n_point){
    # Define quantile values
    L_U <- c(min(d) - k * (max(d) - min(d)), d, max(d) + k * (max(d) - min(d)))
    # Create cumulative distribution by interpolation between quantiles
    cdf <- approx(x=L_U, y=c(0, cumsum(f)), method = "linear", n = n_point)
    return(cdf)
  }
  
  cdf <- mk_dist(data, k, f, n_point) 
  cdf <- as.data.frame(cdf)
  
  q.save <- c() #store the target 'prob' in q form
  
  #index of prob
  for (i in 1:length(prob)){
    TF <- prob[i] < cdf$y
    if(sum(TF)==0){ #target prob = 1
      q.save[i] <- cdf$x[nrow(cdf)]
      next
    }
    index <- 1
    while(TF[index]!=TRUE){
      index <- index + 1
    }
    q.save[i] <- cdf$x[index]
  }
  return(q.save)
}






