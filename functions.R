# functions for plotting and data management ------------------------------

pointReg <- function(m, transpar=0.1, multList=FALSE){

  vm <- visreg(m, scale='response', rug=FALSE, gg=TRUE)
  vm2 <- visreg(m, scale='response', plot=FALSE)
  
  if(multList==TRUE){
    plots <- list()
    for(i in 1:length(vm)){
      vm[[i]]$data$y <- as.integer(vm2[[i]]$res$visregPos)
      plots[[i]] <- vm[[i]] + geom_point(alpha=transpar) + theme_bw() + ylim(c(0,1))
    }
  } else {
    vm$data$y <- as.integer(vm2$res$visregPos)
    plots <- vm + geom_point(alpha=transpar) + theme_bw() + ylim(c(0,1))
  }
  return(plots)
}

txtredx <- function(x)
{
  y <- rep(FALSE, length(x))
  for(i in 2:length(y)){
    if(x[i]==x[i-1]){
      y[i] <- TRUE
    }
  }
  x[y] <- NA
  return(x)
}