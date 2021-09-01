

#required packages
if(!require(fda)) install.packages("fda", repos = "http://cran.us.r-project.org")
if(!require(fda.usc)) install.packages("fda.usc", repos = "http://cran.us.r-project.org")
if(!require(EnvStats)) install.packages("EnvStats", repos = "http://cran.us.r-project.org")

#simulation of wiener process
rpareto_fd <- function(size=n,
                       n_curves=n_curves,
                       other_par=other_par,
                       a=a,
                       pen_par=c(0, 2^seq(-2, 9, length.out = 30)),
                       nb= seq(4, 20, by = 1)){
  
    discr_data <- matrix(NA, ncol=size, nrow=n_curves)
      for(i in 1:n_curves){
            mu_i <- i^(-1)
            Z_i <- EnvStats::rpareto(n, other_par, a )
            Y_ii <- mu_i*Z_i
            Y_iii <- Y_ii - mean(Y_ii)
            Y_i <- cumsum(Y_iii)
            discr_data[i,] = Y_i
      }
    
    
    t <- seq(0,1,len=dim(discr_data)[2])
    range_t <- range(t)
    
    
    fdata_Y_i <- fdata(discr_data, argvals=t)
    
    pen_par <- pen_par
    nb <- nb
    out0 <- optim.basis(fdata_Y_i, lambda = pen_par, numbasis = nb)
    
    return(out0) 
}