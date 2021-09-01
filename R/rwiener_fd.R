



rwiener_fd <- function(size=n, 
                       n_curves=n_curves,
                       pen_par=c(0, 2^seq(-2, 9, length.out = 30)),
                       nb= seq(4, 20, by = 1)){
  
  #generation of a sample by Wiener process (time in rows, curves in columns)
  samp <- mvrnorm(n = size, mu=rep(0,n_curves-1), Sigma=diag(n_curves-1))
  samp <- cbind(as.matrix(rep(0, size), ncol=1), samp)
  samp <- samp/sqrt(size)
  samp <- apply(samp, 1, function(x) cumsum(x))
  
  
  #time interval
  t <- seq(0,1,len=dim(samp)[2])
  range_t <- range(t)
  
  fdata_smap <- fdata(samp, argvals=t)
  pen_par <- pen_par
  nb <- nb
  out0 <- optim.basis(fdata_smap, lambda = pen_par, numbasis = nb)
  
  return(out0)
  
}
