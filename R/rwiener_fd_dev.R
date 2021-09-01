


rwiener_fd <- function(size=n){

 
}



######################################################
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=T)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

#####################################################


#generation of a sample by Wiener process (time in rows, curves in columns)
Sigma <- diag(x = 1, n-1, n-1)
samp <- mvrnorm(n = n, mu=rep(0,n-1), Sigma=Sigma)
samp <- cbind(as.matrix(rep(0, n), ncol=1), samp)
samp <- samp/sqrt(n)
samp <- apply(samp, 1, function(x) cumsum(x))
# plot(samp[,1], type="l")

#time interval
t <- seq(0,1,len=dim(samp)[1])
range_t <- range(t)
#smoothing
# bspline.basis = create.bspline.basis(range_t,
#                                      nbasis=5, norder=2)
# norder =  6
# nt   =  length(t)
# nbasis =  nt + norder - 2
# oscbasis = create.bspline.basis(range_t, nbasis,norder, t)

# roughPenaltyMax    =  2          
# lambda = 10^(-6)  # a positive smoothing parameter: larger -> more smoothing
# OscfdPar =  fdPar(oscbasis, roughPenaltyMax, lambda)


#Performing smoothing
# oscfd = smooth.basis(t,samp,OscfdPar)
#Other helpful output to extract
# oscfd.fd = oscfd$fd
# oscfd.df = oscfd$df
# oscfd.gcv = oscfd$gcv
# 
# yhat = eval.fd(t,oscfd.fd)
# dyhat = eval.fd(t,oscfd.fd,1)
# d2yhat = eval.fd(t,oscfd.fd,2)

# Plot the smoothed curve
# plot(t, samp[,1], type="p")
# lines(t,yhat[,1])

fdata_smap <- fdata(t(samp), argvals=t)

l <- c(0, 2^seq(-2, 9, length.out = 30))
nb <- seq(7, 31, by = 2)
out0 <- optim.basis(fdata_smap, lambda = l, numbasis = nb)
out1 <- optim.np(fdata_smap, type.S = S.NW, par.CV = list(criteria = "GCV"))
out2 <- optim.np(fdata_smap, type.S = S.LLR, par.CV = list(criteria = "GCV"))
out3 <- optim.np(fdata_smap, type.S = S.LPR, par.CV = list(criteria = "GCV"))
out4 <- optim.np(fdata_smap, type.S = S.LCR, par.CV = list(criteria = "GCV"))
out5 <- optim.np(fdata_smap, type.S = S.KNN, h = 3:10)



#performance
contour(nb, l, out0$gcv, ylab = "Lambda", xlab = "Number of basis", 
        main = "GCV criteria by min.basis()")
plot(out1$h, out1$gcv, type = "l", main = "GCV criteria  by min.np() ", 
     xlab = "Bandwidth (h) values",ylab = "GCV criteria", col = 3, lwd = 2)
legend(x = 3, y = 4.3, legend = c("Ker.norm-S.NW", "Ker.norm-S.LLR", "Ker.unif-S.KNN"),
       box.col = "white", lwd = c(2, 2, 2), col = c(3, 4, 2),cex = 0.75)
lines(out2$h,out2$gcv, col = 4, lwd = 2)
lines(out3$h,out3$gcv, col = 2, lwd = 2)


#vizualization
ind <- 2
plot(fdata_smap[ind], main = "", lty = 2, lwd = 2, col = 8, ylab="", xlab="t")
lines(out0$fdata.est[ind, ], col = 1, lty = 1, lwd = 2)
lines(out1$fdata.est[ind, ], col = 3, lty = 1, lwd = 2)
lines(out2$fdata.est[ind, ], col = 4, lty = 1, lwd = 2)
# lines(out4$fdata.est[ind, ], col = 5, lty = 1, lwd = 2)
lines(out5$fdata.est[ind, ], col = 2, lty = 1, lwd = 2)
add_legend("topright", legend = c("Curve","Bspline basis",
                                  "Ker.norm-S.NW", "Ker.norm-S.LLR", "S.KNN"),
       lty = c(2, 1, 1, 1, 1), lwd = 2, col = c(8, 1, 3, 4, 2), box.col = "white", cex=0.6, horiz=T)
nam <- bquote( paste("Wiener process") )
mytitle = nam
mysubtitle = bquote( paste("Scenario 1:" ~ Y[i]   == (W[i](t) ~ ","~ t %in% (0 ~","~1)) ~ ", i = " ~ .(ind) ))
mtext(side=3, line=1.5, at=-0.9, adj=0, cex=1.2, mytitle)
mtext(side=3, line=0.5, at=-0.9, adj=0, cex=1, mysubtitle)




