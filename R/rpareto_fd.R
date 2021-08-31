

######################################################
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=T)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

#####################################################


rpareto_fd <- function(size=n){
  
  
}
i=100

for(i in 1:n){
mu_i <- i^(-1)
Z_i <- EnvStats::rpareto(n, other_par, a )
Y_ii <- mu_i*Z_i
Y_iii <- Y_ii - mean(Y_ii)
Y_i <- cumsum(Y_iii)
plot(Y_i, type="l")


t <- seq(0,1,len=length(Y_i))
range_t <- range(t)


fdata_Y_i <- fdata(t(Y_i), argvals=t)

l <- c(0, 2^seq(-2, 9, length.out = 30))
nb <- seq(7, 31, by = 2)
out0 <- optim.basis(fdata_Y_i, lambda = l, numbasis = nb)
out1 <- optim.np(fdata_Y_i, type.S = S.NW, par.CV = list(criteria = "GCV"))
out2 <- optim.np(fdata_Y_i, type.S = S.LLR, par.CV = list(criteria = "GCV"))
out3 <- optim.np(fdata_Y_i, type.S = S.LPR, par.CV = list(criteria = "GCV"))
out4 <- optim.np(fdata_Y_i, type.S = S.LCR, par.CV = list(criteria = "GCV"))
out5 <- optim.np(fdata_Y_i, type.S = S.KNN, h = 3:10)



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
ind <- 1
plot(fdata_Y_i[ind], main = "", lty = 2, lwd = 2, col = 8, ylab="", xlab="t")
lines(out0$fdata.est[ind, ], col = 1, lty = 1, lwd = 2)
lines(out1$fdata.est[ind, ], col = 3, lty = 1, lwd = 2)
lines(out2$fdata.est[ind, ], col = 4, lty = 1, lwd = 2)
# lines(out4$fdata.est[ind, ], col = 5, lty = 1, lwd = 2)
lines(out5$fdata.est[ind, ], col = 2, lty = 1, lwd = 2)
add_legend("topright", legend = c("Curve","Bspline basis",
                                  "Ker.norm-S.NW", "Ker.norm-S.LLR", "S.KNN"),
           lty = c(2, 1, 1, 1, 1), lwd = 2, col = c(8, 1, 3, 4, 2), box.col = "white", cex=0.6, horiz=T)
nam <- bquote( paste("Pareto process") )
mytitle = nam
mysubtitle = bquote( paste("Scenario 2:" ~ Y[i]   == Z[i]* mu[i] ~ ", "~ mu[i]==i^-1 ~", i = " ~ .(ind) ))
mtext(side=3, line=1.5, at=-0.9, adj=0, cex=1.2, mytitle)
mtext(side=3, line=0.5, at=-0.9, adj=0, cex=1, mysubtitle)

}