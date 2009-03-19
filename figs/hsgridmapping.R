# Show the mapping of a grid on the horseshoe

library(soap)

fsb <- list(fs.boundary())

# untransformed data
m<-100;n<-50
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

names(fsb[[1]]) <- c("x","y")
insiders<-inSide(fsb,x=xx,y=yy)

# load transformed data
predback.real<-read.csv("../../phd-smoothing/matlab/preal.csv",header=F)
predback.imag<-read.csv("../../phd-smoothing/matlab/pimag.csv",header=F)
grid<-data.frame(x=predback.real,y=predback.imag)
names(grid) <- c("x","y")


# do the plotting
pdf("hsgridmapping.pdf",6,3)
par(mfrow=c(1,2))

# plot the horseshoe
xx<-xx[insiders];yy<-yy[insiders]
plot(xx[seq(1,length(xx),5)],yy[seq(1,length(yy),5)],pch=".",asp=1,xlab="",ylim=c(-1,1),ylab="",cex=0.5,cex.axis=0.5)
lines(fsb[[1]],lwd=3)


# plot the transformed horseshoe
gridy<-grid$y[insiders];gridx<-grid$x[insiders]
plot(gridy[seq(1,length(gridy),10)],gridx[seq(1,length(gridx),10)],pch=".",xlab="",asp=1,ylim=c(-2,2),ylab="",cex=0.5,cex.axis=0.5)
fsb.mapped<-read.csv("../../phd-smoothing/sc-writeup/figs/fsbmapped.csv",header=F)
lines(fsb.mapped$V2,fsb.mapped$V1,lwd=3)

dev.off()
