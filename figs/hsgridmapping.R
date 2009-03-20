# Show the mapping of a grid on the horseshoe

library(soap)

fsb <- list(fs.boundary())

# untransformed data
m<-50;n<-25
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

names(fsb[[1]]) <- c("x","y")
insiders<-inSide(fsb,x=xx,y=yy)

# load transformed data
grid<-read.csv("gridpmapped.csv",header=F)
names(grid) <- c("x","y")


# do the plotting
pdf("hsgridmapping-1.pdf",3,3)

# plot the horseshoe
xx<-xx[insiders];yy<-yy[insiders]
plot(xx,yy,pch=".",asp=1,xlab="",ylim=c(-1,1),ylab="",cex=0.75,cex.axis=0.5,axes=FALSE)
lines(fsb[[1]],lwd=3)
dev.off()

# plot the transformed horseshoe
pdf("hsgridmapping-2.pdf",3,2.5)
plot(grid$y,grid$x,pch=".",xlab="",asp=1,,ylab="",cex=0.5,cex.axis=0.5,axes=FALSE)
fsb.mapped<-read.csv("../../phd-smoothing/sc-writeup/figs/fsbmapped.csv",header=F)
lines(fsb.mapped$V2,fsb.mapped$V1,lwd=1)

dev.off()
