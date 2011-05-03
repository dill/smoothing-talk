# do a pretty 3d plot
library(mdspack)
library(rgl)
 
wt2_plot<-function(){

   ## create a boundary...
   bnd <- read.csv("wt2-verts.csv",header=FALSE)

   names(bnd)<-c("x","y")
   
   # create the grid
   my.grid<-create_refgrid(bnd,210)

   ## do the MDS on the grid 
   # create D
   D.grid<-create_distance_matrix(my.grid$x,my.grid$y,bnd,faster=0)
   #D.grid<-dist(cbind(my.grid$x,my.grid$y))

   # perform mds on D
   grid.mds<-cmdscale(D.grid,eig=TRUE,k=2,x.ret=TRUE)
 
   plot3d(grid.mds$points,xlab="x",ylab="y",zlab="",axes=TRUE,box=FALSE)
   #plot(grid.mds$points)

}

wt2_plot()

