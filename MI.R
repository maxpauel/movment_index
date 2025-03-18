library("png")
library("colorspace")
library("slider")
library("parallel")
library("pheatmap")
library("gplots")
n.cores <- parallel::detectCores()
parallelCluster <- parallel::makeCluster(n.cores,type = "FORK")
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
breaks = seq(0, 50, length.out=256)
gradient1 = colorpanel( sum( breaks[-1]<=2 ), "darkblue", "blue" )
gradient2 = colorpanel( sum( breaks[-1]>2 & breaks[-1]<=5 ), "blue", "green" )
gradient3 = colorpanel( sum( breaks[-1]>5 & breaks[-1]<=10 ), "green", "yellow" )
gradient4 = colorpanel( sum( breaks[-1]>10 ), "yellow", "red" )
col = c(gradient1, gradient2, gradient3, gradient4)

f=function(x){as.matrix(as.data.frame(readPNG(x))[,1:ncol(readPNG(x))]*255)}
  filenames <- list.files("/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/с2с125день45Гц_red/", pattern="*.png", full.names=TRUE)
  list <- parallel::parLapply(parallelCluster,filenames,f)
lm=parallel::parLapply(parallelCluster,list,as.matrix)
n1=parallel::parApply(parallelCluster,simplify2array((list)), 1:2, function(x) mean(unlist(slide(as.vector(x),sd,.before=0,.after=5,.step=10)),na.rm=T))
n1sd=parallel::parApply(parallelCluster,simplify2array((list)), 1:2, sd,na.rm=T)
d5_1=c(quantile(n1),mean(n1),quantile(n1sd),mean(n1sd))
n1s=range01(n1)
n1sds=range01(n1sd)
writePNG(n1s, '/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/result_1/d5_1.png', dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
writePNG(n1sds, '/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/result_1/d5_1_sd.png', dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
tiff('/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/result_1/d5_1.tiff', units="in", width=15.8, height=10.8, res=1024)
pheatmap(n1,col=col,breaks=breaks,show_colnames=F,show_rownames=F,cluster_rows=FALSE, cluster_cols=FALSE,legend=F)
dev.off()
tiff('/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/result_1/d5_1_sd.tiff', units="in", width=15.8, height=10.8, res=1024)
pheatmap(n1sd,col=col,breaks=breaks,show_colnames=F,show_rownames=F,cluster_rows=FALSE, cluster_cols=FALSE,legend=F)
dev.off()

f=function(x){as.matrix(as.data.frame(readPNG(x))[,1:ncol(readPNG(x))]*255)}
  filenames <- list.files("/media/maxpauel/6372EF8A3B0879C1/C2C12/ISA_pub2/videos/с2с125день45Гц_red/", pattern="*.png", full.names=TRUE)
  list <- parallel::parLapply(parallelCluster,filenames,f)
lm=parallel::parLapply(parallelCluster,list,as.matrix)

p1=sum(abs(lm[[15]]-lm[[10]])+abs(lm[[20]]-lm[[10]])+abs(lm[[25]]-lm[[10]])+abs(lm[[30]]-lm[[10]]))/1706400
p2=sum(abs(lm[[155]]-lm[[150]])+abs(lm[[160]]-lm[[150]])+abs(lm[[165]]-lm[[150]])+abs(lm[[170]]-lm[[150]]))/1706400
p3=sum(abs(lm[[255]]-lm[[250]])+abs(lm[[260]]-lm[[250]])+abs(lm[[265]]-lm[[250]])+abs(lm[[270]]-lm[[250]]))/1706400
d5_1=c(p1,p2,p3)
           
