library("png")
library("colorspace")
library("slider")
library("parallel")
library("pheatmap")
library("gplots")
n.cores <- parallel::detectCores()
# Create cluster for parallel calculation. Option type = 'PSOCK' for Windows, 'FORK' for Linux
parallelCluster <- parallel::makeCluster(n.cores,type = "FORK")
# Create function for ranking (for grayscale picture)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
# Set graphic parameters for heatmap
breaks = seq(0, 50, length.out=256)
gradient1 = colorpanel( sum( breaks[-1]<=2 ), "darkblue", "blue" )
gradient2 = colorpanel( sum( breaks[-1]>2 & breaks[-1]<=5 ), "blue", "green" )
gradient3 = colorpanel( sum( breaks[-1]>5 & breaks[-1]<=10 ), "green", "yellow" )
gradient4 = colorpanel( sum( breaks[-1]>10 ), "yellow", "red" )
col = c(gradient1, gradient2, gradient3, gradient4)

# Read stack of frames and get grayscale matricies
f=function(x){as.matrix(as.data.frame(readPNG(x))[,1:ncol(readPNG(x))]*255)}
  filenames <- list.files("/path_to_frames/", pattern="*.png", full.names=TRUE)
  list <- parallel::parLapply(parallelCluster,filenames,f)
lm=parallel::parLapply(parallelCluster,list,as.matrix)
# Get mean standard deviation in sliding window for each pixel
n1=parallel::parApply(parallelCluster,simplify2array((list)), 1:2, function(x) mean(unlist(slide(as.vector(x),sd,.before=0,.after=7,.step=7)),na.rm=T))
# Calculate movement index (method - "sd in sliding window")                      
IS1=mean(n1)
# Get mean standard deviation for each pixel
n2=parallel::parApply(parallelCluster,simplify2array((list)), 1:2, sd,na.rm=T)
# Calculate movement index (method - "sd")                      
IS2=mean(n2)
# Create grayscale pictures for 'sd' and 'sd in sliding window'
n1r=range01(n1)
n2r=range01(n2)
writePNG(n1r, '/your_path/IS1.png', dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
writePNG(n2r, '/your_path/IS2.png', dpi = NULL, asp = NULL, text = NULL, metadata = NULL)
# Create heatmap pictures for 'sd' and 'sd in sliding window'
tiff('/your_path/IS1_heatmap.tiff', units="in", width=15.8, height=10.8, res=512)
pheatmap(n1,col=col,breaks=breaks,show_colnames=F,show_rownames=F,cluster_rows=FALSE, cluster_cols=FALSE,legend=F)
dev.off()
tiff('/your_path/IS2_heatmap.tiff', units="in", width=15.8, height=10.8, res=512)
pheatmap(n1sd,col=col,breaks=breaks,show_colnames=F,show_rownames=F,cluster_rows=FALSE, cluster_cols=FALSE,legend=F)
dev.off()
# Calculate movement index by Fudjita method. The denominator is the number of pixels in the matrix.
IS3=sum(abs(lm[[15]]-lm[[10]])+abs(lm[[20]]-lm[[10]])+abs(lm[[25]]-lm[[10]])+abs(lm[[30]]-lm[[10]]))/1706400
# Calculate movement index by method of extremes
lm=parallel::parLapply(parallelCluster,list,as.matrix)
n3=parallel::parApply(parallelCluster,simplify2array((lm)), 1:2, function(x) mean(unlist(slide(as.vector(x),max,.before=0,.after=7,.step=7))-unlist(slide(as.vector(x),min,.before=0,.after=7,.step=7))))
IS4=mean(n3)
