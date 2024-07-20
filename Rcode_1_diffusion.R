

### Import data ###
library(readxl)
proact_data <- read_xlsx(path = "C:\\Users\\LiBaini\\Documents\\2024_Summer\\EN.625.714.81.SU24 Introductory Stochastic Differential Equations with Applications\\714 project\\Data\\proact_data.xlsx")





########################################################################
########################################################################
########################################################################


### diffusionMap
### https://cran.r-project.org/web/packages/diffusionMap/diffusionMap.pdf


####################################
### Adaptive Regression (Page 4) ###
####################################

library(stats)
library(scatterplot3d)
library(diffusionMap)


## trig function on circle
t=seq(-pi,pi,.01)
x=cbind(cos(t),sin(t))
y = cos(3*t) + rnorm(length(t),0,.1)
tcol = topo.colors(32)
colvec = floor((y-min(y))/(max(y)-min(y))*32); colvec[colvec==0] = 1
scatterplot3d(x[,1],x[,2],y,color=tcol[colvec],pch=20,
              main="Cosine function supported on circle",angle=55,
              cex.main=2,col.axis="gray",cex.symbols=2,cex.lab=2,
              xlab=expression("x"[1]),ylab=expression("x"[2]),zlab="y")
D = as.matrix(dist(x))



## swiss roll data
N=2000
t = (3*pi/2)*(1+2*runif(N))
height = runif(N)
X = cbind(t*cos(t), height, t*sin(t))
X = scale(X) + matrix(rnorm(N*3,0,0.05),N,3)
tcol = topo.colors(32)
colvec = floor((t-min(t))/(max(t)-min(t))*32)
colvec[colvec==0] = 1
scatterplot3d(X,
              xlab=expression("x"[1]),
              ylab=expression("x"[2]),
              zlab=expression("x"[3]),
              xlim=c(-2,2),
              ylim=c(-2,2),
              zlim=c(-2,2),
              main="Swiss Roll, Noise = 0.05",
              pch=18,
              color=tcol[colvec],
              cex.lab=1.5,
              cex.main=1.5,
              col.axis="gray")
D = as.matrix(dist(X))


# 10-fold cross-validation:
AR = adapreg.m(.2, D, t, mmax=25, nfolds=5)
print(paste("optimal model size:",
            AR$mopt,
            "; min. CV risk:",
            round(AR$mincvrisk,4)))

# par(mfrow=c(2,1), mar=c(5,5,4,1))

plot(AR$cvrisks,
     typ='b',
     xlab="Model size",
     ylab="CV risk",
     cex.lab=1.5,
     cex.main=1.5,
     main="CV risk estimates")

plot(t,
     AR$y.hat,
     ylab=expression(hat("Y")),   # Remark: should it be "y_hat" ?
     cex.lab=1.5,
     cex.main=1.5,
     main="Predictions")

abline(0,1, col=2, lwd=2)



##################################
### Diffusion K-means (Page 9) ###
##################################

library(scatterplot3d)

## example with annulus data set
data(annulus)
par(mfrow = c(2,1))
plot(annulus, main = "Annulus Data", pch=20, cex=.7)
D = dist(annulus) # use Euclidean distance

dmap = diffuse(D, eps.val=0.05)  # compute diffusion map
k = 2  # number of clusters
dkmeans = diffusionKmeans(dmap, k)

plot(annulus,
     main = "Colored by diffusion K-means clustering",
     pch = 20,
     cex = .7,
     col = dkmeans$part)
table(dkmeans$part, c(rep(1,500), rep(2,500)))


#####################################################
### Distortion Minimization via K-means (Page 11) ###
#####################################################

data(annulus)
n = dim(annulus)[1]
D = dist(annulus) # use Euclidean distance
dmap = diffuse(D,0.03) # compute diffusion map
km = distortionMin(dmap$X,dmap$phi0,2,dmap$X[sample(n,2),])

plot(annulus, col=km$S, pch=20)
table(km$S, c(rep(1,500), rep(2,500)))


#####################################################################################
### Perform Nystrom Extension to estimate diffusion coordinates of data (Page 13) ###
#####################################################################################

library(stats)
Norig = 1000
Next = 4000
t=runif(Norig+Next)^.7*10
al=.15
bet=.5
x1=bet*exp(al*t)*cos(t)+rnorm(length(t),0,.1)
y1=bet*exp(al*t)*sin(t)+rnorm(length(t),0,.1)
D = as.matrix(dist(cbind(x1, y1)))
Dorig = D[1:Norig, 1:Norig] # training distance matrix
DExt = D[(Norig+1):(Norig+Next),1:Norig] # new data distance matrix

# compute original diffusion map
dmap = diffuse(Dorig,neigen=2)
# use Nystrom extension
dmapExt = nystrom(dmap,DExt)
plot(dmapExt[,1:2],pch=8,col=2,
     main="Diffusion map, black = original, red = new data",
     xlab="1st diffusion coefficient",ylab="2nd diffusion coefficient")
points(dmap$X[,1:2],pch=19,cex=.5)


























