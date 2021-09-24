
rm(list = ls())
#####

#OPTIMAL VARIANCES AND METHODS (PCA + KNN)

#####

#####

library(ggplot2)
library(OpenImageR)
library(bmp)
library(e1071)
library(ggExtra)
library(ggpubr)


# set the working directory to the train with the new images.
###setwd("~/CARLOS III/Project/Train - copia")
# We turn all the pictures into data we can work with
names = list.files(pattern = "bmp")
data = matrix(0, length(names),165*120*3)
for (i in 1:length(names)){
  Im = read.bmp(names[i])
  red = as.vector(Im[,,1])
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[i,] = t(c(red, green, blue))
}

## We create a matrix which contains all the information

aux = seq(1, 480, 6)
train = data[-aux,]
test = data[aux,]

# We separate the data into train and test 

train.scaled=scale(train,center=T,scale=T)

test.scaled=scale(test,center=attr(train.scaled, "scaled:center"),
                  scale=attr(train.scaled, "scaled:scale"))

#We scale and center the data
Sigma_ = train.scaled%*%t(train.scaled)/(nrow(train.scaled)-1)
s = cov(train.scaled)
dim(train.scaled)


Eigen = eigen(Sigma_)
# we use the Eigen function to solve the Sigma matrix
Eigenvalues = Eigen$values

Cummulative.Var = cumsum(Eigenvalues)/sum(Eigenvalues)

a=NULL
for (i in 1:80){
  a=c(a,rep(i,5))
}
#We use his for loop to create a vector that tell us which 
#person corresponds to each image


train = t(t(Eigenfaces)%*%t(train.scaled))
train_ = 

my_knn1 = function(distancia, var){
  
  VarLim=min(which(Cummulative.Var>var))
  # we used 0.96 as our value for the variance since we got the
  # best results beforehand using it.
  
  Eigenvectors = Eigen$vectors[,1:VarLim]
  Eigenfaces = t(train.scaled)%*%Eigenvectors
  
  train = t(t(Eigenfaces)%*%t(train.scaled))
  test = t(t(Eigenfaces)%*%t(test.scaled))
  
  # Now both the test and the train have reduced dimensions,
  # that makes them easier to manipulate
  
  minimum = NULL
  for (i in 1:nrow(test)){
    temp = NULL
    for (j in 1:nrow(train)){
      x = rbind(test[i,],train[j,])
      temp = c(temp, as.numeric(dist(x, method = distancia)))
    }
    
    minimum = c(minimum, which.min(temp))}
  
  return (a[minimum])}

#In our knn function, first we make the data smaller and easier to work with 
#by choosing the a limit of the cumulative variance,then we calculate the distance from
#each image of the test to every image of the train (we can use different methods), 
#then calculate to which image of the train each image of the train has smaller distance with

#Then, using the vector a we created before we can see to which person 
#does the chosen image of the train correspond, and then return a vector 
#with the persons the images of the test correspond to.



variances=c(0.30, 0.50 ,0.70, 0.75, 0.80, 0.85, 0.90, 0.93, 0.96, 0.99)
methods=c("euclidean", "maximum", "manhattan", "canberra")
chek.matrix=matrix(0,10,4)
colnames(chek.matrix)=methods
rownames(chek.matrix)=variances


for (i in 1:10){
  for (j in 1:4){
    f = my_knn1(methods[j], variances[i])
    chek = (f ==  1:80)
    chek.accuracy=length(which(chek == "TRUE"))/80
    chek.matrix[i,j]=chek.accuracy
  }
}
chek.matrix
View(chek.matrix)

which.max(colMeans(chek.matrix))
which.max(rowMeans(chek.matrix))
#Using our knn function we create a matrix that keeps the accuracy of our predictions 
#depending of diferent values of the cumulative variance limit and the methods
#we use to calculate the distances

data = as.data.frame(chek.matrix)





p1 = ggplot(data,aes(x= variances,y=euclidean))+
  geom_density_2d(stat = "identity")+geom_point()+
  labs(title = "Euclidean")+ ylab("")+ylim(0:1)
p2 = ggplot(data, aes(x=variances,y=maximum))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "Maximum")+ylab("")+ylim(0:1)
p3 = ggplot(data, aes(x=variances,y=manhattan))+
  geom_density_2d(stat = "identity")+geom_point()+
  labs(title = "Manhattan")+ ylab("")+ylim(0:1)
p4 = ggplot(data, aes(x=variances,y=canberra))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "Canberra")+ ylab("")+ylim(0:1)
ggarrange(p1,p2,p3,p4,ncol = 2, nrow = 2)



as.matrix(colMeans(data))
as.matrix(rowMeans(data))

#We calculate with which cumulative variance limit and with wich method
#we get the best results



names = list.files(pattern = "bmp")
data = matrix(0, length(names),165*120*3)
for (i in 1:length(names)){
  Im = read.bmp(names[i])
  red = as.vector(Im[,,1])
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[i,] = t(c(red, green, blue))
}



# K-fold
my_knnk = function(k, var){
  aux = seq(k, 480, 6)
  train = data[-aux,]
  test = data[aux,]
  train.scaled=scale(train,center=T,scale=T)
  test.scaled=scale(test,center=attr(train.scaled, "scaled:center"),
                    scale=attr(train.scaled, "scaled:scale"))
  Sigma_ = train.scaled%*%t(train.scaled)/(nrow(train.scaled)-1)
  Eigen = eigen(Sigma_)
  Eigenvalues = Eigen$values
  Cummulative.Var = cumsum(Eigenvalues)/sum(Eigenvalues)
  Cummulative.Var = c(Re(Cummulative.Var))
  VarLim=min(which(Cummulative.Var>var))
  Eigenvectors = Eigen$vectors[,1:VarLim]
  Eigenfaces = t(train.scaled)%*%Eigenvectors
  train = t(t(Eigenfaces)%*%t(train.scaled))
  test = t(t(Eigenfaces)%*%t(test.scaled))
  minimum = NULL
  for (i in 1:nrow(test)){
    temp = NULL
    for (j in 1:nrow(train)){
      x = rbind(test[i,],train[j,])
      temp = c(temp, as.numeric(dist(x, method = "canberra")))
    }
    
    minimum = c(minimum, which.min(temp))}
  
  return (a[minimum])}



variances2=c(0.86, 0.90, 0.96)
ks=seq(1,6)


chek.matrix2=matrix(0,3,6)
colnames(chek.matrix2)=ks
rownames(chek.matrix2)=variances2


for (i in 1:3){
  for (j in 1:6){
    f = my_knnk(ks[j], variances2[i])
    chek = (f==1:80)
    chek.accuracy=length(which(chek == "TRUE"))/80
    chek.matrix2[i,j]=chek.accuracy
  }
}
chek.matrix2
t(as.matrix(colMeans(chek.matrix2)))
as.matrix(rowMeans(chek.matrix2))
which.max(rowMeans(chek.matrix2))
View(chek.matrix2)


data2 = as.data.frame(chek.matrix2)


p1 = ggplot(data2,aes(x=variances2,y=`1`))+
  geom_density_2d(stat = "identity")+geom_point()+
  labs(title = "1 fold")+ ylab("")+ylim(0:1)
p2 = ggplot(data2, aes(x=variances2,y=`2`))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "2 fold")+ylab("")+ylim(0:1)
p3 = ggplot(data2, aes(x=variances2,y=`3`))+
  geom_density_2d(stat = "identity")+geom_point()+
  labs(title = "3 fold")+ ylab("")+ylim(0:1)
p4 = ggplot(data2, aes(x=variances2,y=`4`))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "4 fold")+ ylab("")+ylim(0:1)
p5 = ggplot(data2, aes(x=variances2,y=`5`))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "5 fold")+ ylab("")+ylim(0:1)
p6 = ggplot(data2, aes(x=variances2,y=`6`))+
  geom_density2d(stat = "identity")+geom_point()+
  labs(title = "6 fold")+ ylab("")+ylim(0:1)

ggarrange(p1,p2,p3,p4,p5, p6, ncol = 3, nrow = 2)


#save.image("matrices")
load("matrices")


######

#IDEAL THRESHOLD (PCA + FDA + KNN)

######

######



names = list.files(pattern = "bmp")
data = matrix(0, length(names),165*120*3)
for (i in 1:length(names)){
  Im = read.bmp(names[i])
  red = as.vector(Im[,,1])
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[i,] = t(c(red, green, blue))
}


data.scaled=scale(data,center=T,scale=T)
Sigma_ = data.scaled%*%t(data.scaled)/(nrow(data.scaled)-1)

Eigen = eigen(Sigma_)
Eigenvalues = Eigen$values

Cummulative.Var = cumsum(Eigenvalues)/sum(Eigenvalues)

VarLim=min(which(Cummulative.Var>0.96))
Eigenvectors = Eigen$vectors[,1:VarLim]
Eigenfaces = t(data.scaled)%*%Eigenvectors

data = data.scaled%*%Eigenfaces

#save.image("data.pca")
load("data.pca")


dim(data)
aux = seq(1, 480, 6)

sw=matrix(0,ncol(data),ncol(data))
for (i in aux){
  sw=sw+(cov(data[i:(i+5),])*(5))
}

m = colMeans(data)

sb=matrix(0,ncol(data),ncol(data))
for (i in 1:80){
  sb=sb+((6)*(means[i]-m)%*%t(means[i]-m))
}


eig = eigen(solve(sw)%*%sb)
eigenvector = eig$vectors
eigenvalues = eig$values
eigenvector=Re(eigenvector)
eigenvalues=Re(eigenvalues)


data = as.matrix(data)%*%eigenvector[,1:79]
# fda = lda
Cummulative.Var = cumsum(eigenvalues)/sum(eigenvalues)
nf = min(which(Re(Cummulative.Var)>0.96)) 
train_lda = data%*%eigenvector[,1:nf] 


#save.image("data.pca.fisher")
load("data.pca.fisher")


my_knn2 = function(data){
  distance_matrix = matrix(0, nrow(data), nrow(data))
  for (i in 1:nrow(data)){
    temp = NULL
    for (j in 1:nrow(data)){
      x = rbind(data[i,],data[j,])
      temp = as.numeric(dist(x, method = "canberra"))
      distance_matrix[i,j] = temp
    }
  }
  return (as.vector(distance_matrix[distance_matrix>0]))}
#With this function we calculate all the distances and keep them in a vector

values = my_knn2(data)

df_values1=as.data.frame(values)
ggplot(data= df_values1, aes(x = values))+
  geom_histogram(bins = 60)+xlab("Values")+ylab("")+labs(title = "                All distances for camberra")

V1=NULL
for (i in 1:480){
 V1=c(V1,rep(i,479))
}
# 
# 
V2=NULL
for (i in 1:480){
 x=seq(1,480)
 V2=c(V2,which(x!=i))
}
# 
View(as.data.frame(cbind(values,V1,V2)))

threshold = seq(round(min(values)), round(max(values)), 1)

reales = data[1:420,]
impostores = data[421:480,]
real.impostors = rep.int(c(0,1), times = c(420, 60))
n.reales=420*419
n.impostores=60*59

ftr_=NULL
tfr_=NULL

x = my_knn2(reales)
y = my_knn2(impostores)

for (i in threshold){
  pred.real = (x > i)+0
  pred.impostors = (y > i)+0
  tfr = sum(pred.real == 1)/n.reales
  tfr_ = c(tfr_, tfr)
  
  ftr = sum(pred.impostors == 0)/n.impostores
  ftr_ = c(ftr_, ftr)
} 

ggplot()+
  geom_line(aes(x = threshold, y = tfr_),color = "red")+
  geom_line(aes(x = threshold, y = ftr_),color = "green")

x=tfr_>ftr_

final.threshold=threshold[which(diff(x)!=0)]

ggplot(data= df_values1, aes(x = values))+
  geom_histogram(bins = 60)+xlab("Values")+ylab("")+labs(title = "All distances")+
  geom_vline(xintercept = final.threshold, colour = "red") 

#save.image("data.threshold.pca")
load("data.threshold.pca")

#save.image("data.threshold.pca.fda")
load("data.threshold.pca.fda")

# Load whichever instance you want.

#####

#FINAL FUNCTION AND PREDICTIONS

#####

#####



data = as.matrix(data)%*%eigenvector[,1:79]

names = list.files(pattern = "bmp")
data = matrix(0, length(names),165*120*3)
for (i in 1:length(names)){
  Im = read.bmp(names[i])
  red = as.vector(Im[,,1])
  green = as.vector(Im[,,2])
  blue = as.vector(Im[,,3])
  data[i,] = t(c(red, green, blue))
}

a=NULL
a=0
for (i in 1:80){
  a=c(a,rep(i,5))
}
aux = seq(4, 480, 6)

train = data[-aux,]
test = data[aux,]

final_F = function(train, test, distance, threshold){
  minimum = NULL
  for (i in 1:nrow(test)){
    temp = NULL
    for (j in 1:nrow(train)){
      x = rbind(test[i,],train[j,])
      d=as.numeric(dist(x, method = distance))
      if (d<threshold){
        temp = c(temp, d)
      }
      else{
        temp = c(temp, 100000000)
      }
    }
    if (min(temp)==100000000){
      minimum = c(minimum, 1)
    }
    else{
      minimum = c(minimum, which.min(temp)+1)}
  }
  return (a[minimum])}

pred = final_F(train,test, "canberra", final.threshold)

accuracy = length(which((pred==1:80)=="TRUE"))/length(pred) 


#save.image("FINAL.ALL")
load("FINAL.ALL")
