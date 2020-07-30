myLinearModel=function(alffa, betta, cInitial, vInitial, m){
  cFind=function(cPrev, vPrev, betta, m){
    cPrev - betta*vPrev + m
  }
  vFind=function(alffa, cPrev){
    alffa*cPrev
  }
  
  dataC=c(1:100)
  dataC[1]=cInitial
  dataV=c(1:100)
  dataV[1]=vInitial
  for(i in 2:100){
    dataC[i]=cFind(dataC[i-1], dataV[i-1], betta, m)
    dataV[i]=vFind(alffa, dataC[i-1])
  }
  return(cbind(dataC,dataV))
}

data=myLinearModel(0.5,0.7,1,1,1)
data
plot(data[,1], col="red", type='o', lwd=2, ylim = c(0,3), main = "alfa=0.5, beta=0.7")
points(data[,2],type = 'o',lwd=2)



myNLinearModel=function(alffa, betta, cInitial, vInitial, m){
  cFind=function(cPrev, vPrev, betta, m){
    cPrev - betta*vPrev*cPrev + m
  }
  vFind=function(alffa, cPrev){
    alffa*cPrev
  }
  
  dataC=c(1:100)
  dataC[1]=cInitial
  dataV=c(1:100)
  dataV[1]=vInitial
  for(i in 2:100){
    dataC[i]=cFind(dataC[i-1], dataV[i-1], betta, m)
    dataV[i]=vFind(alffa, dataC[i-1])
  }
  return(cbind(dataC,dataV))
}

data=myNLinearModel(1/2,2,1,1,1)
data
plot(data[,1], col="red", type='o', lwd=2, ylim = c(0,2), main = "alfa=0.5, beta=2")
points(data[,2],type = 'o',lwd=2)

data=myNLinearModel(0.1,0.1,1,1,1)
data
plot(data[,1], col="red", type='o', lwd=2, ylim = c(-10000,10000), main = "alfa=1.1, beta=1.1")
points(data[,2],type = 'o',lwd=2)

myModel=function(vMax, betta, m, k, l, cInitial, vInitial){
  
  sigmoid=function(x,k,l){
    (x^l)/(k^l + x^l)
  }
  
  cFind=function(cPrev, vPrev, betta, m){
    cPrev - betta*vPrev*cPrev + m
  }
  vFind=function(vMax, cPrev, k, l){
    vMax*sigmoid(cPrev,k,l)
  }
  
  dataC=c(1:100)
  dataC[1]=cInitial
  dataV=c(1:100)
  dataV[1]=vInitial
  for(i in 2:100){
    dataC[i]=cFind(dataC[i-1], dataV[i-1], betta, m)
    dataV[i]=vFind(vMax, dataC[i-1], k, l)
  }
  return(cbind(dataC,dataV))
}

data=myModel(2.1,1.1,1,1,1,1,1)
data
plot(data[,1], col="red", type='o', lwd=2, ylim = c(0,2), main = "Vmax=2.1, beta=1.1")
points(data[,2],type = 'o',lwd=2)
