#
#         Test the generalization abilities of learners
#        The data is splitted into Training and Test Set
#        MSE is computed for both training and test set 
#   A total of m splits are made and the average MSE computed
#      MLR, RVM, SVM and Gaussian Processes are considered
#
#              Statistics 846 - Srping 2010
#
#              source('generalization-1.R') 
 
# Clear the screen
  
  cat(rep('\n',126)) 
  graphics.off()
 
# Read in the data 

  library(kernlab)

  fokoue <- 1
  #f <- function(x){-x + sqrt(2)*sin(pi^(3/2)*x^2)}
  f <- function(x){1 + 9*x}
  a    <- -1
  b    <- +1
  
  sig  <- 0.3
  n   <- 300 
  x   <- seq(a,b,length=n)  
  y   <-  f(x)+ rnorm(n, 0, sig)
  XY  <- data.frame(x,y)
  
  n   <- nrow(XY)
  p   <- ncol(XY)
  colnames(XY)[p]<- 'Y'

  m   <- 50
  mse.tr <- mse.te <- matrix(0, ncol=4, nrow=m)
  for(r in 1:m)
  {
     s <- sample(sample(n)) 
     ntr <- round(2*n/3)
     nte <- n - ntr
     id.tr <- s[1:ntr]
     id.te <- (1:n)[-id.tr]
     
     XY.tr <- XY[id.tr,]
     XY.te <- XY[id.te,]
   
     #  Polynomial regression

     k <- 1  
     
     xx <- poly(XY.tr$x,degree=k)
     xy <- data.frame(XY.tr$Y,xx)
     colnames(xy)<-c('Y', paste(rep('x'),1:k,sep=''))
     xxnew <- data.frame(poly(XY.te$x, degree=k))
     colnames(xxnew) <- paste(rep('x'),1:k,sep='')
      
     mod <- lm(Y~., data=xy )
     lm.mod <- step(mod, Y~., 
     direction="both", trace=0, k=log(ntr))  
       
     mse.tr[r,1] <-  mean((fitted(lm.mod)-XY.tr$Y)^2)
     mse.te[r,1] <-  mean((predict(lm.mod,xxnew)-XY.te$Y)^2)

     svm.mod <- ksvm(Y~., data=XY.tr, kernel='laplacedot')
     mse.tr[r,2]   <- mean((fitted(svm.mod)-XY.tr$Y)^2)
     mse.te[r,2]   <- mean((predict(svm.mod,data.frame(x=XY.te[,-p]))-XY.te$Y)^2)  

     rvm.mod <- rvm(Y~., data=XY.tr, kernel='laplacedot')
     mse.tr[r,3]   <- mean((fitted(rvm.mod)-XY.tr$Y)^2)
     mse.te[r,3]   <- mean((predict(rvm.mod,data.frame(x=XY.te[,-p]))-XY.te$Y)^2)  

     gpr.mod <- gausspr(Y~., data=XY.tr, kernel='laplacedot')
     mse.tr[r,4]   <- mean((fitted(gpr.mod)-XY.tr$Y)^2)
     mse.te[r,4]   <- mean((predict(gpr.mod,data.frame(x=XY.te[,-p]))-XY.te$Y)^2)  

     #np.mod <- ksmooth(XY.tr$x, XY.tr$Y)
     #mse.tr[r,5]   <- mean((np.mod$y-XY.tr$Y)^2)
     #mse.te[r,5]   <- mean((predict(np.mod,data.frame(x=XY.te[,-p]))-XY.te$Y)^2)  
      

  }  
    cat(rep('\n',126))
 
    compare <- round(rbind(colMeans(mse.tr), colMeans(mse.te)),4)
    colnames(compare)<- c('MLR', 'SVM', 'RVM', 'GPR')
    rownames(compare)<- c('Training Error','Test Error')
    print(compare)

    std.compare <- round(rbind(apply(mse.tr,2,sd), apply(mse.te,2,sd)),4)
    colnames(std.compare)<- c('MLR', 'SVM','RVM', 'GPR')
    rownames(std.compare)<- c('Training Error','Test Error')
    print(std.compare)

   
   x11()
   boxplot(mse.tr, col = 3:6, names = colnames(compare),
   main= 'Comparative Box plots of Training Error', sub=paste('Fokoue Toy Data #',fokoue,sep=''))
   
   x11()
   boxplot(mse.te, col = 3:6, names = colnames(compare),
   main= 'Comparative Box plots of Test Error', sub=paste('Fokoue Toy Data #',fokoue,sep=''))