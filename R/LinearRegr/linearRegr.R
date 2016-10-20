
# function for generating of input data
"gen" <- function(x){
  return(3*x^2 + 2*x + 2 + rnorm(length(x))*0.5)
}

# trainee examples
X <- runif(6);
Y <- gen(X);

#plot(X,Y);

#Данные для кросс-валидации
Xcv <- runif(50);
Ycv <- gen(Xcv);

#Линейная регрессия по полиномиальному набору данных
train <- data.frame(Y, X, X^2, X^3, X^4, X^5)
colnames(train) <- c('Y', 'X', 'X2', 'X3', 'X4', 'X5')
simple <- lm(Y ~ X+X2+X3+X4+X5, train)
error <- sum((predict(simple, train)-Y)^2)/length(Y)
cat("Train error: ",error,"\n")

cv <- data.frame(Xcv, Xcv^2, Xcv^3, Xcv^4, Xcv^5)
colnames(cv) <- c('X', 'X2', 'X3', 'X4', 'X5')
error <- sum((predict(simple, cv)-Ycv)^2)/length(Ycv)
cat("Cross-validation error: ",error,"\n")

#Построим кривую, описывающую наше решение
x <- (1:100)/100
test = data.frame(x, x^2, x^3, x^4, x^5)
names(test) <- c('X', 'X2', 'X3', 'X4', 'X5')
y0 <- predict(simple, test)

plot(X, Y, ylim=range(y0), xlim=c(0,1))
lines(x,y0, col='red')



train <- cbind(X, X^2, X^3, X^4, X^5)
colnames(train) <- c('X', 'X2', 'X3', 'X4', 'X5')
lasso <- lars(train, Y, type='lasso')

cv <- cbind(Xcv, Xcv^2, Xcv^3, Xcv^4, Xcv^5)
colnames(cv) <- c('X', 'X2', 'X3', 'X4', 'X5')

x <- (1:100)*(max(X)*1.1)/100
test <- cbind(x, x^2, x^3, x^4, x^5)
colnames(test) <- c('X', 'X2', 'X3', 'X4', 'X5')


stats <- NULL
lambda <- 0
plot(X, Y, ylim=c(-5, 10), pch=20, col='red')
points(Xcv, Ycv, pch=20, col='blue')
ls <- NULL
cs <- NULL
for(i in 1:15){
  Yp <- predict(lasso, train, s=lambda, type='fit', mode='lambda')
  trainError <- sum((Yp$fit-Y)^2)/length(Y)
  
  Yp <- predict(lasso, cv, s=lambda, type='fit', mode='lambda')
  cvError <- sum((Yp$fit-Ycv)^2)/length(Ycv)
  
  stats <- rbind(stats, c(lambda, trainError, cvError))
  if (i%%5==1){
    #Построим кривую, описывающую наше решение
    y0 <-  predict(lasso, test, lambda, type='fit', mode='lambda')
    lines(x,y0$fit, col=i)
    ls <- c(ls, paste("lambda=",lambda,sep=''))
    cs <- c(cs, i)
  }
  
  lambda <- ifelse(lambda==0,0.00000001, lambda*10)
}
legend("topleft", c("Train", "Cross-validation"), pch=20, col=c('red', 'blue'))
legend("bottomright",inset=0.05, legend=ls, pch=20, col=cs, text.col=cs, bty="n")


plot(stats[,2], ylim=range(stats[,2:3]), type='l', col='red', ylab="error", xlab="log(lambda)")
lines(stats[,3], col='blue')

#Оптимальное значение lamda=1
coef(lasso, s=1, mode='lambda')




