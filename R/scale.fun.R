scale.fun <- function(x){
  logx=log(x+0.5)
  scale.logx= (logx - mean(logx))/sd(logx)
}