kalmanFilter = function( x )
{
  require(KFAS)
  t = x
  if (class(t) != "ts") {
    t = ts(t)
  }
  ssModel = structSSM( y = t, distribution="Gaussian")
  ssFit = fitSSM(inits=c(0.5*log(var(t)), 0.5*log(var(t))), model = ssModel, nsim=100)
  kfs = KFS( ssFit$model, smoothing="state", nsim=length(t))
#   prediction<-predict(kfs, n.ahead=10)
#   lows<-prediction$y - qnorm(0.95)*sqrt(c(prediction$F))
#   ups<-prediction$y + qnorm(0.95)*sqrt(c(prediction$F))
#   plot.ts(cbind(x, prediction$y, lows, ups), plot.type="single", col=c(1:2,4,4))
  vals = kfs$a
  
  
  lastVal = vals[ length(vals)]
  return(vals)
}