remove(list=ls())
x<-1:20
start<-20
y<-NULL
for(i in 1:20){y[i]<-round(start*1.07^i)}
plot(x,y)

rr<-summary(m1 <- glm(y ~ x, family="poisson"))
pred= predict(m1,type="response")
plot(x,y)
lines(x,pred)
x2<-21:30

newdata = data.frame(x=x2)
newpred<-predict(m1, newdata, type="response")
plot(x,y, xlim=c(1,30), ylim=c(20,200))

x2<-c(x,x2)
y2<-c(pred,newpred)
lines(x2,y2)

# https://drewtyre.rbind.io/post/covid-19_and_r_x/
estimate <- coefficients(m1)[2]
std.error <- rr$coef[ 2, 2]  
var_b = std.error^2
t = log(2) / estimate
var_t = var_b * log(2)^2 / estimate^4
lcl_t = t - sqrt(var_t)*qt(0.975, 12)
ucl_t = t + sqrt(var_t)*qt(0.975, 12)
         
sprintf("%.1f (%.1f, %.1f)", t, lcl_t, ucl_t)

# Compare
log(2)/confint(m1)[2,][1]
log(2)/confint(m1)[2,][2]

?qt