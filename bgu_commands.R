#@ This document was created for everyone who would find it useful
#@ (##) means its a new function, followed by (#) an example
#@ Use Ctrl+Shift+Enter to run all functions at once
#@ For help or further questions contact me at yannco@post.bgu.ac.il or 053-5290202



## Maximum Likelihood
ml <- function(k,n,L){
  ml <- dbinom(k,n,L)
  return(c(round(L,1),round(ml,2)))

}
#example ml(7,10,0.9)

## Basic confidence test, when sd is known
Revah <- function(miu,sd,N,conf){
  sdmean <- sd/sqrt(N)
  Zscore <- qnorm((1-conf/100)/2,0,1)
  return(tibble(sdmid=round(sdmean,2),"a/2"=round(-Zscore,2),
           Lower=round(miu+Zscore*sdmean,2),
           Upper=round(miu-Zscore*sdmean,2), "%"=conf))
}
# example Revah(100,7,25,96)

## confidence test When sd is unkown, we check proportion
Revah_proportion <- function(n,k,confi){
  p <- k/n
  q <- 1-p
  Zscore <- qnorm((1-confi/100)/2,0,1)
  prob <- sqrt((p*q)/n)
  norma <- p*n & q*n >5
  return(tibble(p=round(p,2),q=round(q,2),Lower=round(p+Zscore*prob,3),
           Upper=round(p-Zscore*prob,3), "%"=round(confi),
           "p&q *n > 5?"=ifelse(norma, "YES","NO")))
}
# example Revah_proportion(300,75,95)

## Var and sd as we learned (/N). for (N-1) just use var or sd
Shonut <- function(x){
  sh <- (sum((x-mean(x))))/length(x)
  stt <- sqrt(sh)
  return(tibble(Shonut=round(sh,2), StiyatTeken=round(stt,2)))
}
# example Shonut(c(100,200,123,321,121,232))

## Revah when sigma is unknown, using t distribution and N,sd,miu known
Revah_t <- function(miu,sd,N,confidence){
  N <- N
  miu <- miu
  sdpop <- sqrt(N/(N-1))*sd
  sdmid <- sdpop/sqrt(N)
  t <- qt((1+confidence/100)/2,N-1)
  return(tibble("sd pop"=round(sdpop,4),"sd midg"=round(sdmid,4),
           "t value"=round(t,4),
           "df"=round(N-1,4),Lower=round(miu-t*sdmid,4),
           Upper=round(miu+t*sdmid,4),
           "%"=round(confidence,4)))
}
# example Revah_t(miu=25,sd=12,N=30,confidence=95)

## Standard Error/Taut Teken
sd_pop <- function(N,sd){
  sd <- sqrt(N/(N-1))*sd
  sdd <- sqrt(N/(N-1)*sd)
  return(c("SE"=sd, "Was it var?"=sdd))
}
# example sd_pop(N=50,sd=10), if input is Shonut, use 2nd value

## p value for Positive tail, default alpha=5%, z=1.65
H0_pos <- function(miu,sd,N,x,conf=5){
  sdmid <- sd/sqrt(N)
  Z <- (x-miu)/sdmid
  return(tibble("H0"=ifelse(Z<qnorm((100-conf)/100,0,1),
                       "accepted", "rejected"),
           "Z"=Z,
           "rejection"=round(qnorm((100-conf)/100,0,1),3),
           "alpha"=conf/100, "%"=conf))
}
# example H0_pos(620,80,25,630)

## p value for Negative tail, default alpha=5% z=-1.65
H0_neg <- function(miu,sd,N,x,conf=5){
  sdmid <- sd/sqrt(N)
  Z <- (x-miu)/sdmid
  return(tibble("H0"=ifelse(Z > -qnorm((100-conf)/100,0,1),
                       "accepted", "rejected"),
           "Z"=Z,
           "rejection"=round(-qnorm((100-conf)/100,0,1),3),
           "alpha"=conf/100, "%"=conf))
}
# example H0_neg(620,80,25,590,15)

## p value for two tails, default alpha=5%, z= +-1.96
H0_two <- function(miu,sd,N,x,conf=5){
  sdmid <- sd/sqrt(N)
  Z <- (x-miu)/sdmid
  return(tibble("H0"=ifelse(Z< -qnorm((100-(conf/2))/100,0,1)
                       | Z>qnorm((100-(conf/2))/100,0,1),
                       "rejected", "accepted"),
           "Z"=round(Z,3),
           "Z rejection +-"=round(qnorm((100-(conf/2))/100,0,1),3),
           "alpha +-"=conf/100/2, "%"=conf))
}
# example H0_two(620,80,25,588)

## p value for proportion
H0_prop <- function(p,N,p_mid){
  p <- p/100
  q <- 1-p
  x <- p_mid/100
  sd_pop <- sqrt((p*q)/N)
  Z <- (x-p)/sd_pop
  norma <- p*N & q*N >5
  return(tibble(p=round(p,3), q=round(q,3),
          sd=round(sd_pop,3),Z=round(Z,3),
          "p&q *n > 5?"=ifelse(norma, "YES","NO")))


}
# example H0_prop(p=46,N=36,p_mid=38)

## Are vars equal? for S_kova. defult alpha=0.05
Equality <- function(S1,S2,N1,N2,alpha=0.05){
  stat <- ifelse(S1 <= S2, S2/S1, S1/S2)
  Crit <- ifelse(S1 <= S2, qf((1-alpha), (N2-1),(N1-1)), qf((1-alpha),(N1-1),(N2-1)))
  df <- c((N1-1),(N2-1))
  Answer <- ifelse(Crit>stat,"H0: 'they are euqal' accepted", "H0: REJECTED! they're not equal")
  return(list("S2/S1"=stat, Crit=round(Crit,3), df=df,alpha=alpha, "Equal?"=Answer))
}
# example Equality(S1=20,S2=60,N1=27,N2=15). result is a list (assign and use $)

## Cohen's d when Shonut midgam is known. Input of s1/s2 is Shonut given, not Stiyat teken
cohend.midgam <- function(mu1,mu2,s1,s2,N1, N2=N1, h0=0){
  sdd <- sqrt((s1*N1+s2*N2)/(N1+N2-2))
  h1 <- mu1-mu2
  return(c("Cohen's d"=(h1-h0)/sdd))
}
# example cohend.midgam(mu1=835,mu2=831,s1=12,s2=8.5,N1=11)

cohend.vec <- function(x,y,h0=0){
  mu1 <- mean(x)
  mu2 <- mean(y)
  s1 <- sd(x)*(length(x)-1)
  s2 <- sd(y)*(length(y)-1)
  N1 <- length(x)
  N2 <- length(y)
  sdd <- sqrt((s1*N1+s2*N2)/(N1+N2-2))
  h1 <- mu1-mu2
  return(data.frame("Cohen d"=(h1-h0)/sdd, h1=h1, h0=h0))
}
# example cohend.vec(x,y,h0=2) x&y should be defined first. H0: real difference is 2


## Trigonometry Sin, Cos, tan of alpha
coss <- function(a){
  a=cos(a*pi/180)
  return(a)
}
sinn <- function(a){
  a= sin(a*pi/180)
  return(a)
}
tann <- function(a){
  a= tan(a*pi/180)
  return(a)
}
# example sinn(30) coss(60) tann(45)

## Maslul equasion when g=9.8
maslul <- function(a,x,g=9.8,v0){
  left=tann(a)*x
  right=g/(2*(v0)-(coss(a)))*x
  y=left-right
  return(y)
}
# example maslul(a=45, x=2, v0=10) defult gravity

## Maslul equasion for horizontal throw
maslul_ofki <- function(x,g=9.8,v0){
  y=(4.9*x)/v0
  return(y)
}
#  example maslul_ofki(x=5, g=10 v0=15) gravity is 10

## Degrees to Radians
deg.to.rad <- function(deg){
  rad=(deg/360)*2*pi
  return(rad)
}
# example deg.to.rad(180)

## Fibonacci calculator + Golden ration
fibonacci <- function(x){
  a <- c(0,1)
  gold <- c(0,1)
  x <- x-2
  for (i in 1:x) {
    a[i+2] <- sum(a[i],a[i+1])
    gold[i+2] <- a[i+1]/a[i]
  }
  return(data.frame(Fibonacci=a,"Golden Ratio"=gold))
}
# example fibonacci(20)

## Set mean() to na.rm=TRUE
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
# example mean(c(1,2,3,4,NA,5))
# use also for sum, median, pmax/min, min/min, range etc..

## Bikoret Finder
BikoretFinder <- function(id){
  # id is a string
  id <- strsplit(id, "")
  id <- unlist(id)
  id <- as.vector(as.numeric(id))
  # seperating each digit

  ida <- id[c(1,3,5,7)]
  idb <- id[c(2,4,6,8)]
  idb <- idb*2
  idc <- c(ida,idb)
  # manipulating according to the formula

  # any larger than 10? :
  idd <- idc[idc>=10]
  idd <- as.character(idd)
  idd <- strsplit(idd, "")
  idd <- unlist(idd)
  idd <- as.vector(as.numeric(idd))
  idd <- sum(idd)
  ide <- sum(idd,idc[idc<10],na.rm=T)
  # manipulating data to extract digits
  ide <- strsplit(as.character(ide), "")
  ide <- unlist(ide)
  ide <- as.vector(as.numeric(ide))
  # extracting the final (and important) digit
  FINAL <- 10-ide[2]
  return(FINAL)
}
# example BikoretFinder("011784276")

## Pascal Triangle
Pascal <- function(nrow){
  require(purrr)
  purrr::accumulate(1:nrow,~c(0,.)+c(.,0),.init = 1)
}
# example Pascal(30)

## Semi & Partial cors using ppcor
s.p.cor <- function(x,y,z){
  require(ppcor)
  p=ppcor::pcor.test(x,y,z)
  sp=ppcor::spcor.test(x,y,z)
  psp=data.frame(type=c("partial","semipartial"),
                 estimate=c(as.numeric(p[1]),as.numeric(sp[1])),
                 p.value=c(as.numeric(p[2]),as.numeric(sp[2])))
  return(psp)
}
# example s.p.cor(x.predictor = Talent,x.to.clean = Ses,DV=y)
# @ cor(resid(lm(DV~x.to.clean)),resid(lm(x.predictor~x.to.clean))) in base R

## Semi and Partial correlation
semi.partial.cor <- function(DV,x.predictor,x.to.clean){
  semi_partial <- cor(resid(lm(DV~x.to.clean)),
                 resid(lm(x.predictor~x.to.clean)))
partial <- cor(DV,resid(lm(x.predictor~x.to.clean)))
return(data.frame(semi_partial=semi_partial,partial=partial,
           df=length(DV)-3,n=length(DV)))

}
# example semi.partial.cor(DV = Salary,x.predictor = Grades,x.to.clean = Motivation)
# @ x.to.clean is the var we want to keep constant
# @ and get direct effect of x.predictor

## Getting a & b for simple linear regression
ab_simple_reg <- function(x,y){
  b <- cor(x,y)*sd(y,na.rm=T)/sd(x,na.rm=T)
  a <- mean(y,na.rm=T)-b*mean(x,na.rm = T)
  return(c(a=a,b=b, y="a+b*x"))
}
# example ab_reg(predictor,DV)

## Miss functions
miss <- function(x){
  x <- na.omit(x)
  Mo = as.numeric(names(table(x))[table(x)==max(table(x))])
  miss1 = length(c(x-Mo)[c(x-Mo)!=0])
  miss2 = sum(abs(x-median(x)))
  miss3 = round(sum((x-mean(x))),2)
  return(c(miss1=miss1, miss2=miss2, miss3=miss3,
           Mode = if(length(Mo)>1){
             print("more than 1 Mode:")
             print(Mo)
           } else { print(Mo)},
           Md=median(x),Mean=round(mean(x),2)))
}
# example miss(c(1,2,3,2,3,4)) *1st argument is mode

## SS functions (model is lm() model)
SS_model <- function(model){
  SST = round(var(model$model[,1])*(length(model$model[,1])-1),3)
  SSRes = round(sum(resid(model)^2),3)
  SSReg = round(SST-SSRes,3)
  Rsq=round(SSReg/SST,4)
  pred=names(model$coefficients[-1])
  return(c(SST=SST,SSRes=SSRes,SSReg=SSReg, Rsq=Rsq,
           Predictor=pred))
}
# example SS_model(model1)

## Converting Beta to B
beta2b <- function(beta,x,y) beta*sd(y)/sd(x)
# example beta2b(0.45,c(12,24,33),c(41,50,16))

## Generates betas from lm() model input using b formulas
lm.beta <- function(m){
  b <- c()
  p <- c()
  for(i in 2:length(m$coefficients)){
    b[i-1] <- m$coefficients[i]/
      (sd(m$model[,1])/sd(m$model[,i]))
    p[i-1] <- names(m$coefficients[i])
  }
  f <- data.frame(predictors=p,beta=b)
  return(f[order(abs(f[,2]),decreasing = T),,])
}
# example lm.beta(lm(Girth~.,trees))

## Generates betas from lm() model input using cor formulas, or passes to 'lm.beta'
model_beta <- function(m){
  y <- m$model[,1]
  r12 <- cor(m$model[,2],m$model[,3])
  beta <-  (cor(y,m$model[,2])-
              r12*cor(y,m$model[,3]))/(1-(r12^2))
  predictor <- names(m$model[2])
  beta <-  c(beta,(cor(y,m$model[,3])-
             r12*cor(y,m$model[,2]))/(1-(r12^2)))
  predictor <- c(predictor,names(m$model[3]))
  res <- data.frame(predictor,beta)
  # Untill here, function dealt with cor,
  # if there are more than 2 predictors, it passes to 'lm.beta'
  # There is no really need for this function, 'lm.beta' is better
  b <- c()
  p <- c()
  for(i in 2:length(m$coefficients)){
    b[i-1] <- m$coefficients[i]/
      (sd(m$model[,1])/sd(m$model[,i]))
    p[i-1] <- names(m$coefficients[i])
  }
  f <- data.frame(predictors=p,beta=b)
  # this is the end of 'lm.beta'
  return(
    if(length(names(m$model))> 3){
      f[order(f[,2],decreasing = T), ,]
    } else {
      res #no need for decreasing() since its 2 x's only
    }
  )
}
# example model_beta(lm(y~age+sat,data))

mitun <- function(DV,x.pred,x.mod,sdH=1,sdM=0,sdL=-1){
  x.pred.c <- scale(x.pred,T,F) # scaling variables
  x.mod.c <- scale(x.mod,T,F) # scaling variables
  moderate <- x.pred.c*x.mod.c # creating moderate variable
  m <- lm(DV~x.pred.c+x.mod.c+moderate) # new model
  # Extracting coeffs
  a <- coef(m)[1]
  b1 <- coef(m)[2]
  b2 <- coef(m)[3]
  b3 <- coef(m)[4]
  # Slopes
  mH <- b1+b3*sd(x.mod.c)*sdH
  mM <- b1+b3*sd(x.mod.c)*sdM
  mL <- b1+b3*sd(x.mod.c)*sdL
  # Intercepts
  interH <- a+b2*sd(x.mod.c)*sdH
  interM <- a+b2*sd(x.mod.c)*sdM
  interL <- a+b2*sd(x.mod.c)*sdL
  # Preparing functions for gg graph
  funH <- function(x.pred.c) interH+x.pred.c*mH
  funM <- function(x.pred.c) interM+x.pred.c*mM
  funL <- function(x.pred.c) interL+x.pred.c*mL
  # gg prep (df, legend, lables...)
  data <- data.frame(x.pred.c,DV)
  SD_Moderator = paste0("+",sdH, " SD")
  colL = paste0(sdL, " SD")
  colM <- ifelse(sdM==0,"Mean",
                 ifelse(sdM>0,paste0("+",sdM, " SD"),paste0(sdM," SD")))
  # basic gg graph
  library(ggplot2)
  g <- ggplot(data,aes(x.pred.c,DV))+geom_point(alpha=0.4,
                                                shape=sample(22,1))+labs(fill="Moderator SD")
  # advanced gg graph
  gg <- g+stat_function(fun = funH,aes(color=SD_Moderator),
                        size=0.8)+
    stat_function(fun=funM,aes(color=colM),size=0.6)+
    stat_function(fun=funL,aes(color=colL),size=0.4)+
    xlab("Predictor values")
  print(summary(m))
  print(data.frame(SD=c(sdH,sdM,sdL),intercepts=c(interH,interM,interL),
                   slopes=c(mH,mM,mL)))
  # NOTE when sdH < 0 & coding shit
  print(ifelse(sdH<0,"In the legend, ignore + sign",
               sample(c("WTF?! Its great!","this is the best thing i've seen",
                        "holy shit this function is awesome!!"),1,
                      prob = c(.1,.10,.8))))

  return(gg)
}
# mitun(trees$Girth,trees$Height,trees$Volume)

## Mediation model
mediation <- function(DV,x.pred,x.med,alpha=5){
  al <- alpha/100
  x.model <- summary(lm(DV~x.pred))
  c <- coef(x.model)[2,c(1,4)]
  m.model <- summary(lm(x.med~x.pred))
  a <- coef(m.model)[2,c(1,4)]
  full.model <- summary(lm(DV~x.med+x.pred))
  b <- coef(full.model)[2,c(1,4)]
  c.tag <- coef(full.model)[3,c(1,4)]
  test <- round(c.tag[1]+a[1]*b[1],5)==round(c[1],5)
  sig <- unique(c(c[2]<al,a[2]<al,
                  b[2]<al,c.tag[2]<al))
  paths <- data.frame(
    a=round(a,4),b=round(b,4),
    "c.tag"=round(c.tag,4),c=round(c,4))
  rownames(paths) <- c("Mekadem","P VALUE")
  ifelse(test,print("a*b+c'=c is correct"),
         "a*b+c'=c went wrong")
  print(ifelse(sig,"all paths are significant",
               "NOT all paths are significant, see table"))

  print(ifelse(as.numeric(round(c.tag[1],3))==0 |
                 as.numeric(c.tag[2])>al,
               "c tag is close to zero, or isn't significant",
               "There's a direct and significant effect"))

  return(paths)
}
# example mediation(DV,x.pred,x.med,alpha = 1)

## Mode function
mode <- function(x) (as.numeric(names(table(x))[table(x)==max(table(x))]))
# example mode(sample(10,11,T))



## Draw a Probability Mass Function, PMF (or CMF)
draw <- function(X,p,cumulative=F,sort.prob=F,round=3){
  P <- abs(p)
  len=c("Please make sure there's either 1 probability (like in a dice),\n  or same length as X (Mishtane Mikri)")
  if(length(P)!=1&length(P)!=length(X)){stop(len)}
  t=sum(X*P)
  ogp=P
  if(cumulative){for(i in 1:length(P)){
    P[i]=sum(P[i],P[i-1])}}

  a=data.frame(X=X,prob=round(P,round))
  print(ggplot(a,aes(X,prob,fill=prob))+
          geom_col()+
          geom_label(aes(label=round(prob,round)),
                     color="white",alpha=0.6,nudge_y = 0.02)+
          scale_x_continuous(breaks = X)+
          theme(legend.position = "none")+
          scale_y_continuous(breaks = NULL))
  cat("Tohelet:", t,"\n")
  cat("Shonut:",sum((X-t)^2*ogp),"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  if(sum(ogp)>1){cat("\nWARNING:\nProbabilities add up tp more than 1.\n")}
  return(a)

}
# example draw(X=c(0:3),p=c(0.1,0.2,0.3,0.4))

## Draw a binomal distribution
draw.binom <- function(X,n=max(X),p,cumulative=F,sort.prob=F,round=3){
  p <- abs(p)
  errp=c("\nProbability MUST be 0 >= p <= 1")
  if(p<0 | p>1){stop(errp)}
  if(max(X)>n){n=max(X)}
  toh=n*p
  sho=round(n*p*(1-p),round)

  if(cumulative){prob=pbinom(X,n,p)}
  else {prob=dbinom(X,n,p)}
  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.02)
  cat("Tohelet =",toh,"\n")
  cat("Shonut =",sho,"\n\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  print(g)
  return(a)
}
# example draw.binom(0:4,n=9,p=0.2,T)

draw.geom <- function(X,p,cumulative=F,decreasing=F,round=3){
  p <- abs(p)
  X <- abs(X)
  # Validation argument
  errp=c("\nProbability MUST be smaller than 1")
  if(p>1){stop(errp)}
  ifelse(X==0 & length(X)==1,X <-X+1 ,"")
  # if prob is greater than 1 or used x=0, stop.
  toh=1/p
  sho=(1-p)/(p^2)
  if(cumulative){prob=pgeom(X-1,p)}
  else {prob=dgeom(X-1,p)}

  a=data.frame(x=X,"prob"=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+
    geom_point()+geom_col(alpha=0.7)+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.02)+
    xlab("MISHTANE MIKRI")+labs(caption = "This function uses x=1 as succsses in 1st try")
  cat("Tohelet =",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(decreasing){a=a[order(a[,2],decreasing = T), ,]}
  print(g)
  return(a)
}
# example draw.geom(0:3,0.2,Cumulative = T,decreasing = T)


draw.pois <- function(X,lam,cumulative=F,sort.prob=F,round=3){
  lam <- abs(lam)
  X <- abs(X)
  # Making the function robust
  lamog=lam
  if(cumulative){prob=ppois(X,lam)}
  else {prob=dpois(X,lam)}

  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.02)+
    xlab("MISHTANE MIKRI")
  cat("Tohelet =",lamog,"\n")
  cat("Shonut =",lamog,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  print(g)
  return(a)
}

draw.exp <- function(X,rate,cumulative=T,sort.prob=F,round=3,x.range=NULL){
  rate <- abs(rate)
  X <- abs(X)
  # Making the function robust
  toh=round(1/rate,round)
  sho=round(1/(rate^2),round)
  if(cumulative){prob=pexp(X,rate)}
  else {prob=dexp(X,rate)}

  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col(alpha=.4)+
    geom_line(aes(x=X,y=prob,size=1.2))+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.02)+
    xlab("MISHTANE MIKRI")+ylab("density")+
    labs(caption = "for P(x) use cumulative=TRUE ")
  cat("Tohelet (and SD) =",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  if(is.vector(x.range)) {cat("Probability for range is:",round(abs(pexp(x.range[1],rate)-pexp(x.range[2],rate)),round),"\n")}
  print(g)
  return(a)
}


draw.norm <- function(X,mean=0,sd=1,cumulative=T,sort.prob=F,round=3,x.range=NULL){
  toh=mean
  sho=sd^2

  if(cumulative){prob=pnorm(X,mean,sd)}
  else {prob=dnorm(X,mean,sd)}

  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col(alpha=.4)+
    geom_line(aes(x=X,y=prob,size=1.2))+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = c(round(min(X),round),
                                  round(mean(X),round),round(max(X),round),toh))+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.008)+
    xlab("MISHTANE MIKRI")+ylab("density")+
    labs(caption = "for P(x) use cumulative=TRUE ")
  cat("Tohelet = ",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  if(is.vector(x.range)) {cat("Probability for range is:",round(abs(pnorm(x.range[1],mean,sd)-pnorm(x.range[2],mean,sd)),round),"\n")}
  print(g)
  return(a)
}

HOMO <- function(lm_object,res=T,intensity=0.8){
  if(intensity<0 | intensity>1){cat("\nPlease make sure:\t0 > Intensity < 1 ")}
  if(intensity<0){intensity <- abs(intensity)}
  Predictor <- lm_object$model[,2]
  if(res){DV=resid(lm_object)}
  else{DV=lm_object$model[,1]}
  m=coef(lm_object)
  if(res){s=0}else{s=m[2]}
  if(res){a=0}else{a=m[1]}
  g=ggplot(lm_object$model,aes(x=Predictor,y=DV))+
    geom_point(aes(x=Predictor,y=DV,color=as.factor(Predictor)),alpha=intensity)+
    geom_abline(slope = s,intercept = a,size=1.4,color="navy")+
    theme(legend.position = "none")
  print(g)
  return(summary(lm_object))
}
# example HOMO(lm(Total~Quantity,regression1))

#Plot a wordcloud for any wikipedia value out there
for(i in 1:1){
  page="Israel"
  lang="fr" # go to wikipedia in english
  bg_color="white"
  word_color=c("blue","navy", "#75aadb")
  shape="circle" #try star, circle or diamond too (and more..)
  min_freq=10 #plot the minimum frequancy that the word appear
  remove_pattern = c("[.?,]")
  fr.es_remove=c("los a lo son por y el que en es un con las del de la et le les en des a du dans sont ou pour un au une pas ont")
  en_remove=c("he will more this its has which were also is not or are if it be the an at from as by for that with were the of and in to a his on was")
  remove_words = str_to_title(unlist(str_split(c(fr.es_remove,en_remove),pattern=" "))) # just write simple text to add or remove words

  web_link <- paste(c("https://"),lang,c(".wikipedia.org/wiki/"),page,sep = "")
  ##
  raw_text=rvest::read_html(web_link) %>% rvest::html_nodes("p") %>%
    rvest::html_text()
  clean.text <- str_to_title(str_remove_all(
    unlist(str_split(raw_text,pattern = " ")),
    pattern = remove_pattern))
  words.freq <- data.frame(words=clean.text,freq=1) %>%
    group_by(words) %>% summarise(f=sum(freq)) %>%
    arrange(desc(f)) %>% filter(!words %in%remove_words)
  ##
  word_col <- sample(word_color,nrow(words.freq[words.freq$f>min_freq,]),T)
  cloud=wordcloud2::wordcloud2(words.freq[words.freq$f>min_freq,],
                               color = word_col,backgroundColor = bg_color,shape=shape)
  ##
  print(cloud)
  print(words.freq[words.freq$f>min_freq,])
  rm(list=ls())
}

password.generator <- function(pass_length,pass_num,seed=NULL){
  set.seed(seed)
  password <- function(pass_length){
    paste(sample(c(LETTERS,letters,0:9),
                 pass_length,T),collapse = "")}
  pass <- c()
  for(i in 1:pass_num){
    pass[i] <- password(pass_length)
  }
  return(data.frame(pass))
}
sampdist <- function(data,func,n,samp_size,replace=TRUE,hist=FALSE){
  loops <- c()
  for( i in 1:n){
    data_loops <- sample(data,samp_size,replace)
    loops[i] <- func(data_loops)
  }
  if(hist){hist(loops)}
  return(loops)
}
menifa <- function(n=100,slope_range=c(0,1),
                   intercepts=0,alpha_range=c(0,0.4),
                   line_color=colors(),line_sizes=c(0.5,1),
                   line_types=1){
  require(ggplot2)
  line_sizes <- abs(line_sizes)
  if(length(slope_range)){slope_range <- c(slope_range,slope_range)}
  if(length(alpha_range)){alpha_range <- c(alpha_range,alpha_range)}

  ggplot()+
    geom_abline(slope=seq(slope_range[1],slope_range[2],length=n),
                intercept = sample(intercepts,n,T),
                alpha=sample(seq(alpha_range[1],alpha_range[2],by=0.15),n,T),
                color=sample(line_color,n,T),
                size=sample(line_sizes,n,T),
                linetype=sample(line_types,n,T)


    )
}

for(. in 1:2){
  x11()
  print(menifa())}
# Install multiple packages
use.package <- function(packages,library.only=TRUE){
  ifelse(library.only,"",lapply(packages,install.packages,character.only=T))
  lapply(packages, require, character.only = T)
}
linear.trans <- function(x,b=1,a=0){
  og <- x
  y <- map_dbl(x,~b*.+a)
  print(
    data.frame(vector=c("old","new"),
               sd=c(sd(og),sd(y)),
               mean=c(mean(og),mean(y))

    ))
  cat("\n")
  return(y)
}
## Mudulu function
logic_func <- function(mone,mechane){
  mone%/%mechane+mone%%mechane/mechane==mone/mechane}

## Helper function to show length of CI as function of p/q variance
length_of_ci <- function(N,conf=95,x_prop=F)
{
  results <- matrix(ncol=2,nrow=N)
  if(x_prop){
    div=N
  } else {div=1}
  
  for(k in 1:N){
    results[k,2] <- prop.ci(N,k,conf)$Upper-prop.ci(N,k,conf)$Lower
    results[k,1] <- k/div
    
  }
  colnames(results) <- c("k","length of ci")
  plot(results,main=paste("Length of CI as function of K succsess\n in",N,"observations.",sep=" "),
       sub=paste("Confidence Level:",conf,sep=" "),
       font.sub = 3,cex.main=1.2)
}
