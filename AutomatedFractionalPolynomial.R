## set to  the file with  the excel file that I send to you as an example
# setwd("C:/Users/Hon Yiu So/OneDrive - oakland.edu/MaryThompson/CLSA/Alexandra")
# setwd("~/OneDriveOakland/OneDrive - oakland.edu/MaryThompson/CLSA/Alexandra")

comp2r <- read.csv("CLSASmallExample.csv", header=TRUE)
cent.vec <- c(2.5,5,50,95,97.5)


comp2r$WLK_TIMEMS_COM<- comp2r$HWT_DHT_M_TRM
comp2r$WGHTS_ANALYTIC_COM<-comp2r$WGHTS_ANALYTIC_TRM

comp2r$SEX_ASK_COM  <- comp2r$SEX_ASK_TRM=="M"
comp2r$AGE_NMBR_COM <- comp2r$AGE_NMBR_TRM
comp2r$WGHTS_GEOSTRAT_COM<- comp2r$GEOSTRAT_TRM 

Target.Var <- "WLK_TIMEMS_COM"
## use na.omit to remove missing data (so far not much statisical research has been done in the missing data in GAMLSS)
comp2r.f <-  as.data.frame(na.omit(comp2r[,c(Target.Var,"AGE_NMBR_COM", "SEX_ASK_COM","WGHTS_ANALYTIC_COM")]))
## selecting SEX==1  (I  believe it means "male")
compmales<-  comp2r.f[which(comp2r.f$SEX_ASK_COM==1), ]

Target.Var <- "WLK_TIMEMS_COM"
## use na.omit to remove missing data (so far not much statisical research has been done in the missing data in GAMLSS)
comp2r.f <-  as.data.frame(na.omit(comp2r[,c(Target.Var,"AGE_NMBR_COM", "SEX_ASK_COM","WGHTS_ANALYTIC_COM", "WGHTS_GEOSTRAT_COM")]))
## selecting SEX==0  (I  believe it means "male")
compmales<-  comp2r.f[which(comp2r.f$SEX_ASK_COM==0), ]
y <- compmales$WLK_TIMEMS_COM
cent.vec <- c(2.5,5,50,95,97.5)


#### the function that can find Fractional Polynomial quantiles Automatically 
#### It finds the best non- crossing Fractional Polynomial for different quantiles
library(quantreg)

Frac.Poly.Best.AIC<-function( response.var= "WLK_TIMEMS_COM"
                              , data = compmales
                              , weights=data$WGHTS_ANALYTIC_COM
                              , cent.vec=c(.025,.05,.5,.95,.975)){
  ## Define the New.X here 
  New.X <-data.frame(AGE_NMBR_COM=seq(from=45,to=86,by=1))
  
  New.X$AGE_NMBR_COM_p.5<- (New.X$AGE_NMBR_COM)^0.5
  New.X$AGE_NMBR_COM_n.5<- (New.X$AGE_NMBR_COM)^(-0.5)
  New.X$AGE_NMBR_COM_n1<- (New.X$AGE_NMBR_COM)^(-1)
  New.X$AGE_NMBR_COM_p2<- (New.X$AGE_NMBR_COM)^(2)
  New.X$AGE_NMBR_COM_n2<- (New.X$AGE_NMBR_COM)^(-2)
  
  ### modifies the datasets 

  data$AGE_NMBR_COM_p.5<- (data$AGE_NMBR_COM)^0.5
  data$AGE_NMBR_COM_n.5<- (data$AGE_NMBR_COM)^(-0.5)
  data$AGE_NMBR_COM_n1 <- (data$AGE_NMBR_COM)^(-1)
  data$AGE_NMBR_COM_p2 <- (data$AGE_NMBR_COM)^(2)
  data$AGE_NMBR_COM_n2 <- (data$AGE_NMBR_COM)^(-2)
  
  ## Define the fraction polynomial candidates here 
  ## all possible cases 
  All.possible <- expand.grid(  c("","+ AGE_NMBR_COM " ) 
                                ,c("","+ log(AGE_NMBR_COM) ")
                                ,c("","+ AGE_NMBR_COM_p.5 " )
                                ,c("","+ AGE_NMBR_COM_n.5 " )
                                ,c("","+ AGE_NMBR_COM_n1 "  )
                                ,c("","+ AGE_NMBR_COM_p2 "  )
                                ,c("","+ AGE_NMBR_COM_n2 "  )
                                ,stringsAsFactors  = FALSE)
  
  All.possible<-  (cbind( paste(response.var," ~ 1", sep=""),All.possible))
  
  faction.poly<- apply(All.possible, 1, paste0, collapse =" ")  
  
  ### Testing models for best combination of fit 
  ### It works with my example 
  
  frac.rq<- vector("list", length(faction.poly))
  AIC.frac.rq<- array( NA, c(length(faction.poly),length(cent.vec) ) ) 
  
  for( i in 1:length(faction.poly) ){
    tryCatch({   # try catch to ignore the errors (problematic fitting) 
      
      # frac.rq[[i]]<-rq( as.formula(faction.poly[i])
      #                   , tau = cent.vec,
      #                   data = data, weights=weights)  

eval(parse(text= paste0( "frac.rq[[i]]<-rq( ", faction.poly[i],
                        ", tau = cent.vec,
                        data = data, weights=weights)")))      
      
      
          },error=function(e){})
    if(!is.null( frac.rq[[i]]))  AIC.frac.rq[i,]<-AIC( frac.rq[[i]])
  }
  
  ## best model for different centiles  
  #best.AIC <- apply(AIC.frac.rq, 2, function(x) which(x==min(x, na.rm=TRUE))) 
  ## for centiles c(.025,.05,.5,.95,.975) , the responding "best"model are
  
  #faction.poly[best.AIC]
  
  
  ## plotting the results: 
  
  #plot(WLK_TIMEMS_COM  ~AGE_NMBR_COM,  data=compmales)
  
  # pred.quantile <- NA* predict(frac.rq[[1 ]], newdata = New.X)
  # 
  # for( i in 1:dim(pred.quantile)[2]){
  #   pred.quantile[,i]<- predict(frac.rq[[ best.AIC[i] ]], newdata = New.X)[,i]
  # #  lines(c(pred.quantile[,i]) ~New.X$AGE_NMBR_COM, col=2,lwd=2) 
  #   }
  
  ## fine tuning: some quantile are crossing 
  detect.crossing<- function(pred.quantile){
    ## find the quantile closes to median 
    pred.quantile.dim<-dim(pred.quantile)
    med<- order( abs(as.numeric( gsub("tau= ", "", colnames(pred.quantile))) - 0.5))[1] 
    
    check.vec<- c(- apply(pred.quantile[,2:med-1] - pred.quantile[,2:med]  ,2,max), 0,
                  apply(pred.quantile[,(med+1): pred.quantile.dim[2]]-  pred.quantile[,(med+1): pred.quantile.dim[2]-1],2,min) )
    names(check.vec) <-  colnames(pred.quantile)
    check.vec
  }  
  
  ##  fractional polynomial models with best AICs 
  
  best.AICs<-  apply(AIC.frac.rq, 2, order) 
  
  temp.best.AIC  <- dim(best.AICs)[1] *  (1:dim(best.AICs)[2]-1)
  
  
  model.candidates <- best.AICs[1,]
  
  ### matrices for choosing 
  right.prob.func<- function(med.pos , max.pos = 2*med.pos-1, best.n = 4 ){
    dim.n<- (max.pos - med.pos)
    ## just try the best  AICs
    mat<-eval(parse(text=   paste0("expand.grid(", 
                                   paste(rep( paste0("c(0:",best.n,")",collapse =""),dim.n),collapse=","),")"
                                   ,collapse = "") ))
    #mat.vec<-  rowSums(mat* outer(apply(mat,1,max)+1,c(dim.n:1),"^"))
    mat.vec<-  apply(mat,1,max)* max(mat)^(dim.n+1)+ rowSums(mat)* max(mat)^(dim.n) +
      rowSums(mat* outer(apply(mat,1,max)+1,c(dim.n:1-1),"^"))
    eval(parse(text=paste0( "cbind(", 
                            paste0( 1:med.pos*0,collapse=",")
                            ,", mat[order(mat.vec),])",collapse="")))
  }
  
  
  left.prob.func<- function( med.pos, max.pos = 2*med.pos-1, best.n=4){
    right.prob.func(med.pos,best.n=best.n)[,max.pos:1]
  }
  
  
  
  pred.quantile <- NA* predict(frac.rq[[1]], newdata = New.X)
  
  for( i in 1:dim(pred.quantile)[2]){
    pred.quantile[,i]<- predict(frac.rq[[ best.AICs[1,i] ]], newdata = New.X)[,i]
    #  lines(c(pred.quantile[,i]) ~New.X$AGE_NMBR_COM, col=2,lwd=2)
  }
  
  temp<- detect.crossing(pred.quantile)
  
  med.pos <-  which(temp==0) 
  prob.pos<-  which(temp<0)  
  left.prob <- right.prob<- FALSE
  if( length(prob.pos)>0){
    left.prob <- min(prob.pos)<med.pos
    right.prob<- max(prob.pos)>med.pos
  } 
  
  ## solving the right.prob
  right.mat<- as.matrix(right.prob.func(med.pos, length(temp), best.n = dim(best.AICs)[1] ) )
  
  i<-1
  while (right.prob && i < dim(right.mat)[1] ){
    i<-i+1
    
    model.candidates<- best.AICs [ c(temp.best.AIC +  right.mat[i,] +1) ]
    pred.quantile <- NA* predict(frac.rq[[1 ]], newdata = New.X)
    
    #plot(WLK_TIMEMS_COM  ~AGE_NMBR_COM,  data=compmales)
    
    for( j in 1:dim(pred.quantile)[2]){
      pred.quantile[,j]<- predict(frac.rq[[ model.candidates[j] ]], newdata = New.X)[,j]
      # lines(c(pred.quantile[,j]) ~New.X$AGE_NMBR_COM, col=2,lwd=2)
    }
    
    temp<- detect.crossing(pred.quantile)
    
    med.pos <-  which(temp==0) 
    prob.pos<-  which(temp<0)  
    left.prob <- right.prob<- FALSE
    if( length(prob.pos)>0){
      left.prob <- min(prob.pos)<med.pos
      right.prob<- max(prob.pos)>med.pos
    } 
    
  }
  
  ## 
  
  best.right <- right.mat[i,]
  
  
  
  
  ## solving the left.prob
  left.mat<- as.matrix(left.prob.func(med.pos, length(temp), best.n = dim(best.AICs)[1] ) )
  
  i<-1
  while (left.prob && i < dim(left.mat)[1] ){
    i<-i+1
    
    model.candidates<- best.AICs [ c(temp.best.AIC +  left.mat[i,] +1) ]
    pred.quantile <- NA* predict(frac.rq[[1]], newdata = New.X)
    
    #plot(WLK_TIMEMS_COM  ~AGE_NMBR_COM,  data=compmales)
    
    for( j in 1:dim(pred.quantile)[2]){
      pred.quantile[,j]<- predict(frac.rq[[ model.candidates[j] ]], newdata = New.X)[,j]
      # lines(c(pred.quantile[,j]) ~New.X$AGE_NMBR_COM, col=2,lwd=2)
    }
    
    temp<- detect.crossing(pred.quantile)
    
    med.pos <-  which(temp==0) 
    prob.pos<-  which(temp<0)  
    left.prob <- right.prob<- FALSE
    if( length(prob.pos)>0){
      left.prob <- min(prob.pos)<med.pos
      right.prob<- max(prob.pos)>med.pos
    } 
    
  }
  
  ## 
  
  best.left <- left.mat[i,]
  
  ## the rank of AICs actually used  
  best.pos<-c(best.left + best.right  +1)
  
  best.ans<- best.AICs [ c(temp.best.AIC +  best.left + best.right  +1) ]
  
  
  pred.quantile <- NA* predict(frac.rq[[1]], newdata = New.X)
  eval(parse(text= paste( "plot(",response.var, "  ~ AGE_NMBR_COM,  data=data)",sep="")))
  
  for( j in 1:dim(pred.quantile)[2]){
    pred.quantile[,j]<- predict(frac.rq[[ best.ans[j] ]], newdata = New.X)[,j]
    lines(c(pred.quantile[,j]) ~New.X$AGE_NMBR_COM, col=2,lwd=2)
  }
  
  list(
  summary=data.frame( Tau=cent.vec, Best.Model = faction.poly[best.ans],
              AIC= AIC.frac.rq[ c(temp.best.AIC + best.ans  )], 
              Best.pos= best.pos, 
              Best.model.no= best.ans)
  ,fit.quantiles=pred.quantile 
  ,model.fit=frac.rq )
  
}



BestFrac<-Frac.Poly.Best.AIC(response.var= "WLK_TIMEMS_COM"
                   , data = compmales
                   , weights=compmales$WGHTS_ANALYTIC_COM
                   , cent.vec=c(.025,.05,.5,.95,.975))


#show the summary
BestFrac$summary
#show the predicted quantile
BestFrac$fit.quantiles


#corresponding fitted models 
BestFrac$model.fit[BestFrac$summary$Best.model.no]

## the aics
lapply( BestFrac$model.fit[BestFrac$summary$Best.model.no] ,AIC)




## adding one more variable and test again
compmales$HWT_WGHT_KG_TRM <- comp2r[which(comp2r$SEX_ASK_COM==0),]$HWT_WGHT_KG_TRM

BestFrac<-Frac.Poly.Best.AIC(response.var= "HWT_WGHT_KG_TRM"
                   , data = compmales
                   , weights=compmales$WGHTS_ANALYTIC_COM
                   , cent.vec=c(.025,.05,.5,.95,.975))

#show the summary
BestFrac$summary
#show the predicted quantile
BestFrac$fit.quantiles

#corresponding fitted models for tau = c(.025,.05,.5,.95,.975)
BestFrac$model.fit[BestFrac$summary$Best.model.no]

## the corresponding aics 
lapply( BestFrac$model.fit[BestFrac$summary$Best.model.no] ,AIC)

