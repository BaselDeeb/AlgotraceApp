###### Accuracy Functions#####
one.var.model<-function(Use.CV,i, myform,loop.vars, SPY, train, test1, test2 ,test3,output.var,Criterion,char,method){
  if(Use.CV=="No")
    return(one.var.model.class(i, myform,loop.vars,train, test1, test2 ,test3,output.var,Criterion,char,method))
  if(Use.CV=="Yes")
    return(one.var.model.class.cv(i, myform,loop.vars,SPY,output.var,Criterion,char,method))
}
#########################################
########For models Summary###############
get.Results <- function(i,test,row.divission,prediction,output.var,loop.vars,Criterion,method,classification.threshold) {
  tryCatch({
    
    ind1<-row.divission[['ind1']]
    ind2<-row.divission[['ind2']]
    ind3<-row.divission[['ind3']]
    if(method=="Classification"){
      prob1<-prediction[ind1]
      prob2<-prediction[ind2]
      prob3<-prediction[ind3]
      
      pred1 <- ifelse(prob1>classification.threshold,1,0) 
      pred2 <- ifelse(prob2>classification.threshold,1,0) 
      pred3 <- ifelse(prob3>classification.threshold,1,0)  
      
      ##table 1
      test1x <- CV.Table(test[[output.var]][ind1],pred1)
      ##table 2
      test2x <- CV.Table(test[[output.var]][ind2],pred2)
      ##table 3
      test3x <- CV.Table(test[[output.var]][ind3],pred3)
      
      test1xpropHits<-(test1x[1,1]+test1x[2,2])/sum(test1x)
      test1xpropHits0<-test1x[1,1]/(test1x[1,1]+test1x[2,1])
      test1xpropHits1<-test1x[2,2]/(test1x[2,2]+test1x[1,2])
      #F measure#
      Precision<-test1xpropHits1
      Recall<-test1x[2,2]/(test1x[2,1]+test1x[2,2])
      test1xF<-2*Recall*Precision/(Recall+Precision)
      ##auc##
      test1xauc<-auc_roc(prob1,test[[output.var]][ind1])
      
      
      test2xpropHits<-(test2x[1,1]+test2x[2,2])/sum(test2x)
      test2xpropHits0<-test2x[1,1]/(test2x[1,1]+test2x[2,1])
      test2xpropHits1<-test2x[2,2]/(test2x[2,2]+test2x[1,2])
      #F measure#
      Precision<-test2xpropHits1
      Recall<-test2x[2,2]/(test2x[2,1]+test2x[2,2])
      test2xF<-2*Recall*Precision/(Recall+Precision)
      ##auc##
      test2xauc<-auc_roc(prob2,test[[output.var]][ind2])
      
      
      test3xpropHits<-(test3x[1,1]+test3x[2,2])/sum(test3x)
      test3xpropHits0<-test3x[1,1]/(test3x[1,1]+test3x[2,1])
      test3xpropHits1<-test3x[2,2]/(test3x[2,2]+test3x[1,2])
      #F measure#
      Precision<-test3xpropHits1
      Recall<-test3x[2,2]/(test3x[2,1]+test3x[2,2])
      test3xF<-2*Recall*Precision/(Recall+Precision)
      ##auc##
      test3xauc<-auc_roc(prob3,test[[output.var]][ind3])
      
      if(Criterion=="Accuracy")
        stability<-mean(c(abs(test1xpropHits-test2xpropHits),abs(test1xpropHits-test3xpropHits),abs(test2xpropHits-test3xpropHits)),na.rm=TRUE)
      if(Criterion=="Accuracy 0")
        stability<-mean(c(abs(test1xpropHits0-test2xpropHits0),abs(test1xpropHits0-test3xpropHits0),abs(test2xpropHits0-test3xpropHits0)),na.rm=TRUE)
      if(Criterion=="Precision")
        stability<-mean(c(abs(test1xpropHits1-test2xpropHits1),abs(test1xpropHits1-test3xpropHits1),abs(test2xpropHits1-test3xpropHits1)),na.rm=TRUE)
      if(Criterion=="F measure")
        stability<-mean(c(abs(test1xF-test2xF),abs(test1xF-test3xF),abs(test2xF-test3xF)),na.rm=TRUE)
      if(Criterion=="AUC")
        stability<-mean(c(abs(test1xauc-test2xauc),abs(test1xauc-test3xauc),abs(test2xauc-test3xauc)),na.rm=TRUE)
      
      Table<-test1x+test2x+test3x

      Accuracy<-(Table[1,1]+Table[2,2])/sum(Table)
      Accuracy_0 <- Table[1,1]/(Table[1,1]+Table[2,1])
      Precision <- Table[2,2]/(Table[2,2]+Table[1,2])
      False_Positive <- Table[1,2]/(Table[1,1]+Table[1,2])
      False_Negative <- Table[2,1]/(Table[2,1]+Table[2,2])
      #Diff##
      Diff0<-Table[1,1]-Table[2,1]
      Diff1<-Table[2,2]-Table[1,2]
      #Lift
      if(sum(Table[2,])==0){
        Lift_1<-NA
      } else{
        temp.mean<-sum(Table[2,])/sum(Table)
        Lift_1<-100*(Precision/temp.mean) 
      }
      #F measure#
      Recall<-Table[2,2]/(Table[2,1]+Table[2,2])
      F_measure<-2*Recall*Precision/(Recall+Precision)
      ##auc##
      y<-test[[output.var]]
      prob<-c(prob1,prob2,prob3)
      testxauc<-auc_roc(prob,y)
      AUC<-testxauc
      Gini<-2*testxauc-1
      
      tmp.df <- data.frame(Accuracy, stability,Accuracy_0, Precision,Recall,False_Positive,False_Negative,
                           Lift_1,Diff0,Diff1,F_measure,AUC,Gini,check.names=F)
      names(tmp.df) <- c('Accuracy', 'Stability','Accuracy 0','Precision','Recall','False Positive','False Negative',
                         'Lift 1(%)','Diff 0','Diff 1','F measure','AUC','Gini')
      rownames(tmp.df) <- i
      return(tmp.df)
    }
    if(method=="Estimation"){
      pred1<-prediction[ind1]
      pred2<-prediction[ind2]
      pred3<-prediction[ind3]
      
      diff1<- mean(abs(test[[output.var]][ind1]-pred1),na.rm =TRUE)/mean(abs(test[[output.var]][ind1]),na.rm =TRUE)
      diff2<- mean(abs(test[[output.var]][ind2]-pred2),na.rm =TRUE)/mean(abs(test[[output.var]][ind2]),na.rm =TRUE)
      diff3<- mean(abs(test[[output.var]][ind3]-pred3),na.rm =TRUE)/mean(abs(test[[output.var]][ind3]),na.rm =TRUE)
      
      R2<-R2.calulation(act=test[[output.var]],pred=prediction)
      if(!is.na(loop.vars)){
        R2.Adj<-1-((1-R2)*(length(test[[output.var]])-1))/(length(test[[output.var]])-length(loop.vars)-1)
      } else{
        R2.Adj<-NA
      }
      Cor<-cor(test[[output.var]],prediction,use ="complete.obs",method="spearman")
      RMSE<- sqrt(mean((test[[output.var]]-prediction)^2,na.rm =TRUE))
      MAE<- mean(abs(test[[output.var]]-prediction),na.rm =TRUE)
      Norm_abs_Difference<- mean(abs(test[[output.var]]-prediction),na.rm =TRUE)/mean(abs(test[[output.var]]),na.rm =TRUE)
      Difference<- mean(test[[output.var]]-prediction,na.rm =TRUE)
      Norm_Difference<- mean(test[[output.var]]-prediction,na.rm =TRUE)/mean(test[[output.var]],na.rm =TRUE)
      
      Stability<- (abs(diff1-diff2)+ abs(diff2-diff3)+abs(diff1-diff3))/3
      Sum_output<-sum(test[[output.var]],na.rm =TRUE)
      Sum_pred<-sum(prediction,na.rm =TRUE)
      Abs_Sum_Difference<-abs(Sum_output-Sum_pred)
      Avg_output<-mean(test[[output.var]],na.rm =TRUE)
      Avg_pred<-mean(prediction,na.rm =TRUE)
      tmp.df <- data.frame(MAE,Norm_abs_Difference,Difference,Norm_Difference, Stability,Sum_output,Sum_pred,Abs_Sum_Difference,Sum_output/Sum_pred,Avg_output,Avg_pred,RMSE,Cor,R2,R2.Adj,check.names=F)
      names(tmp.df) <- c('MAE','Norm abs Difference','Difference','Norm Difference','Stability','Sum output','Sum pred','Abs Sum Difference','Act div Pred','Avg output','Avg pred','RMSE','Cor(y,yhat)','R2','R2 Adj')
      rownames(tmp.df) <- i
      return(tmp.df)
    }
  },
  error=function(err) {return(NULL)})
}

########R2 Calculation##############
R2.calulation<-function(act,pred){
  if(length(act)!=length(pred))
    return(NA)
  ind<-which(complete.cases(act) & complete.cases(pred))
  if(length(ind)==0)
    return(NA)
  act<-act[ind]
  pred<-pred[ind]
  SS.residuals <- sum((act - pred)^2)
  SS.total<- sum((act - mean(act))^2)
  return(1-(SS.residuals/SS.total))
}


#########Factor to Numeric#################
fac.levels.into.num<-function(Data,fac.name,target.name){
  table<-table(Data[[fac.name]])/sum(table(Data[[fac.name]]))
  if(sum(table>0.02)>0){
    table<-table[table>0.02]
  } else{
    table<-table[which.max(table)]
  }
  uni<-names(table)
  temp<-NULL
  for(i in uni){
    ind<-which(Data[[fac.name]]==i)
    if(length(ind)>0)
      temp<-rbind(temp,data.frame("level"=i,"num"=round(mean(Data[[target.name]][ind],na.rm=TRUE),round.digits)))
  }
  temp<-rbind(temp,data.frame("level"="new_level_algotrace",num=round(mean(Data[[target.name]],na.rm=TRUE),round.digits)))
  return(temp)
}


turn.fac.to.num<-function(vec,table){
  levels<-as.character(table$level[!table$level %in% "new_level_algotrace"])
  temp<-unname(sapply(as.character(vec),function(char){
  if(is.na(char) || char=="")
    return(NA)
  if(char %in% levels){
    return(table[which(table$level==char),"num"])
  } else{
    return(table[which(table$level=="new_level_algotrace"),"num"])
  }
}))
  return(temp)
}
########Balance the Data################
balance<-function(data,target,method){
  tryCatch({
    orig.data<-data
    if(method=="Hybrid"){
      if(table(data[[target]])["1"]>table(data[[target]])["0"]){
        num<-round(sum(table(data[[target]]))/2-table(data[[target]])["0"])
        ind1<-which(data[[target]]==1)
        ind0<-c(1:nrow(data))[-ind1]
        ###Take from 1 and give to 0
        set.seed(1234)
        places.to.change<-sample(ind1,num,replace=FALSE)
        set.seed(1234)
        data[places.to.change,]<-data[sample(ind0,num,replace=TRUE),]
      } else{
        num<-round(sum(table(data[[target]]))/2-table(data[[target]])["1"])
        ind1<-which(data[[target]]==1)
        ind0<-c(1:nrow(data))[-ind1]
        ###Take from 0 and give to 1
        set.seed(1234)
        places.to.change<-sample(ind0,num,replace=FALSE)
        set.seed(1234)
        data[places.to.change,]<-data[sample(ind1,num,replace=TRUE),]
      }
      return(data)
    } else{
      num<-table(data[[target]])["1"]
      if(num>10){
        ind1<-which(data[[target]]==1)
        ind0<-c(1:nrow(data))[-ind1]
        set.seed(1234)
        ind0<-sample(ind0,num,replace=TRUE)
        data<-data[c(ind0,ind1),]
        
        return(data)
      } else{
        return(data)
      }
    }
  },error=function(err) {
    return(orig.data)
  })
}
########Detect near zero variance#######
nearzeroVar <- function(data) {
  out <- apply(data, 2, function(x) {var(x)})
  ind<-which(out<0.001)
  return(names(out)[ind])
}
######point-biserial correlation#########
#https://www.rdocumentation.org/packages/ltm/versions/1.1-1/topics/biserial.cor
cont.dummy.correlation<-function(x,y){
  #x continues
  #y dummy
  ind0<-which(y==0)
  p0<-length(ind0)/length(y)
  m0<-mean(x[ind0],na.rm=TRUE)
  m1<-mean(x[-ind0],na.rm=TRUE)
  r<-(m1-m0)*sqrt(p0*(1-p0))/sd(x,na.rm=TRUE)
  return(r)
}
##Correlation
cor.nv<-function(u,v){
  if(length(u)!=length(v))
    stop("vectors dont have the same length")
  cor<-tryCatch({
    if(all(na.omit(u) %in% 0:1)){##u dummy
      if(all(na.omit(v) %in% 0:1)){##u and v dummy
        return(mean(u==v,na.rm=T))
      } else{##u dummy v continues
        return(abs(cont.dummy.correlation(v,u)))
      }
    } else{##u continues
      if(all(na.omit(v) %in% 0:1)){##u continues and v dummy
        return(abs(cont.dummy.correlation(u,v)))
      } else{##u continues v continues
        return(abs(cor(u,v,method="spearman", use = "complete.obs")))
      }
    }
  },error=function(err){
    return(NULL)
  })
  return(cor)
}
########For vars.summary################
update.Type<-function(data){
  table<-data.frame(Type=rep(NA,ncol(data)))  
  table$Type <- sapply(1:ncol(data), function(x) { class(data[[x]])})
  
  return(table)
}
update.vars.summary<-function(data){
  temp<-data.frame(Type=rep(NA,ncol(data)))
  temp$Type <- sapply(1:ncol(data), function(x) { class(data[[x]])})
  temp$Levels <- sapply(1:ncol(data), function(x) { length(unique(data[[x]]))})
  temp$Missing <- sapply(1:ncol(data), function(x) { 
    if(is.numeric(data[[x]])){return(round(sum(is.na(data[[x]]))/ nrow(data),2))}
    return(round((sum(is.na(data[[x]]))+sum(data[[x]] %in% c("","NaN","#DIV/0!"),na.rm = TRUE))/ nrow(data),2))
  })#Missing.Percentage
  
  temp[,c("Min","Q1","Median","Mean","Q3","Max")]<-NA
  temp.list<-lapply(1:ncol(data), function(x) {
    if(is.numeric(data[[x]])==FALSE){return(c(NA,NA,NA,NA,NA,NA))}
    summ<-summary(data[[x]])
    return(round(summ,2))})
  
  temp[,c("Min","Q1","Median","Mean","Q3","Max")]<-do.call("rbind",temp.list)
  
  return(temp)
}
##########################################
####Finding Outliers and Replacing them#####
find.outliers<-function(x,Outliers.settings){  #replace.outlier.with.median
  if(!is.numeric(x)){return(NULL)}
  tryCatch({  
    ####Replace the Outlier with median######
    if(is.null(Outliers.settings)){
      c1.q1<-0.01
      c1.q2<-0.99
      c2<-3
      c3<-3
    } else{
      c1.q1<-Outliers.settings[["Criterion 1"]][1]
      c1.q2<-Outliers.settings[["Criterion 1"]][2]
      c2<-Outliers.settings[["Criterion 2"]]
      c3<-Outliers.settings[["Criterion 3"]]
    }
    ## 95% quantile
    quantiles <- quantile(x, c(c1.q1, c1.q2 ), na.rm=TRUE)
    ind1<-union(which(x < quantiles[1]),which(x > quantiles[2])) 
    ##median
    ind2<-which((abs(x - median(x, na.rm=TRUE)) / (mad(x, na.rm=TRUE)+0.001)) > c2)
    ##mean
    ind3<-which((abs(x - mean(x, na.rm=TRUE)) / (sd(x, na.rm=TRUE)+0.001)) > c3)
    
    places<-1:length(x)
    
    outlier.place<-which(as.numeric(places %in% ind1)+
                           as.numeric(places %in% ind2)+as.numeric(places %in% ind3)>1)  
    
    return(outlier.place)
  } , error=function(err){
    print(err$message)
    return(NULL)
  })
}
####Getting necessary.variables.found######
Retrieving.necessary.variables.found<-function(temp.necessary.variables.found,SPY,
                                               output.var,WL.params,Rpart.params,xgb.params,method){
  if(is.null(temp.necessary.variables.found))
    return(NULL)
  
  Myform<-list()
  Models<-list()
  P.Var<-temp.necessary.variables.found
  chars<-names(P.Var[["Vars"]])
  for(char in chars){
    tryCatch({
    loop.vars<-P.Var[["Vars"]][[char]]
    if(char %in% c("Logistic","Naive Logistic")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-glm(formula,family=binomial('logit'),data=SPY)
    }
    if(char %in% c("Linear","Naive Linear")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-lm(formula,data=SPY) 
    }
    if(char %in% c("Weighted Logistic","Naive Weighted Logistic")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-glm(formula,family=binomial('logit'),data=SPY,weights=WL.params)
    } 
    if(char %in% c("Weighted Linear","Naive Weighted Linear")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-lm(formula,data=SPY,weights=WL.params) 
    } 
    if(char %in% c("Negative Binomial","Naive Negative Binomial")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-glm.nb(formula,data=SPY)
    } 
    if(char %in% c("Quantile","Naive Quantile")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Model<-rq(formula,tau = .5, method = "pfn",data=SPY)
    }
    if(char %in% c("Xgboost","Naive Xgboost")){
      formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1"))
      SPY.matrix<- sparse.model.matrix(formula, SPY)  
      set.seed(xgb.seed);Model <- xgboost(data = SPY.matrix, label = SPY[[output.var]],nround=xgb.params[["nround"]], params=xgb.params[["params"]],verbose=0)
    }
    if(char %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"))) 
      if(method=="Classification"){
        Model<-rpart(formula,data=SPY,method='class',control=rpart.control(minsplit=Rpart.params$minsplit,cp=Rpart.params$cp,maxdepth=Rpart.params$maxdepth)) 
      } else{
        Model<-rpart(formula,data=SPY,control=rpart.control(minsplit=Rpart.params$minsplit,cp=Rpart.params$cp,maxdepth=Rpart.params$maxdepth)) 
      }
    }
    if(char %in% c("Rforest","Naive Rforest")){
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"))) 
      if(method=="Classification"){
        temp.SPY<-SPY   
        temp.SPY[[output.var]]<-as.factor(temp.SPY[[output.var]])
        set.seed(12);Model<-ranger(formula, data = temp.SPY, num.trees = 5, write.forest = TRUE,classification=TRUE,probability =TRUE,importance='impurity') 
      } else{
        set.seed(12);Model<-ranger(formula, data = SPY, num.trees = 5, write.forest = TRUE,classification=FALSE,importance='impurity') 
      }
    }
    if(char %in% c("Neural Network","Naive Neural Network")){
      formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1"))
      if(method=="Classification"){
        SPY.matrix<- data.matrix(sparse.model.matrix(formula, SPY)) 
        mx.set.seed(0)
        Model <- mx.mlp(SPY.matrix, SPY[[output.var]], hidden_node=5,activation='relu', out_node=2, out_activation="softmax",
                        num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                        eval.metric=mx.metric.accuracy)
      } else{
        data <- mx.symbol.Variable("data")
        fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)
        lro <- mx.symbol.LinearRegressionOutput(fc1)
        SPY.matrix<- data.matrix(sparse.model.matrix(formula, SPY)) 
        mx.set.seed(0)
        Model <- mx.model.FeedForward.create(lro, X=SPY.matrix, y=SPY[[output.var]],
                                             ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                             learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse) 
      }
    }
    Myform[[paste(char)]]<-formula
    Models[[paste(char)]]<-Model
    }, error=function(err){})
  }
  P.Var[["Myform"]]<-Myform
  P.Var[["Models"]]<-Models
  return(P.Var)
}

#####For ggvis####
all_values <- function(x) {
     if(is.null(x)) return(NULL)
     paste0(names(x), ": ", format(x), collapse = "<br />")
}

####For Data Table###
cut.value<-function(str,n){
  if(nchar(str)<=n)
    return(str)
  temp<-strtrim(str,n)
  temp<-str_trim(temp, "right")
  return(paste0(temp,"..."))
}

####Function for Est.explore.data.criterion########
criterions.funcion<-function(x,criterion){
  if(criterion=="Mean")
    return(round(mean(x,na.rm = TRUE),round.digits))
  if(criterion=="Median")
    return(round(median(x,na.rm = TRUE),round.digits))
  if(criterion=="Min")
    return(round(min(x,na.rm = TRUE),round.digits))
  if(criterion=="Max")
    return(round(max(x,na.rm = TRUE),round.digits))
  if(criterion=="Q1")
    return(round(quantile(x,prob=0.25,na.rm = TRUE),round.digits))
  if(criterion=="Q3")
    return(round(quantile(x,prob=0.75,na.rm = TRUE),round.digits))
}
####R-squared####
rsq <- function(x, y) summary(lm(y~x))$r.squared
#rsq <- function (x, y) cor(x, y, use="complete.obs") ^ 2
####This Function strips the glm from all it's unnecessary components######

### Predict interval###

predict.interval<-function(prediction){
  lower<-round(prediction-qnorm(0.975)*sd(prediction)/sqrt(length(prediction)),3)
  upper<-round(prediction+qnorm(0.975)*sd(prediction)/sqrt(length(prediction)),3)
  return(paste0('[',lower,',',upper,']'))
}



cut.nv<- function(data,intervals_num) {
  quantile_seq<-seq(1/intervals_num, 1,1/intervals_num)
  overall_quantile<-unname(quantile(data, probs = quantile_seq,na.rm=TRUE))
  overall_quantile<-data.frame(quantile=quantile_seq,value=overall_quantile)
  lag_value<-c(NA,overall_quantile$value[-nrow(overall_quantile)])
  overall_quantile<-cbind(overall_quantile,lag_value)
  overall_quantile$lag_value<-ifelse(overall_quantile$quantile==1/intervals_num,min(data,na.rm=TRUE),overall_quantile$lag_value)
  overall_quantile$value<-ifelse(round(overall_quantile$quantile,8)==1,max(data,na.rm=TRUE)+1,overall_quantile$value)
  overall_quantile<-subset(overall_quantile,value!=lag_value)
  overall_quantile$value<-round(overall_quantile$value,3)
  overall_quantile$lag_value<-round(overall_quantile$lag_value,3)
  overall_quantile$Intervals<-paste0("[",overall_quantile[,"lag_value"],",",overall_quantile[,"value"],")")
  
  
  temp<-sapply(data,function(x,tab=overall_quantile){
    tryCatch({
      ind<-which(x>=tab$lag_value & x<tab$value)
      if(length(ind)==1){
        return(as.character(tab[ind,"Intervals"])) 
      } else{
        return(NA)
      }
    }, error=function(err){return(NA)})
  })
  
  if(length(levels(temp))<intervals_num)
    warning(paste("Only ", length(unique(temp)), " intervals generated, not ", intervals_num, " as requested", sep = "")) 
  
  temp<-as.factor(temp)
  lev<-overall_quantile$Intervals[overall_quantile$Intervals %in% temp]
  temp<-factor(temp,levels=lev)
  
  return(temp)
}


### Group distance #####
group.distance<-function(x){
  len<-length(x)
  distances<-NULL
  for(i in 1:len){
    for(j in 1:len){
      if(j<=i){next}
      distances<-c(distances,abs(x[[i]]-x[[j]]))
    }
  }
  return(mean(distances,na.rm=TRUE))
}
###########
#### Finding the Mode for a vector#####
Mode <- function(x) {
  x<-cleaned.vec(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#ROC/AUC
getROC_AUC = function(probs, true_Y){
  probsSort = sort(probs, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix)  
  
  roc_y = true_Y[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)    
  
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(list(stack_x=stack_x, stack_y=stack_y, auc=auc))
}

getROC_AUC_for_multiple_models <- function(df){
  probsSort <- sort(df$value, decreasing = TRUE, index.return = TRUE)
  val <- unlist(probsSort$x)
  idx <- unlist(probsSort$ix)  
  
  roc_y <- df$target[idx];
  stack_x <- cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y <- cumsum(roc_y == 1)/sum(roc_y == 1)    
  
  auc <- sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])
             *stack_y[2:length(roc_y)])
  
  df <- data.frame(Model=max(as.character(df$name)), stack_x, stack_y)
  
  return(df)
}


###### Finding the participating names in not.fix columns #####
Not.fix.columns<-function(data,target,not.include,not.fix,not.fix.outliers){
  if(!is.null(not.fix) && length(not.fix)>0){
    not.fix<-not.fix[not.fix %in% names(data)]
    not.fix<-not.fix[!(not.fix %in% c(not.include,target))] 
  } else{
    not.fix<-NULL
  }
  ### dealing with not to fix outliers variables ###
  if(!is.null(not.fix.outliers) && length(not.fix.outliers)>0){
    not.fix.outliers<-not.fix.outliers[not.fix.outliers %in% names(data)]
    not.fix.outliers<-not.fix.outliers[!(not.fix.outliers %in% c(not.include,target))] 
    temp <- sapply(not.fix.outliers, function(x) {
      return (is.numeric(data[[x]])==TRUE)}
    ) 
    not.fix.outliers<-not.fix.outliers[which(temp==TRUE)]
  } else{
    not.fix.outliers<-NULL
  }
  
  return(list("not.fix"=not.fix,
              "not.fix.outliers"=not.fix.outliers))
} #End of Not.fix.columns
##################################
### NA assign###
na.assign<-function(vec,ind){
  if(length(ind)>0){
    vec[ind]<-NA
  } 
  return(vec)
} 

cleaned.vec<-function(vec){
  if(is.numeric(vec)){
    ind.na<-which(is.na(vec)==TRUE) 
    ind.inf<-which(is.infinite(vec)==TRUE)
    ind<-unique(c(ind.na,ind.inf))  
    if(length(ind)>0){
      return(vec[-ind])
    } else{
      return(vec)
    }
  } else{#is.factor(vec)
    temp<-as.character(vec)
    ind.na<-which(is.na(temp)==TRUE) 
    ind.blank<-which(temp %in% c("","NaN","#DIV/0!"))
    ind<-unique(c(ind.na,ind.blank))  
    if(length(ind)>0){
      return(vec[-ind])
    } else{
      return(vec)
    }
  }
}

cleaned.places<-function(vec){
  if(is.numeric(vec)){
  temp<-sapply(vec,function(x){
    return(!is.na(x) && !is.infinite(x))
  })
  } else{
  temp<-sapply(vec,function(x){
    x<-as.character(x)
    return(!is.na(x) && !(x %in% c("","NaN","#DIV/0!")))
  })
  }
  return(temp)
}

###Create is NA variables###
Create.is.na.vars.for.modeling<-function(data,not.include){
  Names<-names(data)
  vars<-NULL
  for(i in Names){
    if(i %in% not.include)
      next
    if(mean(is.na(data[[i]]))<0.05)  # && for.modeling=="Yes"
      next
    vars<-c(vars,i)
    data[,paste0("is.na",i)]<-as.numeric(is.na(data[[i]]))
  }
  return(list("data"=data,"vars"=vars))
}

Create.is.na.vars.for.prediction<-function(data,vars){
  for(i in vars)
    data[,paste0("is.na",i)]<-as.numeric(is.na(data[[i]]))
  return(data)
}
### Fix Data ###
Fix.Data<-function(data,target,not.include,chosen.not.fix,Imputation.Value,Outliers.settings){ 
  tryCatch({
    #not.fix
    #not.fix.outliers.numeric
    not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
    
    ### Dealing with missing values ###
    for(i in names(data)){   
      if(i %in% c(target,not.include)){next}
      if(is.numeric(data[[i]])==TRUE){
        data[[i]]<-as.numeric(data[[i]])
        ##Replace NA,NaN,Inf with the mean of the rest
        ind.na<-which(is.na(data[[i]])==TRUE) 
        ind.inf<-which(is.infinite(data[[i]])==TRUE)
        ind<-unique(c(ind.na,ind.inf))  
        if(length(ind)>0){
          if(Imputation.Value=="Mean")
            data[ind,i]<-mean(data[[i]][-ind])
          if(Imputation.Value=="Median")
            data[ind,i]<-median(data[[i]][-ind]) 
        }
      }
      if(is.factor(data[[i]])==TRUE){
        temp<-as.character(data[[i]])
        ##Replace NA,'NaN',Blank,#DIV/0! with Mode
        ind.na<-which(is.na(data[[i]])==TRUE) 
        ind.blank<-which(temp %in% c("","NaN","#DIV/0!"))
        ind<-unique(c(ind.na,ind.blank))  
        if(length(ind)>0)
          temp[ind]<-Mode(temp[-ind])
        data[,i]<-as.factor(temp)
      }
    }
    ### Dealing with Outliers###
    for(i in names(data)){   
      if((i %in% c(target,not.include))==TRUE){next}
      if(is.numeric(data[[i]])==TRUE){
        ##Replace Outliers with the median of the rest
        if(!(i %in% not.fix.outliers)){
          outliers.ind<-find.outliers(data[[i]],Outliers.settings)  
          if(length(outliers.ind)>0)
            data[outliers.ind,i]<-median(data[[i]][-outliers.ind], na.rm=TRUE)
        }
      }
    }
    return(data)
  },error=function(err){
    print(err$message)
    return(data)
  })
}  



#######Create Validation Table######
CV.Table<-function(x,y){
  testx<-xtabs(~x+y)
  if(dim(testx)[1] == 0 || dim(testx)[2] == 0){
    testx<-matrix(rep(0,4),ncol=2)
  } else{
    if (dim(testx)[1] == 1) {
      if(rownames(testx)=="0"){
        testx <- rbind(testx, c(0,0))
      } else{ #rownames(testx)=="1"
        testx <- rbind(c(0,0),testx)
      }
    }
    
    if (dim(testx)[2] == 1) {
      if(colnames(testx)=="0"){
        testx <- cbind(testx, c(0,0))
      } else{ #colnames(testx)=="1"
        testx <- cbind(c(0,0),testx)
      }
    }
  }
  rownames(testx)<-colnames(testx)<-c("0","1")
  return(testx)
}

#stratified shuffle
stratified.shuffle<-function(data,division.rate,not.include){
  tryCatch({
    data<-as.data.table(data)
    temp.data<-data[,names(data)[!names(data) %in% not.include],with=FALSE]
    temp.data<-temp.data[,names(temp.data)[sapply(names(temp.data),function(x){is.numeric(temp.data[[x]])})],with=FALSE]
    if(ncol(temp.data)==0)
      return(data)
    
    while(TRUE){
      num.of.folds<-5
      set.seed(12)
      groups<-kmeans(temp.data,num.of.folds)
      
      if(round(min(table(groups$cluster))*division.rate*0.1)>0)
        break
      
      num.of.folds<-num.of.folds-1
    }
    
    
    ind<-vector(mode="list",length=5)
    for(i in 1:num.of.folds){
      num<-length(which(groups$cluster==i))
      ##shuffle groups$cluster==i
      set.seed(12)
      temp<-sample(which(groups$cluster==i),size=num,replace=FALSE)
      ind[[1]]<-c(ind[[1]],temp[1:round(division.rate*0.7*num)])
      ind[[2]]<-c(ind[[2]],temp[(round(division.rate*0.7*num)+1):round(division.rate*0.8*num)])
      ind[[3]]<-c(ind[[3]],temp[(round(division.rate*0.8*num)+1):round(division.rate*0.9*num)])
      ind[[4]]<-c(ind[[4]],temp[(round(division.rate*0.9*num)+1):round(division.rate*num)])
      ind[[5]]<-c(ind[[5]],temp[(round(division.rate*num)+1):num])
    }
    
    Ind<-c(ind[[1]],ind[[2]],ind[[3]],ind[[4]],ind[[5]])
    #data<-data[Ind,]
    return(Ind)  #list("data"=data,"Ind"=Ind)
  },error=function(err){
    Ind<-1:nrow(data)
    return(Ind)
  })
}


#sampling from the data 
data_sample<-function(data,percent,Action){
  
  if(Action=="No")
    Output.data<-data
  
  if(Action=="Just Shuffle")
    Output.data<-data[sample(1:nrow(data),size=nrow(data),replace=FALSE),,drop=FALSE]
  
  if(Action=="Yes")
    Output.data<-data[sample(1:nrow(data),size=round((percent)*as.numeric(nrow(data))),replace=FALSE),,drop=FALSE]
  
  if(nrow(Output.data)>0)
    rownames(Output.data)<-1:nrow(Output.data)
  
  return(Output.data)
}

#######Delete rows with height rstandard
delete.rows.with.height.rstandard<-function(train,test,output.var,not.include){
  tryCatch({
    vars<-names(train)[!(names(train) %in% c(output.var,not.include))]
    myform <- as.formula(paste(as.symbol(output.var), " ~ ", paste(vars, collapse= "+")))
    model<-lm(myform,data=train)
    temp.data<-train
    rstandard <- abs(rstandard(model))
    temp.data<-cbind(temp.data,rstandard)
    temp.data<-temp.data[order(-temp.data$rstandard),]
    model1<-lm(myform,data=temp.data)
    model2<-lm(myform,data=temp.data[-1,])
    model3<-lm(myform,data=temp.data[-c(1,2),])
    model4<-lm(myform,data=temp.data[-c(1,2,3),])
    diff1<-mean(abs(test[[output.var]]-predict(model1,test,type='response')),na.rm=TRUE)/mean(abs(test[[output.var]]),na.rm=TRUE)
    diff2<-mean(abs(test[[output.var]]-predict(model2,test,type='response')),na.rm=TRUE)/mean(abs(test[[output.var]]),na.rm=TRUE)
    diff3<-mean(abs(test[[output.var]]-predict(model3,test,type='response')),na.rm=TRUE)/mean(abs(test[[output.var]]),na.rm=TRUE)
    diff4<-mean(abs(test[[output.var]]-predict(model4,test,type='response')),na.rm=TRUE)/mean(abs(test[[output.var]]),na.rm=TRUE)
    ind<-which.min(c(diff1,diff2,diff3,diff4))
    if(ind==1)
      return(temp.data[,!(names(temp.data) %in% "rstandard"),with=FALSE])
    if(ind==2)
      return(temp.data[-1,!(names(temp.data) %in% "rstandard"),with=FALSE])
    if(ind==3)
      return(temp.data[-c(1,2),!(names(temp.data) %in% "rstandard"),with=FALSE])
    if(ind==4)
      return(temp.data[-c(1,2,3),!(names(temp.data) %in% "rstandard"),with=FALSE])
  }, error=function(err){
    return(train)
  })
}
#######Finding best parameters for Rpart
find.best.parameters.for.rpart<-function(data,method,output.var,vars){
  #####
  set.seed('595')
  ind<-sample(1:nrow(data),size=round((2/3)*nrow(data)),replace=FALSE)
  train<-data[ind,]
  test<-data[-ind,]
  ##Combinations to check
  searchGrid = expand.grid(
    minsplit = c(10,20,30,40),
    cp=c(0.001,0.01),
    maxdepth=c(20,30)
  )
  
  formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(vars, collapse= "+"))) 
  ErrorsHyperparameters <- apply(searchGrid, 1, function(parameterList){
    
    #Extract Parameters to test
    currentMinsplit <- parameterList[["minsplit"]]
    currentCp <- parameterList[["cp"]]
    currentMaxdepth <- parameterList[["maxdepth"]]
    
    if(method=="Classification"){
      Model<-rpart(formula,data=train,method='class',control=rpart.control(minsplit=currentMinsplit,cp=currentCp,maxdepth=currentMaxdepth)) 
      prob<-round(predict(Model,newdata=test)[,'1'],round.digits)
      pred <- ifelse(prob>0.5, 1, 0)
      Table<-CV.Table(test[[output.var]],pred)
      error<-(Table[1,2]+Table[2,1])/sum(Table)  #misclassification
    } else{
      Model<-rpart(formula,data=train,control=rpart.control(minsplit=currentMinsplit,cp=currentCp,maxdepth=currentMaxdepth)) 
      pred<-round(predict(model,newdata=test),round.digits)
      error<-sqrt(mean((test[[output.var]]-pred)^2,na.rm =TRUE))
    }
    return(c(error,currentMinsplit,currentCp,currentMaxdepth)) 
  })
  
  ErrorsHyperparameters<-as.data.frame(ErrorsHyperparameters)
  rownames(ErrorsHyperparameters)<-c("error","minsplit","cp","maxdepth")
  ind.min<-which.min(ErrorsHyperparameters["error",])
  params<-as.list(ErrorsHyperparameters[c("minsplit","cp","maxdepth"),ind.min]) 
  names(params)<-c("minsplit","cp","maxdepth") 
  return(params)
}
#######Finding best parameters for xgboost ######
find.best.parameters.for.xgb<-function(dtrain,method,eval_metric_chosen,classification.threshold,base_score){
  ##Combinations to check
  searchGrid = expand.grid(
    eta=c(0.01,0.1,0.5),
    max_depth = c(6,8,10),
    min_child_weight=c(1,8,15),
    gamma=c(0,1,10),
    subsample=c(0.5, 0.75, 1)
  )
  
  if(method=="Classification"){
    obj<-"binary:logistic"
    if(eval_metric_chosen=="Error" && classification.threshold!=0.5){
      eval_metric<-paste0("error@",classification.threshold)
      test_val_name<-paste0("test_",eval_metric,"_mean")
    } else{
      eval_metric<-tolower(eval_metric_chosen)
      test_val_name<-paste0("test_",eval_metric,"_mean")
    }
  } else{ #Estimation
    obj<-"reg:linear"
    eval_metric<-"rmse"
    test_val_name<-"test_rmse_mean"
  }
  
  ErrorsHyperparameters <- apply(searchGrid, 1, function(parameterList){
    
    #Extract Parameters to test
    currentEta <- parameterList[["eta"]]
    currentMax_depthRate <- parameterList[["max_depth"]]
    currentMin_child_weightRate <- parameterList[["min_child_weight"]]
    currentGamma <- parameterList[["gamma"]]
    currentSubsampleRate <- parameterList[["subsample"]]
    
    set.seed(xgb.seed);xgboostModelCV <- xgb.cv(data =  dtrain, nrounds = 100, nfold = 5, 
                                          "base_score"=base_score,
                                          "eval_metric" = eval_metric,#verbose = FALSE, 
                                          "objective" = obj, 
                                          "eta" = currentEta, 
                                          "max_depth"=currentMax_depthRate,
                                          "min_child_weight"=currentMin_child_weightRate,
                                          "gamma"=currentGamma,
                                          "subsample" = currentSubsampleRate,
                                          "scale_pos_weight" = 1.6,
                                          "reg_alpha" = 8,
                                          "reg_lambda" = 1.3)
    
    if(eval_metric!='auc'){
      test_val<-min(as.data.frame(xgboostModelCV$evaluation_log)[,test_val_name])
      nround<-which.min(as.data.frame(xgboostModelCV$evaluation_log)[,test_val_name])
      return(c(test_val,nround,currentEta,currentMax_depthRate,currentMin_child_weightRate,currentGamma,
               currentSubsampleRate))
    } else{
      test_val<-max(as.data.frame(xgboostModelCV$evaluation_log)[,test_val_name])
      nround<-which.max(as.data.frame(xgboostModelCV$evaluation_log)[,test_val_name])
      return(c(test_val,nround,currentEta,currentMax_depthRate,currentMin_child_weightRate,currentGamma,
               currentSubsampleRate))
    }
  })
  
  ErrorsHyperparameters<-as.data.frame(ErrorsHyperparameters)
  rownames(ErrorsHyperparameters)<-c("test_val","nround","eta","max_depth","Min_child_weight","gamma","subsample")
  if(eval_metric!='auc'){
    ind.min<-which.min(ErrorsHyperparameters["test_val",])
    params<-as.list(ErrorsHyperparameters[c("eta","max_depth","Min_child_weight","gamma","subsample"),ind.min])
    nround<-ErrorsHyperparameters["nround",ind.min]
  } else{
    ind.max<-which.max(ErrorsHyperparameters["test_val",])
    params<-as.list(ErrorsHyperparameters[c("eta","max_depth","Min_child_weight","gamma","subsample"),ind.max]) 
    nround<-ErrorsHyperparameters["nround",ind.max]
  }
  names(params)<-c("eta","max_depth","Min_child_weight","gamma","subsample") 
  params[["objective"]]<-obj
  #####
  params[["scale_pos_weight"]]<-1.6
  params[["reg_alpha"]]<-8
  params[["reg_lambda"]]<-1.3
  ###
  return(list(params=params,nround=nround)) 
}
######ROC Calculation#########
calculate_roc1 <- function(pred,act, n) {
  
  tpr <- function(pred,act, threshold) {
    sum(pred >= threshold & act == 1) / sum(act == 1)
  }
  
  fpr <- function(pred,act, threshold) {
    sum(pred >= threshold & act == 0) / sum(act == 0)
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(pred,act, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(pred,act, th))
  
  return(roc)
}


cut.mean<-function(x){ 
  return(as.numeric(ifelse(x>mean(x,na.rm=T),1,0)))
} 
cut.4.pieces<-function(x){
  return(as.numeric(ifelse(x>mean(x,na.rm=T),ifelse(x>1.5*mean(x,na.rm=T),4,3),ifelse(x>0.5*mean(x,na.rm=T),2,1))))
}
Scale<-function(x){
  if(length(unique(x))<2)
    return(x)
  if(length(unique(x))>=2)
    return(as.numeric(scale(x,center=TRUE,scale=TRUE)))
}



#num of words
num.of.words<-function(C){
  return(sapply(C,function(i){
    if(is.na(i)){
      return(NA)
      } else{
    return(length(unlist(strsplit(as.character(i)," "))))
  }}))
}
#num of chars
num.of.chars<-function(C){
  return(nchar(gsub(" ", "", C)))
}




confusion_matrix <- function(dataframe,  plot.it = TRUE,  # cutoff= 0.2,
                             xlab = c("Actual = 0", "Actual = 1"),
                             ylab = c("Predicted = 0", "Predicted = 1"), title = NULL) {
  stopifnot(is.data.frame(dataframe) &&
              all(c('Predicted', 'Actual') %in% colnames(dataframe)))
  stopifnot(is.numeric(dataframe$Predicted) && is.numeric(dataframe$Actual))
  
  
  categories <- dataframe$Predicted * 2 + dataframe$Actual
  confusion <- matrix(tabulate(1 + categories, 4), nrow = 2)
  colnames(confusion) <- ylab
  rownames(confusion) <- xlab
  if (plot.it) fourfoldplot(confusion, color = c("#CC6666", "#99CC99"),
                            conf.level = 0, margin = 1, main = title)
  confusion 
} 


base.vars<-function(loop.vars,data){
if(is.null(loop.vars)){return(NULL)}
basic.vars<-NULL
for(i in loop.vars){
  if(i %in% names(data))
    basic.vars<-c(basic.vars,i)
  if(!(i %in% names(data))){
    for(j in names(data)){
      if(!is.na(grepl(j,i)) && grepl(j,i)==TRUE)
        basic.vars<-c(basic.vars,j)
    }
  }
}
basic.vars<-unique(basic.vars)
return(basic.vars)
}


standarDates <- function(string_col) {
  patterns = c('[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]','[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]','[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]')
  formatdates = c('%Y/%m/%d','%d/%m/%Y','%Y-%m-%d')
  standardformat='%Y-%m-%d'
  for(i in 1:3){
    if(length(string_col[!is.na(string_col)])==0){next}
    if(all(grepl(patterns[i], string_col[!is.na(string_col)]))){
      aux=as.Date(string_col,format=formatdates[i])
      return(as.Date(format(aux, standardformat)))
    }
  }
  return(string_col)
}




New_Data_prediction_error<-function(basic.vars,newdata){
  if(!all(basic.vars %in% names(newdata))){
    var<-basic.vars[!(basic.vars %in% names(newdata))]
    table<-data.frame("Variables"=var)
    return(table)
  } else{
    return(NULL)
  }}


split <-function(path){
  parts <- strsplit(path, "\\.")[[1]]
  last <- parts[length(parts)]
  return(last)
}



combined_prob<-function(vec){
  ind.clean_from_vec<- !complete.cases(vec)
  ind.clean_from_vec<-which(ind.clean_from_vec==TRUE)
  if(length(ind.clean_from_vec)>0)
    vec<-vec[-ind.clean_from_vec]
  if(length(vec)>0){
    prob<-round(mean(vec),round.digits)
  } else{
    prob<-NA
  }
    return(prob)
  }

weighted.prob<-function(x,weights){
  if(is.null(x))
    return(NULL)
  mutual.names<-intersect(names(x),names(weights))
 prob<-rep(0,length(x[[1]]))
  for(i in mutual.names)
  prob<-prob+x[[i]]*weights[i]/sum(weights[mutual.names])
 prob<-round(prob,round.digits)
   return(prob)
}

get_val_ggvis <- function(x) {
  if (is.null(x)) return(NULL)
   paste("Value is",x$Value)
}
 
rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,,drop=FALSE]
  return(a1.without.a2.rows)
}


write.all.combinations<-function(x,n){
  x<-unlist(strsplit(as.character(x)," "))
  signs<-c("`","~","!","@","#","$","%","^","&","*","(",")","-","_","+","=",
           "[","]","{","}","\\","|","'",":",";","/","?",".",",",">","<")
  if(is.na(x) || length(x)<n)
    return(NULL)
  all.vals<-NULL
  for(i in 1:(length(x)-n+1)){
    num.of.signs<-sum(signs %in% x[i:(i+n-1)])
    if(i+n-1+num.of.signs>length(x))
      next
    all.vals<-c(all.vals,paste(x[i:(i+n-1+num.of.signs)],collapse=" "))
  }
  return(all.vals[!is.na(all.vals)])
}

with.and.without.signs<-function(vec){
  signs<-c("`","~","!","@","#","$","%","^","&","*","(",")","-","_","+","=",
           "[","]","{","}","\\","|","'",":",";","/","?",".",",",">","<")
  add<-unlist(lapply(vec,function(x){
    temp<-unlist(strsplit(x,""))
    temp.vec<-NULL
    if(temp[1] %in% signs)
      temp.vec<-c(temp.vec,paste(temp[-1],collapse=""))
    if(temp[length(temp)] %in% signs)
      temp.vec<-c(temp.vec,paste(temp[-length(temp)],collapse=""))
    if(temp[1] %in% signs && temp[length(temp)] %in% signs)
      temp.vec<-c(temp.vec,paste0(temp[-c(1,length(temp))],collapse=""))
    return(temp.vec)
  }))
  return(c(vec,add))
}

turn.text.to.data<-function(Data,Pos.Exp.table,Neg.Exp.table,Other.Exp.table,table){
  tryCatch({
    Pos.Exp<-unique(tolower(Pos.Exp.table[,"Positive Expressions"]))
    Neg.Exp<-unique(tolower(Neg.Exp.table[,"Negative Expressions"]))
    Other.Exp<-unique(tolower(Other.Exp.table[,"Other Expressions"]))
    
    Pos.Exp<-as.character(Pos.Exp[complete.cases(Pos.Exp)])
    Neg.Exp<-as.character(Neg.Exp[complete.cases(Neg.Exp)])
    Other.Exp<-as.character(Pos.Exp[complete.cases(Other.Exp)])
    
    Pos.Exp<-gsub("^\\s+|\\s+$", "", Pos.Exp)
    Neg.Exp<-gsub("^\\s+|\\s+$", "", Neg.Exp)
    Other.Exp<-gsub("^\\s+|\\s+$", "", Other.Exp)
    
    all.sizes.Pos.Exp<-NULL
    all.sizes.Neg.Exp<-NULL
    all.sizes.Other.Exp<-NULL
    ###make names unique
    temp<-table[,"Name"]
    ind<-which(temp!="")
    if(length(ind)>0){
      temp<-temp[ind]
      table[ind,"Name"]<-make.names(temp,unique = TRUE)
    }  
    vars<-unique(table[,"Variable"][table[,"Variable"]!="" & table[,"Name"]!=""])
    vars<-vars[vars %in% colnames(Data)]
    if(length(vars)>0)
      vars<-vars[sapply(vars,function(x){is.factor(Data[[x]])})]
    ##For CV and Predict Data it would be is.character because we let the function work before converting
    ##char to factor
    if(is.null(vars) || length(vars)==0){
      return(Data)
    }
    
    for(i in vars){
      vec<-as.character(Data[[i]])
      ###Num of words/ Num of chars
      if(!paste0('num_of_words_',i) %in% colnames(Data))
        Data[,paste0('num_of_words_',i)]<-num.of.words(vec)
      if(!paste0('num_of_chars_',i) %in% colnames(Data))
        Data[,paste0('num_of_chars_',i)]<-num.of.chars(vec)
      ###  
      table.per.var<-table[table[,"Variable"]==i & table[,"Name"]!="",]
      all.comb<-list()
      for(row.num in 1:nrow(table.per.var)){
        temp.table<-table.per.var[row.num,]
        #################
        Name<-temp.table[,"Name"]  
        Isolated.Strings<-temp.table[,"Isolated String"][temp.table[,"Isolated String"]!=""]
        Count.Pos.Exp<-temp.table[,"Count Pos Exp"][temp.table[,"Count Pos Exp"]!=""]
        Count.Neg.Exp<-temp.table[,"Count Neg Exp"][temp.table[,"Count Neg Exp"]!=""]
        Count.Other.Exp<-temp.table[,"Count Other Exp"][temp.table[,"Count Other Exp"]!=""]
        Regex<-temp.table[,"Regex"][temp.table[,"Regex"]!=""]
        Replace.Regex<-temp.table[,"Replace Regex"][temp.table[,"Replace Regex"]!=""]
        Replacement<-temp.table[,"Replacement"][temp.table[,"Replacement"]!=""]
        As.Variables<-temp.table[,"As Variable"][temp.table[,"As Variable"]!=""]
        Num.of.words.after<-as.numeric(temp.table[,"Num of words after"][temp.table[,"Num of words after"]!=""])
        Catch.till<-temp.table[,"Catch till"][temp.table[,"Catch till"]!=""]
        As.Variables.using.regex<-temp.table[,"As Variable using regex"][temp.table[,"As Variable using regex"]!=""]
        #################
        sizes<-NULL
        if(length(Isolated.Strings)>0){
          Isolated.Strings<-gsub("^\\s+|\\s+$", "", Isolated.Strings)
          for(str in Isolated.Strings)
            sizes<-c(sizes,length(unlist(strsplit(str," "))))
        }
        
        if("Yes" %in% Count.Pos.Exp){
          if(is.null(all.sizes.Pos.Exp))
            all.sizes.Pos.Exp<-unlist(lapply(Pos.Exp,function(str){
              return(length(unlist(strsplit(str," "))))
            }))
          sizes<-c(sizes,unique(all.sizes.Pos.Exp))
        }
        
        if("Yes" %in% Count.Neg.Exp){
          if(is.null(all.sizes.Neg.Exp))
            all.sizes.Neg.Exp<-unlist(lapply(Neg.Exp,function(str){
              return(length(unlist(strsplit(str," "))))
            }))
          sizes<-c(sizes,unique(all.sizes.Neg.Exp))
        }
        
        if("Yes" %in% Count.Other.Exp){
          if(is.null(all.sizes.Other.Exp))
            all.sizes.Other.Exp<-unlist(lapply(Other.Exp,function(str){
              return(length(unlist(strsplit(str," "))))
            }))
          sizes<-c(sizes,unique(all.sizes.Other.Exp))
        }
        
        if(length(As.Variables)>0){
          var<-gsub("^\\s+|\\s+$", "", var)
          for(var in As.Variables)
            sizes<-c(sizes,length(unlist(strsplit(str," "))))
        }
        
        sizes<-unique(sizes)
        for(n in sizes){
          if(is.null(all.comb[[paste(n)]]))
            all.comb[[paste(n)]]<-lapply(vec,function(x){
              return(tolower(with.and.without.signs(write.all.combinations(x,n))))
            })
        }
        #################
        for(str in tolower(Isolated.Strings)){ ##insensitive
          tryCatch({
            str<-gsub("^\\s+|\\s+$", "", str)
            n<-length(unlist(strsplit(str," ")))
            Data[,paste0(Name,"_str")]<-unlist(lapply(all.comb[[paste(n)]],function(x){
              return(sum(x==str))
            }))
          }, error=function(err){})
        }
        
        if("Yes" %in% Count.Pos.Exp){
          tryCatch({
            sum<-vector(mode="numeric",length=length(vec))
            for(n in unique(all.sizes.Pos.Exp)){
              temp.Pos.Exp<-Pos.Exp[which(all.sizes.Pos.Exp==n)]
              ###all arguments in temp.Other.Exp are of the same size
              sum<-sum+unlist(lapply(all.comb[[paste(n)]],function(x){
                sum(x %in% temp.Pos.Exp)
              }))
            }
            Data[,paste0(Name,"_Count.Pos.Exp")]<-sum
          }, error=function(err){})
        }
        
        if("Yes" %in% Count.Neg.Exp){
          tryCatch({
            sum<-vector(mode="numeric",length=length(vec))
            for(n in unique(all.sizes.Neg.Exp)){
              temp.Pos.Exp<-Neg.Exp[which(all.sizes.Neg.Exp==n)]
              ###all arguments in temp.Other.Exp are of the same size
              sum<-sum+unlist(lapply(all.comb[[paste(n)]],function(x){
                sum(x %in% temp.Neg.Exp)
              }))
            }
            Data[,paste0(Name,"_Count.Neg.Exp")]<-sum
          }, error=function(err){})
        }
        
        if("Yes" %in% Count.Other.Exp){
          tryCatch({
            sum<-vector(mode="numeric",length=length(vec))
            for(n in unique(all.sizes.Other.Exp)){
              temp.Other.Exp<-Other.Exp[which(all.sizes.Other.Exp==n)]
              ###all arguments in temp.Other.Exp are of the same size
              sum<-sum+unlist(lapply(all.comb[[paste(n)]],function(x){
                sum(x %in% temp.Other.Exp)
              }))
            }
            Data[,paste0(Name,"_Count.Other.Exp")]<-sum
          }, error=function(err){})
        }
        
        for(reg in Regex){ 
          tryCatch({
            temp<-lapply(vec,function(x){length(unlist(str_extract_all(pattern=reg,x)))})
            Data[,paste0(Name,"_reg")]<-unlist(temp)
          }, error=function(err){})
        }
        
        for(reg in Replace.Regex){     
          tryCatch({
            temp<-str_replace_all(vec,pattern=reg,replacement = Replacement)
            Data[,paste0(Name,"_Replace.Regex")]<-gsub("^\\s+|\\s+$", "", temp)
          }, error=function(err){})
        }
        
        for(var in tolower(As.Variables)){
          tryCatch({
            if(length(Num.of.words.after)>0 && length(Catch.till)>0)
              next
            var<-gsub("^\\s+|\\s+$", "", var)
            n<-length(unlist(strsplit(var," ")))
            ind<-unlist(lapply(all.comb[[paste(n)]],function(x){
              return(var %in% x)
            }))
            ind<-which(ind==TRUE)
            if(length(ind)==0)
              next
            if(length(Num.of.words.after)>0){  
              temp<-vec[ind]
              signs<-c("`","~","!","@","#","$","%","^","&","*","(",")","-","_","+","=",
                       "[","]","{","}","\\","|","'",":",";","/","?",".",",",">","<")
              temp<-unlist(lapply(temp,function(x){
                ans<-unlist(strsplit(x,var,fixed=TRUE))[-1]
                if(length(ans)==0)
                  retrurn(NA)
                ans<-gsub("^\\s+|\\s+$", "", ans)
                level<-character(0)
                for(i in 1:length(ans)){
                  temp.ans<-ans[i]
                  temp.ans<-unlist(strsplit(as.character(temp.ans)," "))
                  num.of.signs<-sum(signs %in% temp.ans)
                  
                  if(length(temp.ans)<(Num.of.words.after+num.of.signs))
                    next
                  if(i==1){
                    level<-paste0(level, paste(temp.ans[1:(Num.of.words.after+num.of.signs)],collapse=" "))
                  } else{
                    level<-paste0(level,paste0("/", paste(temp.ans[1:(Num.of.words.after+num.of.signs)],collapse=" ")))
                  }
                }
                return(level)
              }))
              Data[,paste0(Name,"_var")]<-NA
              Data[[paste0(Name,"_var")]][ind]<-temp
            }
            if(length(Catch.till)>0){
              temp<-vec[ind]
              temp<-unlist(lapply(temp,function(x){
                x<-unlist(strsplit(x,Catch.till,fixed=TRUE))
                level<-character(0)
                for(i in 1:length(x)){
                  ans<-unlist(strsplit(x[i],var,fixed=TRUE))[2]
                  if(is.na(ans))
                    next
                  ans<-gsub("^\\s+|\\s+$", "", ans)
                  if(i==1){
                    level<-paste0(level,ans)
                  } else{
                    level<-paste0(level,paste0("/",ans))
                  }
                }
                return(level)
              }))
              Data[,paste0(Name,"_var")]<-NA
              Data[[paste0(Name,"_var")]][ind]<-temp
            }
          }, error=function(err){})
        }
        
        for(reg in As.Variables.using.regex){ 
          tryCatch({
            temp<-lapply(vec,function(x){
              return(paste(unlist(str_extract_all(pattern=reg, x)),collapse="/"))
            })
            Data[,paste0(Name,"_var_using_reg")]<-unlist(temp)
          }, error=function(err){})
        }
        #######
      }#End for(row.num in 1:nrow(temp.table))
    }#End for(i in vars)
    return(Data)
  }, error=function(err){
    return(Data)
  })
}
##the length of decimal number after the dot
#decim.num<-function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1} 

turn.ratio.combination.to.data<-function(Data,table,is.interval){
  if(is.null(Data) || is.null(table) || is.null(is.interval)){return(NULL)}
  if("Variable" %in% names(table)){
    ratio.comb.data<-lapply(1:nrow(table),function(i){
      level<-as.character(table[i,"Level"])
      var<-as.character(table[i,"Variable"])
      num<-as.numeric(paste(table[i,"Ratio of success"]))
      name<-paste0("comb",i,"_",var)
      if(is.interval[var]=="Yes"){
        interval<-level %>% str_match_all("[0-9.Inf]+") %>% unlist %>% as.numeric
        col<-data.frame(ifelse(Data[[var]]>interval[1] & Data[[var]]<=interval[2],num,0))  
      } else{
        col<-data.frame(ifelse(Data[[var]]==level,num,0))
      }
      names(col)<-name
      return(col)
    })
    
    ratio.comb.data<-do.call("cbind",ratio.comb.data)
    char_chosen_for_ratio.as.dummies<-unique(as.character(table[,"Variable"]))
  } else{
    ratio.comb.data<-lapply(1:nrow(table),function(i){
      var1<-as.character(table[i,"Variable 1"])
      level1<-as.character(table[i,"Level 1"])
      var2<-as.character(table[i,"Variable 2"])
      level2<-as.character(table[i,"Level 2"])
      num<-as.numeric(paste(table[i,"Ratio of success"]))
      name<-paste0("comb",i,"_",var1,"_",var2)
      if(is.interval[var1]=="Yes"){
        interval<-level1 %>% str_match_all("[0-9.Inf]+") %>% unlist %>% as.numeric
        cond1<-Data[[var1]]>interval[1] & Data[[var1]]<=interval[2]
      } else{
        cond1<-Data[[var1]]==level1 
      }
      
      if(is.interval[var2]=="Yes"){
        interval<-level2 %>% str_match_all("[0-9.Inf]+") %>% unlist %>% as.numeric
        cond2<-Data[[var2]]>interval[1] & Data[[var2]]<=interval[2]
      } else{
        cond2<-Data[[var2]]==level2 
      }
      col<-data.frame(ifelse(cond1 & cond2,num,0))
      names(col)<-name
      return(col)
    })
    ratio.comb.data<-do.call("cbind",ratio.comb.data)
    char_chosen_for_ratio.as.dummies<-unique(c(as.character(table[,"Variable 1"]),as.character(table[,"Variable 2"])))
  }
  if(ncol(ratio.comb.data)==nrow(table))
  ratio.comb.data[,"max_of_ratio_comb"]<-apply(ratio.comb.data,1,max)
  Data<-cbind(Data,ratio.comb.data)#as.data.frame(cbind(Data,ratio.comb.data))

  return(Data)
}


###########Model Dependant##################
predict.model<-function(model,myform,test,thresold,model.type,method){ 

  prob<-NULL
  pred<-NULL 

  if(method=="Classification"){
    if(model.type %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic")){
      prob<-round(predict(model,newdata=test,type="response"),round.digits)
      pred<-ifelse(prob>thresold, 1, 0)  
    }
    if(model.type %in% c("Xgboost","Naive Xgboost")){
      Test<- sparse.model.matrix(myform, test) 
      prob <- round(predict(model,Test),round.digits) 
      pred <-  ifelse(prob>thresold, 1, 0) 
    }
    if(model.type %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree")){
      prob<-round(predict(model,newdata=test)[,'1'],round.digits)
      pred <- ifelse(prob>thresold, 1, 0)
    }
    if(model.type %in% c("Rforest","Naive Rforest")){
      prob<-round(predict(model,test)$predictions[,'1'],round.digits)
      pred <- ifelse(prob>thresold, 1, 0)
    }
    if(model.type %in% c("Neural Network","Naive Neural Network")){
      Test<- sparse.model.matrix(myform, test)
      prob<- tryCatch({
        round(t(predict(model, data.matrix(Test)))[,2],round.digits)  
      }, error=function(err){
        return(round(t(predict(model, Test))[,2],round.digits))
      })
      pred <-  ifelse(prob>thresold, 1, 0)
    }
    output<-list(prob=prob,pred=pred)
  }
  if(method=="Estimation"){
    if(model.type %in% c("Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                         "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
      pred<-round(predict(model,newdata=test,type='response'),round.digits)
    }
    if(model.type %in% c("Xgboost","Naive Xgboost")){
      Test<- sparse.model.matrix(myform, test) 
      pred <- round(predict(model,Test),round.digits) 
    }
    if(model.type %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree")){
      pred<-round(predict(model,newdata=test),round.digits)
    }
    if(model.type %in% c("Rforest","Naive Rforest")){
      pred<-round(predict(model,test)$predictions,round.digits)
    }
    if(model.type %in% c("Neural Network","Naive Neural Network")){
      Test<- sparse.model.matrix(myform, test)
      pred<-tryCatch({
        round(as.numeric(predict(model,data.matrix(Test))),round.digits)   
      }, error=function(err){
        return(round(as.numeric(predict(model,Test)),round.digits))
      }) 
    }
    output<-list(pred=pred)
  }
  return(output)
} 



Myform<-function(output.var,loop.vars,char){
  if(char %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic","Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                 "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile","Recursive Partitioning Tree","Naive Recursive Partitioning Tree","Rforest","Naive Rforest"))
    return(as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"))))
  if(char %in% c("Xgboost","Naive Xgboost","Neural Network","Naive Neural Network"))
    return(as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"),"-1")))
  
}

##transfer the range to [0,1]
range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
##################################
##For Source Listener
##################################
Source.prediction<-function(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                            levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                            uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                            necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved){
  if(!is.null(Users.data) && !is.null(Progression.Vars)){
    #####
    Users.data<-as.data.table(Users.data)
    if(all(c('latitude', 'longitude') %in% tolower(names(Users.data)))){
      for(i in 1:8){
        Users.data[,paste0("latitude_",i)]<-round(Users.data[[which(tolower(names(Users.data))=="latitude")]],i)
        Users.data[,paste0("longitude",i)]<-round(Users.data[[which(tolower(names(Users.data))=="longitude")]],i)
      }
    }
    ##Converting integer64\Logical to numeric
    ind<-sapply(names(Users.data), function(x) {class(Users.data[[x]])[1] %in% c("logical","integer64")})
    for(i in names(Users.data)[ind]){Users.data[,i]<-as.numeric(Users.data[[i]])}   
    
    ##Converting character to Factor
    ind<-sapply(names(Users.data), function(x) {"character" %in% class(Users.data[[x]])})
    for(i in names(Users.data)[ind]){Users.data[,i]<-as.factor(Users.data[[i]])}  
    
    ####Class Date####
    ##Adding columns concerning dates
    ind1<-sapply(names(Users.data), function(x) {"Date" %in% class(Users.data[[x]])})
    ind2<-sapply(names(Users.data), function(x) {"POSIXct" %in% class(Users.data[[x]])})

      for(i in names(Users.data)[ind1]){
        Users.data[,paste0(i,"_day")]<-as.factor(format(Users.data[[i]],"%d"))
        Users.data[,paste0(i,"_week")]<-as.factor(format(Users.data[[i]],"%a"))
        Users.data[,paste0(i,"_month")]<-as.factor(format(Users.data[[i]],"%b"))
        Users.data[,paste0(i,"_quarter")]<-as.factor(quarters(Users.data[[i]]))
        Users.data[,paste0(i,"_month_day")]<-as.factor(format(Users.data[[i]],"%b %d"))
        Users.data[,paste0(i,"_year")]<-as.factor(format(Users.data[[i]],"%Y"))
        
        Users.data[,i]<-as.factor(Users.data[[i]])
      }
      for(i in names(Users.data)[ind2]){
        Users.data[,paste0(i,"_sec")]<-as.factor(format(Users.data[[i]],"%S"))
        Users.data[,paste0(i,"_min")]<-as.factor(format(Users.data[[i]],"%M"))
        Users.data[,paste0(i,"_hour")]<-as.factor(format(Users.data[[i]],"%H"))
        Users.data[,paste0(i,"_day")]<-as.factor(format(Users.data[[i]],"%d"))
        Users.data[,paste0(i,"_month")]<-as.factor(format(Users.data[[i]],"%m"))
        Users.data[,paste0(i,"_year")]<-as.factor(format(Users.data[[i]],"%Y"))
        
        Users.data[,i]<-as.factor(Users.data[[i]])
      }  

    ##Text
    if(!is.null(text.analysis.for.export.saved)){
      Pos.Exp.table<-text.analysis.for.export.saved[["Pos.Exp.table"]]
      Neg.Exp.table<-text.analysis.for.export.saved[["Neg.Exp.table"]]
      Other.Exp.table<-text.analysis.for.export.saved[["Other.Exp.table"]]
      table<-text.analysis.for.export.saved[["table"]]
      Users.data<-turn.text.to.data(Users.data,Pos.Exp.table,Neg.Exp.table,Other.Exp.table,table) 
    }
    
    colnames(Users.data)<-make.names(colnames(Users.data),unique=TRUE) 
    #####
    output.var<-params.to.assign[["output.var"]] 
    vars.not.include<-params.to.assign[["vars.not.include"]] 
    chosen.not.fix<-params.to.assign[["chosen.not.fix"]]
    not.fix.vars<-chosen.not.fix[["not.fix"]]
    Imputation.Value<-Settings_Table[["Imputation Value"]]
    if(Method.used=="Classification")
      classification.threshold<-Settings_Table[["Class Threshold"]]
    ####Keeping the original data for output
    Original.SPY.Predict<-Users.data
    SPY.Predict<-Users.data
    ###Checking if the variables have the same class####
    mutual.names<-names(SPY.Predict)[names(SPY.Predict) %in% names(uploaded.data)]
    if(length(mutual.names)>0){
      class.data<-sapply(mutual.names,function(x){return(class(uploaded.data[[x]]))})
      class.newdata<-sapply(mutual.names,function(x){return(class(SPY.Predict[[x]]))})
      int1<-which(class.data=="integer")
      if(length(int1)>0)
        class.data[int1]<-"numeric"
      int2<-which(class.newdata=="integer")
      if(length(int2)>0)
        class.newdata[int2]<-"numeric"
      ind.different.classes<-which(class.data!=class.newdata)
      var<-mutual.names[ind.different.classes]
      if(length(var)>0){
        Type.requested<-class.data[ind.different.classes]
        Type.exist<-class.newdata[ind.different.classes]
        for(i in 1:length(var)){
          if(Type.requested[i]=="numeric")
            SPY.Predict[,var[i]]<-as.numeric(paste(SPY.Predict[[var[i]]]))
          if(Type.requested[i]=="factor")
            SPY.Predict[,var[i]]<-as.factor(SPY.Predict[[var[i]]])
        }
      }
    }

    #####################################
    ##Add ratio combination as dummies
    if(ratio.as.dummies.inside[["Add"]]=="Yes"){
      char<-ratio.as.dummies.inside[["char"]]
      is.interval<-ratio.as.dummies.inside[["is.interval"]]
      err<-New_Data_prediction_error(char,SPY.Predict)
      if(is.null(err)){
        table<-ratio.as.dummies.inside[["table"]]
        Data<-SPY.Predict
        SPY.Predict<-turn.ratio.combination.to.data(Data,table,is.interval)
      }} 
    #####################################
    ######Turn Factors to Numeric########
    if(!is.null(levels.table)){
      temp<-levels.table
      for(i in names(temp)){
        if(i %in% names(SPY.Predict))
          SPY.Predict[,i]<-turn.fac.to.num(SPY.Predict[[i]],temp[[i]])
      }
    }
    ######################################
    if(!is.null(Create.is.na.vars))
      SPY.Predict<-Create.is.na.vars.for.prediction(SPY.Predict,Create.is.na.vars)
    #######Clean the Data
    SPY.Predict<-Fix.Data(SPY.Predict,output.var,vars.not.include,chosen.not.fix,Imputation.Value,Outliers.settings.saved)
    ###First Layer
    if(!is.null(necessary.variables.found.for.first.layer))
      SPY.Predict<-First.layer.prediction(SPY.Predict,necessary.variables.found.for.first.layer,Used.Models.for.first.layer)
    ################################################################
    temp.data<-list()
    temp.data.b<-list()
    basic.names<-names(Original.SPY.Predict)
    models.used<-Used.Models[["models.used"]]
    best.models.used<-Used.Models[["best.models.used"]]
    models.used.weights<-Used.Models[["models.used.weights"]]
    best.models.used.weights<-Used.Models[["best.models.used.weights"]]
    ############
    for(i in models.used){
      tryCatch({
        loop.vars<-Progression.Vars$Vars[[i]] 
        basic.vars<-base.vars(loop.vars,SPY.fixed)
        err<-New_Data_prediction_error(basic.vars,SPY.Predict)
        if(is.null(err)){
          temp.not.fix<-not.fix.vars[not.fix.vars %in% basic.vars]
          not.fix.ind<-sapply(Original.SPY.Predict[,temp.not.fix,with=FALSE],function(x){
            if(is.numeric(x)){
              ind<-union(which(is.na(x)),which(is.infinite(x)))
            } else{
              ind<-union(which(is.na(x)),which(x %in% c("","NaN","#DIV/0!")))
            }
            return(ind)
          })
          not.fix.ind<-unique(as.numeric(unlist(not.fix.ind)))
          ##### 
          model<-Progression.Vars$Models[[i]]
          myform<-Progression.Vars$Myform[[i]]
          if(Method.used=="Classification"){
            prob<-predict.model(model,myform,SPY.Predict,classification.threshold,i,"Classification")[["prob"]]  
            prob<-na.assign(prob,not.fix.ind)
            if(!all(is.na(prob))){
              temp.data[[i]]<-prob
              if(i %in% best.models.used)
                temp.data.b[[i]]<-prob
              prediction<- ifelse(prob>classification.threshold, 1, 0)
              Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_probability")]<-prob
              Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
            }} else{
              prediction<-predict.model(model,myform,SPY.Predict,NULL,i,"Estimation")[["pred"]] 
              prediction<-na.assign(prediction,not.fix.ind)
              if(!all(is.na(prediction))){
                temp.data[[i]]<-prediction
                if(i %in% best.models.used)
                  temp.data.b[[i]]<-prediction
                Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
                Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_lower_limit")]<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
                Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_upper_limit")]<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
              }}
        } 
      }, error=function(err){})
    }#End of loop
    ###########################
    if(length(temp.data)>1){
      if(Method.used=="Classification"){
        tryCatch({
          prob<-apply(as.data.frame(temp.data),1,combined_prob)   
          prediction<- ifelse(prob>classification.threshold, 1, 0)
          Original.SPY.Predict$Ensemble.all.models_predict_probability<-prob
          Original.SPY.Predict$Ensemble.all.models_predict_response<-prediction
        }, error=function(err){})
        ##################################
        tryCatch({  
          prob<-weighted.prob(temp.data,models.used.weights)
          prediction<- ifelse(prob>classification.threshold, 1, 0)
          Original.SPY.Predict$Ensemble.all.models.weighting_predict_probability<-prob
          Original.SPY.Predict$Ensemble.all.models.weighting_predict_response<-prediction
        }, error=function(err){})
      } else{
        tryCatch({
          prediction<-apply(as.data.frame(temp.data),1,function(x){median(x,na.rm=TRUE)})
          Original.SPY.Predict$Ensemble.all.models_predict_response<-prediction
          Original.SPY.Predict$Ensemble.all.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
          Original.SPY.Predict$Ensemble.all.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
        }, error=function(err){})
      }
    }
    if(length(temp.data.b)>1){
      if(Method.used=="Classification"){
        tryCatch({
          prob<-apply(as.data.frame(temp.data.b),1,combined_prob)
          prediction<- ifelse(prob>classification.threshold, 1, 0)
          Original.SPY.Predict$Ensemble.best.models_predict_probability<-prob
          Original.SPY.Predict$Ensemble.best.models_predict_response<-prediction
        }, error=function(err){})
        ##################################
        tryCatch({  
          prob<-weighted.prob(temp.data.b,best.models.used.weights)
          prediction<- ifelse(prob>classification.threshold, 1, 0)
          Original.SPY.Predict$Ensemble.best.models.weighting_predict_probability<-prob
          Original.SPY.Predict$Ensemble.best.models.weighting_predict_response<-prediction
        }, error=function(err){})
      } else{
        tryCatch({
          prediction<-apply(as.data.frame(temp.data.b),1,function(x){median(x,na.rm=TRUE)})
          Original.SPY.Predict$Ensemble.best.models_predict_response<-prediction
          Original.SPY.Predict$Ensemble.best.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
          Original.SPY.Predict$Ensemble.best.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
        }, error=function(err){})
      }
    }
   if(choose_Method_for_Source.listener!="All Models"){
      method<-choose_Method_for_Source.listener
      if(Method.used=="Classification"){
        basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_probability"),paste0(gsub(" ",".",method),"_predict_response"))
      } else{
        basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_response"),paste0(gsub(" ",".",method),"_predict_lower_limit"),
                       paste0(gsub(" ",".",method),"_predict_upper_limit"))
      }
      Original.SPY.Predict<-Original.SPY.Predict[,names(Original.SPY.Predict)[names(Original.SPY.Predict) %in% basic.names],with=FALSE]
   }
    return(Original.SPY.Predict)
  } else{
    return(NULL)
  }
}

####First layer prediction
First.layer.prediction<-function(Data,Progression.Vars,Used.Models.for.first.layer){
models.used<-Used.Models.for.first.layer[["models.used"]]
best.models.used<-Used.Models.for.first.layer[["best.models.used"]]
####
temp.data<-list()
temp.data.b<-list()

for(i in models.used){
  tryCatch({
    myform<-Progression.Vars$Myform[[i]]
    loop.vars<-Progression.Vars$Vars[[i]]
    model<-Progression.Vars$Models[[i]]
    prediction<-predict.model(model,myform,Data,classification.threshold,i,"Estimation")[["pred"]] 
    #For comb models
    temp.data[[i]]<-prediction
    if(i %in% best.models.used)
      temp.data.b[[i]]<-prediction
    Data[,paste0(gsub(" ",".",i),"_predict_response")]<- prediction
  }, error=function(err){}) 
}

if(length(temp.data)>1){
  tryCatch({
    prediction<-apply(as.data.frame(temp.data),1,function(x){median(x,na.rm=TRUE)})
    Data$Ensemble.all.models_predict_response<-prediction
  }, error=function(err){})
}
if(length(temp.data.b)>1){
  tryCatch({
    prediction<-apply(as.data.frame(temp.data.b),1,function(x){median(x,na.rm=TRUE)})
    Data$Ensemble.best.models_predict_response<-prediction
  }, error=function(err){})
}
return(Data)
}


transfer.to.secs<-function(num,unit){
  if(unit=="mins")
    num<-num*60
  if(unit=="hours")
    num<-num*60*60
  if(unit=="days")
    num<-num*60*60*24
  return(num)
}


string.for.tooltip<-function(ind){
  string<-character(0)
  for(i in ind){  #0:n
    string<-c(string,paste(
      "var full_text=",paste0("aData[",i,"]"),"\n",
      "$('",paste0("td:eq(",i,")"),"', nRow).attr('title', full_text);\n"
    ))
  }
  return(string)
}


important.vars.to.keep<-function(model,Model.type){
  if(Model.type %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
    "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
    "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
    vars.to.keep<-tryCatch({
      importance<-summary(model,se="boot")
      importance<-as.data.frame(importance$coefficients)  
      importance<-importance[!rownames(importance) %in% "(Intercept)",,drop=FALSE]
      if("z value" %in% names(importance))
        importance<-importance[,"z value",drop=FALSE]
      if("t value" %in% names(importance))
        importance<-importance[,"t value",drop=FALSE]
      names(importance)<-"Overall"
      importance[,"Overall"]<-abs(importance[,"Overall"])
      importance<-importance[order(-importance$Overall),,drop=FALSE]
      if(nrow(importance)>3){
        temp<-rownames(importance)[1:floor((2/3)*nrow(importance))]
      } else{
        temp<-rownames(importance)
      }
      temp
    }, error=function(err){
      return(NULL)
    })
  } ##End of regressions
  if(Model.type %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree")){
    vars.to.keep<-tryCatch({
    importance<-model$splits
    if(nrow(importance)>0){
      importance<-aggregate(improve~rownames(importance),data=importance,FUN=sum)
      colnames(importance)[colnames(importance)=="rownames(importance)"]<-"Feature"
      colnames(importance)[colnames(importance)=="improve"]<-"Overall"
      importance<-importance[importance$Overall>0,,drop=FALSE]
      importance<-importance[order(-importance$Overall),,drop=FALSE]
      ##
      if(nrow(importance)>3){
        temp<-rownames(importance)[1:floor((2/3)*nrow(importance))]
      } else{
        temp<-rownames(importance)
      }
      temp
    } else{#nrow(importance)==0
      temp<-NULL
    }
    temp
  }, error=function(err){
    return(NULL)
  })
  }
  if(Model.type %in% c("Rforest","Naive Rforest")){
    vars.to.keep<-tryCatch({
      importance<-as.data.frame(model$variable.importance)
      if(nrow(importance)>0){   
        names(importance)<-"Overall"
        importance<-importance[importance$Overall>0,,drop=FALSE]
        importance<-importance[order(-importance$Overall),,drop=FALSE]
        ##
        if(nrow(importance)>3){
          temp<-rownames(importance)[1:floor((2/3)*nrow(importance))]
        } else{
          temp<-rownames(importance)
        }
      } else{#nrow(importance)==0
        temp<-NULL
      }
      temp
    }, error=function(err){
      return(NULL)
    })
  }
  if(Model.type %in% c("Neural Network","Naive Neural Network")){
    vars.to.keep<-NULL
  }
  return(vars.to.keep)
}


get.all.vars<-function(SPY,output.var,vars.not.include,not.imp.vars,analysisMethod){
  
  if (analysisMethod == "Native Variables") {
    include.scale.center<- F
    include.interactions <- F 
    include.power2 <- F 
    include.power0_3 <- F 
    include.power0_5<- F
    include.long_transf <- F 
    include.meanInd<-F
    include.cutFun<-F
    include.lnTrans<-F
    include.sum3continuos<-F
    include.sum2dummy<-F
    include.sum3dummy<-F
    include.sum4dummy<-F
  }
  if (analysisMethod == "Combinations frequencies") {
    include.scale.center<- F
    include.interactions <- F 
    include.power2 <- F 
    include.power0_3 <- F 
    include.power0_5<- F
    include.long_transf <- F 
    include.meanInd<-F
    include.cutFun<-F
    include.lnTrans<-F
    include.sum3continuos<-F
    include.sum2dummy<-T
    include.sum3dummy<-T
    include.sum4dummy<-T
  }  
  if (analysisMethod == "Light Crunching") {
    include.scale.center<- F
    include.interactions <- T 
    include.power2 <- F 
    include.power0_3 <- F 
    include.power0_5<- F
    include.long_transf <- F 
    include.meanInd<-F
    include.cutFun<-F
    include.lnTrans<-F
    include.sum3continuos<-F
    include.sum2dummy<-F
    include.sum3dummy<-F
    include.sum4dummy<-F
  }
  if (analysisMethod == "Deep Crunching") {
    include.scale.center<- T
    include.interactions <- F 
    include.power2 <- T 
    include.power0_3 <- T 
    include.power0_5<- T
    include.long_transf <- T 
    include.meanInd<-T
    include.cutFun<-T
    include.lnTrans<-T
    include.sum3continuos<-T
    include.sum2dummy<-T
    include.sum3dummy<-T
    include.sum4dummy<-T
  }
  if (analysisMethod == "All methods") {
    include.scale.center<- T
    include.interactions <- T 
    include.power2 <- T 
    include.power0_3 <- T 
    include.power0_5<- T
    include.long_transf <- T 
    include.meanInd<-T
    include.cutFun<-T
    include.lnTrans<-T
    include.sum3continuos<-T
    include.sum2dummy<-T
    include.sum3dummy<-T
    include.sum4dummy<-T
  }
  vars.to.check.F.E<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include,not.imp.vars))]
  
  all.vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
  interaction.vars<-NULL
  ## single variable with scale and center = TRUE
  if(include.scale.center){
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]])){return(NULL)}
      return(paste0("Scale(",i,")"))
    })
    all.vars<-c(all.vars,unlist(temp))
  } 
  
  ## indicator variables ifelse(>mean)
  if (include.meanInd) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]])){return(NULL)}
      return(paste0("cut.mean(",i,")"))
    }) 
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## discrete  variables by using cut function
  if (include.cutFun) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]])){return(NULL)}
      return(paste0("cut.4.pieces(",i,")"))
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## continuos variables var1/ln(var2+1)
  if (include.lnTrans) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]])){return(NULL)} #in this loop we deal only with continious numeric variables
      lapply(vars.to.check.F.E,function(j){
        if(!is.numeric(SPY[[j]])){return(NULL)}
        if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
        return(paste0("I(",i,"/log(abs(",j,")+1.001))"))
      })
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## sum of 2 dummy
  if (include.sum2dummy) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]]) || length(unique(SPY[[i]]))!=2){return(NULL)} #in this loop we deal only with dummy variables
      lapply(vars.to.check.F.E,function(j){
        if(!is.numeric(SPY[[j]]) || length(unique(SPY[[j]]))!=2){return(NULL)}         
        if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
        return(paste0("I(",i,"+",j,")"))
      })
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## sum of 3 dummy
  if (include.sum3dummy) {
    temp<-lapply(vars.to.check.F.E,function(i){   
      if(!is.numeric(SPY[[i]]) || length(unique(SPY[[i]]))!=2){return(NULL)} #in this loop we deal only with continious numeric variables
      lapply(vars.to.check.F.E,function(j){   
        if(!is.numeric(SPY[[j]]) || length(unique(SPY[[j]]))!=2){return(NULL)}
        if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
        lapply(vars.to.check.F.E,function(k){
          if(!is.numeric(SPY[[k]]) || length(unique(SPY[[k]]))!=2){return(NULL)}
          if (which(vars.to.check.F.E==k)<=which(vars.to.check.F.E==j)) {return(NULL)}
          return(paste0("I(",i,"+",j,"+",k,")"))
        })
      })
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  
  ## sum of 4 dummy
  if (include.sum4dummy) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if(!is.numeric(SPY[[i]]) || length(unique(SPY[[i]]))!=2){return(NULL)} #in this loop we deal only with continious numeric variables
      lapply(vars.to.check.F.E,function(j){
        if(!is.numeric(SPY[[j]]) || length(unique(SPY[[j]]))!=2){return(NULL)}
        if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
        lapply(vars.to.check.F.E,function(k){
          if(!is.numeric(SPY[[k]]) || length(unique(SPY[[k]]))!=2){return(NULL)}
          if (which(vars.to.check.F.E==k)<=which(vars.to.check.F.E==j)) {return(NULL)}
          lapply(vars.to.check.F.E,function(l){
            if(!is.numeric(SPY[[l]]) || length(unique(SPY[[l]]))!=2){return(NULL)}
            if (which(vars.to.check.F.E==l)<=which(vars.to.check.F.E==k)) {return(NULL)}  
            return(paste0("I(",i,"+",j,"+",k,"+",l,")"))
          })
        })
      })
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## single variable ^ 2
  if (include.power2) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if (!is.numeric(SPY[[i]])) {return(NULL)}  
      return(paste0("I(",i,"^2)"))
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## single variable ^ 0.3
  if (include.power0_3) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if (!is.numeric(SPY[[i]])) {return(NULL)} 
      return(paste0("I(abs(",i,")^0.3)"))
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## single variable ^ 0.5
  if (include.power0_5) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if (!is.numeric(SPY[[i]])) {return(NULL)} 
      return(paste0("I(abs(",i,")^0.5)"))
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## Log variable  
  if (include.long_transf) {
    temp<-lapply(vars.to.check.F.E,function(i){
      if (!is.numeric(SPY[[i]])) {return(NULL)} 
      return(paste0("I(log(abs(",i,")+0.001))"))
    })
    all.vars<-c(all.vars,unlist(temp))
  }
  
  ## In the interaction all vars are used.
  if (include.interactions){
    temp<-lapply(vars.to.check.F.E,function(i){
      lapply(vars.to.check.F.E,function(j){
        if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
        return(paste0(i,":",j))
      })
    })
    interaction.vars<-unlist(temp)
  }
  
  return(list("all.vars"=all.vars,"interaction.vars"=interaction.vars))
}



