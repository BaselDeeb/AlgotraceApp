one.var.model.class <- function(i, myform,loop.vars, train, test1, test2 ,test3, output.var,Criterion,char,method) {
  tryCatch({
    if(method=="Classification"){
      if(char=="Logistic" || char=="Naive Logistic"){
        model<-tryCatch({
          speedglm(myform,family=binomial("logit"),data=train) 
        }, error=function(err){
          glm(myform,family=binomial("logit"),data=train) 
        })
        prob1<-predict(model,newdata=test1,type="response")
        prob2<-predict(model,newdata=test2,type="response")
        prob3<-predict(model,newdata=test3,type="response")
      } 
      if(char=="Weighted Logistic" || char=="Naive Weighted Logistic"){
        model<-tryCatch({
          speedglm(myform,family=binomial("logit"),data=train,weights=log.weights.spy[1:nrow(train)]) 
        }, error=function(err){
          glm(myform,family=binomial("logit"),data=train,weights=log.weights.spy[1:nrow(train)]) 
        })
        prob1<-predict(model,newdata=test1,type="response")
        prob2<-predict(model,newdata=test2,type="response")
        prob3<-predict(model,newdata=test3,type="response")
      } 
      if(char=="Xgboost" || char=="Naive Xgboost"){
        Train<- sparse.model.matrix(myform, train) 
        set.seed(xgb.seed);model <- xgboost(data = Train, label = train[[output.var]],nround=xg.nround, params=xg.params,verbose=0)
        
        Test1<- sparse.model.matrix(myform, test1) 
        prob1<-predict(model,Test1)
        
        Test2<- sparse.model.matrix(myform, test2) 
        prob2<-predict(model,Test2)
        
        Test3<- sparse.model.matrix(myform, test3) 
        prob3<-predict(model,Test3)
      }
      if(char=="Recursive Partitioning Tree" || char=="Naive Recursive Partitioning Tree"){
        model<-rpart(myform,data=train,method="class",control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth)) 
        prob1<-predict(model,newdata=test1)[,'1']
        prob2<-predict(model,newdata=test2)[,'1']
        prob3<-predict(model,newdata=test3)[,'1']
      }
      if(char=="Rforest" || char=="Naive Rforest"){
        temp.train<-train   
        temp.train[[output.var]]<-as.factor(temp.train[[output.var]])
        set.seed(12);model<-ranger(myform, data = temp.train, num.trees = 5, write.forest = TRUE,classification=TRUE,probability =TRUE)
        prob1<-predict(model,test1)$predictions[,'1']
        prob2<-predict(model,test2)$predictions[,'1']
        prob3<-predict(model,test3)$predictions[,'1']
      }
      if(char=="Neural Network" || char=="Naive Neural Network"){
        Train<- data.matrix(sparse.model.matrix(myform, train)) 
        mx.set.seed(0)
        model <- mx.mlp(Train, train[[output.var]], hidden_node=5,activation='relu', out_node=2, out_activation="softmax",
                        num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9, 
                        eval.metric=mx.metric.accuracy)
        
        Test1<- sparse.model.matrix(myform, test1) 
        prob1<- tryCatch({
          t(predict(model, data.matrix(Test1)))[,2]  
        }, error=function(err){
          return(t(predict(model, Test1))[,2])
        })
        
        Test2<- sparse.model.matrix(myform, test2) 
        prob2<- tryCatch({
          t(predict(model, data.matrix(Test2)))[,2]  
        }, error=function(err){
          return(t(predict(model, Test2))[,2])
        })
        
        Test3<- sparse.model.matrix(myform, test3)
        prob3<- tryCatch({
          t(predict(model, data.matrix(Test3)))[,2]  
        }, error=function(err){
          return(t(predict(model, Test3))[,2])
        })
      }
      
      test1$predict_response <- ifelse(prob1>classification.threshold,1,0) 
      test2$predict_response <- ifelse(prob2>classification.threshold,1,0) 
      test3$predict_response <- ifelse(prob3>classification.threshold,1,0)  
      
      
      test1x<-CV.Table(test1[[output.var]],test1$predict_response)
      test2x<-CV.Table(test2[[output.var]],test2$predict_response)
      test3x<-CV.Table(test3[[output.var]],test3$predict_response)

      if(Criterion=="Accuracy"){
        test1xpropHits<-(test1x[1,1]+test1x[2,2])/sum(test1x)
        test2xpropHits<-(test2x[1,1]+test2x[2,2])/sum(test2x)
        test3xpropHits<-(test3x[1,1]+test3x[2,2])/sum(test3x)
        stability<-mean(c(abs(test1xpropHits-test2xpropHits),abs(test1xpropHits-test3xpropHits),abs(test2xpropHits-test3xpropHits)),na.rm=TRUE)
      }
      if(Criterion=="Accuracy 0"){
        test1xpropHits0<-test1x[1,1]/(test1x[1,1]+test1x[2,1])
        test2xpropHits0<-test2x[1,1]/(test2x[1,1]+test2x[2,1])
        test3xpropHits0<-test3x[1,1]/(test3x[1,1]+test3x[2,1])
        stability<-mean(c(abs(test1xpropHits0-test2xpropHits0),abs(test1xpropHits0-test3xpropHits0),abs(test2xpropHits0-test3xpropHits0)),na.rm=TRUE)
      }
      if(Criterion=="Precision"){
        test1xpropHits1<-test1x[2,2]/(test1x[2,2]+test1x[1,2])
        test2xpropHits1<-test2x[2,2]/(test2x[2,2]+test2x[1,2])
        test3xpropHits1<-test3x[2,2]/(test3x[2,2]+test3x[1,2])
        stability<-mean(c(abs(test1xpropHits1-test2xpropHits1),abs(test1xpropHits1-test3xpropHits1),abs(test2xpropHits1-test3xpropHits1)),na.rm=TRUE)
      }
      if(Criterion=="F measure"){
        Precision<-test1x[2,2]/(test1x[2,2]+test1x[1,2])
        Recall<-test1x[2,2]/(test1x[2,1]+test1x[2,2])
        test1xF<-2*Recall*Precision/(Recall+Precision)
        ##
        Precision<-test2x[2,2]/(test2x[2,2]+test2x[1,2])
        Recall<-test2x[2,2]/(test2x[2,1]+test2x[2,2])
        test2xF<-2*Recall*Precision/(Recall+Precision)
        ##
        Precision<-test3x[2,2]/(test3x[2,2]+test3x[1,2])
        Recall<-test3x[2,2]/(test3x[2,1]+test3x[2,2])
        test3xF<-2*Recall*Precision/(Recall+Precision)
        ##
        stability<-mean(c(abs(test1xF-test2xF),abs(test1xF-test3xF),abs(test2xF-test3xF)),na.rm=TRUE)
      }
      if(Criterion=="AUC"){
        test1xauc<-auc_roc(prob1,test1[[output.var]])
        test2xauc<-auc_roc(prob2,test2[[output.var]])
        test3xauc<-auc_roc(prob3,test3[[output.var]])
        stability<-mean(c(abs(test1xauc-test2xauc),abs(test1xauc-test3xauc),abs(test2xauc-test3xauc)),na.rm=TRUE)
      }
      
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
        Lift_1<-NaN
      } else{
        temp.mean<-sum(Table[2,])/sum(Table)
        Lift_1<-100*(Precision/temp.mean) 
      }
      #F measure#
      Recall<-Table[2,2]/(Table[2,1]+Table[2,2])
      F_measure<-2*Recall*Precision/(Recall+Precision)
      #for the AUC#
      not.train<-rbind(test1,test2,test3)
      y<-not.train[[output.var]]
      prob<-c(prob1,prob2,prob3)
      not.trainxauc<-auc_roc(prob,y)
      AUC<-not.trainxauc
      Gini<-2*not.trainxauc-1
      ##
      
      tmp.df <- data.frame(Accuracy, stability,Accuracy_0, Precision,Recall,False_Positive,False_Negative,
                           Lift_1,Diff0,Diff1,F_measure,AUC,Gini,check.names=F)
      names(tmp.df) <- c('Accuracy', 'Stability','Accuracy 0','Precision','Recall','False Positive','False Negative',
                         'Lift 1(%)','Diff 0','Diff 1','F measure','AUC','Gini')
      rownames(tmp.df) <- i
      return(tmp.df)
    }
    if(method=="Estimation"){
      Test<-rbind(test1,test2,test3)
      if(char=="Linear" || char=="Naive Linear"){
        model<-tryCatch({
          speedlm(myform,data=train) 
        }, error=function(err){
          lm(myform,data=train)
        })
        test1$predict_response <- predict(model,newdata=test1,type='response')
        test2$predict_response <- predict(model,newdata=test2,type='response')
        test3$predict_response <- predict(model,newdata=test3,type='response')
        predict_response_Test<-predict(model,newdata=Test,type='response')
        predict_response_train<-predict(model,newdata=train,type='response')
      }
      if(char=="Weighted Linear" || char=="Naive Weighted Linear"){
        model<-tryCatch({
          speedlm(myform,data=train,weights=log.weights.spy[1:nrow(train)]) 
        }, error=function(err){
          lm(myform,data=train,weights=log.weights.spy[1:nrow(train)])
        })
        test1$predict_response <- predict(model,newdata=test1,type='response')
        test2$predict_response <- predict(model,newdata=test2,type='response')
        test3$predict_response <- predict(model,newdata=test3,type='response')
        predict_response_Test<-predict(model,newdata=Test,type='response')
        predict_response_train<-predict(model,newdata=train,type='response')
      }
      if(char=="Negative Binomial" || char=="Naive Negative Binomial"){
        model<-glm.nb(myform,data=train)
        test1$predict_response <- predict(model,newdata=test1,type='response')
        test2$predict_response <- predict(model,newdata=test2,type='response')
        test3$predict_response <- predict(model,newdata=test3,type='response')
        predict_response_Test<-predict(model,newdata=Test,type='response')
        predict_response_train<-predict(model,newdata=train,type='response')
      }
      if(char=="Quantile" || char=="Naive Quantile"){
        model<-rq(myform,tau = .5, method = "pfn",data=train)
        test1$predict_response <- predict(model,newdata=test1,type='response')
        test2$predict_response <- predict(model,newdata=test2,type='response')
        test3$predict_response <- predict(model,newdata=test3,type='response')
        predict_response_Test<-predict(model,newdata=Test,type='response')
        predict_response_train<-predict(model,newdata=train,type='response')
      }
      if(char=="Xgboost" || char=="Naive Xgboost"){
        Train<- sparse.model.matrix(myform, train) 
        set.seed(xgb.seed);model <- xgboost(data = Train, label = train[[output.var]],nround=xg.nround, params=xg.params,verbose=0)
        
        Test1<- sparse.model.matrix(myform, test1) 
        test1$predict_response <- predict(model,Test1)
        
        Test2<- sparse.model.matrix(myform, test2) 
        test2$predict_response <- predict(model,Test2)
        
        Test3<- sparse.model.matrix(myform, test3) 
        test3$predict_response <- predict(model,Test3)
        
        sparse.Test<- sparse.model.matrix(myform, Test) 
        predict_response_Test<-predict(model,sparse.Test)
        predict_response_train<-predict(model,Train)
      }
      if(char=="Recursive Partitioning Tree" || char=="Naive Recursive Partitioning Tree"){
        model<-rpart(myform,data=train,control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth)) 
        test1$predict_response <- predict(model,newdata=test1)
        test2$predict_response <- predict(model,newdata=test2) 
        test3$predict_response <- predict(model,newdata=test3) 
        predict_response_Test<-predict(model,newdata=Test)
        predict_response_train<-predict(model,newdata=train)
      }
      if(char=="Rforest" || char=="Naive Rforest"){
        set.seed(12);model<-ranger(myform, data = train, num.trees = 5, write.forest = TRUE,classification=FALSE)
        test1$predict_response <- predict(model,test1)$predictions
        test2$predict_response <- predict(model,test2)$predictions
        test3$predict_response <- predict(model,test3)$predictions 
        predict_response_Test<-predict(model,Test)$predictions
        predict_response_train<-predict(model,train)$predictions
      }
      if(char=="Neural Network" || char=="Naive Neural Network"){
        data <- mx.symbol.Variable("data")
        fc1 <- mx.symbol.FullyConnected(data, num_hidden=1)
        lro <- mx.symbol.LinearRegressionOutput(fc1)
        Train<- data.matrix(sparse.model.matrix(myform, train)) 
        mx.set.seed(0)
        model <- mx.model.FeedForward.create(lro, X=Train, y=train[[output.var]],
                                             ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                             learning.rate=2e-6, momentum=0.9, eval.metric=mx.metric.rmse)
        
        Test1<- sparse.model.matrix(myform, test1) 
        test1$predict_response<- tryCatch({
          as.numeric(predict(model, data.matrix(Test1))) 
        }, error=function(err){
          return(as.numeric(predict(model, Test1)))
        })
        
        Test2<- sparse.model.matrix(myform, test2) 
        test2$predict_response<- tryCatch({
          as.numeric(predict(model, data.matrix(Test2))) 
        }, error=function(err){
          return(as.numeric(predict(model, Test2)))
        })
        
        Test3<- sparse.model.matrix(myform, test3) 
        test3$predict_response<- tryCatch({
          as.numeric(predict(model, data.matrix(Test3))) 
        }, error=function(err){
          return(as.numeric(predict(model, Test3)))
        })
        
        sparse.Test<- sparse.model.matrix(myform, Test)
        predict_response_Test<-tryCatch({
          as.numeric(predict(model, data.matrix(sparse.Test))) 
        }, error=function(err){
          return(as.numeric(predict(model, sparse.Test)))
        })
        predict_response_train<-tryCatch({
          as.numeric(predict(model, data.matrix(Train))) 
        }, error=function(err){
          return(as.numeric(predict(model, Train)))
        })
      } 
      
      diff1<- mean(abs(test1[[output.var]]-test1$predict_response),na.rm =TRUE)/mean(abs(test1[[output.var]]),na.rm =TRUE)
      diff2<- mean(abs(test2[[output.var]]-test2$predict_response),na.rm =TRUE)/mean(abs(test2[[output.var]]),na.rm =TRUE)
      diff3<- mean(abs(test3[[output.var]]-test3$predict_response),na.rm =TRUE)/mean(abs(test3[[output.var]]),na.rm =TRUE)
      
      R2<-R2.calulation(act=Test[[output.var]],pred=predict_response_Test)
      R2.Adj<-1-((1-R2)*(length(Test[[output.var]])-1))/(length(Test[[output.var]])-length(loop.vars)-1)
      Cor<-cor(Test[[output.var]],predict_response_Test,use ="complete.obs",method="spearman")
      RMSE<- sqrt(mean((Test[[output.var]]-predict_response_Test)^2,na.rm =TRUE))
      MAE<- mean(abs(Test[[output.var]]-predict_response_Test),na.rm =TRUE)
      Norm_abs_Difference<- mean(abs(Test[[output.var]]-predict_response_Test),na.rm =TRUE)/mean(abs(Test[[output.var]]),na.rm =TRUE)
      Difference<- mean(Test[[output.var]]-predict_response_Test,na.rm =TRUE)
      Norm_Difference<- mean(Test[[output.var]]-predict_response_Test,na.rm =TRUE)/mean(Test[[output.var]],na.rm =TRUE)
      
      Stability<- (abs(diff1-diff2)+ abs(diff2-diff3)+abs(diff1-diff3))/3
      Sum_output<-sum(Test[[output.var]],na.rm =TRUE)
      Sum_pred<-sum(predict_response_Test,na.rm =TRUE)
      Abs_Sum_Difference<-abs(Sum_output-Sum_pred)
      Avg_output<-mean(Test[[output.var]],na.rm =TRUE)
      Avg_pred<-mean(predict_response_Test,na.rm =TRUE)
      tmp.df <- data.frame(MAE,Norm_abs_Difference,Difference,Norm_Difference, Stability,Sum_output,Sum_pred,Abs_Sum_Difference,Sum_output/Sum_pred,Avg_output,Avg_pred,RMSE,Cor,R2,R2.Adj,check.names=F)
      names(tmp.df) <- c('MAE','Norm abs Difference','Difference','Norm Difference','Stability','Sum output','Sum pred','Abs Sum Difference','Act div Pred','Avg output','Avg pred','RMSE','Cor(y,yhat)','R2','R2 Adj')
      rownames(tmp.df) <- i
      return(tmp.df)
    }
  },
  error=function(err) {return(NULL)})
}


