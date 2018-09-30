find.best.results<-function(Use.CV,SPY,train,test1,test2,test3,output.var,Criterion,Preserve.Accuracy_01,ChooseAlgorithms,analysisMethod,
                            vars.not.include,method){

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

  ##########################
  ##Starting the vectors
  ########################
  Results<-list()
  Vars<-list()
  Myform<-list()
  Models<-list()
  ########################
  #####check constant Variables (After Fixing the Data)
  vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
  ind<-sapply(vars,function(x){return(length(unique(SPY[[x]]))<2)})
  if(sum(ind)>0)
    vars.not.include<-c(vars.not.include,vars[ind])
  
  #####Check correlation between independent variables
  tryCatch({
    Names<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
    temp.SPY<-as.data.table(sapply(Names,function(x){range01(SPY[[x]])}))
    temp<-lapply(Names,function(i){
      lapply(Names,function(j){   
        if(i<j){
          if(cor.nv(temp.SPY[[i]],temp.SPY[[j]])>0.9)
            return(j)
        } else{
          return(NULL)
        }
      })
    })
    temp<-unique(unlist(temp))
    print(temp)
    vars.not.include<-c(vars.not.include,temp)
  }, error=function(err){})
  
 #####Check F-Test statistic
  vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
  F_test_res<-unlist(lapply(vars,function(i){
    tryCatch({
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", i))
      model<-lm(formula,data=SPY)
      temp<-anova(model)
      return(temp$`Pr(>F)`[1])
    }, error=function(err){
      return(NA)
    })
  }))
  
  F_test_data<-data.frame("vars"=vars,"F_test"=F_test_res)
  temp.ind<-which(!is.na(F_test_data$F_test) & F_test_data$F_test>0.05)
  temp.vars<-as.character(F_test_data$vars[temp.ind])
  print(temp.vars)
  vars.not.include<-c(vars.not.include,temp.vars)
  if(length(names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))])==0)
    return(NULL)
  
  if(length(temp.ind)>0)
    F_test_data<-F_test_data[-temp.ind,,drop=FALSE]
  
  F_test_data<-F_test_data[order(F_test_data$F_test),]
  if(nrow(F_test_data)<=5){
    vars.with.heigth.F_Test<-NULL
  } else{
    vars.with.heigth.F_Test<-as.character(F_test_data$vars[6:length(F_test_data$vars)])
  }
  ###############################################
  SPY1<-SPY[1:round((1/3)*nrow(SPY)),] 
  SPY2<-SPY[(round((1/3)*nrow(SPY))+1):round((2/3)*nrow(SPY)),] 
  SPY3<-SPY[(round((2/3)*nrow(SPY))+1):nrow(SPY),] 
  ##############################################
  Char<-c("Logistic","Weighted Logistic","Linear","Weighted Linear","Negative Binomial","Quantile","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network")
  Char<-Char[Char %in% ChooseAlgorithms]
  for(char in Char){
    tryCatch({ 
    print(char)
    necessary.vars<-NULL
    Progression.Vars<-NULL
    Execute.Loop <- TRUE
    IterVars <- 0  
    
    while (Execute.Loop) {
      print("hey i enterd the while loop")
      newdf <- data.frame()
      vars.to.check<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include,necessary.vars))]
      ##vars.to.check.F.E  =vars.to.check for Features Engineering
      vars.to.check.F.E<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include,vars.with.heigth.F_Test,necessary.vars))]

      ## single variable
      tmp.newdf<-lapply(vars.to.check, function(i){
        loop.vars <- c(necessary.vars, i)
        myform<-Myform(output.var,loop.vars,char)
        return(one.var.model(Use.CV,i,myform,loop.vars, SPY, train,test1,test2,test3, output.var,Criterion,char,method)) 
      })
      newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 

      
      ## single variable with scale and center = TRUE
      if(include.scale.center){
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if(!is.numeric(SPY[[i]])){return(NULL)}
          loop.vars <- c(necessary.vars, paste0("Scale(",i,")")) 
          myform <-Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("Scale(",i,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      } 
      
      ## indicator variables ifelse(>mean)
      if (include.meanInd) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if(!is.numeric(SPY[[i]])){return(NULL)}
          loop.vars <- c(necessary.vars, paste0("cut.mean(",i,")")) 
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("cut.mean(",i,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        }) 
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## discrete  variables by using cut function
      if (include.cutFun) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if(!is.numeric(SPY[[i]])){return(NULL)}
          loop.vars <- c(necessary.vars, paste0("cut.4.pieces(",i,")"))  
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("cut.4.pieces(",i,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## continuos variables var1/ln(var2+1)
      if (include.lnTrans) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if(!is.numeric(SPY[[i]])){return(NULL)} #in this loop we deal only with continious numeric variables
          lapply(vars.to.check.F.E,function(j){
            if(!is.numeric(SPY[[j]])){return(NULL)}
            if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
            loop.vars <- c(necessary.vars, paste0("I(",i,"/log(abs(",j,")+1.001))")) 
            myform <- Myform(output.var,loop.vars,char)
            return(one.var.model(Use.CV,paste0("I(",i,"/log(abs(",j,")+1.001))"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
          })
        })
        
        if(length(vars.to.check.F.E)>0){
        for(i in 1:length(vars.to.check.F.E)){
          if(!is.null(tmp.newdf[[i]]))
            newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf[[i]])), newdf) 
        }}
      }
      
      ## sum of 2 dummy
      if (include.sum2dummy) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if(!is.numeric(SPY[[i]]) || length(unique(SPY[[i]]))!=2){return(NULL)} #in this loop we deal only with dummy variables
          lapply(vars.to.check.F.E,function(j){
            if(!is.numeric(SPY[[j]]) || length(unique(SPY[[j]]))!=2){return(NULL)}         
            if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
            loop.vars <- c(necessary.vars, paste0("I(",i,"+",j,")"))
            myform <- Myform(output.var,loop.vars,char)
            return(one.var.model(Use.CV,paste0("I(",i,"+",j,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
          })
        })
        
        if(length(vars.to.check.F.E)>0){
        for(i in 1:length(vars.to.check.F.E)){
          if(!is.null(tmp.newdf[[i]]))
            newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf[[i]])), newdf) 
        }}
      }
      
      ## sum of 3 dummy
      if (include.sum3dummy) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){   
          if(!is.numeric(SPY[[i]]) || length(unique(SPY[[i]]))!=2){return(NULL)} #in this loop we deal only with continious numeric variables
          lapply(vars.to.check.F.E,function(j){   
            if(!is.numeric(SPY[[j]]) || length(unique(SPY[[j]]))!=2){return(NULL)}
            if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
            lapply(vars.to.check.F.E,function(k){
              if(!is.numeric(SPY[[k]]) || length(unique(SPY[[k]]))!=2){return(NULL)}
              if (which(vars.to.check.F.E==k)<=which(vars.to.check.F.E==j)) {return(NULL)}
              loop.vars <- c(necessary.vars, paste0("I(",i,"+",j,"+",k,")"))
              myform <- Myform(output.var,loop.vars,char)
              return(one.var.model(Use.CV,paste0("I(",i,"+",j,"+",k,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
            })
          })
        })
        
        if(length(vars.to.check.F.E)>0){
        for(i in length(vars.to.check.F.E)){
          if(is.null(tmp.newdf[[i]])){next}
          for(j in length(vars.to.check.F.E)){
            if(is.null(tmp.newdf[[i]][[j]])){next}
            newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf[[i]][[j]])), newdf) 
          }
        }}
      }
      
      
      ## sum of 4 dummy
      if (include.sum4dummy) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
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
                loop.vars <- c(necessary.vars, paste0("I(",i,"+",j,"+",k,"+",l,")"))
                myform <- Myform(output.var,loop.vars,char)
                return(one.var.model(Use.CV,paste0("I(",i,"+",j,"+",k,"+",l,")"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
              })
            })
          })
        })
        
        if(length(vars.to.check.F.E)>0){
        for(i in length(vars.to.check.F.E)){
          if(is.null(tmp.newdf[[i]])){next}
          for(j in length(vars.to.check.F.E)){
            if(is.null(tmp.newdf[[i]][[j]])){next}
            for(k in length(vars.to.check.F.E)){
              if(is.null(tmp.newdf[[i]][[j]][[k]])){next}
              newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf[[i]][[j]][[k]])), newdf) 
            }
          }
        }}
      }
      
      ## single variable ^ 2
      if (include.power2) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if (!is.numeric(SPY[[i]])) {return(NULL)}  
          loop.vars <- c(necessary.vars, paste0("I(",i,"^2)")) 
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("I(",i,"^2)"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method))             
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## single variable ^ 0.3
      if (include.power0_3) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if (!is.numeric(SPY[[i]])) {return(NULL)} 
          loop.vars <- c(necessary.vars, paste0("I(abs(",i,")^0.3)"))
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("I(abs(",i,")^0.3)"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## single variable ^ 0.5
      if (include.power0_5) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if (!is.numeric(SPY[[i]])) {return(NULL)} 
          loop.vars <- c(necessary.vars, paste0("I(abs(",i,")^0.5)"))
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("I(abs(",i,")^0.5)"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## Log variable  
      if (include.long_transf) {
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          if (!is.numeric(SPY[[i]])) {return(NULL)} 
          loop.vars <- c(necessary.vars, paste0("I(log(abs(",i,")+0.001))") )
          myform <- Myform(output.var,loop.vars,char)
          return(one.var.model(Use.CV,paste0("I(log(abs(",i,")+0.001))"), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
        })
        newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf)), newdf) 
      }
      
      ## In the interaction all vars are used.
      if (char!="Recursive Partitioning Tree" && include.interactions){
        tmp.newdf<-lapply(vars.to.check.F.E,function(i){
          lapply(vars.to.check.F.E,function(j){
            if (which(vars.to.check.F.E==j)<=which(vars.to.check.F.E==i)) {return(NULL)}
            loop.vars <- c(necessary.vars, paste0(i,":",j)) 
            myform <- Myform(output.var,loop.vars,char)
            return(one.var.model(Use.CV,paste0(i,":",j), myform,loop.vars, SPY,train,test1,test2,test3, output.var,Criterion,char,method)) 
          })
        })
        
        if(length(vars.to.check.F.E)>0){
        for(i in 1:length(vars.to.check.F.E)){
          if(!is.null(tmp.newdf[[i]]))
            newdf <- rbind(as.data.frame(do.call("rbind",tmp.newdf[[i]])), newdf) 
        }}
      }
      
      ## newdf and Progression.Vars can't be both empty, because if Progression.Vars
      ## is null it means we havent used any variables that's why newdf won't be empty  
      if(nrow(newdf)==0)
        break

      IterVars <- IterVars + 1
      
      ## Checking stability ##
      if(!is.null(Progression.Vars)){
        newdf<-newdf[complete.cases(newdf$Stability) && (newdf$Stability < stability.threshold),]
      } else{
        temp<-newdf[complete.cases(newdf$Stability),]
        if(nrow(temp)>0){
          newdf<-newdf[!is.na(newdf$Stability),]
          tmp.newdf <- newdf[(newdf$Stability < stability.threshold) ,]
          if(nrow(tmp.newdf)==0){
            initial.val<-stability.threshold
            while(nrow(tmp.newdf)==0){
              initial.val<-initial.val+0.05
              tmp.newdf <- newdf[(newdf$Stability < initial.val) ,]
            }
          }
          newdf<-tmp.newdf
        }}
      
      if(nrow(newdf)==0)
        break
      
      ## Checking preserve accuracy 0 & 1  
      if(method=="Classification" && Preserve.Accuracy_01=="Yes"){
        accuracy.threshold_01<-0.75
        newdf <- newdf[!is.na(newdf[,'Accuracy 0']) && !is.na(newdf[,'Precision']),]
        if(nrow(newdf)>0){
        if(!is.null(Progression.Vars)){
          newdf <- newdf[(newdf[,'Accuracy 0'] > accuracy.threshold_01) && (newdf[,'Precision'] > accuracy.threshold_01),]
        } else{
          tmp.newdf <- newdf[(newdf[,'Accuracy 0'] > accuracy.threshold_01) && (newdf[,'Precision'] > accuracy.threshold_01),]
          if(nrow(tmp.newdf)==0){
            initial.val<-accuracy.threshold_01
            while(nrow(tmp.newdf)==0){
              initial.val<-initial.val-0.05
              tmp.newdf <- newdf[(newdf[,'Accuracy 0'] > initial.val) && (newdf[,'Precision'] > initial.val),]
            }
          }
          newdf<-tmp.newdf
        }#End of is.null(Progression.Vars)
      }#End of nrow(newdf)>0
    }#End of method=="Classification" && Preserve.Accuracy_01=="Yes"
      
      if(nrow(newdf)==0)
        break
      
      if(method=="Classification"){
        newdf<-newdf[order(-newdf[,Criterion]), ]
        
        necessary.vars <- c(necessary.vars, rownames(newdf[1,]))
        Progression.Vars <- rbind(Progression.Vars, newdf[1,])
        if(IterVars>1){
          small.diff<-(Progression.Vars[IterVars,Criterion] - Progression.Vars[IterVars - 1,Criterion]) <= accuracy.tolerance
          ##In case smalldiff is na we will set smalldiff as 0 so we will
          ## end the the while loop (the same in the estimation case).
          ##Progression.Vars is not empty in this case because IterVars>1
          ##smalldiff can be na when the criterion is Inf, when there is an Inf between the 
          ##answers we can do order that's why we can choose our first variable.
          small.diff<-ifelse(is.na(small.diff),0,small.diff)
        }}
      
      if(method=="Estimation"){
        if(Criterion %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
          newdf<-newdf[order(newdf[,Criterion]), ]
        } else{
          newdf<-newdf[order(-newdf[,Criterion]), ]
        }
        necessary.vars <- c(necessary.vars, rownames(newdf[1,]))
        Progression.Vars <- rbind(Progression.Vars, newdf[1,])
        if(IterVars>1){ 
          if(Criterion %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
            small.diff <- (Progression.Vars[IterVars-1,Criterion] - Progression.Vars[IterVars,Criterion]) <= difference.tolerance
            small.diff<-ifelse(is.na(small.diff),0,small.diff)
          } else{
            small.diff <- (Progression.Vars[IterVars,Criterion] - Progression.Vars[IterVars-1,Criterion]) <= difference.tolerance
            small.diff<-ifelse(is.na(small.diff),0,small.diff)
          }}
      }
      
      if (rownames(newdf[1,]) == "NA") {
        necessary.vars <- necessary.vars[1:(length(necessary.vars)-1)]
        Progression.Vars <- Progression.Vars[1:(nrow(Progression.Vars)-1),]
        Execute.Loop <- FALSE
      }
      
      if (IterVars >= 2 && small.diff) { 
        print("small diff")
        necessary.vars <- necessary.vars[1:length(necessary.vars)-1]
        Progression.Vars <- Progression.Vars[1:nrow(Progression.Vars)-1,]
        Execute.Loop <- FALSE
      }
    }
    Progression.Vars<-cbind("Variables"=row.names(Progression.Vars),Progression.Vars)
    ######### Here Endes the While Loop ###########
    if(!is.null(Progression.Vars)){  
      if(char=="Logistic"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Logistic", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-glm(formula,family=binomial('logit'),data=SPY) 
      }
      if(char=="Linear"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Linear", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-lm(formula,data=SPY) 
      }
      if(char=="Weighted Logistic"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Weighted Logistic", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-glm(formula,family=binomial('logit'),data=SPY,weights=log.weights.spy) 
      } 
      if(char=="Weighted Linear"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Weighted Linear", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-lm(formula,data=SPY,weights=log.weights.spy) 
      }
      if(char=="Negative Binomial"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Negative Binomial", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-glm.nb(formula,data=SPY) 
      }
      if(char=="Quantile"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
        Result<- one.var.model.class("Quantile", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        Model<-rq(formula,tau = .5, method = "pfn",data=SPY)
      }   
      if(char=="Xgboost"){
        loop.vars <- as.character(Progression.Vars[,1])
        formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1"))
        Result<- one.var.model.class("Xgboost", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        SPY.matrix<- sparse.model.matrix(formula, SPY)  
        set.seed(xgb.seed);Model <- xgboost(data = SPY.matrix, label = SPY[[output.var]],nround=xg.nround, params=xg.params,verbose=0)
      }
      if(char=="Recursive Partitioning Tree"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"))) 
        Result<- one.var.model.class("Recursive Partitioning Tree", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        if(method=="Classification"){
          Model<-rpart(formula,data=SPY,method='class',control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth)) 
        } else{
          Model<-rpart(formula,data=SPY,control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth)) 
        }
      }
      if(char=="Rforest"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+"))) 
        Result<- one.var.model.class("Rforest", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
        if(method=="Classification"){
          temp.SPY<-SPY   
          temp.SPY[[output.var]]<-as.factor(temp.SPY[[output.var]])
          set.seed(12);Model<-ranger(formula, data =temp.SPY , num.trees = 5, write.forest = TRUE,classification=TRUE,probability =TRUE,importance='impurity')
        } else{
          set.seed(12);Model<-ranger(formula, data = SPY, num.trees = 5, write.forest = TRUE,classification=FALSE,importance='impurity')
        }
      }  
      if(char=="Neural Network"){
        loop.vars<-as.character(Progression.Vars[,1]) 
        formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1"))
        Result<- one.var.model.class("Neural Network", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,char,method)
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
 
    Vars[[paste(char)]]<-loop.vars
    Myform[[paste(char)]]<-formula
    Models[[paste(char)]]<-Model
    Results[[paste(char)]]<-Result
   }#End of !is.null(Progression.Vars)
  }, error=function(err) {
    print(err$message)
  }) ##End of TryCatch 
 } ##End of for loop


  if(sum(c("Naive Logistic","Naive Weighted Logistic","Naive Linear","Naive Weighted Linear","Naive Negative Binomial","Naive Quantile","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network") %in% ChooseAlgorithms)>0){
    all.vars<-get.all.vars(SPY,output.var,vars.not.include,vars.with.heigth.F_Test,analysisMethod)
    attach(SPY)
    ##ALL the possible options
    loop.vars<-unname(unlist(all.vars))
    if(length(loop.vars)>0){
      ##Remove the constant
      ind<-sapply(loop.vars,function(x){return(length(unique(cleaned.vec(eval(parse(text=x)))))<2)})
      ind<-which(ind==TRUE)
      if(length(ind)>0){loop.vars<-loop.vars[-ind]} 
      loop.vars<-as.character(loop.vars)
      ##Remove the correlated variables
      cols<-lapply(loop.vars,function(x){eval(parse(text=x))})
      data<-as.data.table(do.call("cbind",cols))
      names(data)<-loop.vars
      tryCatch({
        Names<-names(data)
        temp.data<-as.data.table(sapply(Names,function(x){range01(data[[x]])}))
        temp<-lapply(Names,function(i){
          lapply(Names,function(j){   
            if(i<j){
              if(cor.nv(temp.data[[i]],temp.data[[j]])>0.9)
                return(j)
            } else{
              return(NULL)
            }
          })
        })
        temp<-unique(unlist(temp))
        loop.vars<-loop.vars[!loop.vars %in% temp]
      }, error=function(err){})
    }
    detach(SPY)
    #####F-Test#####
    tryCatch({
    F_test_res<-unlist(lapply(loop.vars,function(i){
      tryCatch({
        formula <- as.formula(paste(as.symbol(output.var), " ~ ", i))
        model<-lm(formula,data=SPY)
        temp<-anova(model)
        return(temp$`Pr(>F)`[1])
      }, error=function(err){
        return(NA)
      })
    }))
    
    F_test_data<-data.frame("vars"=loop.vars,"F_test"=F_test_res)
    temp.ind<-which(!is.na(F_test_data$F_test) & F_test_data$F_test>0.05)
    temp.vars<-as.character(F_test_data$vars[temp.ind])
    loop.vars<-loop.vars[!loop.vars %in% temp.vars]
    }, error=function(err){})
    #####End of F-Test###
    without.interaction.loop.vars<-loop.vars[!(loop.vars %in% all.vars[["interaction.vars"]])]
  }
    
  if("Naive Logistic" %in% ChooseAlgorithms){
     tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-glm(formula,family=binomial('logit'),data=SPY)
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Logistic", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Logistic",method) 
      Model<-glm(new.formula,family=binomial('logit'),data=SPY)
      
      Vars[["Naive Logistic"]]<-new.loop.vars
      Myform[["Naive Logistic"]]<-new.formula
      Models[["Naive Logistic"]]<-Model
      Results[["Naive Logistic"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  } #End of "Naive Logistic" %in% ChooseAlgorithms
  

  if("Naive Linear" %in% ChooseAlgorithms){
    tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-lm(formula,data=SPY)
     # variables.to.drop<-drop.not.important.vars(model,"Naive Linear")
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Linear", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Linear",method) 
      Model<-lm(new.formula,data=SPY) 
      
      Vars[["Naive Linear"]]<-new.loop.vars
      Myform[["Naive Linear"]]<-new.formula
      Models[["Naive Linear"]]<-Model
      Results[["Naive Linear"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of "Naive Linear" %in% ChooseAlgorithms
  
  if("Naive Weighted Logistic" %in% ChooseAlgorithms){
    tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-glm(formula,family=binomial('logit'),data=SPY,weights=log.weights.spy)
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Weighted Logistic", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Weighted Logistic",method) 
      Model<-glm(new.formula,family=binomial('logit'),data=SPY,weights=log.weights.spy)
      
      Vars[["Naive Weighted Logistic"]]<-new.loop.vars
      Myform[["Naive Weighted Logistic"]]<-new.formula
      Models[["Naive Weighted Logistic"]]<-Model
      Results[["Naive Weighted Logistic"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of Naive Weighted Logistic %in% ChooseAlgorithms 
  
  if("Naive Weighted Linear" %in% ChooseAlgorithms){
    tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-lm(formula,data=SPY,weights=log.weights.spy)
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Weighted Linear", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Weighted Linear",method) 
      Model<-lm(new.formula,data=SPY,weights=log.weights.spy) 

      Vars[["Naive Weighted Linear"]]<-new.loop.vars
      Myform[["Naive Weighted Linear"]]<-new.formula
      Models[["Naive Weighted Linear"]]<-Model
      Results[["Naive Weighted Linear"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of Naive Weighted Linear %in% ChooseAlgorithms
  
  if("Naive Negative Binomial" %in% ChooseAlgorithms){
    tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-glm.nb(formula,data=SPY)
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Negative Binomial", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Negative Binomial",method) 
      Model<-glm.nb(new.formula,data=SPY) 
      
      Vars[["Naive Negative Binomial"]]<-new.loop.vars
      Myform[["Naive Negative Binomial"]]<-new.formula
      Models[["Naive Negative Binomial"]]<-Model
      Results[["Naive Negative Binomial"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of "Naive Negative Binomial" %in% ChooseAlgorithms
  
  if("Naive Quantile" %in% ChooseAlgorithms){
    tryCatch({
      ###Checking if there is collinearity
      ###Building the model with all the variables,then excluding those with NA coefficient
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      temp.model<-rq(formula,tau = .5, method = "pfn",data=SPY)
      if(sum(is.na(temp.model$coefficients))>0){
        temp.names<-names(temp.model$coefficients)[is.na(temp.model$coefficients)]
        new.loop.vars<-loop.vars[!loop.vars %in% temp.names]  #base.vars(temp.names,SPY)
      } else{
        new.loop.vars<-loop.vars
      }
      new.formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(new.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Quantile", new.formula,new.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Quantile",method) 
      Model<-rq(new.formula,tau = .5, method = "pfn",data=SPY)

      Vars[["Naive Quantile"]]<-new.loop.vars
      Myform[["Naive Quantile"]]<-new.formula
      Models[["Naive Quantile"]]<-Model
      Results[["Naive Quantile"]]<-Result
      
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of "Naive Quantile" %in% ChooseAlgorithms
    
  if("Naive Xgboost" %in% ChooseAlgorithms){
      tryCatch({
      formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1")) 
      SPY.matrix<- sparse.model.matrix(formula, SPY) # we need this in order to remove the first column that is by product of the matrix function
      Result<-one.var.model.class("Naive Xgboost", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Xgboost",method) 
      set.seed(xgb.seed);Model <- xgboost(data = SPY.matrix, label = SPY[[output.var]],nround=xg.nround, params=xg.params,verbose=0)

      Vars[["Naive Xgboost"]]<-loop.vars
      Myform[["Naive Xgboost"]]<-formula
      Models[["Naive Xgboost"]]<-Model
      Results[["Naive Xgboost"]]<-Result
      }, error=function(err) {
        print(err$message)
      }) ##End of TryCatch 
  }#End of "Naive.Xgboost" %in% ChooseAlgorithms
    
    
  if("Naive Recursive Partitioning Tree" %in% ChooseAlgorithms){
      tryCatch({   
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(without.interaction.loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Recursive Partitioning Tree", formula,without.interaction.loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Recursive Partitioning Tree",method) 
      if(method=="Classification"){
      Model<-rpart(formula,data=SPY,method='class',control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth)) 
      } else{
      Model<-rpart(formula,data=SPY,control=rpart.control(minsplit=rpart.params$minsplit,cp=rpart.params$cp,maxdepth=rpart.params$maxdepth))
      }
      
      Vars[["Naive Recursive Partitioning Tree"]]<-without.interaction.loop.vars
      Myform[["Naive Recursive Partitioning Tree"]]<-formula
      Models[["Naive Recursive Partitioning Tree"]]<-Model
      Results[["Naive Recursive Partitioning Tree"]]<-Result
      }, error=function(err) {
        print(err$message)
      }) ##End of TryCatch 
  }#End of "Naive Recursive Partitioning Tree" %in% ChooseAlgorithms
  
  if("Naive Rforest" %in% ChooseAlgorithms){
    tryCatch({
      formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
      Result<-one.var.model.class("Naive Rforest", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Rforest",method) 
      if(method=="Classification"){
        temp.SPY<-SPY   
        temp.SPY[[output.var]]<-as.factor(temp.SPY[[output.var]])
        set.seed(12);Model<-ranger(formula, data = temp.SPY, num.trees = 5, write.forest = TRUE,classification=TRUE,probability =TRUE,importance='impurity')
      } else{
        set.seed(12);Model<-ranger(formula, data = SPY, num.trees = 5, write.forest = TRUE,classification=FALSE,importance='impurity')
      }
      
      Vars[["Naive Rforest"]]<-loop.vars
      Myform[["Naive Rforest"]]<-formula
      Models[["Naive Rforest"]]<-Model
      Results[["Naive Rforest"]]<-Result
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of "Naive.Rforest" %in% ChooseAlgorithms
  
  if("Naive Neural Network" %in% ChooseAlgorithms){
    tryCatch({
      formula <- as.formula(paste(" ~ ", paste(loop.vars, collapse= "+"),"-1")) 
      Result<-one.var.model.class("Naive Neural Network", formula,loop.vars, SPY,SPY1,SPY2,SPY3, output.var,Criterion,"Naive Neural Network",method) 
      if(method=="Classification"){
        SPY.matrix<- data.matrix(sparse.model.matrix(formula, SPY)) 
        mx.set.seed(0)
        Model <- mx.mlp(SPY.matrix, SPY[[output.var]], hidden_node=5,activation ='relu', out_node=2, out_activation="softmax",
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
      
      Vars[["Naive Neural Network"]]<-loop.vars
      Myform[["Naive Neural Network"]]<-formula
      Models[["Naive Neural Network"]]<-Model
      Results[["Naive Neural Network"]]<-Result
    }, error=function(err) {
      print(err$message)
    }) ##End of TryCatch 
  }#End of "Naive.Neural.Network" %in% ChooseAlgorithms
  
  P.Var<-list(Results=Results,Models=Models,Myform=Myform,Vars=Vars)	
  if(length(P.Var[["Results"]])>0){
    for(i in names(P.Var[["Results"]])){
      if(nrow(P.Var[["Results"]][[i]])==0){
        P.Var[["Vars"]][[i]]<-NULL
        P.Var[["Myform"]][[i]]<-NULL
        P.Var[["Models"]][[i]]<-NULL
        P.Var[["Results"]][[i]]<-NULL
      }
    }
  }
  ###Neural Network####
  tryCatch({
    if(length(P.Var[["Results"]])>0){
      for(i in names(P.Var[["Results"]])){
        if(!(i %in% c("Neural Network","Naive Neural Network")))
          next
        model<-P.Var[["Models"]][[i]]
        if(sum(is.na(as.matrix(model$arg.params[[1]])))>0){
          P.Var[["Vars"]][[i]]<-NULL
          P.Var[["Myform"]][[i]]<-NULL
          P.Var[["Models"]][[i]]<-NULL
          P.Var[["Results"]][[i]]<-NULL
        }
      }
    }
  }, error=function(err) {
    print(err$message)
  })
  
  if(length(P.Var[["Results"]])==0)
    P.Var<-NULL
  

  return(P.Var)
}  
  
