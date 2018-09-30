rm(list = ls())
gc(verbose=TRUE)
options(shiny.error=traceback)
options(scipen=999)
options(na.action='na.pass')
options(warn=-1)
options(bitmapType='cairo')     
options(shiny.maxRequestSize=3000*1024^2)
models_dir <- "C:/AlgoTraceFolder.save" 
Sys.setlocale("LC_ALL", "Hebrew") 

Install_And_Load(Required_Packages)

start.time<-NULL
##Train/Test/CV/Pred
Train.file.loading<-""
Export.uploaded.data.last_action<-""
CV.file.loading<-""
Pred.file.loading<-""
CV.last_action <-""
Pred.last_action <-""
##Text Analysis
text.analysis.phrases.table.export.last_action<-""
text.analysis.phrases.plot.export.last_action<-""
##Ratio Combination
ratio.comb.export.last_action<-""
transfer.ratio.to.dummies.last_action<-""
##Algorithm
not.allow.computing.text<-""
##Run All options
Run.all.options.last_action<-""
##Variables correlation
Choose_Data_Variables_Correlation.last_action<-""
Variables.Correlation.last_action<-""
##Data analysis
Choose_Data_Data_Analysis.last_action<-""
Distribution.frequency.last.action<-""
Distribution.occurance.last.action<-""
Combination.frequency.map.last.action<-""
Combination.map.last.action<-""
Freq.gridplot.last_action<-""
Combination.map.Gridplot.last_action<-""
##Data analysis Export
Distributionfrequencyplot_export.last_action<-""
Distributionfrequencytable_export.last_action<-""
Distributionoccuranceplot_export.last_action<-""
Distributionoccurancetable_export.last_action<-""
Missing_Values_table_export.last_action<-""
Outliers_table_export.last_action<-""
scatter_table_export.last_action<-""
Combfrequencymap_export.last_action<-""   
Combfrequencymaptable_export.last_action<-""  
Combmap_export.last_action<-"" 
Combmaptable_export.last_action<-""
####Deciles Accuracy/AVG
deciles.accuracy.last_action<-""
deciles.avg.last_action<-"" 
####View Model Formula
ViewModelForm.ms.sql.last_action<-""
ViewModelForm.oracle.last_action<-""
####Deciles Accuracy
deciles.accuracy.cv.last_action<-""
deciles.avg.cv.last_action<-""
##Save & Load
models_found <- ""
last_action <- ""

###For formatRound
round.digits<<-3
###For AllowComputingAnalysis.toListen
AllowComputingAnalysis.logical<-FALSE
##Xgboost  
xgboost.parameters<-list(eta = 0.01, gamma = 1,  min_child_weight = 1,
                         subsample = 0.75,  max_depth = 6, scale_pos_weight = 1.6, reg_alpha = 8,  reg_lambda = 1.3)
xgboost.nround<-22
xgb.seed<<-20

##Rpart
rpart.parameters<-list(minsplit=20,cp=0.001,maxdepth=30)
##########################################################
########## Functions for Classification #######
##########################################################
source("fun.3categ.r")
source("sql_functions.r")
source("oracle_functions.r")
source("accuracy.funs.3categ.r")
source("accuracy.funs.3categ.cv.r")
##########################################################
########## End Functions for Classification #######
##########################################################

##User name and Password###
Logged = FALSE
my_username <- "admin"
my_password <-"123456"
##############################################
shinyServer(  
  function(input, output, session) {  
    ##########################################
    ######Username and Password Enter#########
    USER <- reactiveValues(Logged = Logged)
    
    observe({ 
      if (USER$Logged == FALSE) {
        if (!is.null(input$Login)) {
          if (input$Login > 0) { 
            Username <- isolate(input$userName)
            Password <- isolate(input$passwd)
            Id.username <- which(my_username == Username)
            Id.password <- which(my_password == Password)
            if (length(Id.username) > 0 & length(Id.password) > 0) {
              if (Id.username == Id.password) {
                USER$Logged <- TRUE
              } 
            } 
          } 
        }
      }    
    })
    
    output$Wrong_details<-renderText({
      if (!is.null(input$Login) && input$Login>0){
        Username <- isolate(input$userName)
        Password <- isolate(input$passwd)
        if(Username==my_username && Password==my_password)
          text<-""
        if(Username==my_username && Password!=my_password)
          text<-"Wrong Password"
        if(Username!=my_username && Password==my_password)
          text<-"Wrong Username"
        if(Username!=my_username && Password!=my_password)
          text<-"Wrong Username and Password"
        text
       } else{
         ""
       }})
    
    observe({
      if(USER$Logged){
        shinyjs::hide("admin_page")
        shinyjs::show("main_content")
      }
    })
    
    observe({
      Sys.sleep(8)
      shinyjs::hide("white_page")
      shinyjs::show("not_white_page")
    })
    #############################################
    currentmodel <- reactiveValues()
    #####for Text Analysis
    textAnalysis <- reactiveValues()
    #####for Variables Correlation
    variablesCorrelation <- reactiveValues()
    #####for Data Analysis
    dataAnalysis <- reactiveValues()
    #####for Outliers Settings
    OutliersSettings <- reactiveValues()
    #####for Var vs Target model
    var.vs.target.Model <- reactiveValues()
    #####for Insights
    modelInsights<- reactiveValues()
    ##########################################################
    ## Loading the input file for model building
    ##########################################################
    ##DBA RHandsontable
    output$ODBC_Train.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.ODBC_Train.table)){
          temp<-currentmodel$Loaded.ODBC_Train.table
          currentmodel$Loaded.ODBC_Train.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$ODBC_CV.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.ODBC_CV.table)){
          temp<-currentmodel$Loaded.ODBC_CV.table
          currentmodel$Loaded.ODBC_CV.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$ODBC_Pred.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.ODBC_Pred.table)){
          temp<-currentmodel$Loaded.ODBC_Pred.table
          currentmodel$Loaded.ODBC_Pred.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    #####MS SQL
    output$MS_sql_Train.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.MS_sql_Train.table)){
          temp<-currentmodel$Loaded.MS_sql_Train.table
          currentmodel$Loaded.MS_sql_Train.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$MS_sql_CV.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.MS_sql_CV.table)){
          temp<-currentmodel$Loaded.MS_sql_CV.table
          currentmodel$Loaded.MS_sql_CV.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$MS_sql_Pred.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.MS_sql_Pred.table)){
          temp<-currentmodel$Loaded.MS_sql_Pred.table
          currentmodel$Loaded.MS_sql_Pred.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    #####MySQL
    output$Mysql_Train.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Mysql_Train.table)){
          temp<-currentmodel$Loaded.Mysql_Train.table
          currentmodel$Loaded.Mysql_Train.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Mysql_CV.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Mysql_CV.table)){
          temp<-currentmodel$Loaded.Mysql_CV.table
          currentmodel$Loaded.Mysql_CV.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Mysql_Pred.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Mysql_Pred.table)){
          temp<-currentmodel$Loaded.Mysql_Pred.table
          currentmodel$Loaded.Mysql_Pred.table<-NULL
        } else{
          parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    #####Oracle
    output$Oracle_Train.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Oracle_Train.table)){
          temp<-currentmodel$Loaded.Oracle_Train.table
          currentmodel$Loaded.Oracle_Train.table<-NULL
        } else{
          parameters<-c("ojdbc.jar Path","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Oracle_CV.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Oracle_CV.table)){
          temp<-currentmodel$Loaded.Oracle_CV.table
          currentmodel$Loaded.Oracle_CV.table<-NULL
        } else{
          parameters<-c("ojdbc.jar Path","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Oracle_Pred.table <- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Oracle_Pred.table)){
          temp<-currentmodel$Loaded.Oracle_Pred.table
          currentmodel$Loaded.Oracle_Pred.table<-NULL
        } else{
          parameters<-c("ojdbc.jar Path","USERID","PASSWORD","Query")
          insert<-c("","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    #####Amazon Redshift
    output$Amazon_Redshift_Train.table<- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Amazon_Redshift_Train.table)){
          temp<-currentmodel$Loaded.Amazon_Redshift_Train.table
          currentmodel$Loaded.Amazon_Redshift_Train.table<-NULL
        } else{
          parameters<-c("Host","DATABASE","PORT","USER","PASSWORD","DRIVER","Query")
          insert<-c("","","","","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    
    output$Amazon_Redshift_CV.table<- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Amazon_Redshift_CV.table)){
          temp<-currentmodel$Loaded.Amazon_Redshift_CV.table
          currentmodel$Loaded.Amazon_Redshift_CV.table<-NULL
        } else{
          parameters<-c("Host","DATABASE","PORT","USER","PASSWORD","DRIVER","Query")
          insert<-c("","","","","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    
    
    output$Amazon_Redshift_Pred.table<- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Amazon_Redshift_Pred.table)){
          temp<-currentmodel$Loaded.Amazon_Redshift_Pred.table
          currentmodel$Loaded.Amazon_Redshift_Pred.table<-NULL
        } else{
          parameters<-c("Host","DATABASE","PORT","USER","PASSWORD","DRIVER","Query")
          insert<-c("","","","","","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    #####Google Big Query
    output$Google_Big_Query_Train.table<- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Google_Big_Query_Train.table)){
          temp<-currentmodel$Loaded.Google_Big_Query_Train.table
          currentmodel$Loaded.Google_Big_Query_Train.table<-NULL
        } else{
          parameters<-c("ProjectID","DatasetId","Query")
          insert<-c("","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Google_Big_Query_CV.table<-renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Google_Big_Query_CV.table)){
          temp<-currentmodel$Loaded.Google_Big_Query_CV.table
          currentmodel$Loaded.Google_Big_Query_CV.table<-NULL
        } else{
          parameters<-c("ProjectID","DatasetId","Query")
          insert<-c("","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    
    output$Google_Big_Query_Pred.table<- renderRHandsontable({
      isolate({
        if(!is.null(currentmodel$Loaded.Google_Big_Query_Pred.table)){
          temp<-currentmodel$Loaded.Google_Big_Query_Pred.table
          currentmodel$Loaded.Google_Big_Query_Pred.table<-NULL
        } else{
          parameters<-c("ProjectID","DatasetId","Query")
          insert<-c("","","")
          temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
        }
      })
      
      rhandsontable(temp,maxRows=nrow(temp)) %>% 
        hot_col("Parameters", readOnly=TRUE) %>%
        hot_col("Insert",width=350,strict = FALSE)  
    })
    #####End of DBA RHandsontable#####

    observeEvent(input$MainSubmit, { 
      SPY<-NULL
      ##Reset the export text
      Export.uploaded.data.last_action<<-""
      tryCatch({
        if(input$DataBase.Train.type=="Flat"){
          inFile <- input$datafile
          if (is.null(inFile)) {
            SPY <- NULL
          } else {
            SPY <-import(inFile$datapath,format =split(inFile$name))
          }}
        if(input$DataBase.Train.type=="Big Files(.csv)"){
          SPY<-fread(file.choose(),header = T, sep =',',data.table=FALSE)
        }
        if(input$DataBase.Train.type=="ODBC"){
          if(is.null(input$ODBC_Train.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$ODBC_Train.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"])
            close(myconn)
          }}  
        if(input$DataBase.Train.type=="MS SQL"){
          if(is.null(input$MS_sql_Train.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$MS_sql_Train.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"])
            close(myconn)
          }}
        if(input$DataBase.Train.type=="MySQL"){
          if(is.null(input$Mysql_Train.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Mysql_Train.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
            close(myconn)
          }}
        if(input$DataBase.Train.type=="Oracle"){
          if(is.null(input$Oracle_Train.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Oracle_Train.table)
            drv <-JDBC("oracle.jdbc.OracleDriver",
                       classPath=Table[Table$Parameters=="ojdbc.jar Path","Insert"]," ") 
            myconn<- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:orcl",Table[Table$Parameters=="USERID","Insert"],
                               Table[Table$Parameters=="PASSWORD","Insert"]) 
            SPY <-dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            dbDisconnect(myconn) 
          }}
        if(input$DataBase.Train.type=="Amazon Redshift"){
          if(is.null(input$Amazon_Redshift_Train.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Amazon_Redshift_Train.table) 
            drv <- dbDriver(Table[Table$Parameters=="DRIVER","Insert"])   
            myconn <- dbConnect(drv, host=Table[Table$Parameters=="Host","Insert"], 
                                dbname=Table[Table$Parameters=="DATABASE","Insert"],
                                port=Table[Table$Parameters=="PORT","Insert"],
                                user=Table[Table$Parameters=="USER","Insert"],
                                password=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])
            dbDisconnect(myconn)
          }}  
        if(input$DataBase.Train.type=="Google Big Query"){
          if(is.null(input$Google_Big_Query_Train.table)){
            SPY <- NULL
          } else{
            gar_auth_service(
              json_file = input$json.file.Train$datapath,
              scope = "https://www.googleapis.com/auth/bigquery"
            )
            Table <- hot_to_r(input$Google_Big_Query_Train.table) 
            SPY<-bqr_query(projectId = Table[Table$Parameters=="ProjectID","Insert"],query =Table[Table$Parameters=="Query","Insert"],
                           datasetId = Table[Table$Parameters=="DatasetId","Insert"],useLegacySql = input$useLegacySql.Train)
          }}
        
      } , error=function(err) {
        Train.file.loading <<- paste("Error occured - File was not Loaded")
      })
      if(!is.null(SPY) && !is.character(SPY) && nrow(SPY)>0){ 
        Train.file.loading <<-""
        SPY<-as.data.table(SPY)
        if(input$Shuffle.Train=="Yes")
          SPY<-data_sample(SPY,NULL,"Just Shuffle")
        Train.file.loading <<- ""
        ##checking if we have longitude and latitude, and make from them extra variables
        if(all(c('latitude', 'longitude') %in% tolower(names(SPY)))){
          for(i in 1:8){
            SPY[,paste0("latitude_",i)]<-round(SPY[[which(tolower(names(SPY))=="latitude")]],i)
            SPY[,paste0("longitude",i)]<-round(SPY[[which(tolower(names(SPY))=="longitude")]],i)
          }
        }
        ##Converting integer64\Logical to numeric
        ind<-sapply(names(SPY), function(x) {class(SPY[[x]])[1] %in% c("logical","integer64")})
        for(i in names(SPY)[ind]){SPY[,i]<-as.numeric(SPY[[i]])}  
        
        ##Converting character to Factor
        ind<-sapply(names(SPY), function(x) {"character" %in% class(SPY[[x]])})
        for(i in names(SPY)[ind]){SPY[,i]<-as.factor(SPY[[i]])}   
        
        ###Class Date####
        ##Adding columns concerning dates
        ind1<-sapply(names(SPY), function(x) {"Date" %in% class(SPY[[x]])})
        ind2<-sapply(names(SPY), function(x) {"POSIXct" %in% class(SPY[[x]])})
        if(input$Dates.as.parts.Train=="No"){
          for(i in names(SPY)[ind1]){SPY[,i]<-as.factor(SPY[[i]])}   
          for(i in names(SPY)[ind2]){SPY[,i]<-as.factor(SPY[[i]])} 
        } else{ 
          for(i in names(SPY)[ind1]){
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_week")]<-as.factor(format(SPY[[i]],"%a"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%b"))
            SPY[,paste0(i,"_quarter")]<-as.factor(quarters(SPY[[i]]))
            SPY[,paste0(i,"_month_day")]<-as.factor(format(SPY[[i]],"%b %d"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }
          for(i in names(SPY)[ind2]){
            SPY[,paste0(i,"_sec")]<-as.factor(format(SPY[[i]],"%S"))
            SPY[,paste0(i,"_min")]<-as.factor(format(SPY[[i]],"%M"))
            SPY[,paste0(i,"_hour")]<-as.factor(format(SPY[[i]],"%H"))
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%m"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }   
        }#End of Dates.as.parts.Train if 
        ###########
        colnames(SPY)<-make.names(colnames(SPY),unique=TRUE) 
      } else{#End of !is.null(SPY) && !is.character(SPY) && ncol(SPY)>0
        SPY<-NULL
        Train.file.loading <<- paste("No Data")
      } #End of !is.null(SPY)
      print("reset the values")
      ########################################
      ###Reset currentmodel
      for(i in names(currentmodel))
        currentmodel[[i]]<-NULL
      ###Reset Text Analysis
      for(i in names(textAnalysis))
        textAnalysis[[i]]<-NULL
      ###Reset Variables Correlation
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ###Reset Data Analysis
      for(i in names(dataAnalysis))
        dataAnalysis[[i]]<-NULL
      ###Reset Var vs Target model
      for(i in names(var.vs.target.Model))
        var.vs.target.Model[[i]]<-NULL
      ###Reset Model Insights
      for(i in names(modelInsights))
        modelInsights[[i]]<-NULL
      ##########################################
      currentmodel$Basic.uploaded.data <- SPY
      currentmodel$uploaded.data <- SPY
      currentmodel$Calculate.filter.data<-FALSE
      ##########################################
      ##text analysis table
      Name<-c("","","")
      Variable<-c("","","")
      As.Variable<-c("","","")
      Num.of.words.after<-c("","","")
      Catch.till<-c("","","")
      As.Variable.using.regex<-c("","","")
      Isolated.String<-c("","","")
      Regex<-c("","","")
      Count.Pos.Exp<-c("","","")
      Count.Neg.Exp<-c("","","")
      Count.Other.Exp<-c("","","")
      Replace.Regex<-c("","","")
      Replacement<-c("","","")
      textAnalysis$text.analysis.fill.table.count <- data.frame("Name"=Name,"Variable"=Variable,
                                                          "Isolated String"=Isolated.String,"Count Pos Exp"=Count.Pos.Exp,
                                                          "Count Neg Exp"=Count.Neg.Exp,"Count Other Exp"=Count.Other.Exp,
                                                          stringsAsFactors=FALSE,check.names = F)   
      
      textAnalysis$text.analysis.fill.table.regex <- data.frame("Name"=Name,"Variable"=Variable,
                                                          "Regex"=Regex,
                                                          stringsAsFactors=FALSE,check.names = F)
      
      textAnalysis$text.analysis.fill.table.replace.regex <- data.frame("Name"=Name,"Variable"=Variable,
                                                                "Replace Regex"=Replace.Regex,
                                                                "Replacement"=Replacement,
                                                                stringsAsFactors=FALSE,check.names = F)   
      
      textAnalysis$text.analysis.fill.table.as.variable <- data.frame("Name"=Name,"Variable"=Variable,
                                                                      "As Variable"=As.Variable,
                                                                      "Num of words after"=Num.of.words.after,
                                                                      "Catch till"=Catch.till,
                                                          stringsAsFactors=FALSE,check.names = F)   
      
      textAnalysis$text.analysis.fill.table.as.variable.using.regex <- data.frame("Name"=Name,"Variable"=Variable,
                                                                      "As Variable using regex"=As.Variable.using.regex,
                                                                      stringsAsFactors=FALSE,check.names = F)   
      
    })
    
    ##Go to Load & Save tab
    observeEvent(input$DataBase.Train.type,{  
      if(input$DataBase.Train.type!="Load")
        return()
      updateTabsetPanel(session, "Front",selected = "Load & Save")
      toggleModal(session, "importdata_Train", toggle = "close")
    })
    ###Loading Text
    output$Train.file.loading <- renderText({
      input$MainSubmit
      Train.file.loading
    })

    ########################Data Filter##################################
    ##Show tab only if we have Data
    observe({
      if(!is.null(currentmodel$Basic.uploaded.data))
        showTab(inputId = "Main", target = "Filter Data")
      if(is.null(currentmodel$Basic.uploaded.data)){
        hideTab(inputId = "Main", target = "Filter Data")
        updateTabsetPanel(session, "Main",selected = "Import Data")
      }
    }) 
    
    observe({  
      if(!(input$Main=="Filter Data" && input$Filter=="Filter"))
        return()
      if(!is.null(currentmodel$Calculate.filter.data) && currentmodel$Calculate.filter.data)
        return()
      if(dim(currentmodel$Basic.uploaded.data)[2]<20)
        return()
      showModal(modalDialog(
        title = "Important message",
        HTML(paste('Filter will take few seconds')),
        footer = tagList(
          actionButton("no.calculate.filter.data", "Exit"),
          actionButton("yes.calculate.filter.data", "Continue")
        )
      ))   
    })
    
    observeEvent(input$no.calculate.filter.data,{
      removeModal()
      currentmodel$Calculate.filter.data<-FALSE
    })
    
    observeEvent(input$yes.calculate.filter.data,{
      removeModal()
      currentmodel$Calculate.filter.data<-TRUE
    })
    
    output$CalculateFilter<-reactive({
      input$datafile
      input$yes.calculate.filter.data
      return(!is.null(currentmodel$Basic.uploaded.data) && !is.null(currentmodel$Calculate.filter.data) &&
               currentmodel$Calculate.filter.data)
    })
    outputOptions(output, 'CalculateFilter', suspendWhenHidden=FALSE)
   
    ##########Slider and Select Inputs for Filter##############
    observe({
      if(is.null(currentmodel$Calculate.filter.data) || !currentmodel$Calculate.filter.data)
        return()
      if(!is.null(currentmodel$Basic.uploaded.data)){ 
        dat<-currentmodel$Basic.uploaded.data
        dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        output$Data.Filter.numeric <- renderUI({
          Names<-names(dat)
          Names<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
          if(length(Names)>0){
            plot_output_list <- lapply(Names, function(i) {
              filtername <- paste("Data_Filter_", i, sep="")
              Min <- min(dat[[i]],na.rm=T)
              Max <- max(dat[[i]],na.rm=T)
              ##value
              isolate({
                if(is.null(currentmodel$Loaded.Numeric.data.filter.table)){
                  value<-c(Min,Max)
                } else{
                  value<-currentmodel$Loaded.Numeric.data.filter.table[[filtername]]
                }
              })
              ###
              if(length(unique(cleaned.vec(dat[[i]])))<=2){
                column(4,sliderInput(filtername, 
                                     label = i, 
                                     min = Min, max = Max, value = value, step = (Max-Min)))
              } else{
                column(4,sliderInput(filtername, 
                                     label = i, 
                                     min = Min, max = Max, value = value,step=(Max-Min)/500))  
              }})
            #####
            isolate({
              if(!is.null(currentmodel$Loaded.Numeric.data.filter.table))
                currentmodel$Loaded.Numeric.data.filter.table<-NULL
            })
            ####
            fluidRow(do.call(tagList, plot_output_list))
          }}) 
        output$Data.Filter.factor <- renderUI({
          Names<-names(dat)
          Names<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
          if(length(Names)>0){
            plot_output_list <- lapply(Names, function(i) {
              filtername <- paste("Data_Filter_", i, sep="")
              ##selected
              isolate({
                if(is.null(currentmodel$Loaded.Factor.data.filter.table)){
                  selected<-"ALL"
                } else{
                  selected<-currentmodel$Loaded.Factor.data.filter.table[[filtername]]
                }
              })
              ##
              column(4,selectInput(filtername, 
                                   label = i, 
                                   choices=c("ALL",unique(as.character(dat[[i]]))),
                                   selected = selected,multiple = TRUE))
            })
            ###
            isolate({
              if(!is.null(currentmodel$Loaded.Factor.data.filter.table))
                currentmodel$Loaded.Factor.data.filter.table<-NULL
            })
            ####
            fluidRow(do.call(tagList, plot_output_list))
          }}) 
      }}) 
    
    
    
    #######################Data Filter actionButton################  
    observeEvent(input$MainSubmit.Data.Filter.Save, { 
      if(!is.null(currentmodel$Basic.uploaded.data)){
        showModal(modalDialog(
          title = "Important message",
          HTML(paste('All of your Inputs and Outputs will be reset<br/>',
                     'Would you like to proceed?')),
          footer = tagList(
            actionButton("no.data.filter.save", "No"),
            actionButton("yes.data.filter.save", "Yes")
          )
        ))
      }
    })
    
    observeEvent(input$no.data.filter.save,{
      removeModal()
    })
    
    observeEvent(input$yes.data.filter.save,{
      removeModal()
      dat<-currentmodel$Basic.uploaded.data
      dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
      Names<-names(dat)
      Names.numeric<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
      Names.factor<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
      ##
      Ind<-1:nrow(dat)
      currentmodel$Numeric.data.filter.table<-list()
      currentmodel$Factor.data.filter.table<-list()
      for(i in Names.numeric){
        temp<-input[[paste("Data_Filter_", i, sep="")]]
        currentmodel$Numeric.data.filter.table[[paste("Data_Filter_", i, sep="")]]<-temp
        if(is.null(temp)){next}
        ind<-union(intersect(which(dat[[i]]>=temp[1]),which(dat[[i]]<=temp[2])),which(!cleaned.places(dat[[i]])))
        Ind<-intersect(Ind,ind)
      }
      for(i in Names.factor){
        temp<-input[[paste("Data_Filter_", i, sep="")]]
        currentmodel$Factor.data.filter.table[[paste("Data_Filter_", i, sep="")]]<-temp
        if(is.null(temp) || "ALL" %in% temp){next}
        ind<-union(which(dat[[i]] %in% temp),which(!cleaned.places(dat[[i]])))
        Ind<-intersect(Ind,ind)
      }
      ########################################
      Vars.Summary<-currentmodel$Vars.Summary
      ###Reset currentmodel
      names.not.to.reset<-c("Basic.uploaded.data","Calculate.filter.data","Data.Filter.Index","Numeric.data.filter.table","Factor.data.filter.table")
      for(i in names(currentmodel)[!names(currentmodel) %in% names.not.to.reset])
        currentmodel[[i]]<-NULL
      ###Reset Variables Correlation
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ###Reset Data Analysis
      for(i in names(dataAnalysis))
        dataAnalysis[[i]]<-NULL
      ###Reset Var vs Target model
      for(i in names(var.vs.target.Model))
        var.vs.target.Model[[i]]<-NULL
      ###Reset Model Insights
      for(i in names(modelInsights))
        modelInsights[[i]]<-NULL
      #######
      currentmodel$Data.Filter.Index<-Ind
      currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data[Ind,]
      ##Update Vars.Summary
      criterion.Names<-c("Type","Levels","Missing","Min","Q1","Median","Mean","Q3","Max")
      Vars.Summary[,criterion.Names]<-update.vars.summary(currentmodel$uploaded.data)
      currentmodel$Vars.Summary<-Vars.Summary
    })
    ################Data Filter Reset actionButton################ 
    observeEvent(input$MainSubmit.Data.Filter.Reset, { 
      if(!is.null(currentmodel$Basic.uploaded.data)){
        showModal(modalDialog(
          title = "Important message",
          HTML(paste('All of your Inputs and Outputs will be reset<br/>',
                     'Would you like to proceed?')),
          footer = tagList(
            actionButton("no.data.filter.reset", "No"),
            actionButton("yes.data.filter.reset", "Yes")
          )
        ))
      }
    })
    
    observeEvent(input$no.data.filter.reset,{
      removeModal()
    })
    
    observeEvent(input$yes.data.filter.reset,{
      removeModal()
      dat<-currentmodel$Basic.uploaded.data
      dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
      Ind<-1:nrow(dat)
      ########################################
      print("reset the values")
      Vars.Summary<-currentmodel$Vars.Summary
      ###Reset currentmodel
      names.not.to.reset<-c("Basic.uploaded.data","Calculate.filter.data","Data.Filter.Index")
      for(i in names(currentmodel)[!names(currentmodel) %in% names.not.to.reset])
        currentmodel[[i]]<-NULL
      ###Reset Variables Correlation
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ###Reset Data Analysis
      for(i in names(dataAnalysis))
        dataAnalysis[[i]]<-NULL
      ###Reset Var vs Target model
      for(i in names(var.vs.target.Model))
        var.vs.target.Model[[i]]<-NULL
      ###Reset Model Insights
      for(i in names(modelInsights))
        modelInsights[[i]]<-NULL
      ########
      currentmodel$Data.Filter.Index<-Ind
      currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data[Ind,]
      ##Update Vars.Summary
      criterion.Names<-c("Type","Levels","Missing","Min","Q1","Median","Mean","Q3","Max")
      Vars.Summary[,criterion.Names]<-update.vars.summary(currentmodel$uploaded.data)
      currentmodel$Vars.Summary<-Vars.Summary
      ########################################
      Names<-names(dat)
      currentmodel$Numeric.data.filter.table<-list()
      currentmodel$Factor.data.filter.table<-list()
      ##
      Names.numeric<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
      for(i in Names.numeric){
        filtername <- paste("Data_Filter_", i, sep="")
        Min <- min(dat[[i]],na.rm=T)
        Max <- max(dat[[i]],na.rm=T)
        updateSliderInput(session,filtername,value=c(Min,Max))
        currentmodel$Numeric.data.filter.table[[filtername]]<-c(Min,Max)
      }
      ##
      Names.factor<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
      for(i in Names.factor){
        filtername <- paste("Data_Filter_", i, sep="")
        updateSelectInput(session,filtername,selected="ALL")
        currentmodel$Factor.data.filter.table[[filtername]]<-"ALL"
      }
    })
    
    ######Filter By Coding####
    Filter.by.coding.last_action<-""
    
    observeEvent(input$MainSubmit.Filter.by.coding, { 
      if(!is.null(currentmodel$Basic.uploaded.data)){
        showModal(modalDialog(
          title = "Important message",
          HTML(paste('All of your Inputs and Outputs will be reset<br/>',
                     'Would you like to proceed?')),
          footer = tagList(
            actionButton("no.Filter.by.coding", "No"),
            actionButton("yes.Filter.by.coding", "Yes")
          )
        ))
      }
    })
    
    
    observeEvent(input$no.Filter.by.coding,{
      removeModal()
      Filter.by.coding.last_action<<-""
    })
    
    observeEvent(input$yes.Filter.by.coding,{
      removeModal()
      SPY<-currentmodel$Basic.uploaded.data
      SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
      Ind<-1:nrow(SPY)
      rule.text<-input$Filter.by.coding.text
      attach(SPY)
      ind<-tryCatch({which(eval(parse(text=rule.text)))},
                    error=function(err){
                      Filter.by.coding.last_action<<-"Error occured"
                      return(NULL)})
      detach(SPY) 
      if(is.null(ind))
        return()
      
      Ind<-intersect(Ind,ind)
        ########################################
      print("reset the values")
      Vars.Summary<-currentmodel$Vars.Summary
      ###Reset currentmodel
      names.not.to.reset<-c("Basic.uploaded.data","Calculate.filter.data","Data.Filter.Index","Numeric.data.filter.table","Factor.data.filter.table")
      for(i in names(currentmodel)[!names(currentmodel) %in% names.not.to.reset])
        currentmodel[[i]]<-NULL
      ###Reset Variables Correlation
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ###Reset Data Analysis
      for(i in names(dataAnalysis))
        dataAnalysis[[i]]<-NULL
      ###Reset Var vs Target model
      for(i in names(var.vs.target.Model))
        var.vs.target.Model[[i]]<-NULL
      ###Reset Model Insights
      for(i in names(modelInsights))
        modelInsights[[i]]<-NULL
      #######
      currentmodel$Data.Filter.Index<-Ind
      currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data[Ind,]
      ##Update Vars.Summary
      criterion.Names<-c("Type","Levels","Missing","Min","Q1","Median","Mean","Q3","Max")
      Vars.Summary[,criterion.Names]<-update.vars.summary(currentmodel$uploaded.data)
      currentmodel$Vars.Summary<-Vars.Summary
    })
    
    output$Filter.by.coding.text.error<- renderText({
      input$no.Filter.by.coding 
      input$yes.Filter.by.coding
      Filter.by.coding.last_action
    })
    
    ######Show Dimensions###
    output$showDataDimensions.Filter<- renderText({
      if (!is.null(currentmodel$uploaded.data)) {
        SPY<-currentmodel$uploaded.data  
        paste("Dimensions", dim(SPY)[1], "X" , dim(SPY)[2])
      } else{
        ""
      }
    })
    
    ##########################End of Data Filter######################################################
 
    ################################################################################
    ############################Text Analysis################################################
    
    ##########Choose Expressions/StopWords#################
    ##################Positive#############################
    ####Import Positive Words File
    observe({
      if(!is.null(textAnalysis$Pos.Exp.table))
        return()
      table<-read.table("Pos_Exp.csv")
      colnames(table)<-"Positive Expressions"         
      textAnalysis$Pos.Exp.table<-table   
    })
    
    pos.exp.last_action<-""  
    observeEvent(input$MainSubmit.for.pos.exp,{
      tryCatch({
        table<-NULL 
        if(input$Our.Pos.Exp=="Yes"){
          table<-read.table("Pos_Exp.csv")
          colnames(table)<-"Positive Expressions"
          pos.exp.last_action<<-""
        } else{ ##The user chooses his stopwords
          inFile <- input$datafile.for.pos.exp
          if (is.null(inFile)) { ##There is no file
            table<-NULL
            pos.exp.last_action<<-paste("Choose a file")
          } else{ ##There is a file
            temp <- import(inFile$datapath,format =split(inFile$name))
            temp<-as.data.frame(temp)
            if(ncol(temp)==1){ ##The file should contain one columns
              table<-temp
              colnames(table)<-"Positive Expressions"
              pos.exp.last_action<<-""
            } else{
              table<-NULL
              pos.exp.last_action<<-paste("The File must contain one column")
            }
          }
        }
        textAnalysis$Pos.Exp.table<-table   
      } , error=function(err) {
        pos.exp.last_action <<- paste("Error occured - File was not Loaded")
      })
    })
    
    output$Pos.Exp.error.text<-renderText({
      input$MainSubmit.for.pos.exp
      pos.exp.last_action
    })    
    
    output$Pos.Exp<-renderRHandsontable({
      if(!is.null(textAnalysis$Pos.Exp.table))
        rhandsontable(textAnalysis$Pos.Exp.table,height = 300,maxRows=nrow(textAnalysis$Pos.Exp.table)) %>% 
        hot_col(colnames(textAnalysis$Pos.Exp.table),width=150,readOnly=TRUE)
    }) 
    
    #######################Negative#############################
    ####Import Negative Words File
    observe({
      if(!is.null(textAnalysis$Neg.Exp.table) && input$Our.Neg.Exp=="Yes")
        return()
      table<-read.table("Neg_Exp.csv")
      colnames(table)<-"Negative Expressions"
      textAnalysis$Neg.Exp.table<-table   
    })
    
    neg.exp.last_action<-""
    observeEvent(input$MainSubmit.for.neg.exp,{
      tryCatch({
        table<-NULL 
        if(input$Our.Neg.Exp=="Yes"){
          table<-read.table("Neg_Exp.csv")
          colnames(table)<-"Negative Expressions"
          neg.exp.last_action<<-""
        } else{ ##The user chooses his stopwords
          inFile <- input$datafile.for.neg.exp
          if (is.null(inFile)) { ##There is no file
            table<-NULL
            neg.exp.last_action<<-paste("Choose a file")
          } else{ ##There is a file
            temp <- import(inFile$datapath,format =split(inFile$name))
            temp<-as.data.frame(temp)
            if(ncol(temp)==1){ ##The file should contain one columns
              table<-temp
              colnames(table)<-"Negative Expressions"
              neg.exp.last_action<<-""
            } else{
              table<-NULL
              neg.exp.last_action<<-paste("The File must contain one column")
            }
          }
        }
        textAnalysis$Neg.Exp.table<-table
      } , error=function(err) {
        neg.exp.last_action <<- paste("Error occured - File was not Loaded")
      })
    })
    
    output$Neg.Exp.error.text<-renderText({
      input$MainSubmit.for.neg.exp
      neg.exp.last_action
    })    
    output$Neg.Exp<-renderRHandsontable({
      if(!is.null(textAnalysis$Neg.Exp.table))
        rhandsontable(textAnalysis$Neg.Exp.table,height = 300,maxRows=nrow(textAnalysis$Neg.Exp.table)) %>% 
        hot_col(colnames(textAnalysis$Neg.Exp.table),width=150,readOnly=TRUE)
    }) 
    
    ####Other
    other.exp.last_action<-""
    observeEvent(input$MainSubmit.for.other.exp,{
      tryCatch({
        table<-NULL 
        inFile <- input$datafile.for.other.exp
        if (is.null(inFile)) { ##There is no file
          table<-NULL
          other.exp.last_action<<-paste("Choose a file")
        } else{ ##There is a file
          temp <- import(inFile$datapath,format =split(inFile$name))
          temp<-as.data.frame(temp)
          if(ncol(temp)==1){ ##The file should contain one columns
            table<-temp
            colnames(table)<-"Other Expressions"
            other.exp.last_action<<-""
          } else{
            table<-NULL
            other.exp.last_action<<-paste("The File must contain one column")
          }
        }
        textAnalysis$Other.Exp.table<-table
      } , error=function(err) {
        other.exp.last_action <<- paste("Error occured - File was not Loaded")
      })
    })
    
    output$Other.Exp.error.text<-renderText({
      input$MainSubmit.for.other.exp
      other.exp.last_action
    })  
    output$Other.Exp<-renderRHandsontable({
      if(!is.null(textAnalysis$Other.Exp.table))
        rhandsontable(textAnalysis$Other.Exp.table,height = 300,maxRows=nrow(textAnalysis$Other.Exp.table)) %>% 
        hot_col(colnames(textAnalysis$Other.Exp.table),width=150,readOnly=TRUE)
    })
    
    
    #################Stopwords###################
    ##Import Stopwords File
    observe({
      if(!is.null(textAnalysis$StopWords.table) && input$Our.StopWords=="Yes")
        return()
      table<-read.table("stopwords.csv")
      colnames(table)<-"StopWords"
      textAnalysis$StopWords.table<-table   
    })
    
    stopwords.last_action<-""
    observeEvent(input$MainSubmit.for.stopwords,{
      tryCatch({
        table<-NULL 
        if(input$Our.StopWords=="Yes"){
          table<-read.table("stopwords.csv")
          colnames(table)<-"StopWords"
          stopwords.last_action<<-""
        } else{ ##The user chooses his stopwords
          inFile <- input$datafile.for.stopwords
          if (is.null(inFile)) { ##There is no file
            table<-NULL
            stopwords.last_action<<-paste("Choose a file")
          } else{ ##There is a file
            temp <- import(inFile$datapath,format =split(inFile$name))
            temp<-as.data.frame(temp)
            if(ncol(temp)==1){ ##The file should contain only one column
              table<-temp
              colnames(table)<-"StopWords"
              stopwords.last_action<<-""
            } else{
              table<-NULL
              stopwords.last_action<<-paste("The File must contain only one column")
            }
          }
        }
        textAnalysis$StopWords.table<-table
      } , error=function(err) {
        stopwords.last_action <<- paste("Error occured - File was not Loaded")
      })
    })
    
    output$StopWords.error.text<-renderText({
      input$MainSubmit.for.stopwords
      stopwords.last_action
    })    
    output$StopWords<-renderRHandsontable({
      if(!is.null(textAnalysis$StopWords.table))
        rhandsontable(textAnalysis$StopWords.table,height = 300,maxRows=nrow(textAnalysis$StopWords.table)) %>% 
        hot_col(colnames(textAnalysis$StopWords.table),width=150,readOnly=TRUE)
    })
    #####################End of Choose Expressions/StopWords#################################

    Have.char<-reactive({
      if(!is.null(currentmodel$uploaded.data) && nrow(currentmodel$uploaded.data)>0){
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        return(sum(sapply(names(SPY),function(x){is.factor(SPY[[x]])}))>0)
      } else{ 
        return(FALSE)
      }
    })
        
    observe({
      if(Have.char()==TRUE)
        showTab(inputId = "Front", target = "Text Analysis")
      if(Have.char()==FALSE)
        hideTab(inputId = "Front", target = "Text Analysis")
    })  
    
    ##Go to View Data
    observeEvent(input$View.Data.Text_Analysis,{  
      updateTabsetPanel(session, "Front",selected = "Main")
    })
    
    output$text_Analysis_char_is_chosen<-reactive({
      return(!is.null(textAnalysis$Char.variable.to.plot) && length(textAnalysis$Char.variable.to.plot[["x"]])>0)
    })
    outputOptions(output, 'text_Analysis_char_is_chosen', suspendWhenHidden=FALSE)
    
    ###########################Show table/Plot##############################
    ##Showing the phrases 
    output$Choose.variable.for.text.analysis<-DT::renderDataTable({
      if (!is.null(currentmodel$uploaded.data)) { 
        SPY <- currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]   
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        temp <- data.frame(Variable=names(SPY), stringsAsFactors=FALSE)
        datatable(temp,selection = "single",caption="Choose Variable",
                  rownames=NULL, options = list(pageLength = 30,ordering=F,searching = FALSE,lengthChange=FALSE))   #dom = 't'
      } else{
        NULL
      }  
    }, server = FALSE)


   observe({
     if (!is.null(input$Choose.variable.for.text.analysis_rows_selected)) {
       row_num<-input$Choose.variable.for.text.analysis_rows_selected
       SPY <- currentmodel$uploaded.data
       SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
       SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
       textAnalysis$text.analysis_chosen.var<-names(SPY)[row_num]
     } else{
       textAnalysis$text.analysis_chosen.var<- NULL
     }
   })
    
    
   observeEvent(input$MainSubmit.Choose.variable.for.text.analysis,{
     if(!is.null(textAnalysis$text.analysis_chosen.var)){
       SPY <- currentmodel$uploaded.data
       char.var.to.show <- textAnalysis$text.analysis_chosen.var
       variable.to.plot<-SPY[[char.var.to.show]]
       if(input$text.analysis.split=="Words"){
         if(input$Words_split>0){
           x<-write.all.combinations(variable.to.plot,input$Words_split)
           if(input$Show.only.Exp=="Neg"){
             temp<-textAnalysis$Neg.Exp.table
             Neg.Exp<-as.character(temp[,"Negative Expressions"])
             x<-x[(x %in% Neg.Exp)]
           }
           if(input$Show.only.Exp=="Pos"){
             temp<-textAnalysis$Pos.Exp.table
             Pos.Exp<-as.character(temp[,"Positive Expressions"])
             x<-x[(x %in% Pos.Exp)]
           }
           if(input$Show.only.Exp=="Other"){
             temp<-textAnalysis$Other.Exp.table
             Other.Exp<-as.character(temp[,"Other Expressions"])
             x<-x[(x %in% Other.Exp)]
           }
           if(input$Exclude.stopwords=="Yes"){   
             temp<-textAnalysis$StopWords.table
             StopWords<-as.character(temp[,"StopWords"])
             x<-x[!(x %in% StopWords)]
           }
           textAnalysis$Char.variable.to.plot<-list(x=x,x.name=char.var.to.show)
         } else{ #input$Words_split<=0 
           textAnalysis$Char.variable.to.plot<-NULL
         }#End of #input$Words_split>0  
       }
       if(input$text.analysis.split=="Sentences"){
         sentences<-unlist(lapply(as.character(variable.to.plot),function(x){unlist(strsplit(x,"[.]|[,]"))}))
         textAnalysis$Char.variable.to.plot<-list(x=sentences,x.name=char.var.to.show)
       }
       if(input$text.analysis.split=="No"){
         textAnalysis$Char.variable.to.plot<-list(x=as.character(variable.to.plot),x.name=char.var.to.show)
       }
     } else{ ##is.null(textAnalysis$text.analysis_chosen.var)
       textAnalysis$Char.variable.to.plot<-NULL
     }
   })
    
    output$text.analysis.variable.table<-DT::renderDataTable({
      if(!is.null(textAnalysis$Char.variable.to.plot)){
        x.name<-textAnalysis$Char.variable.to.plot[["x.name"]]
        SPY <- currentmodel$uploaded.data
        temp<-SPY[,x.name,with=FALSE]
        datatable(temp,selection = "single",caption="Content",rownames=NULL,
                 options=list(
                   columnDefs = list(list(
                     targets  = 0,
                     render  = JS(
                       "function ( data, type ) {",
                       "return type === 'display' && data!==null && data!=='' && data.length > 300 ?",
                       "data.substr(0, 300) +'...' :",
                       "data;",
                       "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(0),
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE))
      } else{
        NULL
      }
    }, server = FALSE)
    
    output$text.analysis.phrases.table<-DT::renderDataTable({
      if(!is.null(textAnalysis$Char.variable.to.plot)){
        x<-textAnalysis$Char.variable.to.plot[["x"]]
        if(length(x)>0){
          temp<-as.data.frame(table(x))
          colnames(temp)<-c("Phrase","Freq")
          temp<-temp[order(-temp$Freq),]
        } else{
          temp<-data.frame(matrix(NA, nrow = 0, ncol = 2))
          colnames(temp)<-c("Phrase","Freq")
        }
        datatable(temp,selection = "single",caption="Content Breakdown",rownames=NULL,
                  options=list(
                    columnDefs = list(list(
                      targets  = 0,
                      render  = JS(
                        "function ( data, type ) {",
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(0),  #we want to have tooltip on phrase (its 0 because rownames=NULL)
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE))
      } else{
        NULL
      }
    }, server = FALSE)
    
    output$text.analysis.phrases.plot<-renderPlot({   
      if(!is.null(textAnalysis$Char.variable.to.plot)){
        x<-textAnalysis$Char.variable.to.plot[["x"]]
        if(length(x)>0){
          temp<-as.data.frame(table(x))
          colnames(temp)<-c("Phrase","Freq")
          p<-ggplot(temp,aes(x=Phrase,y=Freq))+scale_x_discrete(labels = function(x) str_wrap(x, width = 25),limits = temp$Phrase[order(temp$Freq)])+
            geom_bar(color="darkblue", fill="lightblue",stat="identity")+ coord_flip()
          
          p<-p+theme(
            text = element_text(size=15),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.position="none",
            plot.background = element_rect(fill="white")
          )
          
          p
        }}
    }, height=function(){
      temp<-textAnalysis$Char.variable.to.plot[["x"]]
      if(is.null(temp)){
        return(380)
      } else{
        return(max(380,length(unique(temp))*15))
      }})
    
    ################
    ##Export Plot
    observeEvent(input$MainSubmit.text.analysis.phrases.plot.export,{   
      if(!is.null(textAnalysis$Char.variable.to.plot) && length(textAnalysis$Char.variable.to.plot[["x"]])>0){
        if(input$path.text.analysis.phrases.plot.export!=""){
          temp<-as.data.frame(table(textAnalysis$Char.variable.to.plot[["x"]]))
          colnames(temp)<-c("Phrase","Freq")
          p<-ggplot(temp,aes(x=Phrase,y=Freq))+scale_x_discrete(labels = function(x) str_wrap(x, width = 25),limits = temp$Phrase[order(temp$Freq)])+
            geom_bar(color="darkblue", fill="lightblue",stat="identity")+ coord_flip()
          
          p<-p+theme(
            text = element_text(size=15),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.position="none",
            plot.background = element_rect(fill="white")
          ) 
          
          tryCatch({   
            lib<-getwd()
            temp.lib<-"C:/AlgoTraceFolder.export" 
            setwd(temp.lib)
            ext<-input$text.analysis.phrases.plot.export.extension 
            ggsave(file=paste0(input$path.text.analysis.phrases.plot.export,paste0(".",ext)), p, device=ext)
            text.analysis.phrases.plot.export.last_action<<-"File was Exported to C://AlgoTraceFolder.export/"
            setwd(lib) 
          }, error=function(err) {
            text.analysis.phrases.plot.export.last_action <<- "Error occured - File was not Exported"
          })
        } else{
          text.analysis.phrases.plot.export.last_action <<-"Insert File name"
        }
      } else{   
        text.analysis.phrases.plot.export.last_action <<- "No Plot to export"
      }
    })
    
    output$text.analysis.phrases.plot.export.text <- renderText({
      input$MainSubmit.text.analysis.phrases.plot.export
      text.analysis.phrases.plot.export.last_action
    })
    ################
    ##Export Table
    observeEvent(input$MainSubmit.text.analysis.phrases.table.export,{   
      if(!is.null(textAnalysis$Char.variable.to.plot) && length(textAnalysis$Char.variable.to.plot[["x"]])>0){
        if(input$path.text.analysis.phrases.table.export!=""){
          temp<-as.data.frame(table(textAnalysis$Char.variable.to.plot[["x"]]))
          colnames(temp)<-c("Phrase","Freq")
          temp<-temp[order(-temp$Freq),]
          tryCatch({
            write_excel_csv(temp,path = paste0("C://AlgoTraceFolder.export/",input$path.text.analysis.phrases.table.export,".csv"))
            text.analysis.phrases.table.export.last_action<<-"Data was Exported to C://AlgoTraceFolder.export/"
          }, error=function(err) {
            text.analysis.phrases.table.export.last_action <<- "Error occured - Data was not Exported"
          })
        } else{
          text.analysis.phrases.table.export.last_action <<-"Insert File name"
        }
      } else{
        text.analysis.phrases.table.export.last_action <<- "No Data to export"
      }
    })
    
    output$text.analysis.phrases.table.export.text <- renderText({
      input$MainSubmit.text.analysis.phrases.table.export
      text.analysis.phrases.table.export.last_action
    })
    ###########################End of Show table/Plot##############################
    
     observeEvent(input$text.analysis.fill.table.count,{
      if(!is.null(textAnalysis$text.analysis.fill.table.count) && !is.null(input$text.analysis.fill.table.count) &&
         nrow(hot_to_r(input$text.analysis.fill.table.count))>nrow(textAnalysis$text.analysis.fill.table.count)){
        temp<-hot_to_r(input$text.analysis.fill.table.count)
        ind<-which(is.na(temp[nrow(temp),]))
        temp[nrow(temp),ind]<-""
        textAnalysis$text.analysis.fill.table.count<-temp
      } else{
        textAnalysis$text.analysis.fill.table.count <- hot_to_r(input$text.analysis.fill.table.count)
      }
    })
    
    observeEvent(input$text.analysis.fill.table.regex,{
      if(!is.null(textAnalysis$text.analysis.fill.table.regex) && !is.null(input$text.analysis.fill.table.regex) &&
         nrow(hot_to_r(input$text.analysis.fill.table.regex))>nrow(textAnalysis$text.analysis.fill.table.regex)){
        temp<-hot_to_r(input$text.analysis.fill.table.regex)
        ind<-which(is.na(temp[nrow(temp),]))
        temp[nrow(temp),ind]<-""
        textAnalysis$text.analysis.fill.table.regex<-temp
      } else{
        textAnalysis$text.analysis.fill.table.regex <- hot_to_r(input$text.analysis.fill.table.regex)
      }
    })
    
    observeEvent(input$text.analysis.fill.table.replace.regex,{
      if(!is.null(textAnalysis$text.analysis.fill.table.replace.regex) && !is.null(input$text.analysis.fill.table.replace.regex) &&
         nrow(hot_to_r(input$text.analysis.fill.table.replace.regex))>nrow(textAnalysis$text.analysis.fill.table.replace.regex)){
        temp<-hot_to_r(input$text.analysis.fill.table.replace.regex)
        ind<-which(is.na(temp[nrow(temp),]))
        temp[nrow(temp),ind]<-""
        textAnalysis$text.analysis.fill.table.replace.regex<-temp
      } else{
        textAnalysis$text.analysis.fill.table.replace.regex <- hot_to_r(input$text.analysis.fill.table.replace.regex)
      }
    })
    
    observeEvent(input$text.analysis.fill.table.as.variable,{
      if(!is.null(textAnalysis$text.analysis.fill.table.as.variable) && !is.null(input$text.analysis.fill.table.as.variable) &&
         nrow(hot_to_r(input$text.analysis.fill.table.as.variable))>nrow(textAnalysis$text.analysis.fill.table.as.variable)){
        temp<-hot_to_r(input$text.analysis.fill.table.as.variable)
        ind<-which(is.na(temp[nrow(temp),]))
        temp[nrow(temp),ind]<-""
        textAnalysis$text.analysis.fill.table.as.variable<-temp
      } else{
        textAnalysis$text.analysis.fill.table.as.variable <- hot_to_r(input$text.analysis.fill.table.as.variable)
      }
    })
    
    observeEvent(input$text.analysis.fill.table.as.variable.using.regex,{
      if(!is.null(textAnalysis$text.analysis.fill.table.as.variable.using.regex) && !is.null(input$text.analysis.fill.table.as.variable.using.regex) &&
         nrow(hot_to_r(input$text.analysis.fill.table.as.variable.using.regex))>nrow(textAnalysis$text.analysis.fill.table.as.variable.using.regex)){
        temp<-hot_to_r(input$text.analysis.fill.table.as.variable.using.regex)
        ind<-which(is.na(temp[nrow(temp),]))
        temp[nrow(temp),ind]<-""
        textAnalysis$text.analysis.fill.table.as.variable.using.regex<-temp
      } else{
        textAnalysis$text.analysis.fill.table.as.variable.using.regex <- hot_to_r(input$text.analysis.fill.table.as.variable.using.regex)
      }
    })

    
    output$text.analysis.fill.table.count<-renderRHandsontable({
      if(!is.null(textAnalysis$text.analysis.fill.table.count)){
        temp<-textAnalysis$text.analysis.fill.table.count
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        Variables.choices<-names(SPY)
        Exp.choices<-c("No","Yes")
        rhandsontable(temp) %>%  
          hot_col("Name",width=120,strict = FALSE)   %>%
          hot_col("Variable",width=120,type = "autocomplete",source = Variables.choices)   %>%
          hot_col("Isolated String",width=120,strict = FALSE) %>%
          hot_col("Count Pos Exp",width=120,type = "autocomplete",source = Exp.choices) %>%
          hot_col("Count Neg Exp",width=120,type = "autocomplete",source = Exp.choices) %>% 
          hot_col("Count Other Exp",width=120,type = "autocomplete",source = Exp.choices) 
      }}) 
    
    output$text.analysis.fill.table.regex<-renderRHandsontable({
      if(!is.null(textAnalysis$text.analysis.fill.table.regex)){
        temp<-textAnalysis$text.analysis.fill.table.regex
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        Variables.choices<-names(SPY)
        rhandsontable(temp) %>%  
          hot_col("Name",width=120,strict = FALSE)   %>%
          hot_col("Variable",width=120,type = "autocomplete",source = Variables.choices)   %>%
          hot_col("Regex",width=400,strict = FALSE)
      }})
    
    output$text.analysis.fill.table.replace.regex<-renderRHandsontable({
      if(!is.null(textAnalysis$text.analysis.fill.table.replace.regex)){
        temp<-textAnalysis$text.analysis.fill.table.replace.regex
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        Variables.choices<-names(SPY)
        rhandsontable(temp) %>%  
          hot_col("Name",width=120,strict = FALSE)   %>%
          hot_col("Variable",width=120,type = "autocomplete",source = Variables.choices)   %>%
          hot_col("Replace Regex",width=400,strict = FALSE) %>%
          hot_col("Replacement",width=400,strict = FALSE)
      }})
    
    output$text.analysis.fill.table.as.variable<-renderRHandsontable({
      if(!is.null(textAnalysis$text.analysis.fill.table.as.variable)){
        temp<-textAnalysis$text.analysis.fill.table.as.variable
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        Variables.choices<-names(SPY)
        rhandsontable(temp) %>%  
          hot_col("Name",width=120,strict = FALSE)   %>%
          hot_col("Variable",width=120,type = "autocomplete",source = Variables.choices)   %>%
          hot_col("As Variable",width=150,strict = FALSE) %>%
          hot_col("Num of words after",width=150,strict = FALSE) %>%
          hot_col("Catch till",width=150,strict = FALSE) 
      }})
    
    output$text.analysis.fill.table.as.variable.using.regex<-renderRHandsontable({
      if(!is.null(textAnalysis$text.analysis.fill.table.as.variable.using.regex)){
        temp<-textAnalysis$text.analysis.fill.table.as.variable.using.regex
        SPY<-currentmodel$uploaded.data
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        SPY<-SPY[,sapply(names(SPY),function(x){is.factor(SPY[[x]])}),with=FALSE] 
        Variables.choices<-names(SPY)
        rhandsontable(temp) %>%  
          hot_col("Name",width=120,strict = FALSE)   %>%
          hot_col("Variable",width=120,type = "autocomplete",source = Variables.choices)   %>%
          hot_col("As Variable using regex",width=400,strict = FALSE) 
      }})
    
    
    #########################
    observeEvent(input$MainSubmit.text.analysis.fill.table.save, { 
      if(!is.null(currentmodel$Basic.uploaded.data)){
        showModal(modalDialog(
          title = "Important message",
          HTML(paste('All of your Inputs and Outputs will be reset<br/>',
                     'Would you like to proceed?')),
          footer = tagList(
            actionButton("no.text.analysis", "No"),
            actionButton("yes.text.analysis", "Yes")
          )
        ))
      }
    })
    
    observeEvent(input$no.text.analysis, { 
      removeModal()
      textAnalysis$text.analysis.for.export<-NULL
    })
    
    observeEvent(input$yes.text.analysis, { 
      removeModal()
      ###############
      count.table<-tryCatch({hot_to_r(input$text.analysis.fill.table.count)},error=function(err){return(NULL)})  
      regex.table<-tryCatch({hot_to_r(input$text.analysis.fill.table.regex)},error=function(err){return(NULL)})  
      replace.regex.table<-tryCatch({hot_to_r(input$text.analysis.fill.table.replace.regex)},error=function(err){return(NULL)})  
      as.variable.table<-tryCatch({hot_to_r(input$text.analysis.fill.table.as.variable)},error=function(err){return(NULL)})  
      as.variable.using.regex.table<-tryCatch({hot_to_r(input$text.analysis.fill.table.as.variable.using.regex)},error=function(err){return(NULL)})  
      
      temp<-data.frame("Name"="","Variable"="","Isolated String"="","Count Pos Exp"="",
                       "Count Neg Exp"="","Count Other Exp"="","Regex"="",
                       "Replace Regex"="","Replacement"="","As Variable"="","Num of words after"="",
                       "Catch till"="","As Variable using regex"="",
                       stringsAsFactors = FALSE,check.names = F)
      
      if(!is.null(count.table)){
      for(i in 1:nrow(count.table))
        temp<-rbind(temp,data.frame("Name"=count.table[i,"Name"],"Variable"=count.table[i,"Variable"],
                                    "Isolated String"=count.table[i,"Isolated String"],
                                    "Count Pos Exp"=count.table[i,"Count Pos Exp"],
                                    "Count Neg Exp"=count.table[i,"Count Neg Exp"],
                                    "Count Other Exp"=count.table[i,"Count Other Exp"],
                                    "Regex"="",
                                    "Replace Regex"="",
                                    "Replacement"="",
                                    "As Variable"="",
                                    "Num of words after"="",
                                    "Catch till"="",
                                    "As Variable using regex"="",
                                    stringsAsFactors = FALSE,check.names = F))
      }
      
      
      if(!is.null(regex.table)){
      for(i in 1:nrow(regex.table))
        temp<-rbind(temp,data.frame("Name"=regex.table[i,"Name"],"Variable"=regex.table[i,"Variable"],
                                    "Isolated String"="",
                                    "Count Pos Exp"="",
                                    "Count Neg Exp"="",
                                    "Count Other Exp"="",
                                    "Regex"=regex.table[i,"Regex"],
                                    "Replace Regex"="",
                                    "Replacement"="",
                                    "As Variable"="",
                                    "Num of words after"="",
                                    "Catch till"="",
                                    "As Variable using regex"="",
                                    stringsAsFactors = FALSE,check.names = F))
      }
      
      if(!is.null(replace.regex.table)){
      for(i in 1:nrow(replace.regex.table))
        temp<-rbind(temp,data.frame("Name"=replace.regex.table[i,"Name"],"Variable"=replace.regex.table[i,"Variable"],
                                    "Isolated String"="",
                                    "Count Pos Exp"="",
                                    "Count Neg Exp"="",
                                    "Count Other Exp"="",
                                    "Regex"="",
                                    "Replace Regex"=replace.regex.table[i,"Replace Regex"],
                                    "Replacement"=replace.regex.table[i,"Replacement"],
                                    "As Variable"="",
                                    "Num of words after"="",
                                    "Catch till"="",
                                    "As Variable using regex"="",
                                    stringsAsFactors = FALSE,check.names = F))
      }
      
      if(!is.null(as.variable.table)){
      for(i in 1:nrow(as.variable.table))
        temp<-rbind(temp,data.frame("Name"=as.variable.table[i,"Name"],"Variable"=as.variable.table[i,"Variable"],
                                    "Isolated String"="",
                                    "Count Pos Exp"="",
                                    "Count Neg Exp"="",
                                    "Count Other Exp"="",
                                    "Regex"="",
                                    "Replace Regex"="",
                                    "Replacement"="",
                                    "As Variable"=as.variable.table[i,"As Variable"],
                                    "Num of words after"=as.variable.table[i,"Num of words after"],
                                    "Catch till"=as.variable.table[i,"Catch till"],
                                    "As Variable using regex"="",
                                    stringsAsFactors = FALSE,check.names = F))
      }
      
      if(!is.null(as.variable.using.regex.table)){
        for(i in 1:nrow(as.variable.using.regex.table))
          temp<-rbind(temp,data.frame("Name"=as.variable.using.regex.table[i,"Name"],"Variable"=as.variable.using.regex.table[i,"Variable"],
                                      "Isolated String"="",
                                      "Count Pos Exp"="",
                                      "Count Neg Exp"="",
                                      "Count Other Exp"="",
                                      "Regex"="",
                                      "Replace Regex"="",
                                      "Replacement"="",
                                      "As Variable"="",
                                      "Num of words after"="",
                                      "Catch till"="",
                                      "As Variable using regex"=as.variable.using.regex.table[i,"As Variable using regex"],
                                      stringsAsFactors = FALSE,check.names = F))
      }
      ##############
      if(input$transfer.string.to.dummy=="Yes" && !is.null(textAnalysis$Char.variable.to.plot)){
        x<-textAnalysis$Char.variable.to.plot[["x"]]
        x.name<-textAnalysis$Char.variable.to.plot[["x.name"]]
        x<-unique(x)
        transfer.string.to.dummy.table<-data.frame("Name"=rep("",length(x)),"Variable"=rep("",length(x)),
                                                   "Isolated String"=rep("",length(x)),
                                                   "Count Pos Exp"=rep("",length(x)),
                                                   "Count Neg Exp"=rep("",length(x)),
                                                   "Count Other Exp"=rep("",length(x)),
                                                   "Regex"=rep("",length(x)),
                                                   "Replace Regex"=rep("",length(x)),
                                                   "Replacement"=rep("",length(x)),
                                                   "As Variable"=rep("",length(x)),
                                                   "Num of words after"=rep("",length(x)),
                                                   "Catch till"=rep("",length(x)),
                                                   "As Variable using regex"=rep("",length(x)),
                                                   stringsAsFactors = FALSE,check.names = F)
        
        transfer.string.to.dummy.table[,"Name"]<-ifelse(nchar(x)>30,paste0(substr(x,0,30),"..."),x)
        transfer.string.to.dummy.table[,"Variable"]<-x.name
        transfer.string.to.dummy.table[,"Isolated String"]<-x
        temp<-rbind(temp,transfer.string.to.dummy.table)
      }
      temp<-temp[which(temp[,"Variable"]!="" & temp[,"Name"]!=""),]
      ##############
      if(!is.null(textAnalysis$old.text.analysis.fill.table)){
        a1<-temp
        a2<-textAnalysis$old.text.analysis.fill.table
        table<-rows.in.a1.that.are.not.in.a2(a1,a2)
      } else{
        table<-temp
      }
      
      if(nrow(table)>0)
      textAnalysis$old.text.analysis.fill.table<-temp
      #####
      
      if(nrow(table)==0)
        return()
      
      Pos.Exp.table<-textAnalysis$Pos.Exp.table
      Neg.Exp.table<-textAnalysis$Neg.Exp.table
      Other.Exp.table<-textAnalysis$Other.Exp.table
      
      textAnalysis$text.analysis.for.export<-list(Pos.Exp.table=Pos.Exp.table,
                                                  Neg.Exp.table=Neg.Exp.table,
                                                  Other.Exp.table=Other.Exp.table,
                                                  table=table)
      ##performing the same function on the whole data incase there is a filter
      temp.data<-currentmodel$Basic.uploaded.data
      currentmodel$Basic.uploaded.data<-turn.text.to.data(temp.data,Pos.Exp.table,Neg.Exp.table,Other.Exp.table,table) 
      ########################################
      print("reset the values")
      ###Reset currentmodel
      names.not.to.reset<-"Basic.uploaded.data"
      for(i in names(currentmodel)[!names(currentmodel) %in% names.not.to.reset])
        currentmodel[[i]]<-NULL
      ###Reset Variables Correlation
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ###Reset Data Analysis
      for(i in names(dataAnalysis))
        dataAnalysis[[i]]<-NULL
      ###Reset Var vs Target model
      for(i in names(var.vs.target.Model))
        var.vs.target.Model[[i]]<-NULL
      ###Reset Model Insights
      for(i in names(modelInsights))
        modelInsights[[i]]<-NULL
      #############
      currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data
      ###########################################
    }) 
    
    ###################End of Text Analysis################################################
    

    ##########################################################
    #############Main#########################################
    ##########################################################
    ############ View Train and Test Data ####################
    output$Main.View.Train <- DT::renderDataTable({ 
      if (!is.null(currentmodel$uploaded.data)) {
        tryCatch({
        SPY <- currentmodel$uploaded.data
        SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        ind<-which(unlist(lapply(names(SPY),function(i){is.factor(SPY[[i]]) && 
            !all(is.na(nchar(as.character(SPY[[i]])[c(1,2,3)]))) &&
            mean(nchar(as.character(SPY[[i]])[c(1,2,3)]),na.rm=TRUE)>30})))
        datatable(SPY,selection = "single",
                  options=list(
                    columnDefs = list(list(
                      targets  = ind,
                      render  = JS(
                        "function (data, type ) {", 
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(ind),  
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE))
        }, error=function(err){})
      } else{
        NULL
      }
    })
    
    output$Memory_Size_Train_Data<-renderUI({
      if(!is.null(currentmodel$uploaded.data)){
        HTML(paste('Training data size in RAM:<br/>',
                   format(object.size(currentmodel$uploaded.data), units = 'Mb')))
      } else{
        ""
      }
    })
 
    output$Main.View.Test <- DT::renderDataTable({
      if(!is.null(currentmodel$uploaded.data)){
        tryCatch({
        SPY <- currentmodel$uploaded.data
        SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        ind<-which(unlist(lapply(names(SPY),function(i){is.factor(SPY[[i]]) && 
            !all(is.na(nchar(as.character(SPY[[i]])[c(1,2,3)]))) &&
            mean(nchar(as.character(SPY[[i]])[c(1,2,3)]),na.rm=TRUE)>30})))
        datatable(SPY,selection = "single",
                  options=list(
                    columnDefs = list(list(
                      targets  = ind,
                      render  = JS(
                        "function ( data, type ) {",
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(ind),  
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE)) 
        }, error=function(err){})
      } else{
        NULL
      }
    }) 
    ##########################################################
    ## This function spits out the data dimensions of the input file
    ##########################################################
    
    output$showDataDimensions.Train <- renderText({
      if (!is.null(currentmodel$uploaded.data)) {
        tryCatch({
        SPY <- currentmodel$uploaded.data
        SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        paste("Dimensions", dim(SPY)[1], "X" , dim(SPY)[2])
        }, error=function(err){})
      } else{
        ""
      }
    })
    
    output$showDataDimensions.Test <- renderText({
      if (!is.null(currentmodel$uploaded.data)) {
        tryCatch({
        SPY <- currentmodel$uploaded.data
        SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        paste("Dimensions", dim(SPY)[1], "X" , dim(SPY)[2])
        }, error=function(err){})
      } else{
        ""
      }
    })
    #####################Download Data#########################
    ##Export table
    observeEvent(input$MainSubmit.uploaded.data.export,{
      if(!is.null(currentmodel$uploaded.data)){
        if(input$path.uploaded.data.export!=""){  
          tryCatch({
            write_excel_csv(currentmodel$uploaded.data,path = paste0("C://AlgoTraceFolder.export/",input$path.uploaded.data.export,".csv"))
            Export.uploaded.data.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Export.uploaded.data.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Export.uploaded.data.last_action<<-paste("Insert File name")
        }
      } else{
        Export.uploaded.data.last_action<<-paste("No Data to export")
      }
    })

    
    output$uploaded.data.export.text<-renderText({
      input$MainSubmit
      input$MainSubmit.uploaded.data.export
      Export.uploaded.data.last_action
    })
    
    ###########################################################
    ##############################Configure######################################
    ############################ Variables Summary #####################
    ########################### Select variables to exclude/target variables #########################
    observe({
      if(!is.null(currentmodel$Method.saved))
        return()
      currentmodel$Method.saved<-input$Method
    })
    
    observe({
      if (is.null(currentmodel$uploaded.data) || !is.null(isolate(currentmodel$Vars.Summary))) 
        return()
      SPY <- currentmodel$uploaded.data
      temp <- data.frame(Variable=colnames(SPY), stringsAsFactors=FALSE)
      temp$Exclude <- FALSE
      temp$Target <- FALSE
      temp$Unimputed <- FALSE
      temp$Outliers <- FALSE
      temp$Type <- sapply(1:ncol(SPY), function(x) { class(SPY[[x]])})
      temp$Levels <- sapply(1:ncol(SPY), function(x) { length(unique(SPY[[x]]))})
      temp$Missing <- sapply(1:ncol(SPY), function(x) { 
        if(is.numeric(SPY[[x]])){return(round(sum(is.na(SPY[[x]]))/ nrow(SPY),2))}
        return(round((sum(is.na(SPY[[x]]))+sum(SPY[[x]] %in% c("","NaN","#DIV/0!"),na.rm = TRUE))/ nrow(SPY),2))
      })  #Missing.Percentage
      
      temp[,c("Min","Q1","Median","Mean","Q3","Max")]<-NA 
      temp.list<-lapply(1:ncol(SPY), function(x) {
        if(is.numeric(SPY[[x]])==FALSE){return(c(NA,NA,NA,NA,NA,NA))}
        summ<-summary(SPY[[x]])
        return(round(summ,2))})
      
      temp[,c("Min","Q1","Median","Mean","Q3","Max")]<-do.call("rbind",temp.list)
      isolate({currentmodel$Vars.Summary<-temp})
    })
    
    
    output$Vars.Summary <- renderRHandsontable({
      if (!is.null(currentmodel$Vars.Summary)) {
        temp<-currentmodel$Vars.Summary
        choices<-c("integer","numeric","factor")
          rhandsontable(temp,rowHeaders = NULL,maxRows=nrow(temp)) %>%  
            hot_col(col = "Type", type = "dropdown", source = choices,strict=TRUE)  %>%
            hot_col("Variable", readOnly=TRUE) %>%
            hot_col("Levels", readOnly=TRUE) %>%
            hot_col("Missing", readOnly=TRUE) %>%
            hot_col("Min", readOnly=TRUE) %>%
            hot_col("Q1", readOnly=TRUE) %>%
            hot_col("Median", readOnly=TRUE) %>%
            hot_col("Mean", readOnly=TRUE) %>%
            hot_col("Q3", readOnly=TRUE) %>%
            hot_col("Max", readOnly=TRUE)
      } 
    })
    

    ###Updating the rest of the variables checkboxes
    observeEvent(input$Checkall,{
      if (!is.null(input$Vars.Summary)) {
        temp<-hot_to_r(input$Vars.Summary) 
        if(input$Column.to.check.configure=="Exclude")
          temp$Exclude<-TRUE
        if(input$Column.to.check.configure=="Unimputed")
          temp$Unimputed<-TRUE
        if(input$Column.to.check.configure=="Outliers")
          temp$Outliers<-TRUE
        currentmodel$Vars.Summary<-temp
      }})
    
    observeEvent(input$UnCheckall,{
      if (!is.null(input$Vars.Summary)) {
        temp<-hot_to_r(input$Vars.Summary)
        if(input$Column.to.check.configure=="Exclude")
          temp$Exclude<-FALSE
        if(input$Column.to.check.configure=="Unimputed")
          temp$Unimputed<-FALSE
        if(input$Column.to.check.configure=="Outliers")
          temp$Outliers<-FALSE
        currentmodel$Vars.Summary<-temp
      }})
    

    ###Updating the variables 
     observeEvent(input$MainSubmit.Vars.Summary,{
       if (!is.null(input$Vars.Summary)) {
         temp<-hot_to_r(input$Vars.Summary)
         ###reset the ratio combination tab###
         currentmodel$ratio.comb.table.to.show<-NULL
         currentmodel$ratio.comb.table.to.show.saved<-NULL
         currentmodel$ratio.comb.info<-NULL
         ########################################################
         ##Method
         currentmodel$Method.saved<-input$Method
         ##Target
         selected <- temp[temp$Target,"Variable"]  
         if (length(selected)>0) {
          if (length(selected)==1){
            currentmodel$Target.Vars <- selected 
          } else{
            currentmodel$Target.Vars<-NULL
            showModal(modalDialog(
              title = "",
              size="s",
              easyClose = TRUE,
              paste('Only one target variable is allowed')
            ))
          }
         } else { #sum==0
           currentmodel$Target.Vars<-NULL
         }
         ##Exclude
         selected <- temp[which(temp$Exclude==TRUE),"Variable"]  
         if (length(selected)>0) {
           currentmodel$vars.to.exclude <- selected
         } else{
           currentmodel$vars.to.exclude<-NULL
         }
         ##Not Fix
         selected <- temp[which(temp$Unimputed==TRUE),"Variable"]  
         if (length(selected)>0) {
           currentmodel$not.to.fix <- selected
         } else{
           currentmodel$not.to.fix<-NULL
         }
         ##Not Fix Outliers
         selected <- temp[which(temp$Outliers==TRUE),"Variable"]  
         if (length(selected)>0) {
           currentmodel$not.to.fix.outliers <- selected
         } else{
           currentmodel$not.to.fix.outliers<-NULL
         }
         #################################
         ######Change Type################
         SPY<-currentmodel$Basic.uploaded.data
         Type.requested<-temp$Type
         Type.exist<-sapply(1:ncol(SPY), function(x) { class(SPY[[x]])})
         ind<-which(Type.requested!=Type.exist)
         if(length(ind)>0){###There is a change
           new.SPY<-SPY
           temp.table<-data.frame(before=NULL,after=NULL)
           for(i in ind){
             if(Type.requested[i]=="integer"){
               new.SPY[,i]<-as.integer(paste(SPY[[i]]))
               temp.table<-rbind(temp.table,data.frame(before=mean(is.na(SPY[[i]])),after=mean(is.na(new.SPY[[i]]))))
             }
             if(Type.requested[i]=="numeric"){
               new.SPY[,i]<-as.numeric(paste(SPY[[i]]))
               temp.table<-rbind(temp.table,data.frame(before=mean(is.na(SPY[[i]])),after=mean(is.na(new.SPY[[i]]))))
             }
             if(Type.requested[i]=="factor"){
               new.SPY[,i]<-as.factor(SPY[[i]])
               temp.table<-rbind(temp.table,data.frame(before=mean(is.na(SPY[[i]])),after=mean(is.na(new.SPY[[i]]))))
             }
           }
           diff<-temp.table$after-temp.table$before
           diff.ind<-which(diff>0.2)
           
           
           if(length(diff.ind)>0){
             temp.files.for.class.change<<-list(SPY=SPY,new.SPY=new.SPY,ind=ind,temp=temp)
             diff_char<-paste0(diff[diff.ind]*100,"% more")
             showModal(modalDialog(
               title = "Important message",
               HTML(paste('In The following variables NA values will be added:<br/>',
                          paste(paste(1:length(names(SPY)[ind[diff.ind]]), "-", names(SPY)[ind[diff.ind]],"-",diff_char), collapse="<br/>"),
                          '<br/>Would you like to proceed?')),
               footer = tagList(
                 actionButton("no.continue.class.change", "No"),
                 actionButton("yes.continue.class.change", "Yes")
               )
             ))
           } else{ 
             ##Update Vars.Summary 
             criterion.Names<-c("Type","Levels","Missing","Min","Q1","Median","Mean","Q3","Max")
             temp[ind,criterion.Names]<-update.vars.summary(new.SPY[,ind,with=FALSE])
             currentmodel$Vars.Summary<-temp    

             currentmodel$Basic.uploaded.data<-new.SPY
             ind<-currentmodel$Data.Filter.Index
             if(!is.null(ind)){
               currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data[ind,]
             } else{
               currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data  
             }
             
           }#End of sum(temp.vec)==0
         } else{  ###End of length(ind)>0
           currentmodel$Vars.Summary<-temp
         }
       } else {
         currentmodel$Method.saved<-NULL
         currentmodel$Target.Vars <- NULL
         currentmodel$vars.to.exclude <- NULL
         currentmodel$not.to.fix <- NULL
         currentmodel$not.to.fix.outliers <- NULL
       }
   })#End of observe
   
   
   observeEvent(input$no.continue.class.change,{
     removeModal()
     SPY<-temp.files.for.class.change[["SPY"]]
     ind<-temp.files.for.class.change[["ind"]]
     temp<-temp.files.for.class.change[["temp"]]
     ##Returning to the old Vars.Summary
     temp[ind,"Type"]<-update.Type(SPY[,ind,with=FALSE])
     currentmodel$Vars.Summary<-temp
   })
   
   observeEvent(input$yes.continue.class.change,{
     removeModal()
     new.SPY<-temp.files.for.class.change[["new.SPY"]]
     ind<-temp.files.for.class.change[["ind"]]
     temp<-temp.files.for.class.change[["temp"]]
     ##Update Vars.Summary
     criterion.Names<-c("Type","Levels","Missing","Min","Q1","Median","Mean","Q3","Max")
     temp[ind,criterion.Names]<-update.vars.summary(new.SPY[,ind,with=FALSE])
     currentmodel$Vars.Summary<-temp
     
     currentmodel$Basic.uploaded.data<-new.SPY
     ind<-currentmodel$Data.Filter.Index
     if(!is.null(ind)){
       currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data[ind,]
     } else{
       currentmodel$uploaded.data<-currentmodel$Basic.uploaded.data  
     }
   })

    output$TargetVariable.text <- renderText({ 
      if (!is.null(currentmodel$Target.Vars)){
        ##For the Xgboost
        xgboost.parameters[["base_score"]]<<-mean(currentmodel$uploaded.data[[currentmodel$Target.Vars]],na.rm=TRUE)
        paste("The selected target variable is", currentmodel$Target.Vars)
      } 
    })
    ##Exclude  
    output$ExcludedVariables.text <- renderUI({
      if (!is.null(currentmodel$vars.to.exclude)) {
        temp<-currentmodel$vars.to.exclude
        HTML(paste('The following variables will be excluded:<br/>',
                   paste(paste(1:length(temp), "-", temp), collapse="<br/>")))
      }})

    ##Not fix
    output$not.to.fix.Variables.text<-renderUI({
      if (!is.null(currentmodel$not.to.fix)) {
        temp<-currentmodel$not.to.fix
          HTML(paste('The NA in the following variables wont be replaced:<br/>',
            paste(paste(1:length(temp), "-", temp), collapse="<br/>")))
    }})
    ##Not fix outliers
    output$not.to.fix.outliers.Variables.text<-renderUI({
      if (!is.null(currentmodel$not.to.fix.outliers)) {
        temp<-currentmodel$not.to.fix.outliers
        HTML(paste('The Outliers in the following variables wont be replaced:<br/>',
                   paste(paste(1:length(temp), "-", temp), collapse="<br/>")))
    }})
    
    ###################################################
    ###################################################
    
    
    
    ############################################################################
    ## Excluding Variables which Have (Lot of NA/Levels/new Levels in Test Data)
    ## Making sure that train and test data have the same Variables
    ############################################################################
    
    observe({
      ####Fixed.Target####
      if(!is.null(currentmodel$uploaded.data) && nrow(currentmodel$uploaded.data)>0 &&
         !is.null(currentmodel$Target.Vars)){
        Data<-currentmodel$uploaded.data  
        target<-Data[[currentmodel$Target.Vars]]
        if(currentmodel$Method.saved=="Classification"){
          target<-target[cleaned.places(target)]
          uni<-unique(target)
          Fixed.Target<- is.numeric(target) && (setequal(uni,c(0,1))==TRUE)  
          currentmodel$Fixed.Target<-Fixed.Target
          ######
          if(currentmodel$Fixed.Target){
            vec.train<-target[1:round(currentmodel$division.rate*length(target))]
            vec.test<-target[(round(currentmodel$division.rate*length(target))+1):length(target)]
            if(abs(sum(vec.train==0)/length(vec.train)-sum(vec.test==0)/length(vec.test))>0.1)
              showModal(modalDialog(
                title = "",
                size="s",
                easyClose = TRUE,
                paste('Target Distribution differs between train and test')
              ))
          }
        } else{#Estimation
          currentmodel$Fixed.Target<-is.numeric(target)#TRUE
        }
        ##########
        table<-isolate(currentmodel$Vars.Summary)
        Lot.Of.NA<-table[,"Variable"][which(table[,"Missing"]>0.5)]
        currentmodel$Lot.Of.NA<-Lot.Of.NA[!Lot.Of.NA %in% currentmodel$Target.Vars]
        const.vars<-table[,"Variable"][which(table[,"Levels"]<2)]
        currentmodel$const.vars<-const.vars[!const.vars %in% currentmodel$Target.Vars]
        currentmodel$Exclude<-unique(c(currentmodel$Lot.Of.NA,currentmodel$const.vars))
      } else{
        currentmodel$Fixed.Target<-FALSE
        currentmodel$Lot.Of.NA<-NULL
        currentmodel$const.vars<-NULL
        currentmodel$Exclude<-NULL
      }
    })
    
    observe({
      if(!is.null(currentmodel$uploaded.data) && nrow(currentmodel$uploaded.data)>0 &&
         !is.null(currentmodel$Target.Vars)){
        ###For Computing
        currentmodel$Ready.To.Calculate<- length(names(currentmodel$uploaded.data)[!(names(currentmodel$uploaded.data) %in% c(currentmodel$vars.to.exclude,currentmodel$Exclude,currentmodel$Target.Vars))])>0 &&
          currentmodel$Fixed.Target 
        ###For Explore tab
        currentmodel$Have.Target.and.Vars<- length(names(currentmodel$uploaded.data)[!(names(currentmodel$uploaded.data) %in% c(currentmodel$vars.to.exclude,currentmodel$Target.Vars))])>0 &&
          currentmodel$Fixed.Target 
      } else{
        currentmodel$Ready.To.Calculate<-FALSE 
        currentmodel$Have.Target.and.Vars<-FALSE
      }
    })
    
    output$Have_Target_and_Vars <- reactive({
      return(!is.null(currentmodel$Have.Target.and.Vars) && currentmodel$Have.Target.and.Vars)
    })
    outputOptions(output, 'Have_Target_and_Vars', suspendWhenHidden=FALSE)
    
    ##############Warning messages Outputs#######
    output$not.allow.computing.text<-renderText({
      input$AllowComputingAnalysis
      not.allow.computing.text
    })
    
    output$CheckExcludedTab<-renderText({ 
      if(!is.null(currentmodel$Exclude) && length(currentmodel$Exclude[!(currentmodel$Exclude %in% currentmodel$vars.to.exclude)])>0){
        paste("Some of the Variables were excluded, Check Excluded Variables Tab")
      } else{
        ""
      }
    }) 
    

    output$Lot.Of.NA.Variables.Table<-DT::renderDataTable({
      if(!is.null(currentmodel$Lot.Of.NA) && length(currentmodel$Lot.Of.NA[!(currentmodel$Lot.Of.NA %in% currentmodel$vars.to.exclude)])>0){  
        temp<-currentmodel$Lot.Of.NA[!(currentmodel$Lot.Of.NA %in% currentmodel$vars.to.exclude)]
        data<-data.frame("Variables with a Lot of NA's"=temp,check.names = F)
        datatable(data ,selection = "single",
                  options=list(
                    pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE))
      } else{
        NULL
      }
    },server = FALSE)
    
    output$Const.Variables.Table<-DT::renderDataTable({
      if(!is.null(currentmodel$const.vars) && length(currentmodel$const.vars[!(currentmodel$const.vars %in% currentmodel$vars.to.exclude)])>0){  
        temp<-currentmodel$const.vars[!(currentmodel$const.vars %in% currentmodel$vars.to.exclude)]
        data<-data.frame("Constant Variables"=temp,check.names = F)
        datatable(data,selection = "single",
                  options=list(
                    pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE))
      } else{
        NULL
      }
    },server = FALSE)
    

    ##############################
    ## Condition for Explore
    ##############################
    output$Inter.data.analysis.text <-output$Inter.Var.vs.target.model.text<-
      output$Inter.ratio.comb.text<-renderText({
        if(!is.null(currentmodel$uploaded.data)){
          if (!is.null(currentmodel$Target.Vars)){
            SPY<-currentmodel$uploaded.data
            target<-SPY[[currentmodel$Target.Vars]]
            if(!is.numeric(target)){
              warn<-"Target should be numeric"
            } else{
              if(currentmodel$Method.saved=="Classification"){
                target<-target[cleaned.places(target)]
                uni<-unique(target)
                if(!setequal(uni,c(0,1))==TRUE){
                  warn<-"Target's unique values should be 0,1"
                } else{
                  warn<-""
                }
              } else{#Estimation
                warn<-""
              }
            }
          } else{
            warn<-"Choose Target Variable"
          }
        } else{
          warn<-"No Data"
        }
        warn
      }) 
    
    output$WarningTextForTarget.variables.correlation <-renderText({
      if(!is.null(currentmodel$uploaded.data)){
        warn<-""
      } else{
        warn<-"No Data"
      }
      warn
    }) 
    ########################################################################
    ############################ Variables Correlation #####################
    observeEvent(input$MainSubmit.Variables.Correlation,{
      ##Reset the Export text
      Variables.Correlation.last_action<<-""
      ######################
      for(i in names(variablesCorrelation))
        variablesCorrelation[[i]]<-NULL
      ########
      SPY <- currentmodel$uploaded.data
      if(input$Select.Choose.Data.Variables.Correlation=="Data"){
        Choose_Data_Variables_Correlation.last_action<<-"Data is chosen"
      }
      if(input$Select.Choose.Data.Variables.Correlation=="Train"){
        SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        Choose_Data_Variables_Correlation.last_action<<-"Train is chosen"
      }
      if(input$Select.Choose.Data.Variables.Correlation=="Test"){
        SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        Choose_Data_Variables_Correlation.last_action<<-"Test is chosen"
      }
      variablesCorrelation$Variables.Correlation.table<-tryCatch({
        if (!is.null(SPY) && nrow(SPY)>0){ 
          SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
          SPY<-SPY[,colnames(SPY)[!(colnames(SPY) %in% currentmodel$vars.to.exclude)],with=FALSE]
          SPY<-data_sample(SPY,input$Percent.Variables.Correlation,input$AllowSampling.Variables.Correlation)
          SPY <- SPY[,sapply(1:ncol(SPY),function(x){return(is.numeric(SPY[[x]])==TRUE && length(unique(SPY[[x]]))>1)}),with=FALSE]
          
          for(i in 1:ncol(SPY)){  
            ind<-which(!cleaned.places(SPY[[i]]))
            if(length(ind)>0)
              SPY[ind,i]<-mean(SPY[[i]][-ind])
          }
          
          corTable <- round(cor(SPY,method="spearman"),3)
          if(nrow(corTable)>0){
            Correlation.table<-corTable
          } else{
            Correlation.table<-NULL
          }
        } else{#is.null(SPY) || nrow(SPY)==0
          Correlation.table<-NULL
        }
        Correlation.table
      }, error=function(err){
        return(NULL)   
      })
    })
    
    output$Choose.Data.Variables.Correlation.text <- renderText({
      input$MainSubmit.Variables.Correlation
      Choose_Data_Variables_Correlation.last_action
    })

    output$Variables.Correlation<-DT::renderDataTable({  
      if(!is.null(variablesCorrelation$Variables.Correlation.table)){
        corTable<-variablesCorrelation$Variables.Correlation.table
        datatable(corTable ,
                  options=list(scrollX=TRUE,pageLength = 15)) %>% 
          formatStyle(
            colnames(corTable), 
            color = styleInterval(c(-0.99,-0.5, 0.5,0.99), c('blue','red', 'black', 'red','blue'))
          ) 
      } else{
        NULL
      }
    },server = FALSE) 
    
    
    observeEvent(input$MainSubmit.Export.button.Variables.Correlation,{
      if(!is.null(variablesCorrelation$Variables.Correlation.table)){
        temp<-as.data.frame(variablesCorrelation$Variables.Correlation.table)
        corTable<-cbind(rownames(temp),temp)
        names(corTable)[1]<-""
        rownames(corTable)<-NULL
        if(input$path.Variables.Correlation!=""){
          tryCatch({
            write_excel_csv(corTable,path = paste0("C://AlgoTraceFolder.export/",input$path.Variables.Correlation,".csv"))
            Variables.Correlation.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Variables.Correlation.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Variables.Correlation.last_action<<-paste("Insert File name") 
        }
      } else{
        Variables.Correlation.last_action<<-paste("No Data to export")
      }
    })
    

    output$Variables.Correlation.Export.text <- renderText({
      input$MainSubmit
      input$LoadButton
      input$MainSubmit.Vars.Summary
      input$MainSubmit.Variables.Correlation
      input$MainSubmit.Export.button.Variables.Correlation
      Variables.Correlation.last_action
    })
    
    ##########################################################
    ## Data Analysis
    ##########################################################
    ##Choose Data for Analysis
    observe({
      if(!is.null(dataAnalysis$Select.Choose.Data.Data.Analysis))
        return()
      dataAnalysis$Select.Choose.Data.Data.Analysis<-input$Select.Choose.Data.Data.Analysis
    })
    
    observeEvent(input$MainSubmit.Choose.Data.Data.Analysis,{
      showModal(modalDialog(
        title = "",
        HTML(paste('Outputs in this Tab will be reset <br/>',
                   'Would you like to proceed?')),
        footer = tagList(
          actionButton("No.Choose.Data.Data.Analysis", "No"),
          actionButton("Yes.Choose.Data.Data.Analysis", "Yes")
        )
      ))
    })
    
    
    observeEvent(input$No.Choose.Data.Data.Analysis,{
      removeModal()
    })
    
    observeEvent(input$Yes.Choose.Data.Data.Analysis,{
      removeModal()
      names.not.to.reset<-c("Distribution.Rhands","Data.Analysis.Distribution_chosen.var",
                            "Missing.Values.Rhands","Data.Analysis.Missing.Values_chosen.var",
                            "Outliers.Rhands","Data.Analysis.Outliers_chosen.var",
                            "Scatter.Rhands","Data.Analysis.Scatter_chosen.var",
                            "Heat.map.Rhands","Data.Analysis.Heat.map_chosen.var")
      for(i in names(dataAnalysis)[!names(dataAnalysis) %in% names.not.to.reset])   
        dataAnalysis[[i]]<-NULL
      ########
      if(input$Select.Choose.Data.Data.Analysis=="Data"){
        dataAnalysis$Select.Choose.Data.Data.Analysis<-"Data"
        Choose_Data_Data_Analysis.last_action<<-"Data is chosen"
      }
      if(input$Select.Choose.Data.Data.Analysis=="Train"){
        dataAnalysis$Select.Choose.Data.Data.Analysis<-"Train"
        Choose_Data_Data_Analysis.last_action<<-"Train is chosen"
      }
      if(input$Select.Choose.Data.Data.Analysis=="Test"){
        dataAnalysis$Select.Choose.Data.Data.Analysis<-"Test"
        Choose_Data_Data_Analysis.last_action<<-"Test is chosen"
      }
    })
    
    output$Choose.Data.Data.Analysis.text <- renderText({
      input$Yes.Choose.Data.Data.Analysis
      Choose_Data_Data_Analysis.last_action
    })
    #####
   #########For Distribution and Heatmap####
    isValid_CutTheDataSize <- reactive({
      if(input$CutTheData=="Yes"){
        return(!is.na(input$CutTheDataSize) && input$CutTheDataSize>1)
      } else{
        return(TRUE)
      }
    })
    #############################################
    #########Variables Distribution#################### 
    output$Distribution.Rhands <- renderRHandsontable({
      if (!is.null(currentmodel$uploaded.data)) {
        if(!is.null(dataAnalysis$Distribution.Rhands)){
          temp<-dataAnalysis$Distribution.Rhands 
        } else{
        SPY <- currentmodel$uploaded.data
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)
        temp$Choose <- FALSE
        temp<-temp[,c("Choose","Variable")]
        }

        rhandsontable(temp, rowHeaders = NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
          hot_col("Variable", readOnly=TRUE) %>% 
          hot_table(stretchH="last")
      }  
    })

    observe({
      if(!is.null(input$Distribution.Rhands)){
        temp<-hot_to_r(input$Distribution.Rhands)
        selected <- temp[temp$Choose,"Variable"]

        if (length(selected)==0) {
          dataAnalysis$Data.Analysis.Distribution_chosen.var <-NULL
        } 
        if (length(selected)==1) {
          var_old <<- selected
          dataAnalysis$Data.Analysis.Distribution_chosen.var <-selected
        } 
        if (length(selected)>1) {
          temp[which(temp$Variable==var_old),"Choose"]<-FALSE
          dataAnalysis$Distribution.Rhands<-temp
        }
      } else{
        dataAnalysis$Data.Analysis.Distribution_chosen.var<-NULL
      }
    }) 
    
    
    Data.Analysis.Distribution.variable.to.plot<-reactive({
      if (!is.null(dataAnalysis$Data.Analysis.Distribution_chosen.var) && isValid_CutTheDataSize()) { 
        ##Choose Data for Analysis
        SPY <- currentmodel$uploaded.data
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        ###Sample and shuffle
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        Var.to.show <- dataAnalysis$Data.Analysis.Distribution_chosen.var
        variable.to.plot<-SPY[[Var.to.show]]
        if(input$CutTheData=="Yes"){ 
          CutTheDataSize<-input$CutTheDataSize
          if(is.numeric(SPY[[Var.to.show]])){ 
            if(length(unique(SPY[[Var.to.show]]))<=CutTheDataSize){
              variable.to.plot<-as.factor(SPY[[Var.to.show]])
            } else{
              variable.to.plot<-cut.nv(SPY[[Var.to.show]],CutTheDataSize)
            }
          }
          if(class(SPY[[Var.to.show]])=="factor" && length(unique(SPY[[Var.to.show]]))<=CutTheDataSize)
            variable.to.plot<-SPY[[Var.to.show]]
          if(class(SPY[[Var.to.show]])=="factor" && length(unique(SPY[[Var.to.show]]))>CutTheDataSize){
            tab<-table(SPY[[Var.to.show]]) 
            temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
            ind<- which(!(SPY[[Var.to.show]] %in% temp))
            variable.to.plot<-SPY[[Var.to.show]]
            variable.to.plot<-as.character(variable.to.plot)
            variable.to.plot[ind]<-"Others"
            variable.to.plot<-as.factor(variable.to.plot)
          }  
        }
        return(list(x=variable.to.plot,x.name=Var.to.show,
             y=SPY[[currentmodel$Target.Vars]],y.name=currentmodel$Target.Vars))
      } else{
        return(NULL)
      }
    }) 
    
   
    ################Frequency/Occurance Plot/table##########################
    observeEvent(input$MainSubmit.Distribution,{
      ##Reset the Export text
      Distributionfrequencyplot_export.last_action<<-""
      Distributionfrequencytable_export.last_action<<-""
      Distributionoccuranceplot_export.last_action<<-""
      Distributionoccurancetable_export.last_action<<-""
      #######################################
      if(!is.null(Data.Analysis.Distribution.variable.to.plot())){
        x<-Data.Analysis.Distribution.variable.to.plot()[["x"]]
        x.name<-Data.Analysis.Distribution.variable.to.plot()[["x.name"]]
        y<-Data.Analysis.Distribution.variable.to.plot()[["y"]]
        y.name<-Data.Analysis.Distribution.variable.to.plot()[["y.name"]]
        ##Frequency
        dataAnalysis$Data.Analysis.Distribution.Freq.Outputs<-tryCatch({  
          temp<-as.data.frame(table(x))
          names(temp)<-c("Val","Freq")
          temp$Percent<-100*temp[,"Freq"]/sum(temp[,"Freq"])
          temp$Percent<-paste0(sprintf("%.0f", temp$Percent), "%")
          temp$Count<-temp[,"Freq"]
          if(input$UseLog=="Yes"){
            temp$Freq<-round(log(temp$Freq),round.digits)
            names(temp)[names(temp) == "Freq"] <- "Log.Freq"
            y.fun<-"Log.Freq"
            title<-paste("Log Frequency of",x.name)
            y.title<-"Log Frequency"
          } else{
            y.fun<-"Freq"
            title<-paste("Frequency of",x.name)
            y.title<-"Frequency"
          }
          
          p<-ggplot(temp,aes_string(x="Val",y=y.fun))+ geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
            labs(x=paste(paste0(x.name,"'s"),"Levels"),y=y.title)+ggtitle(title)+
            theme(
              text = element_text(size=15),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.background=element_rect(fill='white'),
              plot.background = element_rect(fill="white")
            )
          
          if(input$AddTextToFreq=="Count")
            p<-p+geom_text(aes(label = Count), position = position_stack(vjust = 0.5),size = 4)
          
          if(input$AddTextToFreq=="Percent")
            p<-p+geom_text(aes(label = Percent), position = position_stack(vjust = 0.5),size = 4)
          
          Distribution.frequency.last.action<<-""
          list(plot=p,table=temp)
        },error=function(err) {
          Distribution.frequency.last.action<<-paste("Error Occured, Frequency Plot can't be shown")
          return(NULL)
        })
        ##End of tryCatch Frequency
        ##Occurance  
        dataAnalysis$Data.Analysis.Distribution.Occ.Outputs<-tryCatch({
          if(currentmodel$Method.saved=="Classification"){
            table<-data.frame(x,y)
            names(table)<-c("Variable","Target")
            table<-xtabs(~Target+Variable,data=table) 
            if (dim(table)[1] == 1) {#only one row
              if(rownames(table)=="0")
                table <- rbind(table, rep(0,ncol(table)))
              if(rownames(table)=="1")
                table <- rbind(rep(0,ncol(table)),table)
              rownames(table)<-c("0","1")
            }
            temp<-apply(table,2,function(x){round(x["1"]/sum(x),round.digits)})
            temp<-as.data.frame(temp)
            temp$Val<-factor(rownames(temp),levels=rownames(temp))    
            temp<-temp[,c("Val","temp")]
            names(temp)<-c("Val","Mean")
            plot.features<-ggplot(temp,aes(x=Val,y=Mean))+ geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
              labs(x=paste(paste0(x.name,"'s"),"Levels"),y="Mean")+ggtitle(paste("Occurence of",y.name,"In",x.name))+
              geom_text(aes(label = round(Mean,3)), position = position_stack(vjust = 0.5),size = 4)
          }
          if(currentmodel$Method.saved=="Estimation"){
            temp<-data.frame(x=x,y=y)
            criterion<-input$Est.explore.data.criterion.distribution
            if(criterion=="Box.plot"){
              plot.features<-ggplot(temp,aes(x=x,y=y,fill=x))+geom_boxplot()+
                labs(x=x.name,y=y.name)+guides(fill=guide_legend(title=""))
            } else{
              temp<-aggregate(y~x,data=temp,FUN=function(x){criterions.funcion(x,criterion)})
              names(temp)<-c("Val",criterion)
              plot.features<-ggplot(temp,aes_string(x="Val",y=criterion))+geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
                labs(x=paste(paste0(x.name,"'s"),"Levels"),y=criterion)+ggtitle(paste(criterion,"of",y.name,"In each level of",x.name))+
                geom_text(aes(label = round(temp[,criterion],3)), position = position_stack(vjust = 0.5),size = 4)
            }}
          plot.features<-plot.features+
            theme(
              text = element_text(size=15),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.background=element_rect(fill='white'),
              plot.background = element_rect(fill="white")
            ) 
          Distribution.occurance.last.action<<-""
          list(plot=plot.features,table=temp)
        },error=function(err) {
          Distribution.occurance.last.action<<-paste("Error Occured, Occurance Plot can't be shown")
          return(NULL)
        })
      } else{
        dataAnalysis$Data.Analysis.Distribution.Freq.Outputs<-NULL
        dataAnalysis$Data.Analysis.Distribution.Occ.Outputs<-NULL
      }
    })
      
    ##Warning
    output$Distribution.frequency.plot.warning.text<-renderText({
      input$MainSubmit.Distribution
      Distribution.frequency.last.action
    })
    ##Plot
    output$Distribution.frequency.plot<-output$Distribution.frequency.plot.zoom<-renderPlot({
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs))
        dataAnalysis$Data.Analysis.Distribution.Freq.Outputs[["plot"]]
    }, bg="transparent")
    

    ##Export Plot
    observeEvent(input$MainSubmit.Distributionfrequencyplot_export,{
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs)){
        if(input$path.Distributionfrequencyplot_export!=""){
          tryCatch({
            lib<-getwd()
            temp.lib<-"C:/AlgoTraceFolder.export" 
            setwd(temp.lib)
            ext<-input$Distributionfrequencyplot_export.extension
            ggsave(file=paste0(input$path.Distributionfrequencyplot_export,paste0(".",ext)), dataAnalysis$Data.Analysis.Distribution.Freq.Outputs[["plot"]], device=ext)
            Distributionfrequencyplot_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            setwd(lib) 
          }, error=function(err){
            Distributionfrequencyplot_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Distributionfrequencyplot_export.last_action<<-paste("Insert File name")
        }
      } else{
        Distributionfrequencyplot_export.last_action<<-paste("No Plot to export")
      }
    })
    
    output$Distributionfrequencyplot_export.text<-renderText({
      input$MainSubmit.Distribution
      input$MainSubmit.Distributionfrequencyplot_export
      Distributionfrequencyplot_export.last_action
    })
    

    ##Table
    output$Distribution.frequency.table.zoom<-DT::renderDataTable({ 
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs)){
        Table<-dataAnalysis$Data.Analysis.Distribution.Freq.Outputs[["table"]]
        datatable(Table ,selection = "single",rownames=FALSE,
                  options=list(
                    pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    ##Export table
    observeEvent(input$MainSubmit.Vartoshowfrequencytable_export,{
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs)){
        if(input$path.Distributionfrequencytable_export!=""){
          tryCatch({
            write_excel_csv(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs[["table"]],path = paste0("C://AlgoTraceFolder.export/",input$path.Distributionfrequencytable_export,".csv"))
            Distributionfrequencytable_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Distributionfrequencytable_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Distributionfrequencytable_export.last_action<<-paste("Insert File name")
        }
      } else{
        Distributionfrequencytable_export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Distributionfrequencytable_export.text<-renderText({
      input$MainSubmit.Distribution
      input$MainSubmit.Vartoshowfrequencytable_export
      Distributionfrequencytable_export.last_action
    })
    ###reactive for showing it's actionLink
    output$Distribution_frequency_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Distribution.Freq.Outputs))
    })
    outputOptions(output, 'Distribution_frequency_actionlink', suspendWhenHidden=FALSE)
   

    ################Occurance in Target Plot##########################
     output$Distribution.occurance.plot.warning.text<-renderText({
       input$MainSubmit.Distribution
        Distribution.occurance.last.action
    })
    output$Distribution.occurance.plot<-output$Distribution.occurance.plot.zoom<-renderPlot({
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs))
        dataAnalysis$Data.Analysis.Distribution.Occ.Outputs[["plot"]]
    }, bg="transparent")
    
    ##Export Plot
    observeEvent(input$MainSubmit.Distributionoccuranceplot_export,{
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs)){
        if(input$path.Distributionoccuranceplot_export!=""){
          tryCatch({
            lib<-getwd()
            temp.lib<-"C:/AlgoTraceFolder.export" 
            setwd(temp.lib)
            ext<-input$Distributionoccuranceplot_export.extension
            ggsave(file=paste0(input$path.Distributionoccuranceplot_export,paste0(".",ext)), dataAnalysis$Data.Analysis.Distribution.Occ.Outputs[["plot"]], device=ext)
            Distributionoccuranceplot_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            setwd(lib) 
          }, error=function(err){
            Distributionoccuranceplot_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Distributionoccuranceplot_export.last_action<<-paste("Insert File name")
        }
      } else{
        Distributionoccuranceplot_export.last_action<<-paste("No Plot to export")
      }
    })
    
    output$Distributionoccuranceplot_export.text<-renderText({
      input$MainSubmit.Distribution
      input$MainSubmit.Distributionoccuranceplot_export
      Distributionoccuranceplot_export.last_action
    })
    
    output$Distribution.occurance.table.zoom<-DT::renderDataTable({
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs)){
        Table<-dataAnalysis$Data.Analysis.Distribution.Occ.Outputs[["table"]]
        datatable(Table ,selection = "single",rownames=FALSE,
                  options=list(
                    pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    ##Export table
    observeEvent(input$MainSubmit.Distributionoccurancetable_export,{
      if(!is.null(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs)){
        if(input$path.Distributionoccurancetable_export!=""){
          tryCatch({
            write_excel_csv(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs[["table"]],path = paste0("C://AlgoTraceFolder.export/",input$path.Distributionoccurancetable_export,".csv"))
            Distributionoccurancetable_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Distributionoccurancetable_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Distributionoccurancetable_export.last_action<<-paste("Insert File name")
        }
      } else{
        Distributionoccurancetable_export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Distributionoccurancetable_export.text<-renderText({
      input$MainSubmit.Distribution
      input$MainSubmit.Distributionoccurancetable_export
      Distributionoccurancetable_export.last_action
    })
    ###reactive for showing it's actionLink
    output$Distribution_occurance_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Distribution.Occ.Outputs))
    })
    outputOptions(output, 'Distribution_occurance_actionlink', suspendWhenHidden=FALSE)
    
    ####Grid Plot
     observeEvent(input$MainSubmit.Distribution_gridplot,{ 
      if (!is.null(currentmodel$uploaded.data) && !is.null(currentmodel$Target.Vars) && 
          isValid_CutTheDataSize()) { 
        if(input$path.Distribution_gridplot!=""){
        SPY <- currentmodel$uploaded.data
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        #SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),drop=FALSE]
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        vars<-colnames(SPY)[!(colnames(SPY) %in% currentmodel$vars.to.exclude)]
        plist1<-list()
        plist2<-list()
        
        for(i in vars){
          variable.to.plot<-SPY[[i]]
          if(is.numeric(variable.to.plot))
            variable.to.plot<-round(variable.to.plot,3)
          
          if(input$CutTheData=="Yes"){
            CutTheDataSize<-input$CutTheDataSize
            if(is.numeric(SPY[[i]])){ 
              if(length(unique(SPY[[i]]))<=CutTheDataSize){
                variable.to.plot<-as.factor(round(SPY[[i]],3))  
              } else{
                variable.to.plot<-cut.nv(SPY[[i]],CutTheDataSize)
              }
            }
            if(class(SPY[[i]])=="factor" && length(unique(SPY[[i]]))<=CutTheDataSize)
              variable.to.plot<-SPY[[i]]
            if(class(SPY[[i]])=="factor" && length(unique(SPY[[i]]))>CutTheDataSize){
              tab<-table(SPY[[i]]) 
              temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
              ind<- which(!(SPY[[i]] %in% temp))
              variable.to.plot<-SPY[[i]]
              variable.to.plot<-as.character(variable.to.plot)
              variable.to.plot[ind]<-"Others"
              variable.to.plot<-as.factor(variable.to.plot)
            }  
          }##End of if(input$CutTheData=="Yes")
          x<-variable.to.plot
          x.name<-i
          y<-SPY[[currentmodel$Target.Vars]]
          y.name<-currentmodel$Target.Vars
          ###############################
          #Drawing The Plot
          ###Freq
          tryCatch({
            temp<-as.data.frame(table(x))
            names(temp)<-c("Val","Freq")
            temp$Percent<-100*temp[,"Freq"]/sum(temp[,"Freq"])
            temp$Percent<-paste0(sprintf("%.0f", temp$Percent), "%")
            temp$Count<-temp[,"Freq"]
            if(input$UseLog=="Yes"){
              temp$Freq<-round(log(temp$Freq),round.digits)
              names(temp)[names(temp) == "Freq"] <- "Log.Freq"
              y.fun<-"Log.Freq"
              title<-paste("Log Frequency of",x.name)
              y.title<-"Log Frequency"
            } else{
              y.fun<-"Freq"
              title<-paste("Frequency of",x.name)
              y.title<-"Frequency"
            }
            
            p<-ggplot(temp,aes_string(x="Val",y=y.fun))+ geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
              labs(x=paste(paste0(x.name,"'s"),"Levels"),y=y.title)+ggtitle(title)+
              theme(
                text = element_text(size=15),
                plot.title = element_text(face = "bold"),
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = "lightgray"),
                axis.text.x = element_text(angle = 330),
                legend.background=element_rect(fill='white'),
                plot.background = element_rect(fill="white")
              )
            
            if(input$AddTextToFreq=="Count")
              p<-p+geom_text(aes(label = Count), position = position_stack(vjust = 0.5),size = 4)
            
            if(input$AddTextToFreq=="Percent")
              p<-p+geom_text(aes(label = Percent), position = position_stack(vjust = 0.5),size = 4)
            
            
            plist1[[x.name]]<-p
          },error=function(err) {
          })
          ## Target in each level
          tryCatch({
            if(currentmodel$Method.saved=="Classification"){
              table<-data.frame(x,y)
              names(table)<-c("Variable","Target")
              table<-xtabs(~Target+Variable,data=table) 
              if (dim(table)[1] == 1) {#only one row
                if(rownames(table)=="0")
                  table <- rbind(table, rep(0,ncol(table)))
                if(rownames(table)=="1")
                  table <- rbind(rep(0,ncol(table)),table)
                rownames(table)<-c("0","1")
              }
              temp<-apply(table,2,function(x){round(x["1"]/sum(x),round.digits)})
              temp<-as.data.frame(temp)
              temp$Val<-factor(rownames(temp),levels=rownames(temp)) 
              temp<-temp[,c("Val","temp")]
              names(temp)<-c("Val","Mean")
              p<-ggplot(temp,aes(x=Val,y=Mean))+ geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
                labs(x=paste(paste0(x.name,"'s"),"Levels"),y="Mean")+ggtitle(paste("Occurence of",y.name,"In",x.name))
            }
            if(currentmodel$Method.saved=="Estimation"){
              temp<-data.frame(x=x,y=y)
              criterion<-input$Est.explore.data.criterion.distribution
              if(criterion=="Box.plot"){
                p<-ggplot(temp,aes(x=x,y=y,fill=x))+geom_boxplot()+
                  labs(x=x.name,y=y.name)+guides(fill=guide_legend(title=""))
              } else{
                temp<-aggregate(y~x,data=temp,FUN=function(x){criterions.funcion(x,criterion)})
                names(temp)<-c("Val",criterion)
                p<-ggplot(temp,aes_string(x="Val",y=criterion))+geom_histogram(color="darkblue", fill="lightblue",stat="identity")+#stat_summary(fun.y="mean", geom="bar")+ 
                  labs(x=paste(paste0(x.name,"'s"),"Levels"),y=criterion)+ggtitle(paste(criterion,"of",y.name,"In each level of",x.name))
              }}
            p<-p+theme(
              text = element_text(size=15),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.background=element_rect(fill='white'),
              plot.background = element_rect(fill="white")
            ) 
            plist2[[x.name]]<-p
          },error=function(err) {
          })
          
          
        }#End of for loop
        n1 <- length(plist1)
        n2 <- length(plist2)
        lib<-getwd()
        temp.lib<-"C:/AlgoTraceFolder.export" 
        setwd(temp.lib)
        ext<-input$Distribution_gridplot.extension
        for(i in 1:n1){
          tryCatch({
          ggsave(file=paste0(input$path.Distribution_gridplot,"_Freq",i,paste0(".",ext)), plist1[[i]], device=ext)
          },error=function(err) {
          })  
        }
        for(i in 1:n2){
          tryCatch({  
          ggsave(file=paste0(input$path.Distribution_gridplot,"_Occurance",i,paste0(".",ext)), plist2[[i]], device=ext)
          },error=function(err) {
          }) 
        } 
        Freq.gridplot.last_action<<-paste("Plots File was Exported to C://AlgoTraceFolder.export/")
        setwd(lib) 
        } else{ 
        Freq.gridplot.last_action<<-paste("Insert File name")
        }
      } 
    })
    
    output$Distribution_gridplot.text <- renderText({
      input$MainSubmit.Distribution_gridplot
      Freq.gridplot.last_action
    })
    #####################
    #############################################
    #########Missing Values######################
    output$Missing.Values.Rhands<- renderRHandsontable({
      if (!is.null(currentmodel$uploaded.data)) { 
        if(!is.null(dataAnalysis$Missing.Values.Rhands)){
          temp<-dataAnalysis$Missing.Values.Rhands 
        } else{
          SPY <- currentmodel$uploaded.data
          vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
          temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)
          temp$Choose <- FALSE
          temp<-temp[,c("Choose","Variable")]
        }
        
        rhandsontable(temp, rowHeaders = NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
          hot_col("Variable", readOnly=TRUE) %>% 
          hot_table(stretchH="last")
      } 
    })
    
    observe({
      if(!is.null(input$Missing.Values.Rhands)){
        temp<-hot_to_r(input$Missing.Values.Rhands)
        selected <- temp[temp$Choose,"Variable"]
        
        if (length(selected)==0){
          dataAnalysis$Data.Analysis.Missing.Values_chosen.var<-NULL 
        }
        if (length(selected)==1) {
          Missing_Values.var.name_old <<- selected
          dataAnalysis$Data.Analysis.Missing.Values_chosen.var<-selected
        }
        if (length(selected)>1) {
          temp[which(temp$Variable==Missing_Values.var.name_old),"Choose"]<-FALSE
          dataAnalysis$Missing.Values.Rhands<-temp
        }
      } else{
        dataAnalysis$Data.Analysis.Missing.Values_chosen.var<-NULL
      }  
    })
    
    observeEvent(input$MainSubmit.Missing.Values,{
      ##When pressing Submit Export text is reset
      Missing_Values_table_export.last_action<<-""
      if (!is.null(dataAnalysis$Data.Analysis.Missing.Values_chosen.var)) { 
        ##Choose Data
        SPY <- currentmodel$uploaded.data
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        ##
        Var.to.show <- dataAnalysis$Data.Analysis.Missing.Values_chosen.var
        if(is.numeric(SPY[[Var.to.show]])){
          temp.val<-sum(is.na(SPY[[Var.to.show]])==TRUE) 
        } else{
          temp.val<-sum(is.na(SPY[[Var.to.show]]))+sum(SPY[[Var.to.show]] %in% c("","NaN","#DIV/0!"),na.rm = TRUE)
        }
        data<-data.frame(x=c("Missing Values","Non Missing Values"),percent=c(temp.val/nrow(SPY),1-temp.val/nrow(SPY)),
                         count=c(temp.val,nrow(SPY)-temp.val))
        ##Plot
        if(input$AddTextToFreq=="No")
          p<-ggplot(data,aes(x=x,y=percent))+geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="",y=Var.to.show)
        if(input$AddTextToFreq=="Count")
          p<-ggplot(data,aes(x=x,y=percent))+geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="",y=Var.to.show)+geom_text(aes(label = round(count,3)), position = position_stack(vjust = 0.5),size = 4)
        if(input$AddTextToFreq=="Percent")
          p<-ggplot(data,aes(x=x,y=percent))+geom_histogram(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="",y=Var.to.show)+geom_text(aes(label = round(percent,3)), position = position_stack(vjust = 0.5),size = 4)
        
        p<-p+theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.background=element_rect(fill='white'),
          plot.background = element_rect(fill="white")
        ) 
        dataAnalysis$Data.Analysis.Missing.Values.Outputs<-list(data=data,plot=p,var.name=Var.to.show)
      } else{
        dataAnalysis$Data.Analysis.Missing.Values.Outputs<-NULL
      } 
    })
    
    
    output$Missing.Values.plot<-renderPlot({
      if(!is.null(dataAnalysis$Data.Analysis.Missing.Values.Outputs))
        dataAnalysis$Data.Analysis.Missing.Values.Outputs[["plot"]]
    }, bg="transparent")
    
    ###reactive for showing it's actionLink
    output$Missing_Values_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Missing.Values.Outputs))
    })
    outputOptions(output, 'Missing_Values_actionlink', suspendWhenHidden=FALSE)
    
    observeEvent(input$MainSubmit.Missing.Values.table.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Missing.Values.Outputs)){
        if(input$path.Missing.Values.table.export!=""){
          tryCatch({
            data<-dataAnalysis$Data.Analysis.Missing.Values.Outputs[["data"]]
            var.name<-dataAnalysis$Data.Analysis.Missing.Values.Outputs[["var.name"]]
            names(data)[names(data)=="x"]<-var.name
            write_excel_csv(data,path = paste0("C://AlgoTraceFolder.export/",input$path.Missing.Values.table.export,".csv"))
            Missing_Values_table_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err) {
            Missing_Values_table_export.last_action <<- "Error occured - Data was not Exported"
          })
        } else{
          Missing_Values_table_export.last_action<<-paste("Insert File name")
        }
      } else{
        Missing_Values_table_export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Missing.Values.table.export.text <- renderText({
      input$MainSubmit.Missing.Values
      input$MainSubmit.Missing.Values.table.export
      Missing_Values_table_export.last_action
    })
    ############################################
    #########Outliers Plot######################
    output$Outliers.Rhands<- renderRHandsontable({
      if (!is.null(currentmodel$uploaded.data)) { 
        if(!is.null(dataAnalysis$Outliers.Rhands)){
          temp<-dataAnalysis$Outliers.Rhands 
        } else{
        SPY <- currentmodel$uploaded.data
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)
        temp$Choose <- FALSE
        temp<-temp[,c("Choose","Variable")]
        }
        
        rhandsontable(temp, rowHeaders = NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
          hot_col("Variable", readOnly=TRUE) %>% 
          hot_table(stretchH="last") 
      } 
    })
    
    observe({
      if(!is.null(input$Outliers.Rhands)){
        temp<-hot_to_r(input$Outliers.Rhands)
        selected <- temp[temp$Choose,"Variable"]
        
        if (length(selected)==0){
          dataAnalysis$Data.Analysis.Outliers_chosen.var<-NULL 
        }
        if (length(selected)==1) {
          var_with_outlier_old <<- selected
          dataAnalysis$Data.Analysis.Outliers_chosen.var<-selected
        }
        if (length(selected)>1) {
          temp[which(temp$Variable==var_with_outlier_old),"Choose"]<-FALSE
          dataAnalysis$Outliers.Rhands<-temp
        }
      } else{
        dataAnalysis$Data.Analysis.Outliers_chosen.var<-NULL
      }  
    }) 
    
    observe({
      if(!is.null(dataAnalysis$Data.Analysis.Outliers_chosen.var)){
        nrow.spy <- nrow(currentmodel$uploaded.data)
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          nrow.spy<-round(currentmodel$division.rate*nrow.spy)
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-nrow.spy-round(currentmodel$division.rate*nrow.spy)
        if(input$AllowSampling.Data.Analysis=="Yes")
          nrow.spy<-round(nrow.spy*input$Percent_Data_Analysis)
        if(nrow.spy>5000){
          text<-"Computation will take time"
        } else{
          text<-""
        }
      } else{
        text<-""
      }
      output$Slow.Computation.Outliers.text<-renderText({
        text
      })
    })
    
    observeEvent(input$MainSubmit.Outliers,{
      ##When pressing Submit Export text is reset
      Outliers_table_export.last_action<<-""
      if (!is.null(dataAnalysis$Data.Analysis.Outliers_chosen.var)) { 
        ##Choose Data
        SPY <- currentmodel$uploaded.data
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        ##
        Var.to.show <- dataAnalysis$Data.Analysis.Outliers_chosen.var
        temp<-data.frame(Index=1:nrow(SPY))
        temp$Value<-SPY[[Var.to.show]]
        ##Outliers
        kk<<-list(SPY[[Var.to.show]],OutliersSettings$Outliers.settings.saved)
        Outliers.ind<-find.outliers(SPY[[Var.to.show]],OutliersSettings$Outliers.settings.saved)
        temp.vec<-rep("No",nrow(SPY))
        temp.vec[Outliers.ind]<-"Yes"
        temp$Outlier<-temp.vec
        dataAnalysis$Data.Analysis.Outliers.Outputs<-list(data=temp,var.name=Var.to.show)
      } else{
        dataAnalysis$Data.Analysis.Outliers.Outputs<-NULL
      } 
    }) 
    
    
    ###reactive for showing it's actionLink
    output$Outliers_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Outliers.Outputs))
    })
    outputOptions(output, 'Outliers_actionlink', suspendWhenHidden=FALSE)
    
    
   observe({ 
     if(!is.null(dataAnalysis$Data.Analysis.Outliers.Outputs)){
       data<-dataAnalysis$Data.Analysis.Outliers.Outputs[["data"]]
       var.name<-dataAnalysis$Data.Analysis.Outliers.Outputs[["var.name"]]
       Mean<-mean(data$Outlier=="Yes",na.rm=TRUE)
       reactive({
       data %>% ggvis(x= ~Index, y= ~Value, fill= ~Outlier) %>% 
         scale_nominal("fill",domain = c("No","Yes"), range = c('blue','orange')) %>%
         layer_points(size := 100) %>% add_tooltip(all_values, "hover") %>%  
         set_options(height = 600, width = 800) %>% add_axis("y", title = var.name) %>%
         add_axis("x", title = paste("Index (% of Outliers is",round(100*Mean,3),")")) 
         }) %>%  bind_shiny("Outliers_plot") 
     }     
   }) 
    
    
    output$Outliers_table.zoom<-DT::renderDataTable({
      if(!is.null(dataAnalysis$Data.Analysis.Outliers.Outputs)){
        data<-dataAnalysis$Data.Analysis.Outliers.Outputs[["data"]]
          var.name<-dataAnalysis$Data.Analysis.Outliers.Outputs[["var.name"]]
          names(data)[names(data)=="Value"]<-var.name
          datatable(data ,selection = "single",rownames=FALSE,
                    options=list(
                      pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }  
    },server=TRUE) 

       
    
    observeEvent(input$MainSubmit.Outliers.table.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Outliers.Outputs)){
        if(input$path.Outliers.table.export!=""){
          tryCatch({
            data<-dataAnalysis$Data.Analysis.Outliers.Outputs[["data"]]
            var.name<-dataAnalysis$Data.Analysis.Outliers.Outputs[["var.name"]]
            names(data)[names(data)=="Value"]<-var.name
            write_excel_csv(data,path = paste0("C://AlgoTraceFolder.export/",input$path.Outliers.table.export,".csv"))
            Outliers_table_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err) {
            Outliers_table_export.last_action <<- "Error occured - Data was not Exported"
          })
        } else{
          Outliers_table_export.last_action<<-paste("Insert File name")
        }
      } else{
        Outliers_table_export.last_action<<-paste("No Data to export")
      }
    })
 
    
      output$Outliers.table.export.text <- renderText({
        input$MainSubmit.Outliers
        input$MainSubmit.Outliers.table.export
        Outliers_table_export.last_action
      })
 
    ##########Outliers Settings################
      observeEvent(input$MainSubmit.Outliers.settings.save,{
        dataAnalysis$Data.Analysis.Outliers.Outputs<-NULL
        OutliersSettings$Outliers.settings.saved<-list()
        OutliersSettings$Outliers.settings.saved[["Criterion 1"]]<-input$Outliers.criterion1
        OutliersSettings$Outliers.settings.saved[["Criterion 2"]]<-input$Outliers.criterion2
        OutliersSettings$Outliers.settings.saved[["Criterion 3"]]<-input$Outliers.criterion3
      })
      
      observeEvent(input$MainSubmit.Outliers.settings.reset,{
        dataAnalysis$Data.Analysis.Outliers.Outputs<-NULL
        OutliersSettings$Outliers.settings.saved<-NULL    
        updateSliderInput(session,"Outliers.criterion1",value=c(0.01,0.99))
        updateSliderInput(session,"Outliers.criterion2",value=3)
        updateSliderInput(session,"Outliers.criterion3",value=3)
      })
      
      
      
      
    #############################################
    #########Scatter Plot######################
    output$Scatter.Rhands<- renderRHandsontable({
        if (!is.null(currentmodel$uploaded.data)) { 
          if(!is.null(dataAnalysis$Scatter.Rhands)){
            temp<-dataAnalysis$Scatter.Rhands 
          } else{
          SPY <- currentmodel$uploaded.data
          vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
          temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)
          temp$X <- FALSE
          temp$Y <- FALSE
          temp$Fill <- FALSE
          temp<-temp[,c("X","Y","Fill","Variable")]
          }
          
          rhandsontable(temp, rowHeaders = NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
            hot_col("Variable", readOnly=TRUE) %>% 
            hot_table(stretchH="last") 
        } 
      })

      observe({#input$Main.Vars.to.show.scatter,
      if (!is.null(input$Scatter.Rhands)) { 
        temp<-hot_to_r(input$Scatter.Rhands)
        selected1 <- temp[temp$X,"Variable"]
        selected2 <- temp[temp$Y,"Variable"]
        selected3 <- temp[temp$Fill,"Variable"]
        
        ##Var 1
        if (length(selected1)==0){
          Var.to.show1<-NULL
        }
        if (length(selected1)==1) {
          Var.to.show1 <- selected1#vars[selected1]
          var_old1 <<- selected1
        }
        if (length(selected1)>1) {
          temp[which(temp$Variable==var_old1),"X"]<-FALSE
          dataAnalysis$Scatter.Rhands<-temp
          }
        ##Var 2
        if (length(selected2)==0){
          Var.to.show2<-NULL
        }
        if (length(selected2)==1) {
          Var.to.show2 <- selected2#vars[selected2]
          var_old2 <<- selected2
        }
        if (length(selected2)>1) {
          temp[which(temp$Variable==var_old2),"Y"]<-FALSE
          dataAnalysis$Scatter.Rhands<-temp
        }
        ##Var 3
        if (length(selected3)==0){
          Var.to.show3<-NULL
        }
        if (length(selected3)==1) {
          Var.to.show3 <- selected3
          var_old3 <<- selected3
        }
        if (length(selected3)>1) {
          temp[which(temp$Variable==var_old3),"Fill"]<-FALSE
          dataAnalysis$Scatter.Rhands<-temp
        }
        ##X & Var2
        if(length(selected1)==1 && length(selected2)==1 && length(selected3)==1){
          dataAnalysis$Data.Analysis.Scatter_chosen.var<-c(Var.to.show1,Var.to.show2,Var.to.show3)
        } else{
          dataAnalysis$Data.Analysis.Scatter_chosen.var<-NULL
        }
      } else{
        dataAnalysis$Data.Analysis.Scatter_chosen.var<-NULL
      } 
    })  
      
      
      observe({
        if(!is.null(dataAnalysis$Data.Analysis.Scatter_chosen.var)){
          nrow.spy <- nrow(currentmodel$uploaded.data)
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
            nrow.spy<-round(currentmodel$division.rate*nrow.spy)
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
            nrow.spy<-nrow.spy-round(currentmodel$division.rate*nrow.spy)
          if(input$AllowSampling.Data.Analysis=="Yes")
            nrow.spy<-round(nrow.spy*input$Percent_Data_Analysis)
          if(nrow.spy>5000){
            text<-"Computation will take time"
          } else{
            text<-""
          }
        } else{
          text<-""
        }
        output$Slow.Computation.Scatter.text<-renderText({
          text
        })
      })
      
      
      observeEvent(input$MainSubmit.scatter,{
        ##Reset the Export text
        scatter_table_export.last_action<<-""
        if(!is.null(dataAnalysis$Data.Analysis.Scatter_chosen.var)){
        ##Choose Data
          SPY <- currentmodel$uploaded.data
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
            SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
            SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        ##
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        temp<-data.frame(Index=1:nrow(SPY))
        Var.to.show1<-dataAnalysis$Data.Analysis.Scatter_chosen.var[1]
        Var.to.show2<-dataAnalysis$Data.Analysis.Scatter_chosen.var[2]
        Var.to.show3<-dataAnalysis$Data.Analysis.Scatter_chosen.var[3]
        temp$X<-SPY[[Var.to.show1]]
        temp$Y<-SPY[[Var.to.show2]]
        temp$Fill<-SPY[[Var.to.show3]]
        dataAnalysis$Data.Analysis.Scatter.Outputs<-list(data=temp,var.name=c(Var.to.show1,Var.to.show2,Var.to.show3))
        } else{
        dataAnalysis$Data.Analysis.Scatter.Outputs<-NULL  
        }
      })

      output$Scatter_actionlink<-reactive({
        return(!is.null(dataAnalysis$Data.Analysis.Scatter.Outputs))
      })
      outputOptions(output, 'Scatter_actionlink', suspendWhenHidden=FALSE)
 
      
      observe({
        if(!is.null(dataAnalysis$Data.Analysis.Scatter.Outputs)){
          data<-dataAnalysis$Data.Analysis.Scatter.Outputs[["data"]]
          var.names<-dataAnalysis$Data.Analysis.Scatter.Outputs[["var.name"]]
          data %>% ggvis(x= ~X, y= ~Y, fill= ~Fill) %>% layer_points(size := 100) %>% 
            add_tooltip(all_values, "hover") %>% set_options(height = 600, width = 800) %>%
            add_axis("x", title = var.names[1]) %>%
            add_axis("y", title = var.names[2]) %>%
            add_legend("fill", title = var.names[3]) %>%
            scale_numeric("fill", range = c("lightblue", "darkblue")) %>%
            bind_shiny("scatter_plot") 
        }
      })
      
    
      output$scatter_table.zoom<-DT::renderDataTable({
        if(!is.null(dataAnalysis$Data.Analysis.Scatter.Outputs)){
          data<-dataAnalysis$Data.Analysis.Scatter.Outputs[["data"]]
            var.names<-dataAnalysis$Data.Analysis.Scatter.Outputs[["var.name"]]
            datatable(data ,selection = "single",rownames=FALSE,
                      options=list(
                        pageLength = 15,lengthChange=FALSE)) 
        } else{
          NULL
        }  
      },server=TRUE)

      observeEvent(input$MainSubmit.scatter.table.export,{
        if(!is.null(dataAnalysis$Data.Analysis.Scatter.Outputs)){
          if(input$path.scatter.table.export!=""){
            tryCatch({
              data<-dataAnalysis$Data.Analysis.Scatter.Outputs[["data"]]
              write_excel_csv(data,path = paste0("C://AlgoTraceFolder.export/",input$path.scatter.table.export,".csv"))
              scatter_table_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            }, error=function(err) {
              scatter_table_export.last_action <<- "Error occured - Data was not Exported"
            })
          } else{
            scatter_table_export.last_action<<-paste("Insert File name")
          }
        } else{
          scatter_table_export.last_action<<-paste("No Data to export")
        }
      })
      
      
      output$scatter.table.export.text <- renderText({
        input$MainSubmit.scatter
        input$MainSubmit.scatter.table.export
        scatter_table_export.last_action
      })
      

      

    #####################################
    #############Combination Map#########
    ###Choosing Variables for Combination Map###
    output$Heat.map.Rhands<- renderRHandsontable({
      if (!is.null(currentmodel$uploaded.data)) {   
        if(!is.null(dataAnalysis$Heat.map.Rhands)){
          temp<-dataAnalysis$Heat.map.Rhands 
        } else{
        SPY <- currentmodel$uploaded.data
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)
        temp$X <- FALSE
        temp$Y <- FALSE
        temp<-temp[,c("X","Y","Variable")] 
        }
        
        rhandsontable(temp, rowHeaders = NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
          hot_col("Variable", readOnly=TRUE) %>% 
          hot_table(stretchH="last") 
      } 
    })
    
     observe({
      if (!is.null(input$Heat.map.Rhands)){  
        temp<-hot_to_r(input$Heat.map.Rhands)
        selected1 <- temp[temp$X,"Variable"]
        selected2 <- temp[temp$Y,"Variable"]
        ##X
        if (length(selected1)==0){
          Var1.for.comb.map <- NULL
        }
        if (length(selected1)==1) {
          Var1.for.comb.map <- selected1
          var1_old_for_comb_map <<- selected1
        } 
        if (length(selected1)>1) {
          temp[which(temp$Variable==var1_old_for_comb_map),"X"]<-FALSE
          dataAnalysis$Heat.map.Rhands<-temp
          } 
        
        ##Var2
        if (length(selected2)==0){
          Var2.for.comb.map <- NULL
        }
        if (length(selected2)==1) {
          Var2.for.comb.map <- selected2
          var2_old_for_comb_map <<- selected2
        }
        if (length(selected2)>1) {
          temp[which(temp$Variable==var2_old_for_comb_map),"Y"]<-FALSE
          dataAnalysis$Heat.map.Rhands<-temp
          }
        
        
        if(length(selected1)==1 && length(selected2)==1){
          dataAnalysis$Data.Analysis.Heat.map_chosen.var<-c(Var1.for.comb.map,Var2.for.comb.map)
        } else{
          dataAnalysis$Data.Analysis.Heat.map_chosen.var<-NULL
        }
      } else{
        dataAnalysis$Data.Analysis.Heat.map_chosen.var<-NULL
    }
  }) 
    

     Data.Analysis.Heat.map.variable.to.plot<-reactive({
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map_chosen.var) && isValid_CutTheDataSize()){
        ##Choose Data
        SPY <- currentmodel$uploaded.data
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
          SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
        if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
          SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]
        ###Sample and shuffle
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        Var1.for.comb.map<-dataAnalysis$Data.Analysis.Heat.map_chosen.var[1]
        Var2.for.comb.map<-dataAnalysis$Data.Analysis.Heat.map_chosen.var[2]
          if(input$CutTheData=="No"){
            first.var<-SPY[[Var1.for.comb.map]]
            second.var<-SPY[[Var2.for.comb.map]]
          } else{  
            CutTheDataSize<-input$CutTheDataSize
            if(is.numeric(SPY[[Var1.for.comb.map]])){
              if(length(unique(SPY[[Var1.for.comb.map]]))<=CutTheDataSize){
                first.var<-as.factor(SPY[[Var1.for.comb.map]])
              } else{
                first.var<-cut.nv(SPY[[Var1.for.comb.map]],CutTheDataSize)
              }
            }
            if(class(SPY[[Var1.for.comb.map]])=="factor" && length(unique(SPY[[Var1.for.comb.map]]))<=CutTheDataSize)
              first.var<-SPY[[Var1.for.comb.map]]
            if(class(SPY[[Var1.for.comb.map]])=="factor" && length(unique(SPY[[Var1.for.comb.map]]))>CutTheDataSize){
              tab<-table(SPY[[Var1.for.comb.map]]) 
              temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
              ind<- which(!(SPY[[Var1.for.comb.map]] %in% temp))
              first.var<-SPY[[Var1.for.comb.map]]
              first.var<-as.character(first.var)
              first.var[ind]<-"Others"
              first.var<-as.factor(first.var)
            } 
            
            if(is.numeric(SPY[[Var2.for.comb.map]])){ 
              if(length(unique(SPY[[Var2.for.comb.map]]))<=CutTheDataSize){
                second.var<-as.factor(SPY[[Var2.for.comb.map]])
              } else{
                second.var<-cut.nv(SPY[[Var2.for.comb.map]],CutTheDataSize)  
              }
            }
            if(class(SPY[[Var2.for.comb.map]])=="factor" && length(unique(SPY[[Var2.for.comb.map]]))<=CutTheDataSize)
              second.var<-SPY[[Var2.for.comb.map]]
            if(class(SPY[[Var2.for.comb.map]])=="factor" && length(unique(SPY[[Var2.for.comb.map]]))>CutTheDataSize){
              tab<-table(SPY[[Var2.for.comb.map]]) 
              temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
              ind<- which(!(SPY[[Var2.for.comb.map]] %in% temp))
              second.var<-SPY[[Var2.for.comb.map]]
              second.var<-as.character(second.var)
              second.var[ind]<-"Others"
              second.var<-as.factor(second.var)
            }
          }
          return(list(data=data.frame(target=SPY[[currentmodel$Target.Vars]],first.var=first.var,second.var=second.var),names=c(Var1.for.comb.map,Var2.for.comb.map)))
      } else{ 
        return(NULL) 
      }
    })  
    ##Showing the frequency of each combination##
     observeEvent(input$MainSubmit.vars.for.comb.map,{
       ##Reset the Export text
       Combfrequencymap_export.last_action<<-""
       Combfrequencymaptable_export.last_action<<-""
       Combmap_export.last_action<<-""
       Combmaptable_export.last_action<<-""
       if(!is.null(Data.Analysis.Heat.map.variable.to.plot())){
         Data<-Data.Analysis.Heat.map.variable.to.plot()[["data"]]
         names<-Data.Analysis.Heat.map.variable.to.plot()[["names"]]
         ##Frequency
         dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs<-tryCatch({ 
           Data[,"frequency"]<-1 
           prop.table<-aggregate(frequency ~ first.var + second.var, data=Data, FUN=sum) 
           if(input$HeatMap=="percent")
             prop.table[,"frequency"]<-round(prop.table[,"frequency"]/sum(prop.table[,"frequency"]),3)
           
           if(input$HeatMap=="row percent"){
             for(i in unique(prop.table[,"second.var"])){
               ind<-which(prop.table[,"second.var"]==i)
               prop.table[ind,"frequency"]<-round(prop.table[ind,"frequency"]/sum(prop.table[ind,"frequency"]),3)
             }
           }
           if(input$HeatMap=="column percent"){
             for(i in unique(prop.table[,"first.var"])){
               ind<-which(prop.table[,"first.var"]==i)
               prop.table[ind,"frequency"]<-round(prop.table[ind,"frequency"]/sum(prop.table[ind,"frequency"]),3)
             }
           }
           if(input$HeatMap=="row mean"){
             for(i in unique(prop.table[,"second.var"])){
               ind<-which(prop.table[,"second.var"]==i)
               prop.table[ind,"frequency"]<-round(mean(prop.table[ind,"frequency"]),3)
             }
           }
           if(input$HeatMap=="column mean"){
             for(i in unique(prop.table[,"first.var"])){
               ind<-which(prop.table[,"first.var"]==i)
               prop.table[ind,"frequency"]<-round(mean(prop.table[ind,"frequency"]),3)
             }
           }
           
           p<-ggplot(prop.table, aes(x = first.var, y = second.var))+ geom_tile(aes(fill = frequency),  colour = "white")+ 
             scale_fill_gradient(low = "white",high = "steelblue") + geom_text(label = prop.table$frequency)+ 
             labs(x=names[1],y=names[2])+
             ggtitle("Count of each combination")+
             theme(
               text = element_text(size=15),
               plot.title = element_text(face = "bold"),
               panel.background = element_rect(fill = "white"),
               panel.grid.major = element_line(colour = "lightgray"),
               axis.text.x = element_text(angle = 330),
               legend.background=element_rect(fill='white'),
               plot.background = element_rect(fill="white")
             ) 
           colnames(prop.table)<-c("X","Y","frequency")
           Combination.frequency.map.last.action<<-""
           list(plot=p,table=prop.table)
         },error=function(err) {
           Combination.frequency.map.last.action<<-paste("Error Occured, Combination frequency map can't be shown")
           return(NULL)
         })
         ###
         ##Occurace 
         dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs<-tryCatch({
           if(currentmodel$Method.saved=="Classification"){
             criterion<-"Mean"
             title<-paste("Occurence of",currentmodel$Target.Vars,"in each combination")#"Percentage of 1 in each combination"
           }
           if(currentmodel$Method.saved=="Estimation"){
             criterion<-input$Est.explore.data.criterion.heat_map
             title<-paste(criterion,"of",currentmodel$Target.Vars,"in each combination")#"Mean of Target in each combination"
           }
           prop.table<-aggregate(target ~ first.var + second.var, data=Data, FUN=function(x){criterions.funcion(x,criterion)})
           
           p<-ggplot(prop.table, aes(x = first.var, y = second.var))+ geom_tile(aes(fill = target),  colour = "white") + 
             scale_fill_gradient(low = "white",  high = "steelblue") + geom_text(label = round(prop.table$target,3))+ 
             labs(x=names[1],y=names[2])+ggtitle(title)+
             theme(
               text = element_text(size=15),
               plot.title = element_text(face = "bold"),
               panel.background = element_rect(fill = "white"),
               panel.grid.major = element_line(colour = "lightgray"),
               axis.text.x = element_text(angle = 330),
               legend.background=element_rect(fill='white'),
               plot.background = element_rect(fill="white")
             ) 
           colnames(prop.table)<-c("X","Y",paste(criterion,"of target"))
           prop.table[,3]<-round(prop.table[,3],round.digits)
           Combination.map.last.action<<-""
           list(plot=p,table=prop.table)
         },error=function(err) {
           Combination.map.last.action<<-paste("Error Occured, Combination map can't be shown")
           return(NULL)
         })
       } else{
         dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs<-NULL
         dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs<-NULL
       }
     }) 
      
    output$Combination.frequency.map.warning<-renderText({
      input$MainSubmit.vars.for.comb.map
      Combination.frequency.map.last.action
    })
    output$Combination.frequency.map <-output$Combination.frequency.map.zoom<- renderPlot({
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs))
        dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs[["plot"]]
    }, bg="transparent")   
    
    ##Export Plot
    observeEvent(input$MainSubmit.Combfrequencymap.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs)){
        if(input$path.Combfrequencymap.export!=""){
          tryCatch({
            lib<-getwd()
            temp.lib<-"C:/AlgoTraceFolder.export" 
            setwd(temp.lib)
            ext<-input$Combfrequencymap_export.extension
            ggsave(file=paste0(input$path.Combfrequencymap.export,paste0(".",ext)), dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs[["plot"]], device=ext)
            Combfrequencymap_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            setwd(lib) 
          }, error=function(err){
            Combfrequencymap_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Combfrequencymap_export.last_action<<-paste("Insert File name")
        }
      } else{
        Combfrequencymap_export.last_action<<-paste("No Plot to export")
      }
    })
    
    output$Combfrequencymap.export.text<-renderText({
      input$MainSubmit.vars.for.comb.map
      input$MainSubmit.Combfrequencymap.export
      Combfrequencymap_export.last_action
    })
    
  
    output$Combination.frequency.map.table.zoom<- DT::renderDataTable({ 
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs)){
        Table<-dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs[["table"]]
        datatable(Table ,selection = "single",rownames=FALSE,
                  options=list(
                    pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
  
    
    ##Export table
    observeEvent(input$MainSubmit.Combfrequencymaptable.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs)){
        if(input$path.Combfrequencymaptable.export!=""){
          tryCatch({
            write_excel_csv(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs[["table"]],path = paste0("C://AlgoTraceFolder.export/",input$path.Combfrequencymaptable.export,".csv"))
            Combfrequencymaptable_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Combfrequencymaptable_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Combfrequencymaptable_export.last_action<<-paste("Insert File name")
        }
      } else{
        Combfrequencymaptable_export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Combfrequencymaptable.export.text<-renderText({
      input$MainSubmit.vars.for.comb.map
      input$MainSubmit.Combfrequencymaptable.export
      Combfrequencymaptable_export.last_action
    })
    
    ###reactive for showing it's actionLink
    output$Combination_frequency_map_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Heat.map.Freq.Outputs))
    })
    outputOptions(output, 'Combination_frequency_map_actionlink', suspendWhenHidden=FALSE)
    

    ##Mean of the target variable in each combination##
    output$Combination.map.warning<-renderText({
      input$MainSubmit.vars.for.comb.map
      Combination.map.last.action
    }) 
    output$Combination.map <-output$Combination.map.zoom<- renderPlot({
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs))
        dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs[["plot"]]
    }, bg="transparent") 
    
    ##Export Plot
    observeEvent(input$MainSubmit.Combmap.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs)){
        if(input$path.Combmap.export!=""){
          tryCatch({
            lib<-getwd()
            temp.lib<-"C:/AlgoTraceFolder.export" 
            setwd(temp.lib)
            ext<-input$Combmap_export.extension
            ggsave(file=paste0(input$path.Combmap.export,paste0(".",ext)), dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs[["plot"]], device=ext)
            Combmap_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            setwd(lib) 
          }, error=function(err){
            Combmap_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Combmap_export.last_action<<-paste("Insert File name")
        }
      } else{
        Combmap_export.last_action<<-paste("No Plot to export")
      }
    })
    
    output$Combmap.export.text<-renderText({
      input$MainSubmit.vars.for.comb.map
      input$MainSubmit.Combmap.export
      Combmap_export.last_action
    })
    
    output$Combination.map.table.zoom<- DT::renderDataTable({ 
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs)){
        Table<-dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs[["table"]]
        datatable(Table ,selection = "single",rownames=FALSE,
                  options=list(
                    pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)


    ##Export table
    observeEvent(input$MainSubmit.Combmaptable.export,{
      if(!is.null(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs)){
        if(input$path.Combmaptable.export!=""){
          tryCatch({
            write_excel_csv(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs[["table"]],path = paste0("C://AlgoTraceFolder.export/",input$path.Combmaptable.export,".csv"))
            Combmaptable_export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            Combmaptable_export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          Combmaptable_export.last_action<<-paste("Insert File name")
        }
      } else{
        Combmaptable_export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Combmaptable.export.text<-renderText({
      input$MainSubmit.vars.for.comb.map
      input$MainSubmit.Combmaptable.export
      Combmaptable_export.last_action
    })
    
    ###reactive for showing it's actionLink
    output$Combination_map_actionlink<-reactive({
      return(!is.null(dataAnalysis$Data.Analysis.Heat.map.Occ.Outputs))
    })
    outputOptions(output, 'Combination_map_actionlink', suspendWhenHidden=FALSE)
    
    ####Grid plot#####
    observeEvent(input$MainSubmit.Combination.map.gridplot,{ 
      if (!is.null(currentmodel$uploaded.data) && !is.null(currentmodel$Target.Vars) && 
          isValid_CutTheDataSize()) { 
        if(input$path.Combination.map.gridplot!=""){
          SPY <- currentmodel$uploaded.data
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Train")
            SPY<-SPY[1:round(currentmodel$division.rate*nrow(SPY)),]
          if(dataAnalysis$Select.Choose.Data.Data.Analysis=="Test")
            SPY<-SPY[(round(currentmodel$division.rate*nrow(SPY))+1):nrow(SPY),]

        ###Sample and shuffle
        SPY<-data_sample(SPY,input$Percent_Data_Analysis,input$AllowSampling.Data.Analysis)
        vars<-names(SPY)[!(names(SPY) %in% currentmodel$vars.to.exclude)]
        plist1<-list()
        plist2<-list()
        for(Var1.for.comb.map in vars){
          for(Var2.for.comb.map in vars){
            if(Var2.for.comb.map < Var1.for.comb.map){next}
            first.var<-SPY[[Var1.for.comb.map]]
            second.var<-SPY[[Var2.for.comb.map]]
            if(input$CutTheData=="No"){
              first.var<-SPY[[Var1.for.comb.map]]
              second.var<-SPY[[Var2.for.comb.map]]
            } else{  
              CutTheDataSize<-input$CutTheDataSize
              if(is.numeric(SPY[[Var1.for.comb.map]])){
                if(length(unique(SPY[[Var1.for.comb.map]]))<=CutTheDataSize){
                  first.var<-as.factor(SPY[[Var1.for.comb.map]])
                } else{
                  first.var<-cut.nv(SPY[[Var1.for.comb.map]],CutTheDataSize)
                }
              }
              if(class(SPY[[Var1.for.comb.map]])=="factor" && length(unique(SPY[[Var1.for.comb.map]]))<=CutTheDataSize)
                first.var<-SPY[[Var1.for.comb.map]]
              if(class(SPY[[Var1.for.comb.map]])=="factor" && length(unique(SPY[[Var1.for.comb.map]]))>CutTheDataSize){
                tab<-table(SPY[[Var1.for.comb.map]]) 
                temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
                ind<- which(!(SPY[[Var1.for.comb.map]] %in% temp))
                first.var<-SPY[[Var1.for.comb.map]]
                first.var<-as.character(first.var)
                first.var[ind]<-"Others"
                first.var<-as.factor(first.var)
              } 
              
              if(is.numeric(SPY[[Var2.for.comb.map]])){ 
                if(length(unique(SPY[[Var2.for.comb.map]]))<=CutTheDataSize){
                  second.var<-as.factor(SPY[[Var2.for.comb.map]])
                } else{
                  second.var<-cut.nv(SPY[[Var2.for.comb.map]],CutTheDataSize)
                }
              }
              if(class(SPY[[Var2.for.comb.map]])=="factor" && length(unique(SPY[[Var2.for.comb.map]]))<=CutTheDataSize)
                second.var<-SPY[[Var2.for.comb.map]]
              if(class(SPY[[Var2.for.comb.map]])=="factor" && length(unique(SPY[[Var2.for.comb.map]]))>CutTheDataSize){
                tab<-table(SPY[[Var2.for.comb.map]]) 
                temp<-names(tab[order(-tab)])[1:(CutTheDataSize-1)] 
                ind<- which(!(SPY[[Var2.for.comb.map]] %in% temp))
                second.var<-SPY[[Var2.for.comb.map]]
                second.var<-as.character(second.var)
                second.var[ind]<-"Others"
                second.var<-as.factor(second.var)
              }
            }
            Data<-data.frame(target=SPY[[currentmodel$Target.Vars]],first.var=first.var,second.var=second.var)
            names<-c(Var1.for.comb.map,Var2.for.comb.map)
            ###############################
            #Drawing The Plot
            
            tryCatch({
              if(currentmodel$Method.saved=="Classification"){
                criterion<-"Mean"
                title<-paste("Occurence of",currentmodel$Target.Vars,"in each combination")#"Percentage of 1 in each combination"
              }
              if(currentmodel$Method.saved=="Estimation"){
                criterion<-input$Est.explore.data.criterion.heat_map
                title<-paste(criterion,"of",currentmodel$Target.Vars,"in each combination")#"Mean of Target in each combination"
              }
              prop.table<-aggregate(target ~ first.var + second.var, data=Data, FUN=function(x){criterions.funcion(x,criterion)})
              
              p<-ggplot(prop.table, aes(x = first.var, y = second.var))+ geom_tile(aes(fill = target),  colour = "white") + 
                scale_fill_gradient(low = "white",  high = "steelblue") + geom_text(label = round(prop.table$target,3))+ 
                labs(x=names[1],y=names[2])+ggtitle(title)+
                theme(
                  text = element_text(size=15),
                  plot.title = element_text(face = "bold"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(colour = "lightgray"),
                  axis.text.x = element_text(angle = 330),
                  legend.background=element_rect(fill='white'),
                  plot.background = element_rect(fill="white")
                ) 
              plist1[[paste(names,collapse=" & ")]]<-p
            },error=function(err) {
            })
            tryCatch({
              Data[,"frequency"]<-1 
              prop.table<-aggregate(frequency ~ first.var + second.var, data=Data, FUN=sum) 
              if(input$HeatMap=="percent")
                prop.table[,"frequency"]<-round(prop.table[,"frequency"]/sum(prop.table[,"frequency"]),3)
              
              if(input$HeatMap=="row percent"){
                for(i in unique(prop.table[,"second.var"])){
                  ind<-which(prop.table[,"second.var"]==i)
                  prop.table[ind,"frequency"]<-round(prop.table[ind,"frequency"]/sum(prop.table[ind,"frequency"]),3)
                }
              }
              if(input$HeatMap=="column percent"){
                for(i in unique(prop.table[,"first.var"])){
                  ind<-which(prop.table[,"first.var"]==i)
                  prop.table[ind,"frequency"]<-round(prop.table[ind,"frequency"]/sum(prop.table[ind,"frequency"]),3)
                }
              }
              if(input$HeatMap=="row mean"){
                for(i in unique(prop.table[,"second.var"])){
                  ind<-which(prop.table[,"second.var"]==i)
                  prop.table[ind,"frequency"]<-round(mean(prop.table[ind,"frequency"]),3)
                }
              }
              if(input$HeatMap=="column mean"){
                for(i in unique(prop.table[,"first.var"])){
                  ind<-which(prop.table[,"first.var"]==i)
                  prop.table[ind,"frequency"]<-round(mean(prop.table[ind,"frequency"]),3)
                }
              }
              p<-ggplot(prop.table, aes(x = first.var, y = second.var))+ geom_tile(aes(fill = frequency),  colour = "white")+ 
                scale_fill_gradient(low = "white",high = "steelblue") + geom_text(label = prop.table$frequency)+ 
                labs(x=names[1],y=names[2])+
                ggtitle("Count of each combination")+
                theme(
                  text = element_text(size=15),
                  plot.title = element_text(face = "bold"),
                  panel.background = element_rect(fill = "white"),
                  panel.grid.major = element_line(colour = "lightgray"),
                  axis.text.x = element_text(angle = 330),
                  legend.background=element_rect(fill='white'),
                  plot.background = element_rect(fill="white")
                ) 
              plist2[[paste(names,collapse=" & ")]]<-p
            },error=function(err) {
            })
          }}#End of for loop
        n1 <- length(plist1)
        n2<- length(plist2)
        lib<-getwd()
        temp.lib<-"C:/AlgoTraceFolder.export" 
        setwd(temp.lib)
        ext<-input$Combination.map.gridplot.extension
        for(i in 1:n1){
          tryCatch({
          ggsave(file=paste0(input$path.Combination.map.gridplot,"_Occurance",i,paste0(".",ext)), plist1[[i]], device=ext)
          },error=function(err) {
          })
        }
        for(i in 1:n2){
          tryCatch({
          ggsave(file=paste0(input$path.Combination.map.gridplot,"_Freq",i,paste0(".",ext)), plist2[[i]], device=ext)
          },error=function(err) {
          })
        }
        Combination.map.Gridplot.last_action<<-paste("Plots File was Exported to C://AlgoTraceFolder.export/")
        setwd(lib)
      } else{
        Combination.map.Gridplot.last_action<<-paste("Insert File name")
      }
    } 
  })
    
    output$Combination.map.Gridplot.text <- renderText({
      input$MainSubmit.Combination.map.gridplot
      Combination.map.Gridplot.last_action
    })
    ####################################################
    ##########End of Data Analysis###############
    ####################################################
    ########## Var vs Target model######################
    output$Var.vs.target.model_rhTable<-renderRHandsontable({
      if (!is.null(currentmodel$uploaded.data) && !is.null(currentmodel$Target.Vars)) {
        if(!is.null(var.vs.target.Model$Var.vs.target.model_rhTable.saved)){
          temp<-var.vs.target.Model$Var.vs.target.model_rhTable.saved 
        } else{
          output.var<-currentmodel$Target.Vars
          Data <- currentmodel$uploaded.data
          Data<-Data[cleaned.places(Data[[output.var]]),] #without na target
          vars<-names(Data)[!(names(Data) %in% c(output.var,currentmodel$vars.to.exclude))]
          temp <- data.frame(Variable=vars, stringsAsFactors=FALSE)  
          temp$Choose <- FALSE
          temp<-temp[,c("Choose","Variable")]
        }
        rhandsontable(temp,rowHeaders=NULL,maxRows=nrow(temp),height=min(350,nrow(temp)*35)) %>% 
          hot_col("Variable",readOnly=TRUE) %>% 
          hot_table(stretchH="last") 
      }})
    
    
    observe({
      if(!is.null(input$Var.vs.target.model_rhTable)){
        temp<-hot_to_r(input$Var.vs.target.model_rhTable)
        selected <- temp[temp$Choose,"Variable"]
        
        if (length(selected)==0) {
          var.vs.target.Model$Var.vs.target.model_chosen.var<-NULL
        }
        if (length(selected)==1) {
          Var.to.evaluate_old <<- selected
          var.vs.target.Model$Var.vs.target.model_chosen.var<-selected
        }
        if (length(selected)>1) {
          temp[which(temp$Variable==Var.to.evaluate_old),"Choose"]<-FALSE
        }
        var.vs.target.Model$Var.vs.target.model_rhTable.saved<-temp
      } else{
        var.vs.target.Model$Var.vs.target.model_chosen.var<-NULL
      }
    }) 
    
    observeEvent(input$MainSubmit_Var.vs.target.model,{
      if(!is.null(var.vs.target.Model$Var.vs.target.model_chosen.var)){
        var.vs.target.Model$Var.vs.target.model.Outputs<-tryCatch({  
          output.var<-currentmodel$Target.Vars
          Var.to.evaluate<-var.vs.target.Model$Var.vs.target.model_chosen.var
          myform <- as.formula(paste(as.symbol(output.var), " ~ ", Var.to.evaluate))
          Data <- currentmodel$uploaded.data
          Data<-Data[cleaned.places(Data[[output.var]]),] #without na target
          ###Sample and shuffle
          Data<-data_sample(Data,input$Percent.Var.vs.target.model,input$AllowSampling.Var.vs.target.model)
          SPY<-Data[1:round(currentmodel$division.rate*nrow(Data)),]
          SPY.Test<-Data[(round(currentmodel$division.rate*nrow(Data))+1):nrow(Data),]
          if(currentmodel$Method.saved=="Classification"){
            model<-glm(myform,family=binomial("logit"),data=SPY,na.action = na.omit) 
            table.count<-vector(mode="numeric",length=10)
            table.percent<-vector(mode="numeric",length=10)
            Bins<-vector(mode="character",length=10)
            pred<-predict(model,newdata=SPY.Test,type="response")
            Overall.Acc<-round(mean(SPY.Test[[output.var]]==ifelse(pred>0.5, 1, 0),na.rm=TRUE),round.digits)
            for(i in 1:10){  
              Bins[i]<-paste(0.1*(i-1),"-",0.1*i)
              ind<-intersect(which(pred>=0.1*(i-1)),which(pred<0.1*i))
              table.count[i]<-length(ind)
              table.percent[i]<-mean(SPY.Test[[output.var]][ind]==ifelse(pred[ind]>0.5, 1, 0),na.rm=TRUE)
            }
            table<-data.frame(Bins=Bins,table.count=table.count,table.percent=table.percent,Var.to.evaluate=Var.to.evaluate,Overall.Acc=Overall.Acc)
            p.count<-ggplot(table,aes(x=Bins,y=table.count))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+ylab("Count in each Bin")
            p.percent<-ggplot(table,aes(x=Bins,y=table.percent))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
              labs(x=paste("Bins\nOverall Accuracy:",table$Overall.Acc[1]),y="Accuracy in each Bin")
          } else{ #Estimation
            model<-lm(myform , data=SPY,na.action = na.omit) 
            prediction<-predict(model,newdata=SPY.Test,type='response')
            ##Finding the mean of the prediction in each Bin
            cut_to_levels<-cut.nv(SPY.Test[[output.var]],5)
            cut_to_levels.uni<-unique(cut_to_levels)
            Avg_output<-vector(mode="numeric",length=length(cut_to_levels.uni))
            Avg_pred<-vector(mode="numeric",length=length(cut_to_levels.uni))
            count_in_each_bin<-vector(mode="numeric",length=length(cut_to_levels.uni))
            for(i in 1:length(cut_to_levels.uni)){
              ind<-which(cut_to_levels==cut_to_levels.uni[i])
              count_in_each_bin[i]<-length(ind)
              Avg_output[i]<-mean(SPY.Test[[output.var]][ind],na.rm=TRUE)
              Avg_pred[i]<-mean(prediction[ind],na.rm=TRUE)
            }
            table<-data.frame(Bins=cut_to_levels.uni,Avg_output=Avg_output,Avg_pred=Avg_pred,count_in_each_bin=count_in_each_bin,Var.to.evaluate=Var.to.evaluate)
            p.count<-ggplot(table,aes(x=Bins,y=count_in_each_bin))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+ylab("Count in each Bin")
            p.percent<-ggplot(table,aes(x=Bins,y=value))+ 
              geom_point(aes(y=table[,"Avg_output"],col="Avg Target"),stat="identity",shape=21,size=5,stroke=2)+ #,lwd=3
              geom_point(aes(y=table[,"Avg_pred"],col="Avg Prediction"),stat="identity",shape=21,size=5,stroke=2) + xlab("Target Variable Bins")+
              ggtitle(paste(output.var,"~",table$Var.to.evaluate))
          }
          list(p.count=p.count,p.percent=p.percent)
        }, error=function(err) {
          return(NULL)
        })
      } else{
        var.vs.target.Model$Var.vs.target.model.Outputs<-NULL
      }
    })
    
    output$Var.vs.target.model_Plot.count<-renderPlot({
      if(!is.null(var.vs.target.Model$Var.vs.target.model.Outputs)){
        p<-var.vs.target.Model$Var.vs.target.model.Outputs[["p.count"]]
        
        p<-p+theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.position="none",
          plot.background = element_rect(fill="white")
        )
        p
      }
    }, bg="transparent")    
    
    output$Var.vs.target.model_Plot.percent<-renderPlot({
      if(!is.null(var.vs.target.Model$Var.vs.target.model.Outputs)){
       p<-var.vs.target.Model$Var.vs.target.model.Outputs[["p.percent"]]
       
        p<-p+theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),   
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.position="bottom",   
          plot.background = element_rect(fill="white")
        )
        
        p
      }
    }, bg="transparent")
    ###################################################
    ##########End of Var vs Target model###############
    ###################################################
    #########Ratio Combination###################
    observe({
      if(!is.null(currentmodel$division.rate))
        return()
      currentmodel$division.rate<-input$division.rate.slider
    })
    observeEvent(input$MainSubmit.division.rate,{
      currentmodel$division.rate<-input$division.rate.slider
      ###reset the ratio combination tab###
      currentmodel$ratio.comb.table.to.show<-NULL
      currentmodel$ratio.comb.table.to.show.saved<-NULL
      currentmodel$ratio.comb.info<-NULL
    })
    
    output$Interval.for.ratio.combination<-renderUI({
      if(!is.null(currentmodel$uploaded.data) && !is.null(currentmodel$Target.Vars) && currentmodel$Method.saved=="Estimation"){
      Data<-currentmodel$uploaded.data
      output.var<-currentmodel$Target.Vars
      Data<-Data[cleaned.places(Data[[output.var]]),] #without na target
      SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),] 
      Min <- min(SPY[[output.var]],na.rm=T)
      Max <- max(SPY[[output.var]],na.rm=T)
      ##value
      isolate({
        if(is.null(currentmodel$Loaded.Interval.for.ratio.combination)){
          value<-c(Min,Max)
        } else{
          value<-currentmodel$Loaded.Interval.for.ratio.combination
        }
        currentmodel$Loaded.Interval.for.ratio.combination<-NULL
      })
      ### 
      if(length(unique(cleaned.vec(SPY[[output.var]])))<=2){
        sliderInput("Interval.for.ratio.combination", 
                    label = output.var, 
                    min = Min, max = Max, value = value, step = (Max-Min))
      } else{
        sliderInput("Interval.for.ratio.combination", 
                    label = output.var, 
                    min = Min, max = Max, value = value,step =(Max-Min)/500)  #round((Max-Min)/500,3)
      }
    }}) 
    
    ratio.comb.info<-reactive({
      if(!is.null(currentmodel$uploaded.data) && !is.null(currentmodel$Target.Vars)){
        Data<-currentmodel$uploaded.data
        output.var<-currentmodel$Target.Vars
        Data<-Data[cleaned.places(Data[[output.var]]),] #without na target 
        SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
        SPY<-SPY[,sapply(SPY,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        vars.not.include <- unique(c(currentmodel$vars.to.exclude,currentmodel$Exclude))  
        vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
        
        is.interval<-sapply(vars, function(x) {
          if(is.numeric(SPY[[x]])){
            return("Yes")
          } else{
            return("No")
          }
        })
        
        SPY[,vars]<-as.data.table(sapply(vars,function(x){
          if(is.numeric(SPY[[x]])){
            return(cut.nv(SPY[[x]],10))
          } else{
            return(SPY[[x]])
          }
        }))
        
        return(list("is.interval"=is.interval,"data"=SPY))
      } else{
        return(NULL)
      }
    })
    
    ratio.comb.table<-reactive({
      if(!is.null(ratio.comb.info())){
        is.interval<-ratio.comb.info()[["is.interval"]]
        SPY<-ratio.comb.info()[["data"]]
        output.var<-currentmodel$Target.Vars
        vars.not.include <- unique(c(currentmodel$vars.to.exclude,currentmodel$Exclude))  
        vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
        if(currentmodel$Method.saved=="Estimation"){
          temp.interval<-input$Interval.for.ratio.combination
          temp<-ifelse(SPY[[output.var]]>=temp.interval[1] & SPY[[output.var]]<=temp.interval[2],1,0)
          SPY[,"Est.Value"]<-SPY[[output.var]]
          SPY[,output.var]<-temp
        }
        
        SPY$frequency<-1
        table<-lapply(vars,function(i){
          tryCatch({
            prop.table<-aggregate(SPY[[output.var]] ~ SPY[[i]], FUN=mean)
            count.table<-aggregate(SPY[["frequency"]] ~ SPY[[i]], FUN=sum)
            if(currentmodel$Method.saved=="Estimation")
              est.val.table<-aggregate(SPY[["Est.Value"]] ~ SPY[[i]], FUN=mean)
            ind<-which(prop.table[,ncol(prop.table)]>=0.05)
            if(length(ind)>0){
              temp<-lapply(ind,function(k){
                if(currentmodel$Method.saved=="Classification")
                  return(data.frame(i,as.character(prop.table[k,1]),round(count.table[k,2],3),round(prop.table[k,2],3)))
                if(currentmodel$Method.saved=="Estimation")
                  return(data.frame(i,as.character(prop.table[k,1]),round(count.table[k,2],3),round(prop.table[k,2],3),round(est.val.table[k,2],3)))
                })
              return(do.call("rbind",temp))
            } else{
              return(NULL)
            }
          },error=function(err) {
            return(NULL)
          })
        })

        table<-do.call("rbind",table)
        table<-as.data.frame(table)
        if(nrow(table)>0){
          if(currentmodel$Method.saved=="Classification")
            colnames(table)<-c("Variable","Level","Count","Ratio of success")
          if(currentmodel$Method.saved=="Estimation")
            colnames(table)<-c("Variable","Level","Count","Ratio of success","Mean Value")
          return(table)
        } else{
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
    
    ratio.comb.table.for.couples<-reactive({
      if(!is.null(ratio.comb.info())){
        is.interval<-ratio.comb.info()[["is.interval"]]
        SPY<-ratio.comb.info()[["data"]]
        output.var<-currentmodel$Target.Vars
        vars.not.include <- unique(c(currentmodel$vars.to.exclude,currentmodel$Exclude))  
        vars<-names(SPY)[!(names(SPY) %in% c(output.var,vars.not.include))]
        if(currentmodel$Method.saved=="Estimation"){
          temp.interval<-input$Interval.for.ratio.combination
          temp<-ifelse(SPY[[output.var]]>=temp.interval[1] & SPY[[output.var]]<=temp.interval[2],1,0)
          SPY[,"Est.Value"]<-SPY[[output.var]]
          SPY[,output.var]<-temp
        }
        
        SPY$frequency<-1
        table<-lapply(vars,function(i){
          temp<-lapply(vars,function(j){
            if(which(vars==j)<=which(vars==i)){return(NULL)}
            tryCatch({
              prop.table<-aggregate(SPY[[output.var]] ~ SPY[[i]]+SPY[[j]], FUN=mean)
              count.table<-aggregate(SPY[["frequency"]] ~ SPY[[i]]+SPY[[j]], FUN=sum)
              if(currentmodel$Method.saved=="Estimation")
                est.val.table<-aggregate(SPY[["Est.Value"]] ~ SPY[[i]]+SPY[[j]], FUN=mean)
              ind<-which(prop.table[,ncol(prop.table)]>=0.05)
              if(length(ind)>0){
                if(currentmodel$Method.saved=="Classification")
                  return(data.frame(rep(i,length(ind)),rep(j,length(ind)),as.character(prop.table[ind,1]),as.character(prop.table[ind,2]),round(count.table[ind,3],3),round(prop.table[ind,3],3)))
                if(currentmodel$Method.saved=="Estimation")
                  return(data.frame(rep(i,length(ind)),rep(j,length(ind)),as.character(prop.table[ind,1]),as.character(prop.table[ind,2]),round(count.table[ind,3],3),round(prop.table[ind,3],3),round(est.val.table[ind,3],3)))
              } else{
                return(NULL)
              }
            },error=function(err) {
              return(NULL)
            }) 
          })
          return(do.call("rbind",temp))
        })
        table<-do.call("rbind",table)
        table<-as.data.frame(table)
        if(nrow(table)>0){
          if(currentmodel$Method.saved=="Classification")
            colnames(table)<-c("Variable 1","Variable 2","Level 1","Level 2","Count","Ratio of success")
          if(currentmodel$Method.saved=="Estimation")
            colnames(table)<-c("Variable 1","Variable 2","Level 1","Level 2","Count","Ratio of success","Mean Value")
          return(table)
        } else{
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
    
    
    observeEvent(input$Allow.Computing.ratio.comb,{
      ##Reset the Export text
      ratio.comb.export.last_action<<-""
      transfer.ratio.to.dummies.last_action<<-""
      ##########################
      if(!is.na(input$ratio.comb.count)){
        if(input$ratio.comb.for.couples=="No"){
          if(!is.null(ratio.comb.table())){
            table<-ratio.comb.table()
            currentmodel$ratio.comb.table.to.show<-subset(table,table[,"Count"]>=input$ratio.comb.count & table[,"Ratio of success"]>=input$ratio.comb.mean)
            rownames(currentmodel$ratio.comb.table.to.show)<-NULL
          } else{
            currentmodel$ratio.comb.table.to.show<-NULL
          }
        } else{#input$ratio.comb.for.couples=="Yes"
          if(!is.null(ratio.comb.table.for.couples())){
            table<-ratio.comb.table.for.couples()
            currentmodel$ratio.comb.table.to.show<-subset(table,table[,"Count"]>=input$ratio.comb.count & table[,"Ratio of success"]>=input$ratio.comb.mean)
            rownames(currentmodel$ratio.comb.table.to.show)<-NULL
          } else{
            currentmodel$ratio.comb.table.to.show<-NULL
          }
        }
      } else{#is.na(input$ratio.comb.count)
        currentmodel$ratio.comb.table.to.show<-NULL
      }
    })
    
  
    observeEvent(input$transfer.ratio.to.dummies,{
        currentmodel$ratio.comb.table.to.show.saved<-currentmodel$ratio.comb.table.to.show
        currentmodel$ratio.comb.info<-ratio.comb.info()
        if(!is.null(currentmodel$ratio.comb.table.to.show.saved)){
          transfer.ratio.to.dummies.last_action<<-paste("Ratio combinations will be used as variables")
        } else{
          transfer.ratio.to.dummies.last_action<<-""
        }
    })
    
    observeEvent(input$reset.ratio.to.dummies,{
      currentmodel$ratio.comb.table.to.show.saved<-NULL
      currentmodel$ratio.comb.info<-NULL
      transfer.ratio.to.dummies.last_action<<-""
    })
    
    output$Transfer.ratio.to.dummies.text<-renderText({
      input$Allow.Computing.ratio.comb
      input$transfer.ratio.to.dummies
      input$reset.ratio.to.dummies
      transfer.ratio.to.dummies.last_action
    })
    
    output$Ratio.comb.table.to.show<-DT::renderDataTable({
      if(!is.null(currentmodel$ratio.comb.table.to.show)){
        Table<-currentmodel$ratio.comb.table.to.show
        ind<-which(unlist(lapply(names(Table),function(i){is.factor(Table[,i])})))
        datatable(Table,options=list(
          columnDefs = list(list(
            targets  = ind,
            render  = JS(
              "function ( data, type ) {",
              "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
              "data.substr(0, 30) +'...' :",
              "data;",
              "}"))),
          rowCallback = JS(
            "function(nRow, aData) {",
            string.for.tooltip(ind),
            "}"),
          pageLength = 20)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    observeEvent(input$MainSubmit.Export.button.ratio.comb,{
      if(!is.null(currentmodel$ratio.comb.table.to.show)){# && !is.null(ratio.comb.info())
        #!is.null(currentmodel$ratio.comb.table.to.show)----> !is.null(ratio.comb.info())
        if(input$path.ratio.comb.export!=""){
          tryCatch({
            if (input$What.to.export.ratio.comb == "Table"){
              write_excel_csv(currentmodel$ratio.comb.table.to.show,path = paste0("C://AlgoTraceFolder.export/",input$path.ratio.comb.export,".csv"))
            }
            if (input$What.to.export.ratio.comb == "File"){
              ratio.levels.file<-turn.ratio.combination.to.data(currentmodel$uploaded.data,currentmodel$ratio.comb.table.to.show,ratio.comb.info()[["is.interval"]])
              write_excel_csv(ratio.levels.file,path = paste0("C://AlgoTraceFolder.export/",input$path.ratio.comb.export,".csv"))
            }
            ratio.comb.export.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            ratio.comb.export.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          ratio.comb.export.last_action<<-paste("Insert File name") 
        }
      } else{
        ratio.comb.export.last_action<<-paste("No Data to export")
      }
    })
    
    output$Export.ratio.comb.text <- renderText({
      input$Allow.Computing.ratio.comb
      input$MainSubmit.Export.button.ratio.comb
      ratio.comb.export.last_action
    })
    ##########################################################
    ###############End of Ratio Combination###################
    ##########################################################
    ################Geography map####################
    ##Geography
    Geography.Data<-reactive({
      if(!is.null(currentmodel$uploaded.data) && nrow(currentmodel$uploaded.data)>0){
        dat<-currentmodel$uploaded.data
        dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        temp.names<-names(dat)[!(names(dat) %in% currentmodel$vars.to.exclude)]
        if(all(c('latitude', 'longitude') %in% tolower(temp.names))){
          return(TRUE)
        } else{
          return(FALSE)
        }
      } else{
        return(FALSE)
      }
    })
    
    
    output$Geography<-reactive({
      return(Geography.Data())
    })
    outputOptions(output, 'Geography', suspendWhenHidden=FALSE)
    ####Show/Hide Tab########
    observe({
      if(Geography.Data()==TRUE)
        showTab(inputId = "Explore", target = "Geography")
      if(Geography.Data()==FALSE){
        hideTab(inputId = "Explore", target = "Geography")
        updateTabsetPanel(session, "Explore",selected = "Variables Correlation")
      }
    })   
    #######Slider and Select Input###################
    observe({  
      if(!is.null(currentmodel$uploaded.data) && Geography.Data()==TRUE){ 
        dat<-currentmodel$uploaded.data
        dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        Names<-names(dat)[!(names(dat) %in% currentmodel$vars.to.exclude)]
        ##
        output$GeographyData.Filter.numeric <- renderUI({  
          Names<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
          if(length(Names)>0){
            plot_output_list <- lapply(Names, function(i) {
              filtername <- paste("GeographyData_Filter_", i, sep="")
              Min <- min(dat[[i]],na.rm=T)
              Max <- max(dat[[i]],na.rm=T)
              ##value
              isolate({
                if(is.null(currentmodel$Loaded.Numeric.Geographydata.filter.table)){
                  value<-c(Min,Max)
                } else{
                  value<-currentmodel$Loaded.Numeric.Geographydata.filter.table[[filtername]]
                }
              })
              ###
              if(length(unique(cleaned.vec(dat[[i]])))<=2){
                sliderInput(filtername, 
                            label = i, 
                            min = Min, max = Max, value = value, step = (Max-Min))
              } else{
                sliderInput(filtername, 
                            label = i, 
                            min = Min, max = Max, value = value, step =(Max-Min)/500)
              }})
            #####
            isolate({
              if(!is.null(currentmodel$Loaded.Numeric.Geographydata.filter.table))
                currentmodel$Loaded.Numeric.Geographydata.filter.table<-NULL
            })
            ####
            fluidRow(do.call(tagList, plot_output_list))
          }})  
        output$GeographyData.Filter.factor <- renderUI({
          Names<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
          if(length(Names)>0){
            plot_output_list <- lapply(Names, function(i) {
              filtername <- paste("GeographyData_Filter_", i, sep="")
              ##selected
              isolate({
                if(is.null(currentmodel$Loaded.Factor.Geographydata.filter.table)){
                  selected<-"ALL"
                } else{
                  selected<-currentmodel$Loaded.Factor.Geographydata.filter.table[[filtername]]
                }
              })
              ##
              selectInput(filtername, 
                          label = i, 
                          choices=c("ALL",unique(as.character(dat[[i]]))),
                          selected = selected,multiple = TRUE)
            })
            ###
            isolate({
              if(!is.null(currentmodel$Loaded.Factor.Geographydata.filter.table))
                currentmodel$Loaded.Factor.Geographydata.filter.table<-NULL
            })
            ####
            fluidRow(do.call(tagList, plot_output_list))
          }}) 
      }})
    
    observeEvent(input$MainSubmit.GeographyData.Filter.Save,{   
      if (!is.null(currentmodel$uploaded.data) && Geography.Data()==TRUE) {
        dat<-currentmodel$uploaded.data
        dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        Names<-names(dat)[!(names(dat) %in% currentmodel$vars.to.exclude)]
        Names.numeric<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
        Names.factor<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
        ##
        Ind<-1:nrow(dat)
        if(input$AllowSampling.GeographyData=="Yes"){
          set.seed(12)
          Ind<-intersect(Ind,sample(1:nrow(dat),size=round((input$Percent.GeographyData)*as.numeric(nrow(dat))),replace=FALSE))  
        }
        currentmodel$Numeric.Geographydata.filter.table<-list()
        currentmodel$Factor.Geographydata.filter.table<-list()
        for(i in Names.numeric){ 
          temp<-input[[paste("GeographyData_Filter_", i, sep="")]]
          currentmodel$Numeric.Geographydata.filter.table[[paste("GeographyData_Filter_", i, sep="")]]<-temp
          if(is.null(temp)){next}
          ind<-union(intersect(which(dat[[i]]>=temp[1]),which(dat[[i]]<=temp[2])),which(!cleaned.places(dat[[i]])))
          Ind<-intersect(Ind,ind)
        }
        for(i in Names.factor){
          temp<-input[[paste("GeographyData_Filter_", i, sep="")]]
          currentmodel$Factor.Geographydata.filter.table[[paste("GeographyData_Filter_", i, sep="")]]<-temp
          if(is.null(temp) || "ALL" %in% temp){next}
          ind<-union(which(dat[[i]] %in% temp),which(!cleaned.places(dat[[i]])))
          Ind<-intersect(Ind,ind)
        }
        currentmodel$Map.Filter.Index<-Ind
      } 
    })
    
    observeEvent(input$MainSubmit.GeographyData.Filter.Reset,{
      if (!is.null(currentmodel$uploaded.data) && Geography.Data()==TRUE) {
        dat<-currentmodel$uploaded.data
        dat<-dat[,sapply(dat,function(x){length(cleaned.vec(x))>0}),with=FALSE]
        Ind<-1:nrow(dat)
        if(input$AllowSampling.GeographyData=="Yes"){
          set.seed(12)
          Ind<-intersect(Ind,sample(1:nrow(dat),size=round((input$Percent.GeographyData)*as.numeric(nrow(dat))),replace=FALSE))  
        }  
        currentmodel$Map.Filter.Index<-Ind
        ##
        Names<-names(dat)[!(names(dat) %in% currentmodel$vars.to.exclude)]
        currentmodel$Numeric.Geographydata.filter.table<-list()
        currentmodel$Factor.Geographydata.filter.table<-list()
        ##
        Names.numeric<-Names[sapply(Names,function(i){is.numeric(dat[[i]])})] 
        for(i in Names.numeric){
          filtername <- paste("GeographyData_Filter_", i, sep="")
          Min <- min(dat[[i]],na.rm=T)
          Max <- max(dat[[i]],na.rm=T)
          updateSliderInput(session,filtername,value=c(Min,Max))
          currentmodel$Numeric.Geographydata.filter.table[[filtername]]<-c(Min,Max)
        }
        ##
        Names.factor<-Names[sapply(Names,function(i){is.factor(dat[[i]])})] 
        for(i in Names.factor){
          filtername <- paste("GeographyData_Filter_", i, sep="")
          updateSelectInput(session,filtername,selected="ALL")
          currentmodel$Factor.Geographydata.filter.table[[filtername]]<-"ALL"
        }
      } 
    })
    
    output$showDataDimensions.GeographyData.Filter<- renderText({
      if (!is.null(currentmodel$uploaded.data) && Geography.Data()==TRUE) {
        if(!is.null(currentmodel$Map.Filter.Index)){
          ind<-currentmodel$Map.Filter.Index
          SPY<-currentmodel$uploaded.data[ind,]
        } else{
          SPY<-currentmodel$uploaded.data  
        }
        paste("Dimensions", dim(SPY)[1], "X" , dim(SPY)[2])
      }
    })
    
    output$Geography.map.text<-renderText({
      if (!is.null(currentmodel$Map.Filter.Index)){
        ind<-currentmodel$Map.Filter.Index
        dat<-currentmodel$uploaded.data[ind,]
        if(nrow(dat)==0){
          paste("No Data") 
        } else{
          ""
        }
      } else{
        ""
      }
    })
    
    output$Geography.map<-renderLeaflet({
      if (!is.null(currentmodel$uploaded.data) && Geography.Data()==TRUE){
        if(is.null(currentmodel$Map.Filter.Index)){
          dat<-currentmodel$uploaded.data
        } else{
          ind<-currentmodel$Map.Filter.Index
          dat<-currentmodel$uploaded.data[ind,]
        }
        # Removing empty locations
        names(dat)<-tolower(names(dat))
        dat<-dat[complete.cases(dat[ , c('latitude', 'longitude'),with=FALSE]),]
        if(nrow(dat)==0)
          return()
        ## MAKE CONTOUR LINES
        ## Note, bandwidth choice is based on MASS::bandwidth.nrd()
        kde <- bkde2D(dat[ , c('longitude', 'latitude'),with=FALSE],
                      bandwidth=c(.0045, .0068), gridsize = c(100,100))
        
        CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
        
        ## EXTRACT CONTOUR LINE LEVELS
        LEVS <- as.factor(sapply(CL, `[[`, "level"))
        NLEV <- length(levels(LEVS))
        
        ## CONVERT CONTOUR LINES TO POLYGONS
        pgons <- lapply(1:length(CL), function(i)
          Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
        spgons = SpatialPolygons(pgons)
        
        ## Leaflet map with polygons
        leaflet(spgons) %>% addTiles() %>% 
          addPolygons(color = heat.colors(NLEV, NULL)[LEVS])
        
        ## Leaflet map with points and polygons
        ## Note, this shows some problems with the KDE, in my opinion...
        ## For example there seems to be a hot spot at the intersection of Mayfield and
        ## Fillmore, but it's not getting picked up.  Maybe a smaller bw is a good idea?
        
        leaflet(spgons) %>% addTiles() %>%
          addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
          addCircles(lng = dat$longitude, lat = dat$latitude,
                     radius = .5, opacity = .2, col = "blue")
        
      }})  
    #########################End of Geography map#################################################
    #########################################################
    #############chooseAlgorithms#####################
    output$Slow.Algorithms.text<-renderText({
      input$ChooseAlgorithms
      if(any(input$ChooseAlgorithms %in% c("Xgboost","Naive Xgboost","Neural Network","Naive Neural Network","All Models"))){
        if(input$ChooseAlgorithms=="All Models"){
          temp<-paste("Xgboost, Naive Xgboost, Neural Network, Naive Neural Network are considered slow algorithms")
          temp
        } else{
          slow.models<-input$ChooseAlgorithms[input$ChooseAlgorithms %in% c("Xgboost","Naive Xgboost","Neural Network","Naive Neural Network")]
          if(length(slow.models)==1){
            temp<-paste(slow.models,"is considered slow algorithm")
            temp
          } else{
            temp<-paste(paste(slow.models,collapse=", "),"are considered slow algorithms")
            temp
          }
        }
      } else{
        ""
      }
    })
    #############Update chooseAlgorithms choices First Layer#########
    ###In case we change input$Method
    observe({
      input$LoadButton
      isolate({
        if(!is.null(currentmodel$ChooseAlgorithms.for.first.layer)){
          selected<-currentmodel$ChooseAlgorithms.for.first.layer
          currentmodel$ChooseAlgorithms.for.first.layer<-NULL
        } else{
          selected<-"All Models"
        }   
      })
      updateSelectInput(session,"ChooseAlgorithms.for.first.layer",selected = selected)
    })
    #############Update chooseAlgorithms choices############# 
    ###In case we change input$Method
    observe({
      input$LoadButton
      if(is.null(currentmodel$Method.saved))
        return()
      isolate({
        if(!is.null(currentmodel$ChooseAlgorithms)){
          selected<-currentmodel$ChooseAlgorithms
          currentmodel$ChooseAlgorithms<-NULL
        } else{
          selected<-"All Models"
        }   
      })
      if(currentmodel$Method.saved=="Classification"){
        updateSelectInput(session,"ChooseAlgorithms",choices =c("Logistic","Weighted Logistic","Naive Logistic","Naive Weighted Logistic","Xgboost",
                                                                "Naive Xgboost","Recursive Partitioning Tree","Naive Recursive Partitioning Tree","Rforest","Naive Rforest","Neural Network","Naive Neural Network","All Models"),selected = selected) 
      } else{
        updateSelectInput(session,"ChooseAlgorithms",choices =c("Linear","Weighted Linear","Naive Linear","Naive Weighted Linear","Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile","Xgboost",
                                                                "Naive Xgboost","Recursive Partitioning Tree","Naive Recursive Partitioning Tree","Rforest","Naive Rforest","Neural Network","Naive Neural Network","All Models"),selected = selected)
      }
    })
    ##########In case we choose two layers prediction#######
    observe({
      input$Two.layers.prediction
      if(input$Two.layers.prediction=="No")
        updateSelectInput(session,"ChooseAlgorithms",label="Algorithms")
      if(input$Two.layers.prediction=="Yes")
        updateSelectInput(session,"ChooseAlgorithms",label="Algorithms for second layer")
    })
    ##########################################################
    #######################Run All Options####################
    observeEvent(input$MainSubmit.Run.all.options, {
      currentmodel$Run.all.options.time.taken<-NULL
      if(input$Two.layers.prediction=="Yes"){
        Run.all.options.last_action<<-paste("Method not valid with two layers prediction")
        return()
      }
      if (input$path.Run.all.options=="" || !currentmodel$Ready.To.Calculate || is.null(input$ChooseAlgorithms)) {
        if(input$path.Run.all.options==""){
          Run.all.options.last_action<<-paste("Insert File name")
        } else{
          if(is.null(input$ChooseAlgorithms)){
            Run.all.options.last_action<<-paste("No Algorithms were chosen")
          } else{
            if(is.null(currentmodel$uploaded.data) || nrow(currentmodel$uploaded.data)==0){ 
              Run.all.options.last_action<<-paste("No Data")
            } else{
              if(is.null(currentmodel$Target.Vars)){
                Run.all.options.last_action<<-paste("Choose Target Variable")
              } else{
                if(!currentmodel$Fixed.Target){ 
                  if(currentmodel$Method.saved=="Classification") 
                    Run.all.options.last_action<<-paste("Target Variable should have both values 0,1")
                  if(currentmodel$Method.saved=="Estimation") 
                    Run.all.options.last_action<<-paste("Target Variable should be numeric")
                } else{
                  if(length(names(currentmodel$uploaded.data)[!(names(currentmodel$uploaded.data) %in% c(currentmodel$vars.to.exclude,currentmodel$Exclude,currentmodel$Target.Vars))])==0){
                    Run.all.options.last_action<<-paste("You have no Variables in your model")
                  }
                }
              }
            }
          }
        }
      } else{##Start computing  
        start.time <- Sys.time()
        tryCatch({
          Run.all.options.last_action<<-""
          ###We check if we have Recursive Partitioning Tree or Naive Recursive Partitioning Tree
          if(sum(c("All Models","Recursive Partitioning Tree","Naive Recursive Partitioning Tree") %in% input$ChooseAlgorithms)>0){ 
            Rpart.best.parameters<- c("No","Yes")
          } else{
            Rpart.best.parameters<-input$Rpart.best.parameters
          }
          ###We check if we have Xgboost or Naive Xgboost
          if(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms)>0){ 
            Xgb.best.parameters<- c("No","Yes")
            if(currentmodel$Method.saved=="Classification"){
              Xgb.classification.eval.metric<-c("Logloss","Error","AUC")
            } else{
              Xgb.classification.eval.metric<-input$Xgb.classification.eval.metric
            }
          } else{
            Xgb.best.parameters<-input$Xgb.best.parameters
            Xgb.classification.eval.metric<-input$Xgb.classification.eval.metric
          }
          
          ################
          Data <- currentmodel$uploaded.data
          output.var <- currentmodel$Target.Vars
          vars.not.include <- unique(c(currentmodel$vars.to.exclude,currentmodel$Exclude))  
          vars.not.fix<-currentmodel$not.to.fix
          vars.not.fix.outliers<-currentmodel$not.to.fix.outliers
          
          ##1 without na target
          Data<-Data[cleaned.places(Data[[output.var]]),]
          ####
          nrow.SPY<-round(currentmodel$division.rate*nrow(Data))
          nrow.SPY.Test<-nrow(Data)-nrow.SPY
          if(input$AllowSampling=="Yes")
            nrow.SPY<-round(input$percent*nrow.SPY)
          ##
          if(nrow.SPY<10 || nrow.SPY.Test<3){
            Run.all.options.last_action<<-paste("Rows number is too small")
            return()
          } 
          ####Calculate Models Results for each combination of parameters#####
          ##2 Add ratio combination as dummies
          if(!is.null(currentmodel$ratio.comb.table.to.show.saved)){
            table<-currentmodel$ratio.comb.table.to.show.saved
            is.interval<-currentmodel$ratio.comb.info[["is.interval"]]
            char_chosen_for_ratio.as.dummies<-NULL
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
            ratio.comb.data[,"max_of_ratio_comb"]<-apply(ratio.comb.data,1,max)
            Data<-cbind(Data,ratio.comb.data)
          }
          ##3 Turn Factors To numeric
          Names<-names(Data)
          Names.factor<-Names[sapply(Names,function(i){is.factor(Data[[i]])})] 
          if(length(Names.factor)>0){
            temp<-list()
            SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
            for(i in Names.factor){
              temp[[i]]<-fac.levels.into.num(SPY,i,output.var)
              Data[,i]<-turn.fac.to.num(Data[[i]],temp[[i]])
            }
          }
          ##4 specify the not.fix columns
          chosen.not.fix<-Not.fix.columns(Data,output.var,vars.not.include,vars.not.fix,vars.not.fix.outliers)
          ###
          ####Outliers#####
            Check.not.fix.outliers<-c("Default","No","Yes")
            if(length(chosen.not.fix[["not.fix.outliers"]])==length(names(Data)[!(names(Data) %in% c(output.var,vars.not.include))]))
              Check.not.fix.outliers<-c("Default","No")  ##Default==Yes
            if(length(chosen.not.fix[["not.fix.outliers"]])==0)
              Check.not.fix.outliers<-c("Default","Yes")  ##Default==No
            
            if(currentmodel$Method.saved=="Classification"){
              inputs<-list("Imputation.Value"=c("Mean","Median"),
                           "Check.not.fix.outliers"=Check.not.fix.outliers,
                           "ChooseCriterion"=input$ChooseCriterion,#c("Accuracy","Accuracy 0","Precision","F measure","AUC"),
                           "Differential.Matrix"=input$Differential.Matrix,#c("No","Yes"),
                           "RareEvents"=c("No","Undersample","Hybrid"),
                           "Auto.classification.threshold"=c("No","Yes"),
                           "Rpart.best.parameters"=Rpart.best.parameters,
                           "Xgb.best.parameters"= Xgb.best.parameters,
                           "Xgb.classification.eval.metric"=Xgb.classification.eval.metric,
                           "Use.CV"=c("No","Yes"),
                           "Preserve.Accuracy_01"=c("No","Yes"),
                           "Stratified.Shuffle"=c("No","Yes"))
            } else{#Estimation
              inputs<-list("Imputation.Value"=c("Mean","Median"),
                           "Check.not.fix.outliers"=Check.not.fix.outliers,
                           "ChooseCriterion"=input$ChooseCriterion.Estimation,#c("MAE","Norm abs Difference","Abs Sum Difference","RMSE","R2","R2 adj"),
                           "Differential.Matrix"=input$Differential.Matrix,
                           "RareEvents"=input$RareEvents,
                           "Auto.classification.threshold"=input$Auto.classification.threshold,
                           "Rpart.best.parameters"=Rpart.best.parameters,
                           "Xgb.best.parameters"= Xgb.best.parameters,
                           "Xgb.classification.eval.metric"=Xgb.classification.eval.metric,
                           "Use.CV"=c("No","Yes"),
                           "Preserve.Accuracy_01"=input$Preserve.Accuracy_01,
                           "Stratified.Shuffle"=c("No","Yes"))
            }
            ################
            
            ##############
            orig.Data<-Data
            ##########
            trainning.Results<-NULL
            test.Results<-NULL
            comb.num<-0
            for(Check.not.fix.outliers in inputs[["Check.not.fix.outliers"]]){
              for(Imputation.Value in inputs[["Imputation.Value"]]){
                for(ChooseCriterion in inputs[["ChooseCriterion"]]){
                  for(Differential.Matrix in inputs[["Differential.Matrix"]]){
                    for(RareEvents in inputs[["RareEvents"]]){
                      for(Auto.classification.threshold in inputs[["Auto.classification.threshold"]]){
                        for(Rpart.best.parameters in inputs[["Rpart.best.parameters"]]){
                          for(Xgb.best.parameters in inputs[["Xgb.best.parameters"]]){
                            for(Xgb.classification.eval.metric in inputs[["Xgb.classification.eval.metric"]]){
                              for(Use.CV in inputs[["Use.CV"]]){
                                for(Preserve.Accuracy_01 in inputs[["Preserve.Accuracy_01"]]){
                                  for(Stratified.Shuffle in inputs[["Stratified.Shuffle"]]){
                                    tryCatch({
                                      ###
                                      if(Xgb.best.parameters=="No" && length(inputs[["Xgb.classification.eval.metric"]])>1 &&
                                         Xgb.classification.eval.metric!=inputs[["Xgb.classification.eval.metric"]][1])
                                        next
                                      ###
                                      comb.num<-comb.num+1
                                      Data<-orig.Data
                                      ###
                                      temp.chosen.not.fix<-chosen.not.fix
                                      if(Check.not.fix.outliers=="Yes")
                                        temp.chosen.not.fix[["not.fix.outliers"]]<-names(Data)[!(names(Data) %in% c(output.var,vars.not.include))]
                                      if(Check.not.fix.outliers=="No")
                                        temp.chosen.not.fix[["not.fix.outliers"]]<-character(0)
                                      #########################
                                      ###Create is.NA variables
                                      if(input$create.is.NA.vars=="Yes")
                                        Data<-Create.is.na.vars.for.modeling(Data,vars.not.include)[["data"]]
                                      ##Clean the Data
                                      Data<-Fix.Data(Data,output.var,vars.not.include,temp.chosen.not.fix,Imputation.Value,OutliersSettings$Outliers.settings.saved) 
                                      ##Stratified Shuffle
                                      if(Stratified.Shuffle=="Yes"){
                                        Ind<-stratified.shuffle(Data,currentmodel$division.rate,vars.not.include)
                                        Data<-Data[Ind,]
                                      }
                                        
                                      SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
                                      SPY.Test<-Data[(round(currentmodel$division.rate*nrow(Data))+1):nrow(Data),]
                                      ###Sample and shuffle
                                      SPY<-data_sample(SPY,input$percent,input$AllowSampling)    
                                      
                                      ##In case of rare events in classification 
                                      if(currentmodel$Method.saved=="Classification" && Differential.Matrix=="Yes"){ 
                                        tryCatch({
                                          ## Creating tomeklinks and removing the irrelevant datapoints
                                          set.seed(1234)
                                          tomek = ubTomek(SPY[,names(SPY)[!(names(SPY) %in% output.var)],with=FALSE], SPY[[output.var]])
                                          model_train_tomek = cbind(tomek$X,tomek$Y)
                                          names(model_train_tomek)[length(names(model_train_tomek))] <- output.var
                                          SPY<-model_train_tomek
                                        },error=function(err) {})
                                      }
                                      if(currentmodel$Method.saved=="Classification" && RareEvents!="No") 
                                          SPY<-balance(SPY,output.var,RareEvents)

                                      # Create train,test1,test2,test3 datasets
                                      train<-SPY[1:round(nrow(SPY)*(0.7)),]  
                                      test1<-SPY[(round(nrow(SPY)*(0.7))+1):round(nrow(SPY)*0.8),] 
                                      test2<-SPY[(round(nrow(SPY)*(0.8))+1):round(nrow(SPY)*0.9),] 
                                      test3<-SPY[(round(nrow(SPY)*(0.9))+1):nrow(SPY),] 
                                      not.train<-rbind(test1,test2,test3)

                                      if(currentmodel$Method.saved=="Estimation")
                                          train<-delete.rows.with.height.rstandard(train,not.train,output.var,vars.not.include)

                                      ##################################################
                                      if(currentmodel$Method.saved=="Classification"){
                                        ########## Get the user.params #############
                                        if(Auto.classification.threshold=="No"){
                                          classification.threshold <<- input$classification.threshold#0.5 
                                        } else{
                                          tryCatch({
                                            loop.vars<-names(train)[!(names(train) %in% c(output.var, vars.not.include))]
                                            ind<-sapply(1:length(loop.vars),function(x){return(length(unique(train[[loop.vars[x]]]))<2)})
                                            ind<-which(ind==TRUE)
                                            if(length(ind)>0){loop.vars<-loop.vars[-ind]} 
                                            formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
                                            model<-glm(formula,family=binomial('logit'),data=train)
                                            cutoffs <- seq(0.1,0.9,0.1)
                                            accuracy <- NULL
                                            predicted.probs<-predict(model,newdata=not.train,type="response")
                                            for (cutoff in cutoffs){
                                              prediction <- ifelse(predicted.probs >= cutoff, 1, 0) #Predicting for cut-off
                                              names(prediction)<-NULL
                                              accuracy <- c(accuracy,sum(not.train[[output.var]]==prediction,na.rm=TRUE)/length(prediction))
                                            }
                                            ind<-which.max(accuracy)
                                            classification.threshold <<-cutoffs[ind]
                                          }, error=function(err){
                                            classification.threshold<<-0.5
                                          })
                                        }
                                        stability.threshold <<- input$stability.threshold
                                        accuracy.tolerance <<- input$accuracy.tolerance 
                                        cor.threshold<<-0.5
                                      }
                                      if(currentmodel$Method.saved=="Estimation"){
                                        stability.threshold <<- input$stability.threshold
                                        difference.tolerance <<-input$difference.tolerance 
                                      }
                                      
                                      
                                      ### Saving the Settings for showing them later 
                                      if(currentmodel$Method.saved=="Classification")
                                        Settings_Table<-data.frame(
                                          "Imputation Value"=Imputation.Value,
                                          "Use CV"=Use.CV,
                                          "Auto Class"=Auto.classification.threshold,
                                          "Class Threshold"=classification.threshold,
                                          "Accuracy Tolerance"=accuracy.tolerance,
                                          "Stability Threshold"=stability.threshold,
                                          "Criterion"=ChooseCriterion,
                                          "Preserve Acc 0 & 1"=Preserve.Accuracy_01,
                                          "Differential Matrix"=Differential.Matrix,
                                          "Rare Events"=RareEvents,
                                          "Recursive Partitioning Tree best parameters"=Rpart.best.parameters,
                                          "Xgb best parameters"=Xgb.best.parameters,
                                          "Xgb classification eval metric"=Xgb.classification.eval.metric,
                                          "Analysis Method"=input$analysisMethod,
                                          "Unimputed selected"=ifelse(!is.null(currentmodel$not.to.fix),"Yes","No"),
                                          "Outliers selected"=Check.not.fix.outliers,
                                          "Shuffle Data"=Stratified.Shuffle,
                                          check.names=F)
                                      
                                      if(currentmodel$Method.saved=="Estimation")
                                        Settings_Table<-data.frame(
                                          "Imputation Value"=Imputation.Value,
                                          "Use CV"=Use.CV,
                                          "Criterion"=ChooseCriterion,
                                          "Diff Tolerance"=difference.tolerance,
                                          "Stability Threshold"=stability.threshold,
                                          "Recursive Partitioning Tree best parameters"=Rpart.best.parameters,
                                          "Xgb best parameters"=Xgb.best.parameters,
                                          "Analysis Method"=input$analysisMethod,
                                          "Unimputed selected"=ifelse(!is.null(currentmodel$not.to.fix),"Yes","No"),
                                          "Outliers selected"=Check.not.fix.outliers,
                                          "Shuffle Data"=Stratified.Shuffle,
                                          check.names=F)
                                      ###
                                      
                                      if(sum(c("All Models","Weighted Logistic","Naive Weighted Logistic",
                                               "Weighted Linear","Naive Weighted Linear") %in% input$ChooseAlgorithms)>0){
                                        temp.train<-table(train[[output.var]])
                                        temp.spy<-table(SPY[[output.var]])
                                        log.weights<<-tryCatch({
                                          sapply(train[[output.var]],function(i){1/temp.train[paste(i)]})
                                        },error=function(err){
                                          return(rep(1,length(train[[output.var]])))
                                        })
                                        log.weights.spy<<-tryCatch({
                                          sapply(SPY[[output.var]],function(i){1/temp.spy[paste(i)]})
                                        },error=function(err){
                                          return(rep(1,length(SPY[[output.var]])))
                                        })
                                      } 
                                      
                                      if(sum(c("All Models","Recursive Partitioning Tree","Naive Recursive Partitioning Tree") %in% input$ChooseAlgorithms)>0){
                                        if(Rpart.best.parameters=="Yes"){
                                          vars<-names(train)[!(names(train) %in% c(output.var,vars.not.include))]
                                          if(nrow(train)>1000){
                                            set.seed('595')
                                            ind<-sample(1:nrow(train),0.1*nrow(train),replace=FALSE) 
                                            data<-train[ind,]
                                          } else{
                                            data<-train
                                          }
                                          rpart.params<<-tryCatch({
                                            find.best.parameters.for.rpart(data,currentmodel$Method.saved,output.var,vars)
                                          }, error=function(err){
                                            return(rpart.parameters)   
                                          })
                                        } else{  #Rpart.best.parameters==No
                                          rpart.params<<-rpart.parameters#list(minsplit=20,cp=0.001,maxdepth=30)
                                        }
                                      } 
                                      
                                      if(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms)>0){  
                                        if(Xgb.best.parameters=="Yes"){
                                          tryCatch({
                                            vars<-names(train)[!(names(train) %in% c(output.var,vars.not.include))]
                                            if(nrow(train)>1000){
                                              set.seed('595')
                                              ind<-sample(1:nrow(train),0.1*nrow(train),replace=FALSE) 
                                              data<-train[ind,]
                                            } else{
                                              data<-train
                                            }
                                            formula <- as.formula(paste(" ~ ", paste(vars, collapse= "+"),"-1"))
                                            mat<-sparse.model.matrix(formula, data)
                                            dtrain <- xgb.DMatrix(data = mat,label = data[[output.var]]) 
                                            base_score<-xgboost.parameters[["base_score"]]
                                            temp<-find.best.parameters.for.xgb(dtrain,currentmodel$Method.saved,Xgb.classification.eval.metric,classification.threshold,base_score)
                                            xg.params<<-temp[["params"]]
                                            xg.nround<<-temp[["nround"]]
                                          }, error=function(err){
                                            ##objective
                                            if(currentmodel$Method.saved=="Classification"){
                                              obj<-"binary:logistic"
                                            } else{
                                              obj<-"reg:linear"
                                            }
                                            xg.params<<-xgboost.parameters
                                            xg.params[["objective"]]<<-obj
                                            xg.nround<<-xgboost.nround
                                          })
                                        } else{
                                          ##objective
                                          if(currentmodel$Method.saved=="Classification"){
                                            obj<-"binary:logistic"
                                          } else{
                                            obj<-"reg:linear"
                                          }
                                          xg.params<<-xgboost.parameters
                                          xg.params[["objective"]]<<-obj
                                          xg.nround<<-xgboost.nround
                                        }
                                      }
                                      
                                      
                                      SPY.Test1<-SPY.Test[1:round((1/3)*nrow(SPY.Test)),] 
                                      SPY.Test2<-SPY.Test[(round((1/3)*nrow(SPY.Test))+1):round((2/3)*nrow(SPY.Test)),] 
                                      SPY.Test3<-SPY.Test[(round((2/3)*nrow(SPY.Test))+1):nrow(SPY.Test),]
                                      
                                      row.divission<-list(ind1=1:nrow(SPY.Test1),ind2=(nrow(SPY.Test1)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)),
                                                          ind3=(nrow(SPY.Test1)+nrow(SPY.Test2)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)+nrow(SPY.Test3)))
                                      
                                      ####Algorithms
                                      ChooseAlgorithms<-input$ChooseAlgorithms
                                      if("All Models" %in% ChooseAlgorithms){
                                        if(currentmodel$Method.saved=="Classification")
                                          ChooseAlgorithms<-c("Logistic","Weighted Logistic","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network","Naive Logistic","Naive Weighted Logistic","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network")
                                        if(currentmodel$Method.saved=="Estimation")
                                          ChooseAlgorithms<-c("Linear","Weighted Linear","Negative Binomial","Quantile","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network","Naive Linear","Naive Weighted Linear","Naive Negative Binomial","Naive Quantile","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network")
                                      }
                                      if(!all(train[[output.var]]>=0))
                                        ChooseAlgorithms<-ChooseAlgorithms[!ChooseAlgorithms %in% c("Negative Binomial","Naive Negative Binomial")]
                                      
                                      ###
                                      source("Algorithm.r")
                                      P.Var<-find.best.results(Use.CV,SPY,train,test1,test2,test3,output.var,ChooseCriterion,Preserve.Accuracy_01,ChooseAlgorithms,input$analysisMethod,
                                                               vars.not.include,currentmodel$Method.saved)
                                      ####
                                      for(i in names(P.Var[["Results"]])){
                                        tryCatch({
                                          data<-P.Var[["Results"]][[i]]
                                          data<-data[nrow(data),names(data)[!(names(data) %in% "Variables")],drop=FALSE]
                                          data<-cbind(i,data)  
                                          colnames(data)[1]<-"Method"
                                          data<-cbind(comb.num,data)
                                          colnames(data)[1]<-"comb.num"
                                          if(currentmodel$Method.saved=="Estimation"){
                                            data<-cbind(data,Settings_Table)
                                          } else{
                                            myform<-P.Var$Myform[[i]]
                                            loop.vars<-P.Var$Vars[[i]]
                                            model<-P.Var$Models[[i]]
                                            prob<-predict.model(model,myform,SPY,classification.threshold,i,currentmodel$Method.saved)[["prob"]] 
                                            prediction<-ifelse(prob>classification.threshold, 1, 0) 
                                            table<-CV.Table(SPY[[output.var]], prediction)
                                            data<-cbind(data,
                                                        'Act 0 Pred 0'=table[1,1],
                                                        'Act 0 Pred 1'=table[1,2],
                                                        'Act 1 Pred 0'=table[2,1],
                                                        'Act 1 Pred 1'=table[2,2],
                                                        Settings_Table)
                                          }
                                          trainning.Results<-rbind(trainning.Results,data)
                                        },error=function(err){})
                                      }
                                      
                                      #####Estimation
                                      if(currentmodel$Method.saved=="Estimation"){
                                        for(i in names(P.Var[["Results"]])){
                                          tryCatch({
                                            myform<-P.Var$Myform[[i]]
                                            loop.vars<-P.Var$Vars[[i]]
                                            model<-P.Var$Models[[i]]
                                            prediction<-predict.model(model,myform,SPY.Test,classification.threshold,i,currentmodel$Method.saved)[["pred"]] 
                                            model_summary<-get.Results(i,SPY.Test,row.divission,prediction,output.var,loop.vars,ChooseCriterion,currentmodel$Method.saved,classification.threshold) ##11
                                            ##
                                            if(!is.null(model_summary)){
                                              data<-data.frame('Method'=i,'comb num'=comb.num,model_summary,check.names=F)
                                              data<-cbind(data,Settings_Table)
                                              test.Results<-rbind(test.Results,data)
                                            }
                                          },error=function(err){})
                                        }
                                      } else{##Classification
                                        for(i in names(P.Var[["Results"]])){
                                          tryCatch({    
                                            myform<-P.Var$Myform[[i]]
                                            loop.vars<-P.Var$Vars[[i]]
                                            model<-P.Var$Models[[i]]
                                            prob<-predict.model(model,myform,SPY.Test,classification.threshold,i,currentmodel$Method.saved)[["prob"]] 
                                            prediction<-ifelse(prob>classification.threshold, 1, 0) 
                                            table<-CV.Table(SPY.Test[[output.var]], prediction)
                                            ###
                                            model_summary<- get.Results(i,SPY.Test,row.divission, prob,output.var,NA,ChooseCriterion,currentmodel$Method.saved,classification.threshold) 
                                            if(!is.null(model_summary)){
                                              data<-data.frame('Method'=i,'comb num'=comb.num,model_summary,
                                                               'Act 0 Pred 0'=table[1,1],
                                                               'Act 0 Pred 1'=table[1,2],
                                                               'Act 1 Pred 0'=table[2,1],
                                                               'Act 1 Pred 1'=table[2,2],
                                                               check.names=F)
                                              data<-cbind(data,Settings_Table)
                                              test.Results<-rbind(test.Results,data)
                                            }
                                          },error=function(err){})
                                        }
                                      }##End of Classification
                                    }, error=function(err){})
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          tryCatch({  
            ##
            if(!is.null(trainning.Results))
              write_excel_csv(trainning.Results,path = paste0("C://AlgoTraceFolder.export/",input$path.Run.all.options,"_trainning.csv"))
            if(!is.null(test.Results))
              write_excel_csv(test.Results,path = paste0("C://AlgoTraceFolder.export/",input$path.Run.all.options,"_test.csv"))
            ##
            Run.all.options.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
            showModal(modalDialog(
              title = "",
              HTML(paste('Files for trainning and testing Results <br/>', 
                         'were Exported to C://AlgoTraceFolder.export <br/>',
                         'Would you like to proceed?')),
              footer = tagList(
                actionButton("no.run.to.best.option", "No"),
                actionButton("yes.run.to.best.option", "Yes")
              )
            ))
          }, error=function(err){
            Run.all.options.last_action<<-paste("Error occured - Data was not Exported")
          })
          
          yes.run.to.best.option.list<<-list("trainning.Results"=trainning.Results,
                                             "test.Results"=test.Results,
                                             "ChooseCriterion"=ChooseCriterion)
          
          #########    
          end.time <- Sys.time()      
          time.taken <- end.time - start.time
          currentmodel$Run.all.options.time.taken<-time.taken    
        }, error=function(err){
          Run.all.options.last_action<<-paste("Error occured")
        })  
      }##End of computing 
    }) 
    
    

    output$Run.all.options.text<-renderText({
      input$MainSubmit.Run.all.options
      Run.all.options.last_action
    }) 
    
    output$Run.all.options.Time_Taken<-renderText({
      if(!is.null(currentmodel$Run.all.options.time.taken)){
        temp<-unlist(strsplit(format(currentmodel$Run.all.options.time.taken)," "))
        num<-round(as.numeric(temp[1]),3)
        unit<-temp[2]
        paste(num,unit,"for all options computing")
      }
    })
    
    ##########################################################
    ## Running models, creating outputs
    ##########################################################
    
    observeEvent(input$no.run.to.best.option,{
      removeModal()
    })
    
    observeEvent(input$yes.run.to.best.option,{
      removeModal()
      trainning.Results<-yes.run.to.best.option.list[["trainning.Results"]]
      test.Results<-yes.run.to.best.option.list[["test.Results"]]
      ChooseCriterion<-yes.run.to.best.option.list[["ChooseCriterion"]]
      if(currentmodel$Method.saved=="Classification"){
        if(ChooseCriterion %in% c("Accuracy","Accuracy 0","Precision")){
          ###Step 1 (Remove all the rows with accuracy_1 %in% c(0,1))
          if(sum(!test.Results[,"Precision"] %in% c(0,1))>0){
            test.Results<-test.Results[which(!test.Results[,"Precision"] %in% c(0,1)),]
          }
          ###Step 2 order by Accuracy_1  
          test.Results<-test.Results[order(-test.Results[,"Precision"]),]
          ###Step 3 Top min(nrow(test.Results),20)   
          test.Results<-test.Results[1:min(nrow(test.Results),20),]
          ###Step 4 order by Accuracy  
          test.Results<-test.Results[order(-test.Results[,"Accuracy"]),]
          ###Step 5 pick the first row i which achieves that  abs(test[i,acc]/train[i,acc]-1)<=1.25*abs(test[i+1,acc]/train[i+1,acc]-1) 
          best.row<-NULL
          for(i in 1:(nrow(test.Results)-1)){
            tryCatch({
              #####prev
              method.prev<-test.Results[i,"Method"]
              comb.num.prev<-test.Results[i,"comb num"]
              ind.train.prev<-intersect(which(trainning.Results[,"Method"]==method.prev),which(trainning.Results[,"comb num"]==comb.num.prev))
              if(length(ind.train.prev)>1){ind.train.prev<-ind.train.prev[1]}
              ####next
              method.next<-test.Results[i+1,"Method"]
              comb.num.next<-test.Results[i+1,"comb num"]
              ind.train.next<-intersect(which(trainning.Results[,"Method"]==method.next),which(trainning.Results[,"comb num"]==comb.num.next))
              if(length(ind.train.next)>1){ind.train.next<-ind.train.next[1]}
              ######
              if(abs(test.Results[i,"Accuracy"]/trainning.Results[ind.train.prev,"Accuracy"]-1)<=1.25*abs(test.Results[i+1,"Accuracy"]/trainning.Results[ind.train.next,"Accuracy"]-1)){
                best.row<-i
                break
              }
            },error=function(err){})
          }
          if(is.null(best.row))
            best.row<-1
        } else{
          ##Order test.Results
          test.Results<-test.Results[order(-test.Results[,ChooseCriterion]),]  
          best.row<-1
        }
        ###Outliers
        tryCatch({
          if(as.character(test.Results[best.row,"Outliers selected"]=="Yes")){
            currentmodel$Vars.Summary[,"Outliers"]<-TRUE
            currentmodel$not.to.fix.outliers<-currentmodel$Vars.Summary[,"Variable"]
          }
          if(as.character(test.Results[best.row,"Outliers selected"]=="No")){
            currentmodel$Vars.Summary[,"Outliers"]<-FALSE
            currentmodel$not.to.fix.outliers<-character(0)
          }
        },error=function(err){})
        ###
        tryCatch({updateRadioButtons(session, "Imputation.Value", selected=as.character(test.Results[best.row,"Imputation Value"]))},error=function(err){})
        tryCatch({updateRadioButtons(session, "Stratified.Shuffle", selected=as.character(test.Results[best.row,"Shuffle Data"]))},error=function(err){})
        tryCatch({updateRadioButtons(session, "Use.CV", selected=as.character(test.Results[best.row,"Use CV"]))},error=function(err){})
        tryCatch({updateRadioButtons(session,"Auto.classification.threshold",selected=as.character(test.Results[best.row,"Auto Class"]))},error=function(err){})
        tryCatch({classification.threshold<<-test.Results[best.row,"Class Threshold"]},error=function(err){})
        tryCatch({updateRadioButtons(session, "Preserve.Accuracy_01", selected=as.character(test.Results[best.row,"Preserve Acc 0 & 1"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "Differential.Matrix", selected=as.character(test.Results[best.row,"Differential Matrix"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "RareEvents", selected=as.character(test.Results[best.row,"Rare Events"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "Rpart.best.parameters", selected=as.character(test.Results[best.row,"Recursive Partitioning Tree best parameters"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "Xgb.best.parameters", selected=as.character(test.Results[best.row,"Xgb best parameters"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "Xgb.classification.eval.metric", selected=as.character(test.Results[best.row,"Xgb classification eval metric"]))},error=function(err){}) 
        tryCatch({updateSelectInput(session, "ChooseAlgorithms", selected=as.character(test.Results[best.row,"Method"]))},error=function(err){})
      } else{    
        ##Order test.Results based on criterion
        if(ChooseCriterion %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
          test.Results<-test.Results[order(test.Results[,ChooseCriterion]),]  
        } else{
          test.Results<-test.Results[order(-test.Results[,ChooseCriterion]),]  
        }
        ##
        best.row<-1
        ###Outliers
        tryCatch({
          if(as.character(test.Results[best.row,"Outliers selected"]=="Yes")){
            currentmodel$Vars.Summary[,"Outliers"]<-TRUE
            currentmodel$not.to.fix.outliers<-currentmodel$Vars.Summary[,"Variable"]
          }
          if(as.character(test.Results[best.row,"Outliers selected"]=="No")){
            currentmodel$Vars.Summary[,"Outliers"]<-FALSE
            currentmodel$not.to.fix.outliers<-character(0)
          }
        },error=function(err){})
        ##
        tryCatch({updateRadioButtons(session, "Imputation.Value", selected=as.character(test.Results[best.row,"Imputation Value"]))},error=function(err){})
        tryCatch({updateRadioButtons(session, "Use.CV", selected=as.character(test.Results[best.row,"Use CV"]))},error=function(err){})
        tryCatch({updateRadioButtons(session, "Rpart.best.parameters", selected=as.character(test.Results[best.row,"Recursive Partitioning Tree best parameters"]))},error=function(err){}) 
        tryCatch({updateRadioButtons(session, "Xgb.best.parameters", selected=as.character(test.Results[best.row,"Xgb best parameters"]))},error=function(err){}) 
        tryCatch({updateSelectInput(session, "ChooseAlgorithms", selected=as.character(test.Results[best.row,"Method"]))},error=function(err){})
      }
      showModal(modalDialog(
        title = "",
        paste('Optimal Model was identified'),
        footer = tagList(
          actionButton("Continue.run.to.best.option", "Continue")
        )
      ))
    }) 
    
   
    
    observeEvent(input$Continue.run.to.best.option,{
      removeModal()
      AllowComputingAnalysis.logical<<-TRUE
    })
    
    observeEvent(input$AllowComputingAnalysis, {
      AllowComputingAnalysis.logical<<-TRUE
    })
    
    AllowComputingAnalysis.toListen <- reactive({
      input$AllowComputingAnalysis
      input$Continue.run.to.best.option
      AllowComputingAnalysis.logical
      })

    
    
    observeEvent(AllowComputingAnalysis.toListen(), {
      if(!AllowComputingAnalysis.toListen()){
        return()
      } 
      if (!currentmodel$Ready.To.Calculate || is.null(input$ChooseAlgorithms)) {
        if(is.null(input$ChooseAlgorithms)){
          not.allow.computing.text<<-paste("No Algorithms were chosen")
        } else{
        if(is.null(currentmodel$uploaded.data) || nrow(currentmodel$uploaded.data)==0){ 
          not.allow.computing.text<<-paste("No Data")
        } else{
          if(is.null(currentmodel$Target.Vars)){
            not.allow.computing.text<<-paste("Choose Target Variable")
          } else{
            if(!currentmodel$Fixed.Target){ 
              if(currentmodel$Method.saved=="Classification") 
                not.allow.computing.text<<-paste("Target Variable should have both values 0,1")
              if(currentmodel$Method.saved=="Estimation") 
                not.allow.computing.text<<-paste("Target Variable should be numeric")
            } else{
              if(length(names(currentmodel$uploaded.data)[!(names(currentmodel$uploaded.data) %in% c(currentmodel$vars.to.exclude,currentmodel$Exclude,currentmodel$Target.Vars))])==0){
                not.allow.computing.text<<-paste("You have no Variables in your model")
              }
            }
          }
        }
      }
      continue<-FALSE
      } else{
        continue<-TRUE
      }
      ######################################
      ### creating necessary.variables.found
      if (!continue) {
        AllowComputingAnalysis.logical<<-FALSE
        return()
      }
        not.allow.computing.text<<-""
        print("necessary.variables.found")
        #####
        currentmodel$time.taken<-NULL
        currentmodel$Run.all.options.time.taken<-NULL
        currentmodel$Method.used<-NULL
        currentmodel$ratio.as.dummies.inside<-NULL   
        currentmodel$Create.is.na.vars<-NULL
        currentmodel$params.to.assign<-NULL
        currentmodel$levels.table<-NULL
        currentmodel$SPY.fixed<-NULL
        currentmodel$SPY.Test.fixed<-NULL
        currentmodel$WL.params<-NULL
        currentmodel$Rpart.params<-NULL
        currentmodel$Xgb.params<-NULL
        #####First Layer##
        currentmodel$necessary.variables.found.for.first.layer<-NULL 
        currentmodel$Used.Models.for.first.layer<-NULL
        currentmodel$WL.params.for.first.layer<-NULL
        currentmodel$Rpart.params.for.first.layer<-NULL
        currentmodel$Xgb.params.for.first.layer<-NULL
        ##########
        currentmodel$necessary.variables.found<-NULL
        currentmodel$summary_table<-NULL 
        currentmodel$Residuals_table<-NULL
        currentmodel$Settings_Table<-NULL
        currentmodel$Multiple.ROC<-NULL   
        currentmodel$confusion.table<-NULL 
        currentmodel$summary_trainning_table<-NULL       
        currentmodel$summary_trainning_table.est<-NULL       
        currentmodel$Used.Models<-NULL
        currentmodel$Analysis.DataSet<-NULL  
        ####Deciles Accuracy##
        currentmodel$Loaded.Model.for.deciles.accuracy<-NULL    
        currentmodel$Accuracy.deciles.table<-NULL  
        ####Deciles AVG##
        currentmodel$Loaded.Model.for.deciles.avg<-NULL       
        currentmodel$Avg.deciles.table<-NULL  
        #####Formula
        ##MS SQL
        currentmodel$ViewModelForm.ms.sql<-list() 
        currentmodel$Make.ViewModelForm.ms.sql<-FALSE
        ##Oracle
        currentmodel$ViewModelForm.oracle<-list() 
        currentmodel$Make.ViewModelForm.oracle<-FALSE
        #####CV###                                
        currentmodel$Loaded.choose_Method_for_CV<-NULL 
        currentmodel$Keeping.complete.cases.in.target.CV<-NULL
        currentmodel$prediction.err.CV<-NULL 
        currentmodel$Vars.with.diff.distribution.CV<-NULL
        currentmodel$NA.table.CV<-NULL   
        currentmodel$CV.Data<-NULL  
        currentmodel$confusion.table.cv<-NULL  
        currentmodel$summary_table.cv<-NULL  
        currentmodel$summary_table.est.cv<-NULL 
        currentmodel$Multiple.ROC.cv<-NULL
        currentmodel$Residuals.table.cv<-NULL
        ####Deciles Accuracy for CV##
        currentmodel$Loaded.Model.for.deciles.accuracy.cv<-NULL  
        currentmodel$Accuracy.deciles.table.cv<-NULL 
        ####Deciles AVG for CV##
        currentmodel$Loaded.Model.for.deciles.avg.cv<-NULL 
        currentmodel$Avg.deciles.table.cv<-NULL 
        #####Prediction###
        currentmodel$Loaded.choose_Method_for_Pred<-NULL     
        currentmodel$prediction.err.Pred<-NULL 
        currentmodel$Vars.with.diff.distribution.Pred<-NULL
        currentmodel$NA.table.Pred<-NULL  
        currentmodel$Predict.Data<-NULL
        ######
        for(i in names(modelInsights))
          modelInsights[[i]]<-NULL
        ######For Text Analysis####
        if(!is.null(textAnalysis$text.analysis.for.export)){
          textAnalysis$text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export
        } else{
          textAnalysis$text.analysis.for.export.saved<-NULL
        }
        ##########################################################
        ## This function finds out the best model
        ##########################################################
        start.time <- Sys.time()
        # Load the data
        Data <- currentmodel$uploaded.data
        output.var <- currentmodel$Target.Vars
        vars.not.include <- unique(c(currentmodel$vars.to.exclude,currentmodel$Exclude)) 
        vars.not.fix<-currentmodel$not.to.fix
        vars.not.fix.outliers<-currentmodel$not.to.fix.outliers
        ##1 without na target
        Data<-Data[cleaned.places(Data[[output.var]]),] 
        orig.Data<-Data
        ##check Rows number
        nrow.SPY<-round(currentmodel$division.rate*nrow(Data))
        nrow.SPY.Test<-nrow(Data)-nrow.SPY
        if(input$AllowSampling=="Yes")
          nrow.SPY<-round(input$percent*nrow.SPY)
        if(nrow.SPY<10 || nrow.SPY.Test<3){
          not.allow.computing.text<<-paste("Rows number is too small")
          AllowComputingAnalysis.logical<<-FALSE
          return()
        }
        ###############
        ##
        ##2 Add ratio combination as dummies
        if(!is.null(currentmodel$ratio.comb.table.to.show.saved)){
          table<-currentmodel$ratio.comb.table.to.show.saved
          is.interval<-currentmodel$ratio.comb.info[["is.interval"]]
          char_chosen_for_ratio.as.dummies<-NULL
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
          ratio.comb.data[,"max_of_ratio_comb"]<-apply(ratio.comb.data,1,max)
          Data<-cbind(Data,ratio.comb.data)
          currentmodel$ratio.as.dummies.inside<-list(Add="Yes",table=table,is.interval=is.interval,char=char_chosen_for_ratio.as.dummies)
        } else{
          currentmodel$ratio.as.dummies.inside<-list(Add="No")
        }
        ########End of Ratio Combination#####
        
        ##3 Turn Factors To numeric
        Names<-names(Data)
        Names.factor<-Names[sapply(Names,function(i){is.factor(Data[[i]])})] 
        if(length(Names.factor)>0){
          temp<-list()
          SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
          for(i in Names.factor){
            temp[[i]]<-fac.levels.into.num(SPY,i,output.var)  
            Data[,i]<-turn.fac.to.num(Data[[i]],temp[[i]])
          }
          currentmodel$levels.table<-temp
        } else{
          currentmodel$levels.table<-NULL
        }
        #######################################
        ##4 Clean the Data and sample
        chosen.not.fix<-Not.fix.columns(Data,output.var,vars.not.include,vars.not.fix,vars.not.fix.outliers)
        if(input$create.is.NA.vars=="Yes"){
          temp<-Create.is.na.vars.for.modeling(Data,vars.not.include)
          Data<-temp[["data"]]
          currentmodel$Create.is.na.vars<-temp[["vars"]]
        } else{
          currentmodel$Create.is.na.vars<-NULL
        }
        ##Clean the Data
        Data<-Fix.Data(Data,output.var,vars.not.include,chosen.not.fix,input$Imputation.Value,OutliersSettings$Outliers.settings.saved) 
        ##Stratified Shuffle
        if(input$Stratified.Shuffle=="Yes"){
          Ind<-stratified.shuffle(Data,currentmodel$division.rate,vars.not.include)  
          Data<-Data[Ind,]
          orig.Data<-orig.Data[Ind,]
        }
        
        SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
        SPY.Test<-Data[(round(currentmodel$division.rate*nrow(Data))+1):nrow(Data),]
        orig.SPY.Test<-orig.Data[(round(currentmodel$division.rate*nrow(orig.Data))+1):nrow(orig.Data),]
        ###Sample and shuffle   ##
        SPY<-data_sample(SPY,input$percent,input$AllowSampling)  

          ###In caes of two layers prediction
          ##########################################################################################################
          if(input$Two.layers.prediction=="Yes"){
            tryCatch({
            #1
            # Create train,test1,test2,test3 datasets
            train<-SPY[1:round(nrow(SPY)*(0.7)),]  
            test1<-SPY[(round(nrow(SPY)*(0.7))+1):round(nrow(SPY)*0.8),] 
            test2<-SPY[(round(nrow(SPY)*(0.8))+1):round(nrow(SPY)*0.9),] 
            test3<-SPY[(round(nrow(SPY)*(0.9))+1):nrow(SPY),] 
            not.train<-rbind(test1,test2,test3)
            #2
            #############
            train<-delete.rows.with.height.rstandard(train,not.train,output.var,vars.not.include)
            #3
            stability.threshold <<- input$stability.threshold
            difference.tolerance <<-input$difference.tolerance 
            #4
            if(sum(c("All Models","Weighted Linear","Naive Weighted Linear") %in% input$ChooseAlgorithms.for.first.layer)>0){
              temp.train<-table(train[[output.var]])
              temp.spy<-table(SPY[[output.var]])
              log.weights<<-tryCatch({
                sapply(train[[output.var]],function(i){1/temp.train[paste(i)]})
              },error=function(err){
                return(rep(1,length(train[[output.var]])))
              })
              log.weights.spy<<-tryCatch({
                sapply(SPY[[output.var]],function(i){1/temp.spy[paste(i)]})
              },error=function(err){
                return(rep(1,length(SPY[[output.var]])))
              })
              currentmodel$WL.params.for.first.layer<-log.weights.spy
            } else{
              currentmodel$WL.params.for.first.layer<-NULL
            }  
            
            if(sum(c("All Models","Recursive Partitioning Tree","Naive Recursive Partitioning Tree") %in% input$ChooseAlgorithms.for.first.layer)>0){
              rpart.params<<-rpart.parameters
              currentmodel$Rpart.params.for.first.layer<-rpart.params
            } else{
              currentmodel$Rpart.params.for.first.layer<-NULL
            }
            
            
            if(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms.for.first.layer)>0){  
              xg.params<<-xgboost.parameters
              xg.params[["objective"]]<<-"reg:linear"
              xg.nround<<-xgboost.nround
              currentmodel$Xgb.params.for.first.layer<-list(params=xg.params,nround=xg.nround)
            } else{
              currentmodel$Xgb.params.for.first.layer<-NULL
            }
            
            # Create SPY.Test1,SPY.Test2,SPY.Test3 datasets
            SPY.Test1<-SPY.Test[1:round((1/3)*nrow(SPY.Test)),] 
            SPY.Test2<-SPY.Test[(round((1/3)*nrow(SPY.Test))+1):round((2/3)*nrow(SPY.Test)),] 
            SPY.Test3<-SPY.Test[(round((2/3)*nrow(SPY.Test))+1):nrow(SPY.Test),] 
            
            ####Algorithms
            ChooseAlgorithms<-input$ChooseAlgorithms.for.first.layer
            if("All Models" %in% ChooseAlgorithms){
              ChooseAlgorithms<-c("Linear","Weighted Linear","Negative Binomial","Quantile","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network","Naive Linear","Naive Weighted Linear","Naive Negative Binomial","Naive Quantile","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network")
            }
            if(!all(train[[output.var]]>=0))
              ChooseAlgorithms<-ChooseAlgorithms[!ChooseAlgorithms %in% c("Negative Binomial","Naive Negative Binomial")]
            
            ###
            source("Algorithm.r")
            P.Var<-find.best.results("No",SPY,train,test1,test2,test3,output.var,"MAE",input$Preserve.Accuracy_01,ChooseAlgorithms,"Native Variables",
                                     vars.not.include,"Estimation")
            models.used<-names(P.Var$Results[sapply(P.Var$Results,nrow)>0])
            temp<-P.Var$Results[models.used]
            temp.data<-data.frame()
            best.models.used<-NULL

            if(length(models.used)>0){
              for(i in 1:length(temp)){
                temp.data[i,1]<-names(temp)[i]
                table<-temp[[i]]
                temp.data[i,2]<-table[nrow(table),'MAE']
              }
              names(temp.data)<-c("name","criterion")
              temp.data<-temp.data[order(temp.data$criterion),] 
            } #End of length(models.used)>0
            
            ###Best models used
            if(nrow(temp.data)>3){
              best.models.used<-as.character(temp.data$name[1:2])
              if(!is.na(temp.data$criterion[3]) && temp.data$criterion[3]<=1.1*temp.data$criterion[2])
                best.models.used<-c(best.models.used,as.character(temp.data$name[3]))
            }
            
            currentmodel$Used.Models.for.first.layer<-list(models.used=models.used,best.models.used=best.models.used)
            
            currentmodel$necessary.variables.found.for.first.layer<-P.Var
            Progression.Vars<-P.Var
            ######

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
            
            SPY <- Data[1:round(currentmodel$division.rate*nrow(Data)),]
            SPY.Test<-Data[(round(currentmodel$division.rate*nrow(Data))+1):nrow(Data),]
            ##In case of an Error it surly wont be from the last two lines and then SPY and SPY.Test wont be updated
            }, error=function(err){})
          }
          ####End of two layers prediction
         ##5 In case of rare events in classification 
         if(currentmodel$Method.saved=="Classification" && input$Differential.Matrix=="Yes"){ 
           tryCatch({
           ## Creating tomeklinks and removing the irrelevant datapoints
           set.seed(1234)
           tomek = ubTomek(SPY[,names(SPY)[!(names(SPY) %in% output.var)],with=FALSE], SPY[[output.var]])
           model_train_tomek = cbind(tomek$X,tomek$Y)
           names(model_train_tomek)[length(names(model_train_tomek))] <- output.var
           SPY<-model_train_tomek
           },error=function(err) {})
         }
        if(currentmodel$Method.saved=="Classification" && input$RareEvents!="No")
            SPY<-balance(SPY,output.var,input$RareEvents) 
 
        #########################################
          currentmodel$params.to.assign<-list(output.var=output.var,vars.not.include=vars.not.include,
                                              chosen.not.fix=chosen.not.fix)  
        #########################################  
       
        # Create train,test1,test2,test3 datasets
        train<-SPY[1:round(nrow(SPY)*(0.7)),]  
        test1<-SPY[(round(nrow(SPY)*(0.7))+1):round(nrow(SPY)*0.8),] 
        test2<-SPY[(round(nrow(SPY)*(0.8))+1):round(nrow(SPY)*0.9),] 
        test3<-SPY[(round(nrow(SPY)*(0.9))+1):nrow(SPY),] 
        not.train<-rbind(test1,test2,test3)
        #############
        ##6
        if(currentmodel$Method.saved=="Estimation")
          train<-delete.rows.with.height.rstandard(train,not.train,output.var,vars.not.include)
          
          if(currentmodel$Method.saved=="Classification"){
            ########## Get the user.params #############
            if(input$Auto.classification.threshold=="No"){
            classification.threshold <<- input$classification.threshold#0.5 
            } else{
              tryCatch({
              loop.vars<-names(train)[!(names(train) %in% c(output.var, vars.not.include))]
              ind<-sapply(1:length(loop.vars),function(x){return(length(unique(train[[loop.vars[x]]]))<2)})
              ind<-which(ind==TRUE)
              if(length(ind)>0){loop.vars<-loop.vars[-ind]} 
              formula <- as.formula(paste(as.symbol(output.var), " ~ ", paste(loop.vars, collapse= "+")))
              model<-glm(formula,family=binomial('logit'),data=train)
              cutoffs <- seq(0.1,0.9,0.1)
              accuracy <- NULL
              #not.train<-rbind(test1,test2,test3)
              predicted.probs<-predict(model,newdata=not.train,type="response")
              for (cutoff in cutoffs){
                prediction <- ifelse(predicted.probs >= cutoff, 1, 0) #Predicting for cut-off
                names(prediction)<-NULL
                accuracy <- c(accuracy,sum(not.train[[output.var]]==prediction,na.rm=TRUE)/length(prediction))
              }
              ind<-which.max(accuracy)
              classification.threshold <<-cutoffs[ind]
              }, error=function(err){
                classification.threshold<<-0.5
              })
            }
            stability.threshold <<- input$stability.threshold
            accuracy.tolerance <<- input$accuracy.tolerance 
            cor.threshold<<-0.5
          }
          if(currentmodel$Method.saved=="Estimation"){
            stability.threshold <<- input$stability.threshold
            difference.tolerance <<-input$difference.tolerance 
          }
          
          
          ### Saving the Settings for showing them later 
          if(currentmodel$Method.saved=="Classification")
            table<-data.frame(
              "Imputation Value"=input$Imputation.Value,
              "Use CV"=input$Use.CV,
              "Auto Class"=input$Auto.classification.threshold,
              "Class Threshold"=classification.threshold,
              "Accuracy Tolerance"=input$accuracy.tolerance,
              "Stability Threshold"=input$stability.threshold,
              "Criterion"=input$ChooseCriterion,
              "Preserve Acc 0 & 1"=input$Preserve.Accuracy_01,
              "Differential Matrix"=input$Differential.Matrix,
              "Rare Events"=input$RareEvents,
              "Recursive Partitioning Tree best parameters"=input$Rpart.best.parameters,
              "Xgb best parameters"=input$Xgb.best.parameters,
              "Xgb classification eval metric"=input$Xgb.classification.eval.metric,
              "Analysis Method"=input$analysisMethod,
              "Unimputed selected"=ifelse(!is.null(currentmodel$not.to.fix),"Yes","No"),
              "Outliers selected"=ifelse(!is.null(currentmodel$not.to.fix.outliers),"Yes","No"),
              check.names=F)
          
          if(currentmodel$Method.saved=="Estimation")
            table<-data.frame(
              "Imputation Value"=input$Imputation.Value,
              "Use CV"=input$Use.CV,
              "Criterion"=input$ChooseCriterion.Estimation,
              "Diff Tolerance"=input$difference.tolerance,
              "Stability Threshold"=input$stability.threshold,
              "Recursive Partitioning Tree best parameters"=input$Rpart.best.parameters,
              "Xgb best parameters"=input$Xgb.best.parameters,
              "Analysis Method"=input$analysisMethod,
              "Unimputed selected"=ifelse(!is.null(currentmodel$not.to.fix),"Yes","No"),
              "Outliers selected"=ifelse(!is.null(currentmodel$not.to.fix.outliers),"Yes","No"),
              check.names=F)
          
          currentmodel$Settings_Table<-table
          ###
          
          if(sum(c("All Models","Weighted Logistic","Naive Weighted Logistic",
                   "Weighted Linear","Naive Weighted Linear") %in% input$ChooseAlgorithms)>0){
            temp.train<-table(train[[output.var]])
            temp.spy<-table(SPY[[output.var]])
            log.weights<<-tryCatch({
              sapply(train[[output.var]],function(i){1/temp.train[paste(i)]})
            },error=function(err){
              return(rep(1,length(train[[output.var]])))
            })
            log.weights.spy<<-tryCatch({
              sapply(SPY[[output.var]],function(i){1/temp.spy[paste(i)]})
            },error=function(err){
              return(rep(1,length(SPY[[output.var]])))
            })
            currentmodel$WL.params<-log.weights.spy
          } else{
            currentmodel$WL.params<-NULL
          }  
          
          if(sum(c("All Models","Recursive Partitioning Tree","Naive Recursive Partitioning Tree") %in% input$ChooseAlgorithms)>0){
            if(input$Rpart.best.parameters=="Yes"){
              vars<-names(train)[!(names(train) %in% c(output.var,vars.not.include))]
              if(nrow(train)>1000){
                set.seed('595')
                ind<-sample(1:nrow(train),0.1*nrow(train),replace=FALSE) 
                data<-train[ind,]
              } else{
                data<-train
              }
              rpart.params<<-tryCatch({
                find.best.parameters.for.rpart(data,currentmodel$Method.saved,output.var,vars)
              }, error=function(err){
                return(rpart.parameters)  
              })
            } else{  #Rpart.best.parameters==No
              rpart.params<<-rpart.parameters
            }
            currentmodel$Rpart.params<-rpart.params
          } else{
            currentmodel$Rpart.params<-NULL
          }
          
          if(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms)>0){  
            if(input$Xgb.best.parameters=="Yes"){
              tryCatch({
                vars<-names(train)[!(names(train) %in% c(output.var,vars.not.include))]
                if(nrow(train)>1000){
                  set.seed('595')
                  ind<-sample(1:nrow(train),0.1*nrow(train),replace=FALSE) 
                  data<-train[ind,]
                } else{
                  data<-train
                }
                formula <- as.formula(paste(" ~ ", paste(vars, collapse= "+"),"-1"))
                mat<-sparse.model.matrix(formula, data)
                dtrain <- xgb.DMatrix(data = mat,label = data[[output.var]]) 
                base_score<-xgboost.parameters[["base_score"]]
                temp<-find.best.parameters.for.xgb(dtrain,currentmodel$Method.saved,input$Xgb.classification.eval.metric,classification.threshold,base_score)
                xg.params<<-temp[["params"]]
                xg.nround<<-temp[["nround"]]
              }, error=function(err){
                ##objective
                if(currentmodel$Method.saved=="Classification"){
                  obj<-"binary:logistic"
                } else{
                  obj<-"reg:linear"
                }
                xg.params<<-xgboost.parameters
                xg.params[["objective"]]<<-obj
                xg.nround<<-xgboost.nround
              })
            } else{
              ##objective
              if(currentmodel$Method.saved=="Classification"){
                obj<-"binary:logistic"
              } else{
                obj<-"reg:linear"
              }
              xg.params<<-xgboost.parameters
              xg.params[["objective"]]<<-obj
              xg.nround<<-xgboost.nround
            }
            currentmodel$Xgb.params<-list(params=xg.params,nround=xg.nround)
          } else{
            currentmodel$Xgb.params<-NULL
          }
     

          if(currentmodel$Method.saved=="Classification")
            criterion<-input$ChooseCriterion
          if(currentmodel$Method.saved=="Estimation")
            criterion<-input$ChooseCriterion.Estimation
          
          currentmodel$SPY.fixed<-SPY
          currentmodel$Method.used<-currentmodel$Method.saved
          currentmodel$SPY.Test.fixed<-SPY.Test
          
          # Create SPY.Test1,SPY.Test2,SPY.Test3 datasets
          SPY.Test1<-SPY.Test[1:round((1/3)*nrow(SPY.Test)),] 
          SPY.Test2<-SPY.Test[(round((1/3)*nrow(SPY.Test))+1):round((2/3)*nrow(SPY.Test)),] 
          SPY.Test3<-SPY.Test[(round((2/3)*nrow(SPY.Test))+1):nrow(SPY.Test),] 
          

          ####Algorithms
          ChooseAlgorithms<-input$ChooseAlgorithms
          if("All Models" %in% ChooseAlgorithms){
            if(currentmodel$Method.saved=="Classification")
              ChooseAlgorithms<-c("Logistic","Weighted Logistic","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network","Naive Logistic","Naive Weighted Logistic","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network")
            if(currentmodel$Method.saved=="Estimation")
              ChooseAlgorithms<-c("Linear","Weighted Linear","Negative Binomial","Quantile","Xgboost","Recursive Partitioning Tree","Rforest","Neural Network","Naive Linear","Naive Weighted Linear","Naive Negative Binomial","Naive Quantile","Naive Xgboost","Naive Recursive Partitioning Tree","Naive Rforest","Naive Neural Network")
          }
          if(!all(train[[output.var]]>=0))
            ChooseAlgorithms<-ChooseAlgorithms[!ChooseAlgorithms %in% c("Negative Binomial","Naive Negative Binomial")]
          
          ###
        source("Algorithm.r")
          P.Var<-find.best.results(input$Use.CV,SPY,train,test1,test2,test3,output.var,criterion,input$Preserve.Accuracy_01,ChooseAlgorithms,input$analysisMethod,
                                        vars.not.include,currentmodel$Method.saved)
          ##################################
          ###models.used & best.models.used
          models.used<-names(P.Var$Results[sapply(P.Var$Results,nrow)>0])
          temp<-P.Var$Results[models.used]
          temp.data<-data.frame()
          best.models.used<-NULL
          models.used.weights<-NULL
          best.models.used.weights<-NULL
          if(length(models.used)>0){
            if(currentmodel$Method.saved=="Classification"){
              ###The Chosen criterion of each model###
              for(i in 1:length(temp)){
                temp.data[i,1]<-names(temp)[i]
                table<-temp[[i]]
                temp.data[i,2]<-table[nrow(table),input$ChooseCriterion]
              }
              names(temp.data)<-c("name","criterion")
              temp.data<-temp.data[order(-temp.data$criterion),]
              temp.data<-temp.data[!is.na(temp.data$criterion) & temp.data$criterion>0,]
              ##############
              if(nrow(temp.data)>0){
                models.used.weights<-sapply(temp.data$name,function(x){
                  return(temp.data[which(temp.data$name==x),"criterion"])
                })
                names(models.used.weights)<-temp.data$name
              } else{
                models.used.weights<-NULL
              }
            } else{ #Estimation
              for(i in 1:length(temp)){
                temp.data[i,1]<-names(temp)[i]
                table<-temp[[i]]
                temp.data[i,2]<-table[nrow(table),input$ChooseCriterion.Estimation]
              }
              names(temp.data)<-c("name","criterion")
              if(input$ChooseCriterion.Estimation %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){ 
                temp.data<-temp.data[order(temp.data$criterion),] 
              } else{
                temp.data<-temp.data[order(-temp.data$criterion), ] 
              }
            }
          } #End of length(models.used)>0

            ###Best models used
            if(nrow(temp.data)>3){
              if(currentmodel$Method.saved=="Classification"){
                best.models.used<-as.character(temp.data$name[1:2])
                if(!is.na(temp.data$criterion[3]) && temp.data$criterion[3]>=0.9*temp.data$criterion[2])
                  best.models.used<-c(best.models.used,as.character(temp.data$name[3]))
                ###############
                #table.b<-temp.data[temp.data$name %in% best.models.used,]
                best.models.used.weights<-sapply(best.models.used,function(x){
                  return(temp.data[which(temp.data$name==x),"criterion"])
                })
                names(best.models.used.weights)<-best.models.used
              } else{ #Estimation
                best.models.used<-as.character(temp.data$name[1:2])
                if(input$ChooseCriterion.Estimation %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){ 
                  if(!is.na(temp.data$criterion[3]) && temp.data$criterion[3]<=1.1*temp.data$criterion[2])
                    best.models.used<-c(best.models.used,as.character(temp.data$name[3]))
                } else{
                  if(!is.na(temp.data$criterion[3]) && temp.data$criterion[3]>=0.9*temp.data$criterion[2])
                    best.models.used<-c(best.models.used,as.character(temp.data$name[3]))
                }}#End of Estimation
            }
          
          currentmodel$Used.Models<-list(models.used=models.used,best.models.used=best.models.used,
                                         models.used.weights=models.used.weights,best.models.used.weights=best.models.used.weights)
          
        
        currentmodel$necessary.variables.found <- P.Var
        ############## End of finding necessary.variables.found #########################
        Progression.Vars <- currentmodel$necessary.variables.found
        if(is.null(currentmodel$necessary.variables.found)){
          not.allow.computing.text<<-paste("No Algorithms were found")
        } else{
        if(currentmodel$Method.saved=="Estimation"){
          ####### summary_trainning_table #########
          Table<-NULL
          for(i in names(Progression.Vars$Results)){
            tryCatch({
              temp.Table<-Progression.Vars$Results[[i]]
              temp.Table<-temp.Table[nrow(temp.Table),!names(temp.Table) %in% "Variables"]
              if(!is.null(temp.Table))
              Table<-rbind(Table,data.frame("Method"=i,temp.Table,check.names=F))
            }, error=function(err){})
          }
          if(!is.null(Table)){
            if(input$ChooseCriterion.Estimation %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
              Table<-Table[order(Table[,input$ChooseCriterion.Estimation]), ] 
            } else{
              Table<-Table[order(-Table[,input$ChooseCriterion.Estimation]), ] 
            }}
          currentmodel$summary_trainning_table.est <- Table
          #####Creating all the predictions#######
          temp.data<-list()
          temp.data.b<-list()
          all.models<-models.used
          row.divission<-list(ind1=1:nrow(SPY.Test1),ind2=(nrow(SPY.Test1)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)),
                              ind3=(nrow(SPY.Test1)+nrow(SPY.Test2)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)+nrow(SPY.Test3)))

          for(i in models.used){
            tryCatch({
              myform<-Progression.Vars$Myform[[i]]
              loop.vars<-Progression.Vars$Vars[[i]]
              model<-Progression.Vars$Models[[i]]
              prediction<-predict.model(model,myform,SPY.Test,classification.threshold,i,currentmodel$Method.saved)[["pred"]] 
              #For comb models
              temp.data[[i]]<-prediction
              if(i %in% best.models.used)
                temp.data.b[[i]]<-prediction
              orig.SPY.Test[,paste0(gsub(" ",".",i),"_predict_response")]<- prediction
              orig.SPY.Test[,paste0(gsub(" ",".",i),"_predict_interval")]<-predict.interval(prediction)
            }, error=function(err){}) 
          }
          
          if(length(temp.data)>1){
            tryCatch({
            prediction<-apply(as.data.frame(temp.data),1,function(x){median(x,na.rm=TRUE)})
            orig.SPY.Test$Ensemble.all.models_predict_response<-prediction
            orig.SPY.Test$Ensemble.all.models_predict_interval<-predict.interval(prediction)
            all.models<-c(all.models,"Ensemble all models")
            }, error=function(err){})
          }
          if(length(temp.data.b)>1){
            tryCatch({
            prediction<-apply(as.data.frame(temp.data.b),1,function(x){median(x,na.rm=TRUE)})
            orig.SPY.Test$Ensemble.best.models_predict_response<-prediction
            orig.SPY.Test$Ensemble.best.models_predict_interval<-predict.interval(prediction)
            all.models<-c(all.models,"Ensemble best models")
            }, error=function(err){})
          }
          
          ####################Analysis.DataSet###########################
          currentmodel$Analysis.DataSet<-orig.SPY.Test
          ####################Summary Table#############################
          Table<-NULL
          residuals.table<-NULL
          target<-orig.SPY.Test[[output.var]]
          for(i in all.models){
            tryCatch({
              prediction<-orig.SPY.Test[[paste0(gsub(" ",".",i),"_predict_response")]]
              residuals.table<-rbind(residuals.table,data.frame("Model"=rep(i,length(prediction)),"Norm_Residuals"=((target-prediction)-mean(target-prediction,na.rm=TRUE))/sd(target-prediction,na.rm=TRUE),"Target"=target,"Prediction"=prediction))
              loop.vars<-ifelse(i %in% names(Progression.Vars$Vars),Progression.Vars$Vars[[i]],NA)
              model_summary<-get.Results(i,SPY.Test,row.divission,prediction,output.var,loop.vars,input$ChooseCriterion.Estimation,currentmodel$Method.saved,classification.threshold) 
              if(!is.null(model_summary))
              Table<-rbind(Table,data.frame("Method"=i,model_summary,check.names=F))   
            }, error=function(err){})
          }
          if(!is.null(Table)){
          if(input$ChooseCriterion.Estimation %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
          Table<-Table[order(Table[,input$ChooseCriterion.Estimation]), ] 
          } else{
            Table<-Table[order(-Table[,input$ChooseCriterion.Estimation]), ] 
          }}
          currentmodel$Residuals_table <- residuals.table
          currentmodel$summary_table <- Table   
        } #End of Estimatiion
        
          if(currentmodel$Method.saved=="Classification"){
            ####### summary_trainning_table #########
            Table<-NULL
            for(i in names(Progression.Vars$Results)){
              tryCatch({
                temp.Table<-Progression.Vars$Results[[i]]
                temp.Table<-temp.Table[nrow(temp.Table),!names(temp.Table) %in% "Variables"]
                if(!is.null(temp.Table))
                Table<-rbind(Table,data.frame("Method"=i,temp.Table,check.names=F))
              }, error=function(err){})
            }
            if(!is.null(Table))
              Table<-Table[order(-Table[,input$ChooseCriterion]), ]
            currentmodel$summary_trainning_table <- Table
            ##################################################
            #####Creating all the predictions#######
            temp.data<-list()
            temp.data.b<-list()
            all.models<-models.used
            row.divission<-list(ind1=1:nrow(SPY.Test1),ind2=(nrow(SPY.Test1)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)),
                                ind3=(nrow(SPY.Test1)+nrow(SPY.Test2)+1):(nrow(SPY.Test1)+nrow(SPY.Test2)+nrow(SPY.Test3)))
            
            
            for(i in models.used){
              tryCatch({
                myform<-Progression.Vars$Myform[[i]]
                loop.vars<-Progression.Vars$Vars[[i]]
                model<-Progression.Vars$Models[[i]]
                prob<-predict.model(model,myform,SPY.Test,classification.threshold,i,currentmodel$Method.saved)[["prob"]] 
                prediction<-ifelse(prob>classification.threshold, 1, 0) 
                #For comb and weighted comb models
                temp.data[[i]]<-prob
                if(i %in% best.models.used)
                  temp.data.b[[i]]<-prob
                orig.SPY.Test[,paste0(gsub(" ",".",i),"_predict_probability")]<- prob
                orig.SPY.Test[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
              }, error=function(err){})
            }
            
            if(length(temp.data)>1){
              tryCatch({
                prob<-apply(as.data.frame(temp.data),1,combined_prob)
                prediction<-ifelse(prob>classification.threshold, 1, 0) 
                orig.SPY.Test$Ensemble.all.models_predict_probability<-prob
                orig.SPY.Test$Ensemble.all.models_predict_response<-prediction 
                all.models<-c(all.models,"Ensemble all models")
              }, error=function(err){})
              ############################################
              if(!is.null(models.used.weights)){
                tryCatch({
                  prob<-weighted.prob(temp.data,models.used.weights)
                  prediction<-ifelse(prob>classification.threshold, 1, 0) 
                  orig.SPY.Test$Ensemble.all.models.weighting_predict_probability<-prob
                  orig.SPY.Test$Ensemble.all.models.weighting_predict_response<-prediction 
                  all.models<-c(all.models,"Ensemble all models weighting")
                }, error=function(err){})
              }
            }
            if(length(temp.data.b)>1){
              tryCatch({
                prob<-apply(as.data.frame(temp.data.b),1,combined_prob)
                prediction<-ifelse(prob>classification.threshold, 1, 0) 
                orig.SPY.Test$Ensemble.best.models_predict_probability<-prob
                orig.SPY.Test$Ensemble.best.models_predict_response<-prediction   
                all.models<-c(all.models,"Ensemble best models")
              }, error=function(err){})
              ############################################
              tryCatch({
                prob<-weighted.prob(temp.data.b,best.models.used.weights)
                prediction<-ifelse(prob>classification.threshold, 1, 0) 
                orig.SPY.Test$Ensemble.best.models.weighting_predict_probability<-prob
                orig.SPY.Test$Ensemble.best.models.weighting_predict_response<-prediction
                all.models<-c(all.models,"Ensemble best models weighting")
              }, error=function(err){})
            }
            ####################Analysis.DataSet###########################
            currentmodel$Analysis.DataSet<-orig.SPY.Test
            ####### creating Confusion.Table ############
            Table<-NULL
            for(i in all.models){
              tryCatch({
                prob<-orig.SPY.Test[[paste0(gsub(" ",".",i),"_predict_probability")]] 
                prediction<-ifelse(prob>classification.threshold, 1, 0) 
                ##
                model_summary<- get.Results(i,SPY.Test,row.divission, prob,output.var,NA,input$ChooseCriterion,currentmodel$Method.saved,classification.threshold)  
                if(!is.null(model_summary)){
                  model_summary<-data.frame('Method'=i,model_summary,check.names=F)  
                  Table<-rbind(Table,model_summary)
                }
                ##confusion.table
                Conf.table<-CV.Table(orig.SPY.Test[[output.var]], prediction)
                colnames(Conf.table) <- paste("Predicted", colnames(Conf.table), sep=" ")
                rownames(Conf.table) <- paste("Actual", rownames(Conf.table), sep=" ")
                Conf.table.percent<-sapply(1:ncol(Conf.table),function(x){return(round(Conf.table[,x]/sum(Conf.table[,x]),round.digits))})
                colnames(Conf.table.percent)<-colnames(Conf.table)
                rownames(Conf.table.percent)<-c("Upper Table in %",rep(" ",(nrow(Conf.table.percent)-1)))
                currentmodel$confusion.table[[i]]<-rbind(Conf.table,Conf.table.percent)
              }, error=function(err){})
            }
            
            if(!is.null(Table)){
              Table<-Table[order(-Table[,input$ChooseCriterion]), ]   
              Table<-as.data.frame(Table)
            }
            ####### creating summary_table ##########
            currentmodel$summary_table <- Table
            ############################# Charts ##############################
            
            ### creating data for ROC.Curve
            currentmodel$Multiple.ROC<-data.frame(target=orig.SPY.Test[[output.var]],check.names = FALSE)    
            for(i in all.models){
              tryCatch({
                currentmodel$Multiple.ROC[,i]<-orig.SPY.Test[[paste0(gsub(" ",".",i),"_predict_probability")]]  
              }, error=function(err){})
            }
          } #End of Classification
      } #End of if(!is.null(currentmodel$necessary.variables.found))
        print("end of calculation") 
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        currentmodel$time.taken<-time.taken
        print("time taken")
        print(time.taken)

      AllowComputingAnalysis.logical<<-FALSE
    })
    ###############Memory Size Models###########
    output$Memory_Size_Models<-renderUI({
      if(!is.null(currentmodel$necessary.variables.found)){
        HTML(paste('Models size in RAM:<br/>',
                   format(object.size(currentmodel$necessary.variables.found), units = 'Mb')))
      } else{
        ""
      }
    })   
    ###############Model Formula################
    At_least_one_model_exist<-reactive({    
      if(!is.null(currentmodel$Used.Models)){
        models.used<-currentmodel$Used.Models[["models.used"]]
        models.names<-models.used[!models.used %in% c("Xgboost","Naive Xgboost","Rforest","Naive Rforest","Neural Network","Naive Neural Network")]
        return(length(models.names)>0)
      } else{
        return(FALSE)
      }
    })
    
    observe({  
      if(!At_least_one_model_exist() || input$Prediction_Analysis!="View Model" || input$Choose.formula.lang!="SQL")
        return()
      if(isolate(currentmodel$Make.ViewModelForm.ms.sql)|| length(isolate(currentmodel$ViewModelForm.ms.sql))>0)
        return()   
      if(!is.null(textAnalysis$text.analysis.for.export.saved) ||
         currentmodel$ratio.as.dummies.inside[["Add"]]=="Yes" ||
         !is.null(currentmodel$necessary.variables.found.for.first.layer))
        return()
      showModal(modalDialog(
        title = "Important message",
        HTML(paste('SQL Formulas Calculation will take few seconds')),
        footer = tagList(
          actionButton("no.calculate.formula.ms.sql", "Exit"),
          actionButton("yes.calculate.formula.ms.sql", "Continue")
        )
      ))
    })

    observeEvent(input$no.calculate.formula.ms.sql,{
      removeModal()
      currentmodel$Make.ViewModelForm.ms.sql<-FALSE
    })
    
    observeEvent(input$yes.calculate.formula.ms.sql,{
      removeModal()
      models.used<-currentmodel$Used.Models[["models.used"]]
      best.models.used<-currentmodel$Used.Models[["best.models.used"]]
      ####
      Progression.Vars<-currentmodel$necessary.variables.found
      method<-currentmodel$Method.saved
      Imputation.Value<-currentmodel$Settings_Table[,"Imputation Value"]   
      SPY<-currentmodel$SPY.fixed
      chosen.not.fix<-currentmodel$params.to.assign[["chosen.not.fix"]]
      levels.table<-currentmodel$levels.table    
      ####### creating model formula ##################
      #######
      if ("Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Logistic"]]
          vars<-Progression.Vars$Vars[["Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Logistic"]]<-NULL
      }
      #######
      if ("Linear" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Linear"]]
          vars<-Progression.Vars$Vars[["Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Linear"]]<-NULL
      }
      #######
      if ("Naive Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Logistic"]]
          vars<-Progression.Vars$Vars[["Naive Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]]<-NULL
      }
      #######
      if ("Naive Linear" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Linear"]]
          vars<-Progression.Vars$Vars[["Naive Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Linear"]]<-NULL
      }
      #######
      if ("Weighted Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Weighted Logistic"]]
          vars<-Progression.Vars$Vars[["Weighted Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]]<-NULL
      }
      #######
      if ("Weighted Linear" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Weighted Linear"]]
          vars<-Progression.Vars$Vars[["Weighted Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]]<-NULL
      }
      #######
      if ("Naive Weighted Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Weighted Logistic"]]
          vars<-Progression.Vars$Vars[["Naive Weighted Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]]<-NULL
      }
      #######
      if ("Naive Weighted Linear" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Weighted Linear"]]
          vars<-Progression.Vars$Vars[["Naive Weighted Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]]<-NULL
      }
      #######
      if ("Negative Binomial" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]]<-tryCatch({
          model<-Progression.Vars$Models[["Negative Binomial"]]
          vars<-Progression.Vars$Vars[["Negative Binomial"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Negative.Binomial(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                      vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]]<-NULL
      }
      #######
      if ("Naive Negative Binomial" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Negative Binomial"]]
          vars<-Progression.Vars$Vars[["Naive Negative Binomial"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Negative.Binomial(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                      vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]]<-NULL
      }
      #######
      if ("Quantile" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Quantile"]]<-tryCatch({
          model<-Progression.Vars$Models[["Quantile"]]
          vars<-Progression.Vars$Vars[["Quantile"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Quantile"]]<-NULL
      }
      #######
      if ("Naive Quantile" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Quantile"]]
          vars<-Progression.Vars$Vars[["Naive Quantile"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          sql.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]]<-NULL
      }
      
      ###Temporary###
      currentmodel$ViewModelForm.ms.sql[["Xgboost"]]<-NULL
      currentmodel$ViewModelForm.ms.sql[["Naive Xgboost"]]<-NULL
      
      #######
      if ("Recursive Partitioning Tree" %in% models.used) {
        currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]]<-tryCatch({
          model<-Progression.Vars$Models[["Recursive Partitioning Tree"]]
          vars<-Progression.Vars$Vars[["Recursive Partitioning Tree"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          
          old.rules<-sql_parse_tree(SPY,model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                vars_not.fix=vars_not.fix,levels.table=levels.table,Imputation.Value)
          new.rules<-replace(old.rules,model,method)
          new.rules
        }, error=function(err){
          return(NULL)   
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]]<-NULL
      }
      #######
      if ("Naive Recursive Partitioning Tree" %in% models.used) { 
        currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Recursive Partitioning Tree"]]
          vars<-Progression.Vars$Vars[["Naive Recursive Partitioning Tree"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          
          old.rules<-sql_parse_tree(SPY,model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                vars_not.fix=vars_not.fix,levels.table=levels.table,Imputation.Value)
          new.rules<-replace(old.rules,model,method)
          new.rules
        }, error=function(err){
          return(NULL)   
        })
      } else{
        currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]]<-NULL
      }
      
      ###Random Forest###
      currentmodel$ViewModelForm.ms.sql[["Rforest"]]<-NULL
      currentmodel$ViewModelForm.ms.sql[["Naive Rforest"]]<-NULL
      
      ###Neural Network###
      currentmodel$ViewModelForm.ms.sql[["Neural Network"]]<-NULL
      currentmodel$ViewModelForm.ms.sql[["Naive Neural Network"]]<-NULL
      
      ###Ensemble.all.models
      if(method=="Estimation"){
        if(length(models.used)<=1){
          currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]]<-NULL
        } else{
          if(sum(sapply(models.used,function(x){is.null(currentmodel$ViewModelForm.ms.sql[[x]])}))==0){
            currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]] <-tryCatch({
              string<-character(0)
              string<-c(string,paste("IF EXISTS (SELECT * FROM sysobjects WHERE id = object_id(N'[dbo].[comb_table]')\n",
                                     "AND OBJECTPROPERTY(id, N'IsUserTable') = 1)\n",
                                     "DROP TABLE [dbo].[comb_table];\n",
                                     "CREATE TABLE comb_table (\n",
                                     "id_comb float,\n",
                                     "pred_L float,\n",
                                     "pred_N float,\n",
                                     "pred_WL float,\n",
                                     "pred_WN float,\n",
                                     "pred_Q float,\n",
                                     "pred_NQ float,\n",
                                     "pred_NB float,\n",
                                     "pred_N_NB float,\n",
                                     "pred_R float,\n",
                                     "pred_N_R float\n",
                                     ")\n\n"))
              #add id column to the source_table
              string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n"))
              string<-c(string,paste("ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                     "Go\n"))
              #####
              string<-c(string,paste("INSERT INTO comb_table (id_comb)\n",
                                     "SELECT id_algotrace\n",
                                     "FROM source_table\n"))
              ####Model L
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Linear"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_L=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Linear"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model WL
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WL=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model WN
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WN=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model Q
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Quantile"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Quantile"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_Q=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model NQ
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_NQ=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model NB
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_NB=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N.NB
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_NB=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_R=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N.R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_R=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              
              string<-c(string,paste(
                "IF Object_ID('dbo.temp_med') IS NOT NULL\n",
                "DROP Table dbo.temp_med;\n",
                "GO\n",
                "CREATE TABLE temp_med (\n",
                "id int,\n",
                "val float\n",
                ")\n",
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_L FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_N FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_WL FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_WN FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_Q FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_NQ FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_NB FROM comb_table\n", 
                "INSERT INTO temp_med\n",
                "select id_comb, pred_N_NB FROM comb_table\n", 
                "INSERT INTO temp_med\n",
                "select id_comb, pred_R FROM comb_table\n",
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_N_R FROM comb_table\n"                      
              ))
              ####comb column
              string<-c(string,paste(
                "IF Object_ID('dbo.med_view') IS NOT NULL\n",
                "DROP VIEW dbo.med_view;\n",
                "GO\n",
                "create view med_view as\n", 
                "(SELECT comb_table.id_comb,\n",
                "(\n",
                "(SELECT MAX(val) FROM\n",
                "(SELECT TOP 50 PERCENT val FROM temp_med where comb_table.id_comb = temp_med.id and val is not NULL ORDER BY val) AS BottomHalf)\n", 
                "+\n",
                "(SELECT MIN(val) FROM\n",
                "(SELECT TOP 50 PERCENT val FROM temp_med where comb_table.id_comb = temp_med.id and val is not NULL ORDER BY val DESC) AS TopHalf)\n", 
                ") / 2 AS Median\n",
                "from comb_table)\n", 
                "go\n\n",
                
                "update source_table\n",
                "set source_table.prediction_algotrace=med_view.Median\n",
                "from source_table\n",
                "inner join med_view on\n",
                "source_table.id_algotrace=med_view.id_comb\n"))
              string
            }, error=function(err){
              return(NULL) 
            })
          } else{
            currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]] <-NULL 
          }
        }#End of if(length(models.used)>1 && xgboost and naive xgboost are not in models.used)
        
        if(length(best.models.used)<=1){
          currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]]<-NULL
        } else{
          ###Ensemble.best.models 
          if(sum(sapply(best.models.used,function(x){is.null(currentmodel$ViewModelForm.ms.sql[[x]])}))==0){
            currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]]<-tryCatch({
              string<-character(0)
              string<-c(string,paste("IF EXISTS (SELECT * FROM sysobjects WHERE id = object_id(N'[dbo].[comb_table]')\n",
                                     "AND OBJECTPROPERTY(id, N'IsUserTable') = 1)\n",
                                     "DROP TABLE [dbo].[comb_table];\n",
                                     "CREATE TABLE comb_table (\n",
                                     "id_comb float,\n",
                                     "pred_L float,\n",
                                     "pred_N float,\n",
                                     "pred_WL float,\n",
                                     "pred_WN float,\n",
                                     "pred_Q float,\n",
                                     "pred_NQ float,\n",
                                     "pred_NB float,\n",
                                     "pred_N_NB float,\n",
                                     "pred_R float,\n",
                                     "pred_N_R float\n",
                                     ")\n\n"))
              #add id column to the source_table
              string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n"))
              string<-c(string,paste("ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                     "Go\n"))
              #####
              string<-c(string,paste("INSERT INTO comb_table (id_comb)\n",
                                     "SELECT id_algotrace\n",
                                     "FROM source_table\n"))
              ####Model L
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Linear"]]) && ("Linear" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_L=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Linear"]]) && ("Naive Linear" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model WL
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]]) && ("Weighted Linear" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Weighted Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WL=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model WN
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]]) && ("Naive Weighted Linear" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Linear"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WN=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model Q
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Quantile"]]) && ("Quantile" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Quantile"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_Q=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model NQ
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]]) && ("Naive Quantile" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Quantile"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_NQ=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model NB
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]]) && ("Negative Binomial" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Negative Binomial"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_NB=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N.NB
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]]) && ("Naive Negative Binomial" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Negative Binomial"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_NB=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]]) && ("Recursive Partitioning Tree" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_R=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              ####Model N.R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]]) && ("Naive Recursive Partitioning Tree" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_R=(select prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where source_table.id_algotrace=comb_table.id_comb)\n"))
              }
              
              string<-c(string,paste(
                "IF Object_ID('dbo.temp_med') IS NOT NULL\n",
                "DROP Table dbo.temp_med;\n",
                "GO\n",
                "CREATE TABLE temp_med (\n",
                "id int,\n",
                "val float\n",
                ")\n",
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_L FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_N FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_WL FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_WN FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_Q FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_NQ FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_NB FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_N_NB FROM comb_table\n", 
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_R FROM comb_table\n",
                "INSERT INTO temp_med\n", 
                "select id_comb, pred_N_R FROM comb_table\n"                      
              ))
              ####comb column
              string<-c(string,paste(
                "IF Object_ID('dbo.med_view') IS NOT NULL\n",
                "DROP VIEW dbo.med_view;\n",
                "GO\n",
                "create view med_view as\n", 
                "(SELECT comb_table.id_comb,\n",
                "(\n",
                "(SELECT MAX(val) FROM\n",
                "(SELECT TOP 50 PERCENT val FROM temp_med where comb_table.id_comb = temp_med.id and val is not NULL ORDER BY val) AS BottomHalf)\n", 
                "+\n",
                "(SELECT MIN(val) FROM\n",
                "(SELECT TOP 50 PERCENT val FROM temp_med where comb_table.id_comb = temp_med.id and val is not NULL ORDER BY val DESC) AS TopHalf)\n", 
                ") / 2 AS Median\n",
                "from comb_table)\n", 
                "go\n\n",
                
                "update source_table\n",
                "set source_table.prediction_algotrace=med_view.Median\n",
                "from source_table\n",
                "inner join med_view on\n",
                "source_table.id_algotrace=med_view.id_comb\n"))
              string
            }, error=function(err){
              return(NULL) 
            })
          } else{
            currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]] <-NULL 
          }
        }#End of if(length(best.models.used)>1 && xgboost and naive xgboost are not in best.models.used)
      } else{#Classification
        if(length(models.used)<=1){
          currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]]<-NULL
        } else{
          if(sum(sapply(models.used,function(x){is.null(currentmodel$ViewModelForm.ms.sql[[x]])}))==0){
            currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]]<-tryCatch({
              string<-character(0)
              string<-c(string,paste("IF EXISTS (SELECT * FROM sysobjects WHERE id = object_id(N'[dbo].[comb_table]')\n",
                                     "AND OBJECTPROPERTY(id, N'IsUserTable') = 1)\n",
                                     "DROP TABLE [dbo].[comb_table];\n",
                                     "CREATE TABLE comb_table (\n",
                                     "id_comb float,\n",
                                     "pred_L float,\n",
                                     "pred_N float,\n",
                                     "pred_WL float,\n",
                                     "pred_WN float,\n",
                                     "pred_R float,\n",
                                     "pred_N_R float\n",
                                     ")\n\n"))
              #add id column to the source_table
              string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n"))
              string<-c(string,paste("ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                     "Go\n"))
              #####
              string<-c(string,paste("INSERT INTO comb_table (id_comb)\n",
                                     "SELECT id_algotrace\n",
                                     "FROM source_table\n"))
              ####Model L
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Logistic"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Logistic"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_L=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model N
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model WL
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WL=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model WN
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WN=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_R=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model N.R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]],"\n\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_R=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####comb column
              string<-c(string,paste(
                "UPDATE source_table\n",
                "SET prediction_algotrace= case\n",
                "when\n",
                "(iif(pred_L is NULL,0,1)+\n",
                "iif(pred_N is NULL,0,1)+\n",
                "iif(pred_WL is NULL,0,1)+\n",
                "iif(pred_WN is NULL,0,1)+\n",
                "iif(pred_R is NULL,0,1)+\n",
                "iif(pred_N_R is NULL,0,1))=0 then NULL\n",
                "else\n",
                "(iif(pred_L is NULL,0,pred_L)+\n",
                "iif(pred_N is NULL,0,pred_N)+\n",
                "iif(pred_WL is NULL,0,pred_WL)+\n",
                "iif(pred_WN is NULL,0,pred_WN)+\n",
                "iif(pred_R is NULL,0,pred_R)+\n",
                "iif(pred_N_R is NULL,0,pred_N_R))/\n",
                "(iif(pred_L is NULL,0,1)+\n",
                "iif(pred_N is NULL,0,1)+\n",
                "iif(pred_WL is NULL,0,1)+\n",
                "iif(pred_WN is NULL,0,1)+\n",
                "iif(pred_R is NULL,0,1)+\n",
                "iif(pred_N_R is NULL,0,1))\n",
                "end\n",
                "from comb_table\n",
                "Where id_algotrace=id_comb"))
              string
            }, error=function(err){
              return(NULL) 
            })
          } else{
            currentmodel$ViewModelForm.ms.sql[["Ensemble all models"]] <-NULL 
          } 
        }#End of if(length(models.used)>1 && xgboost and naive xgboost are not in models.used)
        
        ###Ensemble.best.models
        if(length(best.models.used)<=1){
          currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]]<-NULL
        } else{
          if(sum(sapply(best.models.used,function(x){is.null(currentmodel$ViewModelForm.ms.sql[[x]])}))==0){
            currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]]<-tryCatch({
              string<-character(0)
              string<-c(string,paste("IF EXISTS (SELECT * FROM sysobjects WHERE id = object_id(N'[dbo].[comb_table]')\n",
                                     "AND OBJECTPROPERTY(id, N'IsUserTable') = 1)\n",
                                     "DROP TABLE [dbo].[comb_table];\n",
                                     "CREATE TABLE comb_table (\n",
                                     "id_comb float,\n",
                                     "pred_L float,\n",
                                     "pred_N float,\n",
                                     "pred_WL float,\n",
                                     "pred_WN float,\n",
                                     "pred_R float,\n",
                                     "pred_N_R float\n",
                                     ")\n\n"))
              #add id column to the source_table
              string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n"))
              string<-c(string,paste("ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                     "Go\n"))
              #####
              string<-c(string,paste("INSERT INTO comb_table (id_comb)\n",
                                     "SELECT id_algotrace\n",
                                     "FROM source_table\n"))
              ####Model L
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Logistic"]]) && ("Logistic" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Logistic"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_L=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model N
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]]) && ("Naive Logistic" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Logistic"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model WL
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]]) && ("Weighted Logistic" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Weighted Logistic"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WL=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model WN
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]]) && ("Naive Weighted Logistic" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Weighted Logistic"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_WN=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]]) && ("Recursive Partitioning Tree" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Recursive Partitioning Tree"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_R=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####Model N.R
              if(!is.null(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]]) && ("Naive Recursive Partitioning Tree" %in% currentmodel$Used.Models[["best.models.used"]])){
                string<-c(string,paste(currentmodel$ViewModelForm.ms.sql[["Naive Recursive Partitioning Tree"]],"\n"))
                string<-c(string,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                                       "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                                       "Go\n"))
                string<-c(string,paste(
                  "UPDATE comb_table\n",
                  "SET  pred_N_R=prediction_algotrace\n",
                  "FROM source_table\n",
                  "Where id_comb=id_algotrace\n"))
              }
              ####comb column
              string<-c(string,paste(
                "UPDATE source_table\n",
                "SET prediction_algotrace= case\n",
                "when\n",
                "(iif(pred_L is NULL,0,1)+\n",
                "iif(pred_N is NULL,0,1)+\n",
                "iif(pred_WL is NULL,0,1)+\n",
                "iif(pred_WN is NULL,0,1)+\n",
                "iif(pred_R is NULL,0,1)+\n",
                "iif(pred_N_R is NULL,0,1))=0 then NULL\n",
                "else\n",
                "(iif(pred_L is NULL,0,pred_L)+\n",
                "iif(pred_N is NULL,0,pred_N)+\n",
                "iif(pred_WL is NULL,0,pred_WL)+\n",
                "iif(pred_WN is NULL,0,pred_WN)+\n",
                "iif(pred_R is NULL,0,pred_R)+\n",
                "iif(pred_N_R is NULL,0,pred_N_R))/\n",
                "(iif(pred_L is NULL,0,1)+\n",
                "iif(pred_N is NULL,0,1)+\n",
                "iif(pred_WL is NULL,0,1)+\n",
                "iif(pred_WN is NULL,0,1)+\n",
                "iif(pred_R is NULL,0,1)+\n",
                "iif(pred_N_R is NULL,0,1))\n",
                "end\n",
                "from comb_table\n",
                "Where id_algotrace=id_comb"))
              string
            }, error=function(err){
              return(NULL) 
            })
          } else{
            currentmodel$ViewModelForm.ms.sql[["Ensemble best models"]] <-NULL 
          }
        }#End of if(length(best.models.used)>1 && xgboost and naive xgboost are not in best.models.used)
      }
    })
    

    #####Show the View Model page if you have at least one formula####
    output$At_least_one_formula_exist_ms_sql<-reactive({
      return(length(currentmodel$ViewModelForm.ms.sql)>0)
    })
    outputOptions(output, 'At_least_one_formula_exist_ms_sql', suspendWhenHidden=FALSE)
    
    ######### Model formula Outputs
    ####Choose Model 
    output$Model.for.Formula.ui.ms.sql<-renderUI({
      if(length(currentmodel$ViewModelForm.ms.sql)>0){
        models.names<-names(currentmodel$ViewModelForm.ms.sql)
        selectInput("Model.for.Formula.ms.sql",label="Model",choices=models.names)
      }
    })
    
    output$Show.Formula.ms.sql<- renderText({ 
      input$AllowComputingAnalysis
      input$MainSubmit.Model.for.Formula.ms.sql
      if(!is.null(isolate(input$Model.for.Formula.ms.sql)) && !is.null(isolate(currentmodel$ViewModelForm.ms.sql[[input$Model.for.Formula.ms.sql]]))){
        isolate(currentmodel$ViewModelForm.ms.sql[[input$Model.for.Formula.ms.sql]])
      } else{
        ""
      }
    })
    
    ####Export Formula
    observeEvent(input$MainSubmit.Formula.export.ms.sql,{
      if(!is.null(currentmodel$ViewModelForm.ms.sql[[input$Model.for.Formula.ms.sql]])){
        if(input$path.Formula.export.ms.sql!=""){
          tryCatch({
            formula<-currentmodel$ViewModelForm.ms.sql[[input$Model.for.Formula.ms.sql]]
            write.table(data.frame(formula),file = paste0("C://AlgoTraceFolder.export/",input$path.Formula.export.ms.sql,".sql"), row.names=F, col.names=F, quote = FALSE)
            ViewModelForm.ms.sql.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            ViewModelForm.ms.sql.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          ViewModelForm.ms.sql.last_action<<-paste("Insert File name")
        }
      } else{
        ViewModelForm.ms.sql.last_action<<-paste("No Data to export")
      }
    })
    output$Formula.export.text.ms.sql<-renderText({
      input$MainSubmit.Formula.export.ms.sql
      ViewModelForm.ms.sql.last_action
    })
    
    #####End of MS SQL Formula#####
    
    ####Oracle Formula#####
    observe({  
      if(!At_least_one_model_exist() || input$Prediction_Analysis!="View Model" || input$Choose.formula.lang!="Oracle")
        return()
      if(isolate(currentmodel$Make.ViewModelForm.oracle)|| length(isolate(currentmodel$ViewModelForm.oracle))>0)
        return()   
      if(!is.null(textAnalysis$text.analysis.for.export.saved) ||
         currentmodel$ratio.as.dummies.inside[["Add"]]=="Yes" ||
         !is.null(currentmodel$necessary.variables.found.for.first.layer))
        return()
      showModal(modalDialog(
        title = "Important message",
        HTML(paste('Oracle Formulas Calculation will take few seconds')),
        footer = tagList(
          actionButton("no.calculate.formula.oracle", "Exit"),
          actionButton("yes.calculate.formula.oracle", "Continue")
        )
      ))
    })
    
    observeEvent(input$no.calculate.formula.oracle,{
      removeModal()
      currentmodel$Make.ViewModelForm.oracle<-FALSE
    })
    
    observeEvent(input$yes.calculate.formula.oracle,{
      removeModal()
      models.used<-currentmodel$Used.Models[["models.used"]]
      best.models.used<-currentmodel$Used.Models[["best.models.used"]]
      ####
      Progression.Vars<-currentmodel$necessary.variables.found
      method<-currentmodel$Method.saved
      Imputation.Value<-currentmodel$Settings_Table[,"Imputation Value"]   
      SPY<-currentmodel$SPY.fixed
      chosen.not.fix<-currentmodel$params.to.assign[["chosen.not.fix"]]
      levels.table<-currentmodel$levels.table    
      ####### creating model formula ##################
      #######
      if ("Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Logistic"]]
          vars<-Progression.Vars$Vars[["Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Logistic"]]<-NULL
      }
      #######
      if ("Linear" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Linear"]]
          vars<-Progression.Vars$Vars[["Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Linear"]]<-NULL
      }
      #######
      if ("Naive Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Logistic"]]
          vars<-Progression.Vars$Vars[["Naive Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Logistic"]]<-NULL
      }
      #######
      if ("Naive Linear" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Linear"]]
          vars<-Progression.Vars$Vars[["Naive Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Linear"]]<-NULL
      }
      #######
      if ("Weighted Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Weighted Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Weighted Logistic"]]
          vars<-Progression.Vars$Vars[["Weighted Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Weighted Logistic"]]<-NULL
      }
      #######
      if ("Weighted Linear" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Weighted Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Weighted Linear"]]
          vars<-Progression.Vars$Vars[["Weighted Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Weighted Linear"]]<-NULL
      }
      #######
      if ("Naive Weighted Logistic" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Weighted Logistic"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Weighted Logistic"]]
          vars<-Progression.Vars$Vars[["Naive Weighted Logistic"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Weighted Logistic"]]<-NULL
      }
      #######
      if ("Naive Weighted Linear" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Weighted Linear"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Weighted Linear"]]
          vars<-Progression.Vars$Vars[["Naive Weighted Linear"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Weighted Linear"]]<-NULL
      }
      #######
      if ("Negative Binomial" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Negative Binomial"]]<-tryCatch({
          model<-Progression.Vars$Models[["Negative Binomial"]]
          vars<-Progression.Vars$Vars[["Negative Binomial"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Negative.Binomial(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                      vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Negative Binomial"]]<-NULL
      }
      #######
      if ("Naive Negative Binomial" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Negative Binomial"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Negative Binomial"]]
          vars<-Progression.Vars$Vars[["Naive Negative Binomial"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Negative.Binomial(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                      vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Negative Binomial"]]<-NULL
      }
      #######
      if ("Quantile" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Quantile"]]<-tryCatch({
          model<-Progression.Vars$Models[["Quantile"]]
          vars<-Progression.Vars$Vars[["Quantile"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Quantile"]]<-NULL
      }
      #######
      if ("Naive Quantile" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Quantile"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Quantile"]]
          vars<-Progression.Vars$Vars[["Naive Quantile"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          oracle.rules.Logistic(model=model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                             vars_not.fix=vars_not.fix,method=method,levels.table=levels.table,Imputation.Value)
        }, error=function(err){
          return(NULL)    
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Quantile"]]<-NULL
      }
      
      ###Temporary###
      currentmodel$ViewModelForm.oracle[["Xgboost"]]<-NULL
      currentmodel$ViewModelForm.oracle[["Naive Xgboost"]]<-NULL
      
      #######
      if ("Recursive Partitioning Tree" %in% models.used) {
        currentmodel$ViewModelForm.oracle[["Recursive Partitioning Tree"]]<-tryCatch({
          model<-Progression.Vars$Models[["Recursive Partitioning Tree"]]
          vars<-Progression.Vars$Vars[["Recursive Partitioning Tree"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          
          old.rules<-oracle_parse_tree(SPY,model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                vars_not.fix=vars_not.fix,levels.table=levels.table,Imputation.Value)
          new.rules<-replace(old.rules,model,method)
          new.rules
        }, error=function(err){
          return(NULL)   
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Recursive Partitioning Tree"]]<-NULL
      }
      #######
      if ("Naive Recursive Partitioning Tree" %in% models.used) { 
        currentmodel$ViewModelForm.oracle[["Naive Recursive Partitioning Tree"]]<-tryCatch({
          model<-Progression.Vars$Models[["Naive Recursive Partitioning Tree"]]
          vars<-Progression.Vars$Vars[["Naive Recursive Partitioning Tree"]]
          vars<-base.vars(vars,SPY)
          ##Which variables not to fix
          vars_not.fix<-chosen.not.fix[["not.fix"]]
          vars_not.fix<-vars_not.fix[vars_not.fix %in% vars] 
          ##
          vars_not.fix.outliers<-chosen.not.fix[["not.fix.outliers"]]
          vars_not.fix.outliers<-vars_not.fix.outliers[vars_not.fix.outliers %in% vars] 
          
          old.rules<-oracle_parse_tree(SPY,model,vars=vars,vars_not.fix.outliers=vars_not.fix.outliers,
                                vars_not.fix=vars_not.fix,levels.table=levels.table,Imputation.Value)
          new.rules<-replace(old.rules,model,method)
          new.rules
        }, error=function(err){
          return(NULL)   
        })
      } else{
        currentmodel$ViewModelForm.oracle[["Naive Recursive Partitioning Tree"]]<-NULL
      }
      
      ###Random Forest###
      currentmodel$ViewModelForm.oracle[["Rforest"]]<-NULL
      currentmodel$ViewModelForm.oracle[["Naive Rforest"]]<-NULL
      
      ###Neural Network###
      currentmodel$ViewModelForm.oracle[["Neural Network"]]<-NULL
      currentmodel$ViewModelForm.oracle[["Naive Neural Network"]]<-NULL
      
      ###Ensemble.all.models
      currentmodel$ViewModelForm.oracle[["Ensemble all models"]]<-NULL
      currentmodel$ViewModelForm.oracle[["Ensemble best models"]]<-NULL
    })
    
    
    #####Show the View Model page if you have at least one formula####
    output$At_least_one_formula_exist_oracle<-reactive({
      return(length(currentmodel$ViewModelForm.oracle)>0)
    })
    outputOptions(output, 'At_least_one_formula_exist_oracle', suspendWhenHidden=FALSE)
    
    ######### Model formula Outputs
    ####Choose Model 
    output$Model.for.Formula.ui.oracle<-renderUI({
      if(length(currentmodel$ViewModelForm.oracle)>0){
        models.names<-names(currentmodel$ViewModelForm.oracle)
        selectInput("Model.for.Formula.oracle",label="Model",choices=models.names)
      }
    })
    
    output$Show.Formula.oracle<- renderText({ 
      input$AllowComputingAnalysis
      input$MainSubmit.Model.for.Formula.oracle
      if(!is.null(isolate(input$Model.for.Formula.oracle)) && !is.null(isolate(currentmodel$ViewModelForm.oracle[[input$Model.for.Formula.oracle]]))){
        isolate(currentmodel$ViewModelForm.oracle[[input$Model.for.Formula.oracle]])
      } else{
        ""
      }
    })
    
    ####Export Formula
    observeEvent(input$MainSubmit.Formula.export.oracle,{
      if(!is.null(currentmodel$ViewModelForm.oracle[[input$Model.for.Formula.oracle]])){
        if(input$path.Formula.export.oracle!=""){
          tryCatch({
            formula<-currentmodel$ViewModelForm.oracle[[input$Model.for.Formula.oracle]]
            write.table(data.frame(formula),file = paste0("C://AlgoTraceFolder.export/",input$path.Formula.export.oracle,".sql"), row.names=F, col.names=F, quote = FALSE)
            ViewModelForm.oracle.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            ViewModelForm.oracle.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          ViewModelForm.oracle.last_action<<-paste("Insert File name")
        }
      } else{
        ViewModelForm.oracle.last_action<<-paste("No Data to export")
      }
    })
    output$Formula.export.text.oracle<-renderText({
      input$MainSubmit.Formula.export.oracle
      ViewModelForm.oracle.last_action
    })
    
    
    
    
    ###################################
    ###Reactive Values####
    ##################################################
    ##Data is uploaded
    output$DataIsChosen<-reactive({
      input$datafile
      return(!is.null(currentmodel$uploaded.data))
    })
    outputOptions(output, 'DataIsChosen', suspendWhenHidden=FALSE)
    
    ##Analysis Dataset is calculated
    output$Analysis_DataSet_exist<-reactive({
      return(!is.null(currentmodel$Analysis.DataSet))
    })
    outputOptions(output, 'Analysis_DataSet_exist', suspendWhenHidden=FALSE)
    
    ##CV Data is calculated
    output$CV_Data_exist<-reactive({
      return(!is.null(currentmodel$CV.Data))
    })
    outputOptions(output, 'CV_Data_exist', suspendWhenHidden=FALSE)
    
    ##Predict Data is calculated
    output$Predict_Data_exist<-reactive({
      return(!is.null(currentmodel$Predict.Data))
    })
    outputOptions(output, 'Predict_Data_exist', suspendWhenHidden=FALSE)
    
    ##For Recursive Partitioning Tree parameters
    output$Rpart_Algorithms<-reactive({
      return(sum(c("All Models","Recursive Partitioning Tree","Naive Recursive Partitioning Tree") %in% input$ChooseAlgorithms)>0)
    })
    outputOptions(output, 'Rpart_Algorithms', suspendWhenHidden=FALSE)
    
    ##For Xgboost parameters
    output$Xgboost_Algorithms<-reactive({
      return(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms)>0)
    })
    outputOptions(output, 'Xgboost_Algorithms', suspendWhenHidden=FALSE)
    
    output$Xgboost_best_params_classification<-reactive({
      return(sum(c("All Models","Xgboost","Naive Xgboost") %in% input$ChooseAlgorithms)>0 &&
               currentmodel$Method.saved=="Classification" && input$Xgb.best.parameters=="Yes")
    })
    outputOptions(output, 'Xgboost_best_params_classification', suspendWhenHidden=FALSE)
    
    ##########
    ##Have Results
    output$HaveResults <- reactive({
      return(!is.null(currentmodel$necessary.variables.found))
    }) 
    outputOptions(output, 'HaveResults', suspendWhenHidden=FALSE)
    
    ##Classification
    Is.Classification<-reactive({
      if(!is.null(currentmodel$Method.used)){
        return(currentmodel$Method.used=="Classification")
      } else{
        return(FALSE)
      }
    })
    output$Classification<-reactive({
      return(Is.Classification())
    })
    outputOptions(output, 'Classification', suspendWhenHidden=FALSE)
    
    ##Classification Chosen
    output$Classification_chosen<-reactive({
      if(!is.null(currentmodel$Method.saved)){
        return(currentmodel$Method.saved=="Classification")
      } else{
        return(FALSE)
      }
    })
    outputOptions(output, 'Classification_chosen', suspendWhenHidden=FALSE)
    
    ##Estimation
    Is.Estimation<-reactive({  
      if(!is.null(currentmodel$Method.used)){
        return(currentmodel$Method.used=="Estimation")
      } else{
        return(FALSE)
      }
    })
    output$Estimation<-reactive({
      return(Is.Estimation())
    })
    outputOptions(output, 'Estimation', suspendWhenHidden=FALSE)
    
    ##Estimation Chosen
    output$Estimation_chosen<-reactive({
      if(!is.null(currentmodel$Method.saved)){
        return(currentmodel$Method.saved=="Estimation")
      } else{
        return(FALSE)
      }
    })
    outputOptions(output, 'Estimation_chosen', suspendWhenHidden=FALSE)
    ###
    #Insights
    output$Variables_Importance<-reactive({
      return(!is.null(modelInsights$Variables.Importance))
    })
    outputOptions(output, 'Variables_Importance', suspendWhenHidden=FALSE)
    
    output$Dashboard_CV_Table<-reactive({
      return(!is.null(modelInsights$Dashboard.CV.Table) && Is.Classification())
    })
    outputOptions(output, 'Dashboard_CV_Table', suspendWhenHidden=FALSE)
    
    output$Dashboard_Est_Act_Avg<-reactive({
      return(!is.null(modelInsights$Dashboard.Est.Act_Avg) && Is.Estimation())
    })
    outputOptions(output, 'Dashboard_Est_Act_Avg', suspendWhenHidden=FALSE)
    
    output$Vars_vs_target_Correlation_plot_actionlink<-reactive({
      return(!is.null(modelInsights$Vars.vs.target.Correlation.table))
    })
    outputOptions(output, 'Vars_vs_target_Correlation_plot_actionlink', suspendWhenHidden=FALSE)
    ###########################################################

    

    ##################Outputs###################
    ###############Prediction Table Tab###############
    output$accuracy_barplot.est<-renderPlot({  
      tryCatch({
      if (!is.null(currentmodel$summary_table) && Is.Estimation()) { 
        temp<-currentmodel$summary_table
        if('Abs Difference' %in% names(temp))
          names(temp)[which(names(temp)=='Abs Difference')]<-'MAE'
        
        plot.features<-ggplot(temp,aes(x=temp[,'Method'],color=Criterion))+
          geom_point(aes(y=temp[,'RMSE'],col="RMSE"),stat="identity",shape=21,size=5,stroke=1)+ 
          geom_point(aes(y=temp[,'MAE'],col="MAE"),stat="identity",shape=21,size=6,stroke=1)+ 
          labs(x="Model",y="Value")+ggtitle("Comparison Plot")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        plot.features
      }
      },error=function(err){})
    }, bg="transparent")
    
    output$all_models_Norm_Residuals_plot.est<-renderPlot({   
      if (!is.null(currentmodel$Residuals_table) && Is.Estimation()) {
        plot.features <- ggplot(currentmodel$Residuals_table, aes(factor(Model), Norm_Residuals))+
          geom_violin(aes(fill = Model))+geom_point(alpha = 0.3)+
          labs(x="Model",y="Normalized Residuals")+ggtitle("Normalized Residuals")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        plot.features 
      }
    }, bg="transparent")
    
    output$accuracy_barplot<-renderPlot({   
      tryCatch({
      if (!is.null(currentmodel$summary_table) && Is.Classification()) {
        temp<-currentmodel$summary_table
          plot.features<-ggplot(temp,aes(x=factor(temp[,"Method"]),color=Criterion))+
            geom_point(aes(y=temp[,"Accuracy"],col="Accuracy"),stat="identity",shape=21,size=4,stroke=1)+ 
            geom_point(aes(y=temp[,"F measure"],col="F measure"),stat="identity",shape=21,size=5,stroke=1)+ 
            geom_point(aes(y=temp[,"AUC"],col="AUC"),stat="identity",shape=21,size=6,stroke=1)+ 
            geom_point(aes(y=temp[,"Gini"],col="Gini"),stat="identity",shape=21,size=7,stroke=1)+ 
            labs(x="Model",y="Value")

        plot.features+ggtitle("Comparison Plot")+
          theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.background=element_rect(fill='white'),
          plot.background = element_rect(fill="white")
        )
      }
      },error=function(err){})
    }, bg="transparent")   
    
    output$multiple_roc<-renderPlot({
      if(!is.null(currentmodel$Multiple.ROC) && Is.Classification()){
        tryCatch({
        temp<-currentmodel$Multiple.ROC
        long_df <- as.data.frame(melt(as.data.table(temp), id.vars="target", variable.name="name"))
        model_df <- do.call(rbind, unname(by(long_df, long_df$name, getROC_AUC_for_multiple_models)))
        p <- ggplot(model_df, aes(x=stack_x, y=stack_y, colour=Model)) + geom_line() +
          labs(x="probability of false alarm\n(1-Specificity)",
               y="probability of detection\n(Sensitivity)")+ggtitle("ROC Curve")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        
        p
        
        }, error=function(err){})
      }
    }, bg="transparent")
    
    
    observe({
      if(!is.null(currentmodel$confusion.table) && Is.Classification()){
        output$confusion <- renderUI({
          Names<-names(currentmodel$confusion.table)
          plot_output_list <- lapply(Names, function(i) {
            tablename <- paste("tablename.confusion_", i, sep="")
            column(6, DT::dataTableOutput(tablename))
          })
          
          fluidRow(do.call(tagList, plot_output_list))
        }) 
        
        temp.table<-currentmodel$confusion.table
        Names<-names(currentmodel$confusion.table)
        for (i in Names){
          local({
            my_i <- i
            tablename <- paste("tablename.confusion_", my_i, sep="")
            output[[tablename]] <- DT::renderDataTable({ 
              Table<-temp.table[[my_i]] 
              datatable(Table,caption=my_i,selection = "single",   
                        options=list(info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
            },server = FALSE)
          })
        }  
      }})   
    
    
    
    output$Settings_table<-output$Settings_table.cv<-
      output$Settings_table.est<-output$Settings_table.est.cv<-DT::renderDataTable({
        if(!is.null(currentmodel$Settings_Table) && !is.null(currentmodel$necessary.variables.found)){
          datatable(currentmodel$Settings_Table,caption="Settings",rownames=FALSE,
                    options=list(
                      info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE))
        } else{
          NULL
        }
      },server = FALSE)
    
    output$summary_table<-output$summary_table.est<-DT::renderDataTable({ 
      if(!is.null(currentmodel$summary_table)){
        Table<-currentmodel$summary_table 
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits)
        ##
        datatable(Table,caption="Summary Table",rownames=FALSE, selection = "single",
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    ########################################################################
    ##### Trainning Results #############
    output$summary_trainning_table.est<-DT::renderDataTable({
      if(!is.null(currentmodel$summary_trainning_table.est) && Is.Estimation()){
        Table<-currentmodel$summary_trainning_table.est
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits) 
        ##
        datatable(Table,caption="Summary Table",rownames=FALSE, selection = "single",
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    output$summary_trainning_table<-DT::renderDataTable({
      if(!is.null(currentmodel$summary_trainning_table) && Is.Classification()){
        Table<-currentmodel$summary_trainning_table
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits) 
        ##
        datatable(Table,caption="Summary Table",rownames=FALSE, selection = "single",
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    ############Show Model Summary#############
    ######### Model Summary #########
    ##At least one is not not NULL
    output$At_least_one_Summary_exist<-reactive({
      if(!is.null(currentmodel$necessary.variables.found)){
        Reg.Models<-c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic","Linear","Naive Linear",
                      "Weighted Linear","Naive Weighted Linear","Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")
        Progression.Vars <- currentmodel$necessary.variables.found
        Names<-names(Progression.Vars$Models)
        return(length(Names[Names %in% Reg.Models])>0) 
      } else{
        return(FALSE)
      }
    })
    outputOptions(output, 'At_least_one_Summary_exist', suspendWhenHidden=FALSE)
    
    
    ####Choose Model 
    output$Model.for.Summary.ui<-renderUI({   
      if(!is.null(currentmodel$necessary.variables.found)){
        Reg.Models<-c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic","Linear","Naive Linear",
                      "Weighted Linear","Naive Weighted Linear","Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")
        Progression.Vars <- currentmodel$necessary.variables.found
        Names<-names(Progression.Vars$Models)
        choices<-Names[Names %in% Reg.Models]
        selectInput("Model.for.Summary",label="Model",choices=choices)
      }
    })
    
    output$Show.Summary<- renderPrint({ 
      if(!is.null(input$Model.for.Summary) && !is.null(currentmodel$necessary.variables.found$Models[[input$Model.for.Summary]])){
        model<-currentmodel$necessary.variables.found$Models[[input$Model.for.Summary]]
        print(summary(model))
      } 
    })
    #############End of Model Summary###################################   

    ##################################################################################
    ###################Accuracy Deciles Tab#############################
    ###############Creating Data for Accuracy Deciles###############
    #### Accuracy Deciles ###
     observe({
       if(Is.Classification())
         showTab(inputId = "Prediction_Analysis", target = "Accuracy Deciles")
       if(Is.Estimation()){
         hideTab(inputId = "Prediction_Analysis", target = "Accuracy Deciles")
         updateTabsetPanel(session, "Prediction_Analysis",selected = "Prediction Table")
       }
     })
    
    
    output$Model.for.deciles.accuracy.ui<-renderUI({
      if(!is.null(currentmodel$Used.Models) && Is.Classification()){
        models.used<-currentmodel$Used.Models[["models.used"]]
        best.models.used<-currentmodel$Used.Models[["best.models.used"]]
        models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
        best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
        
        choices<-models.used
        if(length(models.used)>1)
          choices<-c(choices,"Ensemble all models")  
        if(length(models.used.weights)>1)
          choices<-c(choices,"Ensemble all models weighting")  
        if(length(best.models.used)>1)
          choices<-c(choices,"Ensemble best models")
        if(length(best.models.used.weights)>1)
          choices<-c(choices,"Ensemble best models weighting")
        ###In case we loaded a model
        isolate({
          if(!is.null(currentmodel$Loaded.Model.for.deciles.accuracy)){
            selected<-currentmodel$Loaded.Model.for.deciles.accuracy
            currentmodel$Loaded.Model.for.deciles.accuracy<-NULL
          } else{
            selected<-NULL
          } 
        })
        selectInput("Model.for.deciles.accuracy", label="Model", choices=choices, selected=selected,width='200px')
      }
    })
    
    observeEvent(input$MainSubmit.Deciles.Accuracy,{ 
      if(!is.null(currentmodel$Analysis.DataSet) && !is.null(input$Model.for.deciles.accuracy)){ 
        currentmodel$Accuracy.deciles.table<-tryCatch({  
          SPY.Test <- currentmodel$Analysis.DataSet 
          prob<-SPY.Test[[paste0(gsub(" ",".",input$Model.for.deciles.accuracy),"_predict_probability")]]
          output.var<-currentmodel$params.to.assign[["output.var"]]
          classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
          Deciles.Count<-input$Deciles.Count
          if(!is.na(Deciles.Count) && Deciles.Count>0){
            table.count<-vector(mode="numeric",length=Deciles.Count)
            Bins<-vector(mode="character",length=Deciles.Count)
            
            if(input$Criterion.for.deciles.accuracy=="Accuracy"){
              table.accuracy<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Test[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.accuracy[i]<- (Table[1,1]+Table[2,2])/sum(Table)
                } else{
                  table.accuracy[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.accuracy)
              colnames(table)<-c("Bins","Count in each Bin","Accuracy")
            }
            if(input$Criterion.for.deciles.accuracy=="Accuracy 0"){
              table.accuracy_0<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Test[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.accuracy_0[i]<- Table[1,1]/(Table[1,1]+Table[2,1])
                } else{
                  table.accuracy_0[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.accuracy_0)
              colnames(table)<-c("Bins","Count in each Bin","Accuracy 0")
            }
            if(input$Criterion.for.deciles.accuracy=="Precision"){
              table.precision<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Test[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.precision[i]<- Table[2,2]/(Table[1,2]+Table[2,2])
                } else{
                  table.precision[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.precision)
              colnames(table)<-c("Bins","Count in each Bin","Precision")
            }
            if(input$Criterion.for.deciles.accuracy=="Recall"){
              table.recall<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Test[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.recall[i]<-Table[2,2]/(Table[2,1]+Table[2,2])
                } else{
                  table.recall[i]<-NaN
                }
              }
              table<-data.frame(Bins,table.count,table.recall)
              colnames(table)<-c("Bins","Count in each Bin","Recall")
            }
            Accuracy.deciles.table<-table
          } else{#if(Deciles.Count<=0)
            Accuracy.deciles.table<-NULL
          }
          Accuracy.deciles.table
        }, error=function(err) {
          return(NULL)
        })
      } else{
        currentmodel$Accuracy.deciles.table<-NULL
      }
    }) 

    output$deciles.accuracy.table<-DT::renderDataTable({ 
      if(!is.null(currentmodel$Accuracy.deciles.table) && Is.Classification()){
        temp.table<-currentmodel$Accuracy.deciles.table
        temp.names<-as.character(temp.table[,"Bins"])
        table<-as.data.frame(t(temp.table[,colnames(temp.table)[!colnames(temp.table) %in% "Bins"],drop=FALSE]))
        colnames(table)<-temp.names  
        ##Round
        numVars <- sapply(table, is.numeric) 
        table[numVars] <- lapply(table[numVars], round, digits = round.digits)
        ##
        datatable(table,selection = "single", class = 'cell-border stripe',
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL  
      }
    },server = FALSE)
    
    output$deciles.accuracy.count.plot<-renderPlot({ 
      if(!is.null(currentmodel$Accuracy.deciles.table) && Is.Classification()){
        table<-currentmodel$Accuracy.deciles.table
        p<-ggplot(table,aes(x=as.factor(table[,1]),y=table[,2]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y="Count in each Bin")+theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        p
        }}, bg="transparent")
    
    output$deciles.accuracy.plot<-renderPlot({  
      if(!is.null(currentmodel$Accuracy.deciles.table) && Is.Classification()){
        table<-currentmodel$Accuracy.deciles.table
        p<-ggplot(table,aes(x=as.factor(table[,1]),y=table[,3]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y=paste(colnames(table)[3],"in each Bin"))+theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          ) 
        p
       }}, bg="transparent") 
    
    output$deciles_accuracy_table_actionlink <- reactive({
      return(!is.null(currentmodel$Accuracy.deciles.table) && Is.Classification())
    }) 
    outputOptions(output, 'deciles_accuracy_table_actionlink', suspendWhenHidden=FALSE)
    
      
    ####Export Deciles Table
    observeEvent(input$MainSubmit.deciles.accuracy.table,{
      if(!is.null(currentmodel$Accuracy.deciles.table)){
        if(input$path.deciles.accuracy.table!=""){
          tryCatch({
            write_excel_csv(currentmodel$Accuracy.deciles.table,path = paste0("C://AlgoTraceFolder.export/",input$path.deciles.accuracy.table,".csv"))
            deciles.accuracy.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            deciles.accuracy.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          deciles.accuracy.last_action<<-paste("Insert File name")
        }
      } else{
        deciles.accuracy.last_action<<-paste("No Data to export")
      }
    })
    output$deciles.accuracy.table.text<-renderText({
      input$MainSubmit.deciles.accuracy.table
      deciles.accuracy.last_action
    })
    ##################################################################################
    #### Avg Deciles ###
    observe({
      if(Is.Estimation())
        showTab(inputId = "Prediction_Analysis", target = "Avg Deciles")
      if(Is.Classification()){
        hideTab(inputId = "Prediction_Analysis", target = "Avg Deciles")
        updateTabsetPanel(session, "Prediction_Analysis",selected = "Prediction Table")
      }
    })
    
    output$Model.for.deciles.avg.ui<-renderUI({
      if(!is.null(currentmodel$Used.Models) && Is.Estimation()){
        models.used<-currentmodel$Used.Models[["models.used"]]
        best.models.used<-currentmodel$Used.Models[["best.models.used"]]
        choices<-models.used
        if(length(models.used)>1)
          choices<-c(choices,"Ensemble all models")  
        if(length(best.models.used)>1)
          choices<-c(choices,"Ensemble best models")
        ###In case we loaded a model
        isolate({
          if(!is.null(currentmodel$Loaded.Model.for.deciles.avg)){
            selected<-currentmodel$Loaded.Model.for.deciles.avg
            currentmodel$Loaded.Model.for.deciles.avg<-NULL
          } else{
            selected<-NULL
          } 
        })
        selectInput("Model.for.deciles.avg", label="Model" , choices=choices, selected=selected,width='200px')
      }
    })
    ###
    observeEvent(input$MainSubmit.Deciles.Avg,{ 
      if(!is.null(currentmodel$Analysis.DataSet) && !is.null(input$Model.for.deciles.avg)){ 
        currentmodel$Avg.deciles.table<-tryCatch({  
          SPY.Test <- currentmodel$Analysis.DataSet 
          prediction<-SPY.Test[[paste0(gsub(" ",".",input$Model.for.deciles.avg),"_predict_response")]]
          output.var<-currentmodel$params.to.assign[["output.var"]]
          models.used<-currentmodel$Used.Models[["models.used"]]
          Deciles.Count<-input$Deciles.Avg.Count
          if(!is.na(Deciles.Count) && Deciles.Count>0){
            ##Finding the mean of the prediction in each Bin
            cut_to_levels<-cut.nv(SPY.Test[[output.var]],Deciles.Count)
            cut_to_levels.uni<-levels(cut_to_levels)
            Count<-vector(mode="numeric",length=length(cut_to_levels.uni))
            
            if(input$Criterion.for.deciles.avg=="Avg Traget vs Prediction"){
              Avg_target<-vector(mode="numeric",length=length(cut_to_levels.uni))
              Avg_prediction<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Avg_target[i]<-mean(SPY.Test[[output.var]][ind],na.rm=TRUE)
                Avg_prediction[i]<-mean(prediction[ind],na.rm=TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Avg_target,Avg_prediction)
              colnames(table)<-c("Bins","Count in each Bin","Avg Target","Avg Prediction")
            }
            
            if(input$Criterion.for.deciles.avg=="MAE"){
              MAE<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                MAE[i]<-mean(abs(SPY.Test[[output.var]][ind]-prediction[ind]),na.rm =TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,MAE)
              colnames(table)<-c("Bins","Count in each Bin","MAE")
            }
            
            if(input$Criterion.for.deciles.avg=="Norm abs Difference"){
              Norm_abs_Difference<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Norm_abs_Difference[i]<-mean(abs(SPY.Test[[output.var]][ind]-prediction[ind]),na.rm =TRUE)/mean(abs(SPY.Test[[output.var]][ind]),na.rm =TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Norm_abs_Difference)
              colnames(table)<-c("Bins","Count in each Bin","Norm abs Difference")
            }
            
            if(input$Criterion.for.deciles.avg=="Abs Sum Difference"){
              Abs_Sum_Difference<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Abs_Sum_Difference[i]<-abs(sum(SPY.Test[[output.var]][ind],na.rm=TRUE)-sum(prediction[ind],na.rm=TRUE))
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Abs_Sum_Difference)
              colnames(table)<-c("Bins","Count in each Bin","Abs Sum Difference")
            }
            
            if(input$Criterion.for.deciles.avg=="RMSE"){
              RMSE<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                RMSE[i]<-sqrt(mean((SPY.Test[[output.var]][ind]-prediction[ind])^2,na.rm =TRUE))
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,RMSE)
              colnames(table)<-c("Bins","Count in each Bin","RMSE")
            }
            
            Avg.deciles.table<-table
          } else{#if(Deciles.Count<=0)
            Avg.deciles.table<-NULL
          }
          Avg.deciles.table
        }, error=function(err) {
          return(NULL)
        })
      } else{
        currentmodel$Avg.deciles.table<-NULL
      }
    })  
    
    output$Avg.deciles.table<-DT::renderDataTable({
      if(!is.null(currentmodel$Avg.deciles.table) && Is.Estimation()){
        temp.table<-currentmodel$Avg.deciles.table
        temp.names<-as.character(temp.table[,"Bins"])
        table<-as.data.frame(t(temp.table[,colnames(temp.table)[!colnames(temp.table) %in% "Bins"],drop=FALSE]))
        colnames(table)<-temp.names 
        ##Round
        numVars <- sapply(table, is.numeric) 
        table[numVars] <- lapply(table[numVars], round, digits = round.digits)
        ##
        datatable(table,selection = "single", class = 'cell-border stripe',
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    
    output$Avg.deciles.count.plot<-renderPlot({ 
      if(!is.null(currentmodel$Avg.deciles.table) && Is.Estimation()){
        table<-currentmodel$Avg.deciles.table
        p<-ggplot(table,aes(x=table[,"Bins"],y=table[,"Count in each Bin"]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Target Variable Bins",y="Count in each Bin")+scale_x_discrete(limits=table[,"Bins"])+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        p
      }}, bg="transparent")
    
    
    output$Avg.deciles.plot<-renderPlot({
      if(!is.null(currentmodel$Avg.deciles.table) && Is.Estimation()){
        table<-currentmodel$Avg.deciles.table
        if(isolate(input$Criterion.for.deciles.avg)!="Avg Traget vs Prediction"){
        p<-ggplot(table,aes(x=table[,"Bins"],y=table[,3]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y=paste(colnames(table)[3],"in each Bin"))+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          ) 
        } else{
          p<-ggplot(table,aes(x=table[,"Bins"]))+
            geom_point(aes(y=table[,"Avg Target"],col="Avg Target"),stat="identity",shape=21,size=5,stroke=2)+ 
            geom_point(aes(y=table[,"Avg Prediction"],col="Avg Prediction"),stat="identity",shape=21,size=5,stroke=2) + 
            labs(x="Target Variable Bins",y="value",color="variable")+scale_x_discrete(limits=table[,"Bins"])+
            theme(
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.background=element_rect(fill='white'),
              legend.position="bottom",
              plot.background = element_rect(fill="white")
            )
        }
        p
      }
    }, bg="transparent")
    
    
    output$deciles_avg_table_actionlink <- reactive({
      return(!is.null(currentmodel$Avg.deciles.table) && Is.Estimation())
    }) 
    outputOptions(output, 'deciles_avg_table_actionlink', suspendWhenHidden=FALSE)
    
    
    ####Export Deciles Table
    observeEvent(input$MainSubmit.deciles.avg.table,{
      if(!is.null(currentmodel$Avg.deciles.table)){
        if(input$path.deciles.avg.table!=""){
          tryCatch({
            write_excel_csv(currentmodel$Avg.deciles.table,path = paste0("C://AlgoTraceFolder.export/",input$path.deciles.avg.table,".csv"))
            deciles.avg.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            deciles.avg.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          deciles.avg.last_action<<-paste("Insert File name")
        }
      } else{
        deciles.avg.last_action<<-paste("No Data to export")
      }
    })
    output$deciles.avg.table.text<-renderText({
      input$MainSubmit.deciles.avg.table
      deciles.avg.last_action
    })
    ##################################################################################
    ######### Data Set ################
    output$Analysis.DataSet <-DT::renderDataTable({
      if (!is.null(currentmodel$Analysis.DataSet)){
        SPY.Test<-currentmodel$Analysis.DataSet
        #ind<-which(unlist(lapply(names(SPY.Test),function(i){is.factor(SPY.Test[[i]])})))
        ind<-which(unlist(lapply(names(SPY.Test),function(i){is.factor(SPY.Test[[i]]) && 
            !all(is.na(nchar(as.character(SPY.Test[[i]])[c(1,2,3)]))) &&
            mean(nchar(as.character(SPY.Test[[i]])[c(1,2,3)]),na.rm=TRUE)>30})))
        datatable(SPY.Test,selection = "single",
                  options=list(
                    columnDefs = list(list(
                      targets  = ind,
                      render  = JS(
                        "function ( data, type) {",
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(ind),
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    })    

   
    ####################################################
    output$Time_Taken<-renderText({
      if(!is.null(currentmodel$time.taken)){
        temp<-unlist(strsplit(format(currentmodel$time.taken)," "))
        num<-round(as.numeric(temp[1]),3)
        unit<-temp[2]
        paste(num,unit,"for algorithm computing")
    }})
    ################################################################################
      ##########Insights#################
      observe({
        if(!is.null(currentmodel$necessary.variables.found)){
          showTab(inputId = "Outer_Prediction_Analysis", target = "Insights")
        } else{
          hideTab(inputId = "Outer_Prediction_Analysis", target = "Insights")
          updateTabsetPanel(session, "Outer_Prediction_Analysis",selected = "Prediction Analysis")
        }
      })
    ###################Dashboard#############################
    ##### Creating data for Dashboard##########
    
    output$Model.for.dashboard.ui<-renderUI({
      if(!is.null(currentmodel$Used.Models)){
        models.used<-currentmodel$Used.Models[["models.used"]]
        selectInput("Model.for.dashboard",label="Model",choices=models.used)
      }
    })
      
    observe({ 
      if(!is.null(currentmodel$necessary.variables.found) && !is.null(input$Model.for.dashboard)){ 
        tryCatch({
          print("inside dashboard")
          output.var<-currentmodel$params.to.assign[["output.var"]]
          classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
          Progression.Vars<-currentmodel$necessary.variables.found
          loop.vars<-Progression.Vars$Vars[[input$Model.for.dashboard]]
          myform<-Progression.Vars$Myform[[input$Model.for.dashboard]]
          model<-Progression.Vars$Models[[input$Model.for.dashboard]]
          #########Importance of the variables and Variable vs target Correlation########
          if(input$Model.for.dashboard %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
                                              "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                                              "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
            modelInsights$Variables.Importance<-tryCatch({
              coef.glm<-model$coefficients[-1]
              #http://astrostatistics.psu.edu/datasets/2006tutorial/html/quantreg/html/summary.rq.html
              importance<-summary(model) 
              importance<-as.data.frame(importance$coefficients)  
              importance<-importance[!rownames(importance) %in% "(Intercept)",,drop=FALSE]
              if("z value" %in% names(importance))
                importance<-importance[,"z value",drop=FALSE]
              if("t value" %in% names(importance))
                importance<-importance[,"t value",drop=FALSE]
              names(importance)<-"Overall"
              importance[,"Overall"]<-abs(importance[,"Overall"])
              importance$Feature<-as.factor(rownames(importance))
              importance$colour<-ifelse(coef.glm[rownames(importance)]>0,"Pos", "Neg")
              ##
              ind<-sapply(importance$Feature,function(Var){
                temp<-base.vars(Var,currentmodel$SPY.fixed)
                if(length(temp)==1){
                  return(temp %in% names(currentmodel$levels.table))
                } else{ #it would be combination of variable
                  return(FALSE)
                }
              })
              if(sum(ind)>0)
                importance$colour[ind]<-"No colour"
              ##
              importance<-importance[importance$Overall>0,]
              importance
            }, error=function(err){
              return(NULL)
            })
          }
          if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){   
            modelInsights$Variables.Importance<-tryCatch({
            SPY.matrix<- sparse.model.matrix(myform, currentmodel$SPY.fixed)
            importance <- as.data.frame(xgb.importance(colnames(SPY.matrix),model = model)) 
            importance$Feature<-as.factor(importance$Feature)
            importance<-importance[order(-importance$Gain),]
            importance<-importance[!is.na(importance$Gain),]  
            ##
            importance<-importance[importance$Gain>0,]
            importance
            }, error=function(err){
              return(NULL)
            })
          } 
          if(input$Model.for.dashboard %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree")){
            modelInsights$Variables.Importance<-tryCatch({
              importance<-model$splits
              if(nrow(importance)>0){
                importance<-aggregate(improve~rownames(importance),data=importance,FUN=sum)
                colnames(importance)[colnames(importance)=="rownames(importance)"]<-"Feature"
                colnames(importance)[colnames(importance)=="improve"]<-"Overall"
                importance[,"Feature"]<-as.factor(importance[,"Feature"])
                ##
                importance<-importance[importance$Overall>0,]
              } else{#nrow(importance)==0
                importance<-NULL
              }
              importance
            }, error=function(err){
              return(NULL)
            })
          }
          if(input$Model.for.dashboard %in% c("Rforest","Naive Rforest")){
            modelInsights$Variables.Importance<-tryCatch({
              importance<-as.data.frame(model$variable.importance)
              if(nrow(importance)>0){   
                names(importance)<-"Overall"
                importance$Feature<-as.factor(rownames(importance))
                ##
                importance<-importance[importance$Overall>0,]
              } else{#nrow(importance)==0
                importance<-NULL
              }
              importance
            }, error=function(err){
              return(NULL)
            })
          }
          if(input$Model.for.dashboard %in% c("Neural Network","Naive Neural Network")){
            modelInsights$Variables.Importance<-NULL
          } 
          ##########################
          #######################################################
          #Extracting the basic vars from the loop vars 
          modelInsights$Vars.vs.target.Correlation.table<-tryCatch({
            basic.vars<-base.vars(loop.vars,currentmodel$SPY.fixed)  
            ind<-sapply(basic.vars,function(x){is.numeric(currentmodel$uploaded.data[[x]])})
            ind<-which(ind==TRUE)
            if(length(ind)>0){
              basic.vars<-basic.vars[ind]
              Correlation.table <- data.frame(Variable=basic.vars, Correlation=0) 
              for (var.in.loop in basic.vars) 
                Correlation.table$Correlation[Correlation.table$Variable==var.in.loop]<-cor(currentmodel$SPY.fixed[[output.var]],currentmodel$SPY.fixed[[var.in.loop]],method="spearman")	
              Correlation.table<-Correlation.table[order(-abs(Correlation.table$Correlation)),]
              rownames(Correlation.table)<-1:nrow(Correlation.table)
              Vars.vs.target.Correlation.table<-Correlation.table
            } else{
              Vars.vs.target.Correlation.table<-NULL
            }
            Vars.vs.target.Correlation.table
          }, error=function(err){
            return(NULL)
          })  
          ############What If Table (Insert values for the prediction result)##################
          modelInsights$input_list <-tryCatch({
            #Extracting the basic vars from the loop vars 
            basic.vars<-base.vars(loop.vars,currentmodel$SPY.fixed)   
            #########Choosing The Values ######
            input_list <- data.frame(var=basic.vars, val="", stringsAsFactors=FALSE)
            input_list_levels <- vector("list", length(basic.vars))
            
            if(is.null(currentmodel$levels.table)){
              for(i in 1:length(basic.vars)){ ###In  case its an old model with Factors
                if(class(currentmodel$SPY.fixed[[basic.vars[i]]]) %in% c("numeric", "integer")) {
                  input_list$val[i] <- as.character(round(Mode(currentmodel$SPY.fixed[[basic.vars[i]]]), 2))
                }
                if(class(currentmodel$SPY.fixed[[basic.vars[i]]])=="factor") {
                  input_list$val[i] <- Mode(as.character(currentmodel$SPY.fixed[[basic.vars[i]]]))
                  input_list_levels[[i]] <- levels(currentmodel$SPY.fixed[[basic.vars[i]]])
                }
              }} else{ 
                for(i in 1:length(basic.vars)){ 
                  if(class(currentmodel$uploaded.data[[basic.vars[i]]]) %in% c("numeric", "integer")) {
                    input_list$val[i] <- as.character(round(Mode(currentmodel$uploaded.data[[basic.vars[i]]]), 2))
                  }
                  if(class(currentmodel$uploaded.data[[basic.vars[i]]])=="factor") {
                    input_list$val[i] <- Mode(as.character(currentmodel$uploaded.data[[basic.vars[i]]]))
                    input_list_levels[[i]] <- levels(currentmodel$uploaded.data[[basic.vars[i]]])
                  }
                }
              }
            list(input_list=input_list, input_list_levels=input_list_levels)
          }, error=function(err){
            return(NULL)
          })
          #################################################
          ###########Cross Validation Table##############
          if(Is.Classification()==TRUE){
            modelInsights$Dashboard.CV.Table<-tryCatch({
              temp<-input$Model.for.dashboard
              prediction<-predict.model(model,myform,currentmodel$SPY.Test.fixed,classification.threshold,temp,"Classification")[["pred"]]
              data.frame(y=currentmodel$SPY.Test.fixed[[output.var]],pred=prediction)
            }, error=function(err){
              return(NULL)
            })
          }
          #############################################
          ############## Dashboard.Est.Act_Avg ###############
          if(Is.Estimation()==TRUE){
            modelInsights$Dashboard.Est.Act_Avg<-tryCatch({
              temp<-input$Model.for.dashboard
              prediction<-predict.model(model,myform,currentmodel$SPY.Test.fixed,classification.threshold,temp,"Estimation")[["pred"]]
              ##Finding the mean of the prediction in each Bin
              cut_to_levels<-cut.nv(currentmodel$SPY.Test.fixed[[output.var]],5)
              cut_to_levels.uni<-unique(cut_to_levels)
              Avg_output<-vector(mode="numeric",length=length(cut_to_levels.uni))
              Avg_pred<-vector(mode="numeric",length=length(cut_to_levels.uni))
              
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Avg_output[i]<-mean(currentmodel$SPY.Test.fixed[[output.var]][ind],na.rm=TRUE)
                Avg_pred[i]<-mean(prediction[ind],na.rm=TRUE)
              }
              data.frame(Bins=cut_to_levels.uni,Avg_output=Avg_output,Avg_pred=Avg_pred)
            }, error=function(err){
              return(NULL)
            })
          }
        }, error=function(err){})
      }
    }) 
    #########################################
    col <- "lightblue" 
    lab <- paste("Plot was not Found")
    p.err.text <- ggplot() + 
      scale_x_continuous(limits = c(0,2)) + scale_y_continuous(limits = c(0,2)) +
      annotate("text", label=lab, x=1, y=1, colour=col, size=6) +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    ##############################################
    #######Importance of the Variables #######
    
    ####Tab Name #dashboard 
    output$Variables.Importance<-renderPlot({ 
      input$Model.for.dashboard
      if(!is.null(modelInsights$Variables.Importance)){
        temp<-modelInsights$Variables.Importance
        if(nrow(temp)>20){
          if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){
            temp<-temp[order(-temp$Gain),][1:20,]
          } else{
            temp<-temp[order(-temp$Overall),][1:20,]
          }
          y.title<-"Importance (Top 20)"
        } else{
          y.title<-"Importance"
        }
        
        if(input$Model.for.dashboard %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
                                            "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                                            "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
          p <- ggplot(temp, aes(x=Feature, y=Overall, fill=colour)) +  
            labs(x="Variables",y=y.title) + geom_bar(stat="identity") +  coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Overall)]) +
            scale_fill_manual(values=c("Pos"="darkgreen", "Neg"="darkred","No colour"="blue"))
        }
        if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){
           p <- ggplot(temp, aes(x=Feature, y=Gain)) +  
            labs(x="Variables",y=y.title)+geom_bar(stat="identity",fill="blue") + coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Gain)])
        }
        if(input$Model.for.dashboard %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree",
                                            "Rforest","Naive Rforest")){
          p <- ggplot(temp, aes(x=Feature, y=Overall)) +  
            labs(x="Variables",y=y.title)+geom_bar(stat="identity",fill="blue") + coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Overall)])
        }
        p<-p+theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.position="none",
          plot.background = element_rect(fill="white")
        )
        p
       } else{
        p.err.text
       }
      }, height=380)
    
    output$Variables.Importance.zoom<-renderPlot({ 
      input$Model.for.dashboard
      if(!is.null(modelInsights$Variables.Importance)){
        temp<-modelInsights$Variables.Importance
        y.title<-"Importance"
        
        if(input$Model.for.dashboard %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
                                            "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                                            "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
          p <- ggplot(temp, aes(x=Feature, y=Overall, fill=colour)) +  
            labs(x="Variables",y=y.title) + geom_bar(stat="identity") +  coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Overall)]) +
            scale_fill_manual(values=c("Pos"="darkgreen", "Neg"="darkred","No colour"="blue"))
        }
        if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){
          p <- ggplot(temp, aes(x=Feature, y=Gain)) +  
            labs(x="Variables",y=y.title)+geom_bar(stat="identity",fill="blue") + coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Gain)])
        }
        if(input$Model.for.dashboard %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree",
                                            "Rforest","Naive Rforest")){
          p <- ggplot(temp, aes(x=Feature, y=Overall)) +  
            labs(x="Variables",y=y.title)+geom_bar(stat="identity",fill="blue") + coord_flip() +
            scale_x_discrete(labels = function(x) str_wrap(as.character(gsub("_"," ",as.character(x))), width = 35),limits = temp$Feature[order(temp$Overall)])
        }
        p<-p+theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          legend.position="none",
          plot.background = element_rect(fill="white")
        )
        p
      } else{
        p.err.text
      }
    }, height=function(){
      temp<-modelInsights$Variables.Importance
      if(is.null(temp)){
        return(380)
      } else{
        return(max(380,nrow(temp)*15))
      }})
       
    output$Variables.Importance.table.zoom<-DT::renderDataTable({ 
      input$Model.for.dashboard
      if(!is.null(modelInsights$Variables.Importance)){
        temp<-modelInsights$Variables.Importance
      if(input$Model.for.dashboard %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
                                          "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                                          "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
        temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
         Table<-temp[order(-temp$Overall),c("Feature","Overall")]
      }
      if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){
        temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
        Table<-temp[order(-temp$Gain),c("Feature","Gain")]
      }
        if(input$Model.for.dashboard %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree",
                                            "Rforest","Naive Rforest")){
        temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
        Table<-temp[order(-temp$Overall),c("Feature","Overall")]
      }
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits)
        ##
        datatable(Table,selection = "single",rownames=FALSE,
                  options=list(pageLength = 15,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
     
    variables.importance.table.last_action<-""
    ####Export Variables Importance table
    observeEvent(input$MainSubmit.Variables.Importance.table,{
      if(!is.null(modelInsights$Variables.Importance)){
        if(input$path.Variables.Importance.table!=""){
          tryCatch({
            #######
            temp<-modelInsights$Variables.Importance
            if(input$Model.for.dashboard %in% c("Logistic","Naive Logistic","Weighted Logistic","Naive Weighted Logistic",
                                                "Linear","Naive Linear","Weighted Linear","Naive Weighted Linear",
                                                "Negative Binomial","Naive Negative Binomial","Quantile","Naive Quantile")){
              temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
              Table<-temp[order(-temp$Overall),c("Feature","Overall")]
            }
            if(input$Model.for.dashboard %in% c("Xgboost","Naive Xgboost")){
              temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
              Table<-temp[order(-temp$Gain),c("Feature","Gain")]
            }
            if(input$Model.for.dashboard %in% c("Recursive Partitioning Tree","Naive Recursive Partitioning Tree",
                                                "Rforest","Naive Rforest")){
              temp[,"Feature"]<-str_wrap(as.character(gsub("_"," ",as.character(temp[,"Feature"]))), width = 35)
              Table<-temp[order(-temp$Overall),c("Feature","Overall")]
            }
            ##Round
            numVars <- sapply(Table, is.numeric) 
            Table[numVars] <- lapply(Table[numVars], round, digits = round.digits)
            ##
            ############
            write_excel_csv(Table,path = paste0("C://AlgoTraceFolder.export/",input$path.Variables.Importance.table,".csv"))
            variables.importance.table.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            variables.importance.table.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          variables.importance.table.last_action<<-paste("Insert File name")
        }
      } else{
        variables.importance.table.last_action<<-paste("No Data to export")
      }
    })
    output$Variables.Importance.table.text<-renderText({
      input$MainSubmit.Variables.Importance.table
      variables.importance.table.last_action
    })
    
    ####### confusion matrix #######
    output$Dashboard.CV.Table <- renderRHandsontable({ 
      input$Model.for.dashboard
      if(!is.null(modelInsights$Dashboard.CV.Table) && Is.Classification()){
        Tab <- as.data.frame.matrix(table(modelInsights$Dashboard.CV.Table[,"y"], modelInsights$Dashboard.CV.Table[,"pred"]))
        colnames(Tab) <- paste("Predicted", colnames(Tab), sep=" ")
        rownames(Tab) <- paste("Actual", rownames(Tab), sep=" ")
        
        Tab.percent<-sapply(1:ncol(Tab),function(x){return(Tab[,x]/sum(Tab[,x]))})
        Tab.percent<-as.data.frame(Tab.percent)
        colnames(Tab.percent)<-colnames(Tab)
        rownames(Tab.percent)<-rownames(Tab)
        Table<-Tab.percent 
        rhandsontable(Table, height=100,rowHeaderWidth=100,maxRows=nrow(Table)) %>% hot_col(colnames(Table),readOnly=TRUE) %>%
          hot_cols(colWidths = 100)
      }
    })

    output$Dashboard.CV.plot<-renderPlot({
      input$Model.for.dashboard
      if(!is.null(modelInsights$Dashboard.CV.Table) && Is.Classification()){
        ####
        data<-modelInsights$Dashboard.CV.Table
        names(data)[which(names(data)=="y")]<-"Actual"
        names(data)[which(names(data)=="pred")]<-"Predicted"
        confusion = as.data.frame(table(data$Predicted, data$Actual))
        names(confusion) = c("Predicted","Actual","Freq")
        ##Checking if we have all the combinations
        comp.data<-data.frame("Predicted"=c("0","0","1","1"),"Actual"=c("0","1","0","1"),"Freq"=c(0,0,0,0))
        ind<-sapply(1:nrow(comp.data),function(x){
          return(nrow(merge(comp.data[x,c("Predicted","Actual")],confusion))==0)
        })
        if(sum(ind)>0){
          confusion<-as.data.frame(rbind(confusion,comp.data[ind,]))
        }
        rownames(confusion)<-NULL
        ##
        confusion$Match<-ifelse(as.character(confusion$Actual)==as.character(confusion$Predicted),"Yes","No")
        confusion$Actual<-factor(confusion$Actual,levels=c(1,0))
        confusion$Predicted<-factor(confusion$Predicted,levels=c(0,1))
        ####
        p <- ggplot() +
          geom_tile(aes(x=Predicted, y=Actual,fill=Match),data=confusion, color="black",size=0.1) +
          scale_fill_manual(values=c("Yes"="darkgreen", "No"="darkred"))+labs(x="Predicted",y="Actual")+
          scale_x_discrete(position = "top")+geom_text(aes(x=Predicted,y=Actual, label=Freq),data=confusion, colour="black")
        
        p <- p + 
          theme(
            text = element_text(size=15),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            legend.position="none",
            plot.background = element_rect(fill="white")
          )
           
        p
        
      } else{
      p.err.text
     }
    },height=350)
    
    output$Dashboard.Est.Act_Avg.Table<-renderRHandsontable({
      if(!is.null(modelInsights$Dashboard.Est.Act_Avg) && Is.Estimation()){
        Table<-(modelInsights$Dashboard.Est.Act_Avg)
        rhandsontable(Table,rowHeaderWidth=100,maxRows=nrow(Table)) %>% 
          hot_col(colnames(Table),readOnly=TRUE) %>%
          hot_cols(colWidths = 100)
      }
    })

    output$Dashboard.Est.Act_Avg<-renderPlot({
      if(!is.null(modelInsights$Dashboard.Est.Act_Avg) && Is.Estimation()){
        p<-ggplot(modelInsights$Dashboard.Est.Act_Avg,aes(x=factor(modelInsights$Dashboard.Est.Act_Avg[,1]),y=value,color=variable))+
          geom_point(aes(y=modelInsights$Dashboard.Est.Act_Avg[,2],col="Avg Target"),stat="identity",shape=21,size=5,stroke=2)+ #,lwd=3
          geom_point(aes(y=modelInsights$Dashboard.Est.Act_Avg[,3],col="Avg Prediction"),stat="identity",shape=21,size=5,stroke=2) + xlab("Target Variable Bins")+
            theme(
              text = element_text(size=15),
              plot.title = element_text(face = "bold"),
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.position="bottom",
              plot.background = element_rect(fill="white")
            )
        p
      } else{
        p.err.text
      }
    },height=350)
    
    #### Choosing the values ####
    output$input.what.if <- renderRHandsontable({
      input$Model.for.dashboard
      if(!is.null(modelInsights$input_list)) {
        temp <- modelInsights$input_list$input_list
        temp_levels <- modelInsights$input_list$input_list_levels
        colnames(temp) <- c("Variable", "What If")  #Key Drivers
        choices<-NULL
        for(i in 1:length(temp_levels)){
          if(!is.null(temp_levels[[i]])){
            vec<-temp[i,"Variable"]
            choices<-c(choices,levels(currentmodel$SPY.fixed[[vec]])) 
          }} 
        choices<-unique(choices)
        rhandsontable(temp, height=350,maxRows=nrow(temp)) %>% 
          hot_col("Variable", readOnly=TRUE) %>%
          hot_col("What If" ,type = "autocomplete",source = choices ,strict = FALSE) %>% 
          hot_table(stretchH="all")
      }
    })
    
    ### Getting the prediction Results ####
    observe({
      input$Model.for.dashboard
      if(!is.null(currentmodel$necessary.variables.found) && !is.null(input$input.what.if) && !is.null(modelInsights$input_list)){
        modelInsights$What.if.prediction.result<-tryCatch({
          new.data.t <- hot_to_r(input$input.what.if)
          new.data <- matrix(NA,nrow=1,ncol=nrow(new.data.t))
          new.data <- as.data.frame(new.data)
          names(new.data) <- new.data.t[, 1]
          
          if(is.null(currentmodel$levels.table)){
          #convert to numeric/factor depending on the type
          for(i in 1:nrow(new.data.t)){
            if(is.null(modelInsights$input_list$input_list_levels[[i]])) {
              new.data[i] <- as.numeric(new.data.t[i, 2])
            } else {
              new.data[i] <- factor(new.data.t[i, 2], levels=modelInsights$input_list$input_list_levels[[i]])
            }
          }
          } else{ #!is.null(currentmodel$levels.table)
            for(i in 1:nrow(new.data.t)){
              if(is.null(modelInsights$input_list$input_list_levels[[i]])) {
                new.data[i] <- as.numeric(new.data.t[i, 2])
              } else {
                table<-currentmodel$levels.table[[names(new.data)[i]]]
                new.data[i]<-turn.fac.to.num(new.data.t[i, 2],table)#table[which(as.character(table$level)==new.data.t[i, 2]),"num"]
              }
            }
          }
          #make prediction
          if(sum(is.na(new.data))==0) {
            Progression.Vars<-currentmodel$necessary.variables.found
            temp<-input$Model.for.dashboard
            myform<-Progression.Vars$Myform[[temp]]
            model<-Progression.Vars$Models[[temp]]
            if(Is.Classification()==TRUE){
              classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
              What.if.prediction.result<-predict.model(model,myform,new.data,classification.threshold,temp,"Classification")[["prob"]]
            }
              if(Is.Estimation()==TRUE)
              What.if.prediction.result<-predict.model(model,myform,new.data,NULL,temp,"Estimation")[["pred"]]
          } 
          if(sum(is.na(new.data))>0)
            What.if.prediction.result <- NULL
          
          What.if.prediction.result
        }, error=function(err) {
        return(NULL)
        })
      } else{
        modelInsights$What.if.prediction.result<-NULL
      }
    })
    
    
    output$What.if.prediction.result<-renderPlot({
      input$Model.for.dashboard
      if(!is.null(modelInsights$What.if.prediction.result)) {
        if(Is.Classification()==TRUE){
        val <- round(modelInsights$What.if.prediction.result*100, 0)
        val <- min(val, 100)
        val <- max(val, 1)
        col <- colorRampPalette(c("darkred", "darkgreen"))(100)[val]
        lab <- paste(val, "%", sep="")
        temp <- data.frame(x=1, y=val/100)
        
        p <- ggplot(aes(x=x, y=y),data=temp) + geom_bar(fill="#FF3333",colour=col, stat="identity") + 
          annotate("text",label=lab, x=1, y=0.75, colour=col, size=12)+
          theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())+
          theme(plot.title = element_text(size = 15, face = "bold"))
        }
        if(Is.Estimation()==TRUE){
          val <- round(modelInsights$What.if.prediction.result, 2)
          col <- "darkgreen" 
          lab <- paste(val)
          temp <- data.frame(x=1, y=1)
          size<-ifelse(num.of.chars(lab)<=6,12,12*(6/num.of.chars(lab)))
          p <- ggplot() + 
            scale_x_continuous(limits = c(0,2)) + scale_y_continuous(limits = c(0,2)) +
            annotate("text", label=lab, x=1, y=1, colour=col, size=size) +
            theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())
        }
        p
      } else{
        p.err.text
      }
    }, height=350)
    
    #### checking the trend of the prediction results while changing the values of some variable #####
    output$Seq.table <- renderRHandsontable({
      input$Model.for.dashboard
      if(!is.null(modelInsights$input_list)) {
        temp <- modelInsights$input_list$input_list
        temp_levels <- modelInsights$input_list$input_list_levels
        colnames(temp) <- c("Variable", "Start Value")
        temp$Evaluate <- FALSE
        temp$Evaluate[1] <- TRUE
        temp$`End Value` <- ""
        temp$Step <- ""
        temp = temp[, c("Variable", "Evaluate", "Start Value", "End Value", "Step")]
        rhandsontable(temp, height=350,maxRows=nrow(temp)) %>% 
          hot_col("Variable", readOnly=TRUE) %>% 
          hot_table(stretchH="all")
      }
    })
    
    ## Getting the prediction Results ####
    observe({
      input$Model.for.dashboard
      if(!is.null(currentmodel$necessary.variables.found) && !is.null(input$Seq.table) && !is.null(modelInsights$input_list)){
        modelInsights$seq.prediction.result<-tryCatch({
          new.data.t <- hot_to_r(input$Seq.table)
          #do analysis only if exactly one variable is selected
          if (sum(new.data.t$Evaluate)==1) {
            #detect row
            ind <- which(new.data.t$Evaluate)
            xname <- new.data.t$`Variable`[ind]
            #create range values for analysis
            if (is.null(modelInsights$input_list$input_list_levels[[ind]])) {
              #if the variable is numeric, then take values from the table
              #some default values will be taken if no value in the table
              val_start <- as.numeric(new.data.t$`Start Value`[new.data.t$Evaluate])
              #if (is.na(val_start)) val_start <- 0
              val_end <- as.numeric(new.data.t$`End Value`[new.data.t$Evaluate])
              #if (is.na(val_end)) val_end <- val_start
              val_step <- as.numeric(new.data.t$Step[new.data.t$Evaluate])
              #if (is.na(val_step)) val_step <- 1
              if (!is.na(val_start)) {  # && !is.na(val_end) && !is.na(val_step)
                if(!is.na(val_end) && !is.na(val_step)){
                  xvalues <- seq(val_start, val_end, by=val_step)
                } else{
                  xvalues <- val_start #seq(val_start, val_end, by=val_step)
                }
              } else {
                xvalues <- c()
              }
            } else {
              #if the variable is factor, then take levels (but 10 max)
              xvalues <- modelInsights$input_list$input_list_levels[[ind]]
            }
            
            #if previous step was successful, we may build new dataset for making predictions
            if (length(xvalues)>0) {
              new.data <- matrix(NA,nrow=length(xvalues),ncol=nrow(new.data.t))
              new.data <- as.data.frame(new.data)
              names(new.data) <- new.data.t[, 1]
              
              #take other values from the table
              #and convert to numeric/factor depending on the type
              for(i in 1:ncol(new.data)){
                if(is.null(modelInsights$input_list$input_list_levels[[i]])) {
                  if (i==ind) {
                    new.data[i] <- as.numeric(xvalues)
                  } else {
                    new.data[i] <- as.numeric(new.data.t[i, 3])
                  }
                } else {# !is.null(modelInsights$input_list$input_list_levels[[i]])
                  if(is.null(currentmodel$levels.table)){
                    if (i==ind) {
                      new.data[i] <- factor(xvalues, levels=modelInsights$input_list$input_list_levels[[i]])
                    } else {
                      new.data[i] <- factor(new.data.t[i, 3], levels=modelInsights$input_list$input_list_levels[[i]])
                    }
                  } else{# !is.null(currentmodel$levels.table)
                    table<-currentmodel$levels.table[[names(new.data)[i]]]
                    if (i==ind) {
                      new.data[i] <-turn.fac.to.num(xvalues,table)#table[which(as.character(table$level) %in% xvalues),"num"]
                    } else {
                      new.data[i] <- turn.fac.to.num(new.data.t[i, 3],table)#table[which(as.character(table$level)==new.data.t[i, 3]),"num"]
                    }
                  }
                }
              }
              
              #make prediction
              if(sum(is.na(new.data))==0) {
                Progression.Vars<-currentmodel$necessary.variables.found
                temp<-input$Model.for.dashboard
                myform<-Progression.Vars$Myform[[temp]]
                model<-Progression.Vars$Models[[temp]]
                if(Is.Classification()==TRUE){
                  classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
                  prediction<-predict.model(model,myform,new.data,classification.threshold,temp,"Classification")[["prob"]]
                }
                if(Is.Estimation()==TRUE)
                  prediction<-predict.model(model,myform,new.data,NULL,temp,"Estimation")[["pred"]]
                #########
                #save prediction with x values
                ind<-which(new.data.t$Evaluate)
                if (!is.null(modelInsights$input_list$input_list_levels[[ind]])){
                  xvalues<-xvalues[order(prediction)]
                  prediction<-prediction[order(prediction)]
                }
                seq.prediction.result<-list(seq.prediction.result=prediction, xvalues=xvalues, xname=xname)
              } else {
                seq.prediction.result <- NULL
              }
            } else {
              seq.prediction.result <- NULL
            }
          } else{
            seq.prediction.result <- NULL
          }
          
          seq.prediction.result
        }, error=function(err) {
          return(NULL)
        })
      } else{
        modelInsights$seq.prediction.result<-NULL
      }
    })
    

    output$Seq.table.plot<-renderPlot({
      input$Model.for.dashboard
      if(!is.null(modelInsights$seq.prediction.result)) {
        seq<-modelInsights$seq.prediction.result
        if(Is.Classification()==TRUE){
          temp <- data.frame(x=as.character(seq$xvalues), 
                             y=round(seq$seq.prediction.result*100, 0),
                             stringsAsFactors = FALSE)
          p <- ggplot(data=temp, aes(x=x, y=y)) + geom_bar(stat="identity", fill="blue") + 
            xlab(seq$xname) +coord_flip()+
            scale_x_discrete(labels=function(x) str_wrap(as.character(gsub("_"," ",as.character(x)))),limits=as.character(seq$xvalues)) +
            ylab("Probability of 1") + scale_y_continuous(limits=c(0, 100))
        }
        if(Is.Estimation()==TRUE){
          temp <- data.frame(x=as.character(seq$xvalues), 
                             y=round(seq$seq.prediction.result, 2),
                             stringsAsFactors = FALSE)
          p <- ggplot(data=temp, aes(x=x, y=y)) + geom_bar(stat="identity", fill="blue") + 
            xlab(seq$xname) +coord_flip()+
            scale_x_discrete(labels=function(x) str_wrap(as.character(gsub("_"," ",as.character(x)))),limits=as.character(seq$xvalues))
        }
        p<-p+ theme(
          text = element_text(size=15),
          plot.title = element_text(face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(colour = "lightgray"),
          axis.text.x = element_text(angle = 330),
          plot.background = element_rect(fill="white")
        )
        p
      } else{
        p.err.text
      }
    }, height=function(){
       temp<-modelInsights$seq.prediction.result
       if(is.null(temp)){
         return(350)
       } else{
         return(max(350,length(temp$xvalues)*15))
       }})
    
    
    #####Tab Name #vars_vs_target
    output$Vars.vs.target.Correlation.table<-renderRHandsontable({
      if(!is.null(modelInsights$Vars.vs.target.Correlation.table)){
        Correlation.table<-modelInsights$Vars.vs.target.Correlation.table
        Correlation.table[,"Variable"]<-str_wrap(as.character(gsub("_"," ",as.character(Correlation.table[,"Variable"]))), width = 35)
      rhandsontable(Correlation.table,maxRows=nrow(Correlation.table),height=350) %>%
        hot_col(colnames(Correlation.table), readOnly=TRUE) %>% 
        hot_table(stretchH="all")
    }})
    
    output$Vars.vs.target.Correlation.plot<-renderPlot({
      if(!is.null(modelInsights$Vars.vs.target.Correlation.table)){
        Correlation.table<-modelInsights$Vars.vs.target.Correlation.table
        Correlation.table$colour<-ifelse(Correlation.table$Correlation>0,"Pos", "Neg")

        if(nrow(Correlation.table)>20){
          Correlation.table<-Correlation.table[order(-abs(Correlation.table$Correlation)),][1:20,]
          y.title<-"Correlation (Top 20)"
        } else{
          y.title<-"Correlation"
        }
        
        p<-ggplot(Correlation.table, aes(x=Variable, y=Correlation, fill=colour))+
          labs(x="Variables",y=y.title) + geom_bar(stat="identity") +  coord_flip()+
          scale_x_discrete(labels=function(x) str_wrap(as.character(gsub("_"," ",as.character(x)))),limits = Correlation.table$Variable[order(abs(Correlation.table$Correlation))])+
          scale_fill_manual(values=c("Pos"="darkgreen", "Neg"="darkred"))+ggtitle("Variables vs Target Correlation")+
          theme(
            text = element_text(size=15),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.position="none",
            plot.background = element_rect(fill="white")
          )
        p
      } else{
        p.err.text
      }
    }, height=380)
    
    output$Vars.vs.target.Correlation.plot.zoom<-renderPlot({
      if(!is.null(modelInsights$Vars.vs.target.Correlation.table)){
        Correlation.table<-modelInsights$Vars.vs.target.Correlation.table
        Correlation.table$colour<-ifelse(Correlation.table$Correlation>0,"Pos", "Neg")

        p<-ggplot(Correlation.table, aes(x=Variable, y=Correlation, fill=colour))+
          labs(x="Variables",y="Correlation") + geom_bar(stat="identity") +  coord_flip()+
          scale_x_discrete(labels=function(x) str_wrap(as.character(gsub("_"," ",as.character(x)))),limits = Correlation.table$Variable[order(abs(Correlation.table$Correlation))])+
          scale_fill_manual(values=c("Pos"="darkgreen", "Neg"="darkred"))+ggtitle("Variables vs Target Correlation")+
          theme(
            text = element_text(size=15),
            plot.title = element_text(face = "bold"),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.position="none",
            plot.background = element_rect(fill="white")
          )
        p
      } else{
        p.err.text
      }
    }, height=function(){
      temp<-modelInsights$Vars.vs.target.Correlation.table
      if(is.null(temp)){
        return(380)
      } else{
        return(max(380,nrow(temp)*15))
      }})
    
    
  
    ##########Predict and Export#################
    observe({
      if(!is.null(currentmodel$necessary.variables.found)){
        showTab(inputId = "Front", target = "Predict and Export")
      } else{
        hideTab(inputId = "Front", target = "Predict and Export")
      }
    })
    ###########################################
   ##########################################################
   ## Loading the Validation file
   ##########################################################
   
    observeEvent(input$PredictExportSubmit.CV, {
      SPY<-NULL
      tryCatch({
        if(input$DataBase.CV.type=="Flat"){
          inFile <- input$datafileCrossValid
          if (is.null(inFile)) {
            SPY <- NULL
          } else {
            SPY <- import(inFile$datapath,format =split(inFile$name))
          }}
        if(input$DataBase.CV.type=="Big Files(.csv)"){
          SPY<-fread(file.choose(),header = T, sep =',',data.table=FALSE) 
        }
        if(input$DataBase.CV.type=="ODBC"){
          if(is.null(input$ODBC_CV.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$ODBC_CV.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            close(myconn)
          }}
        if(input$DataBase.CV.type=="MS SQL"){
          if(is.null(input$MS_sql_CV.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$MS_sql_CV.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            close(myconn)
          }}
        if(input$DataBase.CV.type=="MySQL"){
          if(is.null(input$Mysql_CV.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Mysql_CV.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            close(myconn)
          }}
        if(input$DataBase.CV.type=="Oracle"){
          if(is.null(input$Oracle_CV.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Oracle_CV.table) 
            drv <-JDBC("oracle.jdbc.OracleDriver",
                       classPath=Table[Table$Parameters=="ojdbc.jar Path","Insert"]," ") 
            myconn<- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:orcl",Table[Table$Parameters=="USERID","Insert"],
                               Table[Table$Parameters=="PASSWORD","Insert"]) 
            SPY <-dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            dbDisconnect(myconn) 
          }}
        if(input$DataBase.CV.type=="Amazon Redshift"){
          if(is.null(input$Amazon_Redshift_CV.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Amazon_Redshift_CV.table) 
            drv <- dbDriver(Table[Table$Parameters=="DRIVER","Insert"])   
            myconn <- dbConnect(drv, host=Table[Table$Parameters=="Host","Insert"], 
                                dbname=Table[Table$Parameters=="DATABASE","Insert"],
                                port=Table[Table$Parameters=="PORT","Insert"],
                                user=Table[Table$Parameters=="USER","Insert"],
                                password=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])
            dbDisconnect(myconn)
          }}  
        if(input$DataBase.CV.type=="Google Big Query"){
          if(is.null(input$Google_Big_Query_CV.table)){
            SPY <- NULL
          } else{
            gar_auth_service(
              json_file = input$json.file.CV$datapath,#"API Project-xxxxxxxxxxxx.json",
              scope = "https://www.googleapis.com/auth/bigquery"
            )
            Table <- hot_to_r(input$Google_Big_Query_CV.table) 
            SPY <- bqr_query(projectId = Table[Table$Parameters=="ProjectID","Insert"],query =Table[Table$Parameters=="Query","Insert"],
                             datasetId = Table[Table$Parameters=="DatasetId","Insert"],useLegacySql = input$useLegacySql.CV)  
          }} 
        
      } , error=function(err) {
        CV.file.loading <<- paste("Error occured - File was not Loaded")
      })
      
      if(!is.null(SPY) && !is.character(SPY) && nrow(SPY)>0){
        CV.file.loading<<-""
        SPY<-as.data.table(SPY)
        ##checking if we have longitude and latitude, and make from them extra variables
        if(all(c('latitude', 'longitude') %in% tolower(names(SPY)))){
          for(i in 1:8){
            SPY[,paste0("latitude_",i)]<-round(SPY[[which(tolower(names(SPY))=="latitude")]],i)
            SPY[,paste0("longitude",i)]<-round(SPY[[which(tolower(names(SPY))=="longitude")]],i)
          }
        }
        ##Converting integer64\Logical to numeric
        ind<-sapply(names(SPY), function(x) {class(SPY[[x]])[1] %in% c("logical","integer64")}) 
        for(i in names(SPY)[ind]){SPY[,i]<-as.numeric(SPY[[i]])}  
        
        ##Converting character to Factor
        ind<-sapply(names(SPY), function(x) {"character" %in% class(SPY[[x]])})
        for(i in names(SPY)[ind]){SPY[,i]<-as.factor(SPY[[i]])} 
        
        ###Class Date####
        ##Adding columns concerning dates
        ind1<-sapply(names(SPY), function(x) {"Date" %in% class(SPY[[x]])})
        ind2<-sapply(names(SPY), function(x) {"POSIXct" %in% class(SPY[[x]])})
        if(input$Dates.as.parts.CV=="No"){
          for(i in names(SPY)[ind1]){SPY[,i]<-as.factor(SPY[[i]])}   
          for(i in names(SPY)[ind2]){SPY[,i]<-as.factor(SPY[[i]])} 
        } else{ 
          for(i in names(SPY)[ind1]){
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_week")]<-as.factor(format(SPY[[i]],"%a"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%b"))
            SPY[,paste0(i,"_quarter")]<-as.factor(quarters(SPY[[i]]))
            SPY[,paste0(i,"_month_day")]<-as.factor(format(SPY[[i]],"%b %d"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }
          for(i in names(SPY)[ind2]){
            SPY[,paste0(i,"_sec")]<-as.factor(format(SPY[[i]],"%S"))
            SPY[,paste0(i,"_min")]<-as.factor(format(SPY[[i]],"%M"))
            SPY[,paste0(i,"_hour")]<-as.factor(format(SPY[[i]],"%H"))
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%m"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }  
        }#End of Dates.as.parts.CV if 
        
        ##Text
        if(!is.null(textAnalysis$text.analysis.for.export.saved)){
          Pos.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Pos.Exp.table"]]
          Neg.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Neg.Exp.table"]]
          Other.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Other.Exp.table"]]
          table<-textAnalysis$text.analysis.for.export.saved[["table"]]
          SPY<-turn.text.to.data(SPY,Pos.Exp.table,Neg.Exp.table,Other.Exp.table,table) 
        }
        
        colnames(SPY)<-make.names(colnames(SPY),unique=TRUE) 
      } else{#End of !is.null(SPY) && !is.character(SPY) && ncol(SPY)>0
        SPY<-NULL
        CV.file.loading <<- paste("No Data")
      }
      currentmodel$uploaded.dataCrossValid <- SPY 
    })
    
   ##Loading Text
   output$CV.file.loading <- renderText({
     input$PredictExportSubmit.CV
     CV.file.loading
   })
   
   ##########################################################
   
    output$choose_Method_for_CV.ui<-renderUI({
      if(!is.null(currentmodel$Used.Models)){
        models.used<-currentmodel$Used.Models[["models.used"]]
        best.models.used<-currentmodel$Used.Models[["best.models.used"]]
        models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
        best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
        
        choices<-models.used
        if(length(models.used)>1)
          choices<-c(choices,"Ensemble all models")  
        if(length(models.used.weights)>1)
          choices<-c(choices,"Ensemble all models weighting")  
        if(length(best.models.used)>1)
          choices<-c(choices,"Ensemble best models")
        if(length(best.models.used.weights)>1)
          choices<-c(choices,"Ensemble best models weighting")
        
        if(length(models.used)>1)
          choices<-c(choices,"All Models")  
        ###In case we loaded a model
      isolate({
      if(!is.null(currentmodel$Loaded.choose_Method_for_CV)){
        selected<-currentmodel$Loaded.choose_Method_for_CV
        currentmodel$Loaded.choose_Method_for_CV<-NULL
      } else{
        selected<-NULL
      } 
      })
      selectInput("choose_Method_for_CV", 
                  label = "Model",
                  choices=choices,selected=selected)
    }})

   ##################PredictExport.Validation.Data and Table##########
   CV.list<-reactive({   
     if(!is.null(currentmodel$uploaded.dataCrossValid) && !is.null(currentmodel$necessary.variables.found)){
       output.var<-currentmodel$params.to.assign[["output.var"]] 
       vars.not.include<-currentmodel$params.to.assign[["vars.not.include"]] 
       chosen.not.fix<-currentmodel$params.to.assign[["chosen.not.fix"]]
       not.fix.vars<-chosen.not.fix[["not.fix"]]
       Imputation.Value<-currentmodel$Settings_Table[["Imputation Value"]]
       ###
       CV.list<-list()
       Data<-currentmodel$uploaded.dataCrossValid
       ##without na target
       if(output.var %in% names(Data)){
         ###Check if target is valid
         target<-Data[[output.var]]
         if(!is.numeric(target)){
           warn<-"Target should be numeric"
         } else{
           if(currentmodel$Method.saved=="Classification"){
             target<-target[cleaned.places(target)]
             uni<-unique(target)
             if(!setequal(uni,c(0,1))==TRUE){
               warn<-"Target's unique values should be 0,1"
             } else{
               warn<-""
             }
           } else{#Estimation
             warn<-""
           }
         }
         if(warn!=""){
           showModal(modalDialog(
             title = "",
             size="s",
             easyClose = TRUE,
             warn
           ))
           return(NULL)
         }
         ########################
         n1<-nrow(Data)
         Data<-Data[cleaned.places(Data[[output.var]]),]
         if((n1-nrow(Data))>0){
           if(nrow(Data)==0){
             CV.list[["Keeping.complete.cases.in.target"]]<-"Target Vairiable has no complete values"
             return(NULL)
           } else{
             CV.list[["Keeping.complete.cases.in.target"]]<-"Target Vairiable non complete values were droped"
           } 
         } else{
           CV.list[["Keeping.complete.cases.in.target"]]<-NULL
         }
       } else{
         CV.list[["Keeping.complete.cases.in.target"]]<-NULL
       }

       if(currentmodel$Method.used=="Classification")
         classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
       ####Keeping the original data for output
       Original.SPY.Validation<-Data
       SPY.Validation<-Data
       ###Checking if the variables have the same class####
       mutual.names<-names(SPY.Validation)[names(SPY.Validation) %in% names(currentmodel$uploaded.data)]
       if(length(mutual.names)>0){
         class.data<-sapply(mutual.names,function(x){return(class(currentmodel$uploaded.data[[x]]))})
         class.newdata<-sapply(mutual.names,function(x){return(class(SPY.Validation[[x]]))})
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
               SPY.Validation[,var[i]]<-as.numeric(paste(SPY.Validation[[var[i]]]))
             if(Type.requested[i]=="factor")
               SPY.Validation[,var[i]]<-as.factor(SPY.Validation[[var[i]]])
           }
         }
       }
       #####################################################
       ##Add ratio combination as dummies
       if(currentmodel$ratio.as.dummies.inside[["Add"]]=="Yes"){
         char<-currentmodel$ratio.as.dummies.inside[["char"]]
         is.interval<-currentmodel$ratio.as.dummies.inside[["is.interval"]]
         err<-New_Data_prediction_error(char,SPY.Validation)
         if(is.null(err)){
           table<-currentmodel$ratio.as.dummies.inside[["table"]]
           Data<-SPY.Validation
           SPY.Validation<-turn.ratio.combination.to.data(Data,table,is.interval)
         }}
       #######################################################
       ######Turn Factors to Numeric########
       if(!is.null(currentmodel$levels.table)){
         temp<-currentmodel$levels.table
         for(i in names(temp)){
           if(i %in% names(SPY.Validation))
             SPY.Validation[,i]<-turn.fac.to.num(SPY.Validation[[i]],temp[[i]])
         }
       }
       
       ###########################################
       if(!is.null(currentmodel$Create.is.na.vars))
         SPY.Validation<-Create.is.na.vars.for.prediction(SPY.Validation,currentmodel$Create.is.na.vars)
       #######Clean the Data
       SPY.Validation<-Fix.Data(SPY.Validation,output.var,vars.not.include,chosen.not.fix,Imputation.Value,OutliersSettings$Outliers.settings.saved) 
       ####################################
       Progression.Vars <- currentmodel$necessary.variables.found
       ##########################################
       if(!is.null(currentmodel$necessary.variables.found.for.first.layer))
         SPY.Validation<-First.layer.prediction(SPY.Validation,currentmodel$necessary.variables.found.for.first.layer,currentmodel$Used.Models.for.first.layer)
       ###########################################################################################################
       
       ###performing wilcoxon test to check whether
       ###each numeric variable has the same distribution in both data 
       ### SPY.Validation && currentmodel$SPY.fixed
       CV.list[["Vars.with.diff.distribution"]]<-tryCatch({
         diff.distribution<-sapply(names(currentmodel$SPY.fixed),function(x){
           return(wilcox.test(currentmodel$SPY.fixed[[x]],SPY.Validation[[x]],alternative ="two.sided")$p.value<0.05)
         })
         
         diff.distribution<-which(diff.distribution==TRUE)
         temp.names<-names(currentmodel$SPY.fixed)[diff.distribution]
         table<-NULL
         for(i in temp.names){
           table<-rbind(table,data.frame("Variable"=i,"data mean"=round(mean(currentmodel$SPY.fixed[[i]],na.rm=TRUE),3),
                                         "new data mean"=round(mean(SPY.Validation[[i]],na.rm=TRUE),3),check.names=F))
         }
         
         table   
       }, error=function(err){
         return(NULL) 
       })
       ############
       temp.data<-list() 
       temp.data.b<-list()   
       basic.names<-names(Original.SPY.Validation)
       NA.table.CV<-NULL
       CV.list[["basic.names"]]<-basic.names
       prediction.err.CV<-list()
       models.used<-currentmodel$Used.Models[["models.used"]]
       best.models.used<-currentmodel$Used.Models[["best.models.used"]]
       models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
       best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
       all.models.for.cv<-NULL
       ############
       for(i in models.used){
         tryCatch({
           loop.vars<-Progression.Vars$Vars[[i]] 
           basic.vars<-base.vars(loop.vars,currentmodel$SPY.fixed)
           err<-New_Data_prediction_error(basic.vars,SPY.Validation)
           if(is.null(err)){
             temp.not.fix<-not.fix.vars[not.fix.vars %in% basic.vars]
             not.fix.ind<-sapply(Original.SPY.Validation[,temp.not.fix,with=FALSE],function(x){
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
             if(currentmodel$Method.used=="Classification"){
               prob<-predict.model(model,myform,SPY.Validation,classification.threshold,i,"Classification")[["prob"]]  
               prob<-na.assign(prob,not.fix.ind)
               NA.table.CV<-rbind(NA.table.CV,data.frame("Method"=i,"Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))  ##11
               if(!all(is.na(prob))){
                 all.models.for.cv<-c(all.models.for.cv,i)
                 temp.data[[i]]<-prob
                 if(i %in% best.models.used)
                   temp.data.b[[i]]<-prob
                 prediction<- ifelse(prob>classification.threshold, 1, 0)
                 Original.SPY.Validation[,paste0(gsub(" ",".",i),"_predict_probability")]<-prob
                 Original.SPY.Validation[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
               }} else{
                 prediction<-predict.model(model,myform,SPY.Validation,NULL,i,"Estimation")[["pred"]] 
                 prediction<-na.assign(prediction,not.fix.ind)
                 NA.table.CV<-rbind(NA.table.CV,data.frame("Method"=i,"Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
                 if(!all(is.na(prediction))){
                   all.models.for.cv<-c(all.models.for.cv,i)
                   temp.data[[i]]<-prediction
                   if(i %in% best.models.used)
                     temp.data.b[[i]]<-prediction
                   Original.SPY.Validation[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
                   Original.SPY.Validation[,paste0(gsub(" ",".",i),"_predict_lower_limit")]<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
                   Original.SPY.Validation[,paste0(gsub(" ",".",i),"_predict_upper_limit")]<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
                 }}
           } else{
             prediction.err.CV[[i]]<-err
           }
         }, error=function(err){})
       }#End of loop
       CV.list[["prediction.err.CV"]]<-prediction.err.CV
       
       if(length(temp.data)>1){
         if(currentmodel$Method.used=="Classification"){
           tryCatch({
             prob<-apply(as.data.frame(temp.data),1,combined_prob)   
             prediction<- ifelse(prob>classification.threshold, 1, 0)
             NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble all models","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
             Original.SPY.Validation$Ensemble.all.models_predict_probability<-prob
             Original.SPY.Validation$Ensemble.all.models_predict_response<-prediction
             all.models.for.cv<-c(all.models.for.cv,"Ensemble all models")
           }, error=function(err){})
           ##################################
           if(!is.null(models.used.weights)){
             tryCatch({  
               prob<-weighted.prob(temp.data,models.used.weights)
               prediction<- ifelse(prob>classification.threshold, 1, 0)
               NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble all models weighting","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
               Original.SPY.Validation$Ensemble.all.models.weighting_predict_probability<-prob
               Original.SPY.Validation$Ensemble.all.models.weighting_predict_response<-prediction
               all.models.for.cv<-c(all.models.for.cv,"Ensemble all models weighting")
             }, error=function(err){})
           }
         } else{
           tryCatch({
             prediction<-apply(as.data.frame(temp.data),1,function(x){median(x,na.rm=TRUE)})
             NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble all models","Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
             Original.SPY.Validation$Ensemble.all.models_predict_response<-prediction
             Original.SPY.Validation$Ensemble.all.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
             Original.SPY.Validation$Ensemble.all.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
             all.models.for.cv<-c(all.models.for.cv,"Ensemble all models")
           }, error=function(err){})
         }
       }
       if(length(temp.data.b)>1){
         if(currentmodel$Method.used=="Classification"){
           tryCatch({
             prob<-apply(as.data.frame(temp.data.b),1,combined_prob)
             prediction<- ifelse(prob>classification.threshold, 1, 0)
             NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble best models","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
             Original.SPY.Validation$Ensemble.best.models_predict_probability<-prob
             Original.SPY.Validation$Ensemble.best.models_predict_response<-prediction
             all.models.for.cv<-c(all.models.for.cv,"Ensemble best models")
           }, error=function(err){})
           ##################################
           tryCatch({  
             prob<-weighted.prob(temp.data.b,best.models.used.weights)
             prediction<- ifelse(prob>classification.threshold, 1, 0)
             NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble best models weighting","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
             Original.SPY.Validation$Ensemble.best.models.weighting_predict_probability<-prob
             Original.SPY.Validation$Ensemble.best.models.weighting_predict_response<-prediction
             all.models.for.cv<-c(all.models.for.cv,"Ensemble best models weighting")
           }, error=function(err){})
         } else{
           tryCatch({
             prediction<-apply(as.data.frame(temp.data.b),1,function(x){median(x,na.rm=TRUE)})
             NA.table.CV<-rbind(NA.table.CV,data.frame("Method"="Ensemble best models","Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
             Original.SPY.Validation$Ensemble.best.models_predict_response<-prediction
             Original.SPY.Validation$Ensemble.best.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
             Original.SPY.Validation$Ensemble.best.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
             all.models.for.cv<-c(all.models.for.cv,"Ensemble best models")
           }, error=function(err){})
         }
       }
       
       CV.list[["NA.table.CV"]]<-NA.table.CV
       CV.list[["CV.Data"]]<-Original.SPY.Validation
       currentmodel$all.models.for.cv<-all.models.for.cv
       
       if(output.var %in% names(Original.SPY.Validation)){
         info.table<-NULL
         Criterion<-as.character(currentmodel$Settings_Table[,"Criterion"])
         ###
         ind1<-1:round((1/3)*nrow(Original.SPY.Validation))
         ind1<-ind1[ind1>=1]
         ind2<-(round((1/3)*nrow(Original.SPY.Validation))+1):round((2/3)*nrow(Original.SPY.Validation))
         ind2<-ind2[ind2>=(round((1/3)*nrow(Original.SPY.Validation))+1)]
         ind3<-(round((2/3)*nrow(Original.SPY.Validation))+1):nrow(Original.SPY.Validation)
         ind3<-ind3[ind3>=(round((2/3)*nrow(Original.SPY.Validation))+1)]
         row.divission<-list(ind1=ind1,ind2=ind2,ind3=ind3)
         
         if(currentmodel$Method.used=="Classification"){
           temp.table<-NULL
           Multiple.ROC.cv<-data.frame(target=Original.SPY.Validation[[output.var]])
           for(i in all.models.for.cv){
             prob<-Original.SPY.Validation[[paste0(gsub(" ",".",i),"_predict_probability")]]
             prediction<-ifelse(prob>classification.threshold, 1, 0)
             ##For ROC
             Multiple.ROC.cv[,i]<-prob
             ##Confusion table
             Tab<-CV.Table(Original.SPY.Validation[[output.var]], prediction)  
             colnames(Tab) <- paste("Predicted", colnames(Tab), sep=" ")
             rownames(Tab) <- paste("Actual", rownames(Tab), sep=" ")
             Tab.percent<-sapply(1:ncol(Tab),function(x){return(round(Tab[,x]/sum(Tab[,x]),round.digits))})
             colnames(Tab.percent)<-colnames(Tab)   
             rownames(Tab.percent)<-c("Upper Table in %",rep(" ",(nrow(Tab.percent)-1)))
             Table<-rbind(Tab,Tab.percent)
             temp.table[[i]] <- Table
             ##
             model_summary<-get.Results(i,Original.SPY.Validation,row.divission, 
                                        prob,output.var,NA,Criterion,"Classification",classification.threshold)
             if(!is.null(model_summary)){
               model_summary<-data.frame('Method'=i,model_summary,check.names=F)
               info.table<-rbind(info.table, model_summary)
               info.table<-as.data.frame(info.table)
             }
           }
           CV.list[["confusion.table.cv"]]<-temp.table
           if(!is.null(info.table))
             info.table<-info.table[order(-info.table[,Criterion]),]  
           CV.list[["summary_table.cv"]]<-info.table
           CV.list[["Multiple.ROC.cv"]]<-Multiple.ROC.cv
         } else{#Estimation
           Residuals.table.cv<-NULL
           for(i in all.models.for.cv){
             tryCatch({
               prediction<-Original.SPY.Validation[[paste0(gsub(" ",".",i),"_predict_response")]]
               Residuals.table.cv<-rbind(Residuals.table.cv,data.frame("Model"=rep(i,length(prediction)),
                                                                       "Norm_Residuals"=((Original.SPY.Validation[[output.var]]-prediction)-mean(Original.SPY.Validation[[output.var]]-prediction,na.rm=TRUE))/sd(Original.SPY.Validation[[output.var]]-prediction,na.rm=TRUE),
                                                                       "Target"=Original.SPY.Validation[[output.var]],"Prediction"=prediction))
               loop.vars<-ifelse(i %in% names(Progression.Vars$Vars),Progression.Vars$Vars[[i]],NA)
               model_summary<-get.Results(i,Original.SPY.Validation,row.divission,
                                          prediction,output.var,loop.vars,Criterion,"Estimation",classification.threshold)
               if(!is.null(model_summary)){
                 info.table<-rbind(info.table,data.frame("Method"=i,model_summary,check.names=F)) 
                 info.table<-as.data.frame(info.table)
               }
             }, error=function(err){})
           }

           if(!is.null(info.table)){
             if(Criterion %in% c('MAE','Norm abs Difference','Abs Sum Difference','RMSE')){
               info.table<-info.table[order(info.table[,Criterion]),] 
             } else{
               info.table<-info.table[order(-info.table[,Criterion]),] 
             }}
           CV.list[["summary_table.est.cv"]]<-info.table
           CV.list[["Residuals.table.cv"]]<-Residuals.table.cv
         }
       }
       return(CV.list)
     } else{
       return(NULL)
     }
   })  
    observeEvent(input$PredictExportSubmit.CV,{
      ##Reset Export text
      CV.last_action <<-""
      deciles.accuracy.cv.last_action<<-""   
      deciles.avg.cv.last_action<<-""
      ###Reset Accuracy Deciles and Avg Deciles outputs
      currentmodel$Accuracy.deciles.table.cv<-NULL
      currentmodel$Avg.deciles.table.cv<-NULL
      ###
      if(length(CV.list())>0){
        Progression.Vars <- currentmodel$necessary.variables.found
        currentmodel$Keeping.complete.cases.in.target.CV<-CV.list()[["Keeping.complete.cases.in.target"]]
        if(input$choose_Method_for_CV!="All Models"){
        method<-input$choose_Method_for_CV
        vars<-Progression.Vars$Vars[[method]]
        vars<-base.vars(vars,currentmodel$SPY.fixed)
        ##Vars.with.diff.distribution.CV
        Vars.with.diff.distribution.CV<-CV.list()[["Vars.with.diff.distribution"]]
        temp<-Vars.with.diff.distribution.CV[Vars.with.diff.distribution.CV$Variable %in% vars,]
        if(is.null(temp) || nrow(temp)==0){
          currentmodel$Vars.with.diff.distribution.CV<-NULL
        } else{
          currentmodel$Vars.with.diff.distribution.CV<-temp
        }
        ##prediction.err.CV
        prediction.err.CV<-CV.list()[["prediction.err.CV"]]
        prediction.err.CV[!names(prediction.err.CV) %in% method]<-NULL
        if(is.null(prediction.err.CV) || length(prediction.err.CV)==0){
          currentmodel$prediction.err.CV<-NULL
        } else{
          currentmodel$prediction.err.CV<-prediction.err.CV
        }
        ##NA table
        NA.table.CV<-CV.list()[["NA.table.CV"]]
        currentmodel$NA.table.CV<-NA.table.CV[NA.table.CV$Method==method,]
        ##CV Data
        basic.names<-CV.list()[["basic.names"]]
        CV.Data<-CV.list()[["CV.Data"]]
        if(currentmodel$Method.used=="Classification"){
          ##CV Data
          basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_probability"),paste0(gsub(" ",".",method),"_predict_response"))
          currentmodel$CV.Data<-CV.Data[,names(CV.Data)[names(CV.Data) %in% basic.names],with=FALSE]
          ##confusion.table.cv
          temp.table<-CV.list()[["confusion.table.cv"]]
          currentmodel$confusion.table.cv<-temp.table[method]
          ##summary_table.cv
          info.table<-CV.list()[["summary_table.cv"]]
          currentmodel$summary_table.cv<-info.table[info.table$Method==method,]
          ##Multiple.ROC.cv
          Multiple.ROC.cv<-CV.list()[["Multiple.ROC.cv"]]
          currentmodel$Multiple.ROC.cv<-Multiple.ROC.cv[,names(Multiple.ROC.cv)[names(Multiple.ROC.cv) %in% c("target",method)],drop=FALSE]
        } else{ #Estimation   
          ##CV Data
          basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_response"),paste0(gsub(" ",".",method),"_predict_lower_limit"),
                         paste0(gsub(" ",".",method),"_predict_upper_limit"))
          currentmodel$CV.Data<-CV.Data[,names(CV.Data)[names(CV.Data) %in% basic.names],with=FALSE]
          ##summary_table.est.cv
          info.table<-CV.list()[["summary_table.est.cv"]]
          currentmodel$summary_table.est.cv<-info.table[info.table$Method==method,]
          ##Residuals.table.cv
          Residuals.table.cv<-CV.list()[["Residuals.table.cv"]]
          currentmodel$Residuals.table.cv<-Residuals.table.cv[Residuals.table.cv$Model==method,]
        }
        } else{ #input$choose_Method_for_CV=="All Models"
        vars<-unique(unlist(Progression.Vars$Vars))
        vars<-base.vars(vars,currentmodel$SPY.fixed)
        ##Vars.with.diff.distribution.CV
        Vars.with.diff.distribution.CV<-CV.list()[["Vars.with.diff.distribution"]]
        temp<-Vars.with.diff.distribution.CV[Vars.with.diff.distribution.CV$Variable %in% vars,]
        if(is.null(temp) || nrow(temp)==0){
          currentmodel$Vars.with.diff.distribution.CV<-NULL
        } else{
          currentmodel$Vars.with.diff.distribution.CV<-temp
        }
        ##prediction.err.CV
        prediction.err.CV<-CV.list()[["prediction.err.CV"]]
        if(is.null(prediction.err.CV) || length(prediction.err.CV)==0){
          currentmodel$prediction.err.CV<-NULL
        } else{
          currentmodel$prediction.err.CV<-prediction.err.CV
        }
        ##NA table
        currentmodel$NA.table.CV<-CV.list()[["NA.table.CV"]]
        ##CV Data
        currentmodel$CV.Data<-CV.list()[["CV.Data"]]
        if(currentmodel$Method.used=="Classification"){
          ##confusion.table.cv
          currentmodel$confusion.table.cv<-CV.list()[["confusion.table.cv"]]
          ##summary_table.cv
          currentmodel$summary_table.cv<-CV.list()[["summary_table.cv"]]
          ##Multiple.ROC.cv
          currentmodel$Multiple.ROC.cv<-CV.list()[["Multiple.ROC.cv"]]
        } else{ #Estimation
          ##summary_table.est.cv
          currentmodel$summary_table.est.cv<-CV.list()[["summary_table.est.cv"]]
          ##Residuals.table.cv
          currentmodel$Residuals.table.cv<-CV.list()[["Residuals.table.cv"]]
        }
      }#End of #input$choose_Method_for_CV=="All Models"
        if(!is.null(currentmodel$Vars.with.diff.distribution.CV) ||
           !is.null(currentmodel$prediction.err.CV) ||
           !is.null(currentmodel$Keeping.complete.cases.in.target.CV))
          showModal(modalDialog(
            title = "",
            size="s",
            easyClose = TRUE,
            paste('Check Show Warnings')
          ))
        } else{ #CV.list is NULL
        currentmodel$Keeping.complete.cases.in.target.CV<-NULL
        currentmodel$Vars.with.diff.distribution.CV<-NULL
        currentmodel$prediction.err.CV<-NULL
        currentmodel$NA.table.CV<-NULL
        currentmodel$CV.Data<-NULL
        currentmodel$confusion.table.cv<-NULL
        currentmodel$summary_table.cv<-NULL
        currentmodel$summary_table.est.cv<-NULL
        currentmodel$Multiple.ROC.cv<-NULL
        currentmodel$Residuals.table.cv<-NULL
    }
  })
    
    output$Memory_Size_CV_Data<-renderUI({
      if(!is.null(currentmodel$CV.Data)){
        HTML(paste('Training data size in RAM:<br/>',
                   format(object.size(currentmodel$CV.Data), units = 'Mb')))
      } else{
        ""
      }
    })
    
    #######Show & Hide Tabs
    Validation_Results_CV<-reactive({
      if(!is.null(currentmodel$params.to.assign) && !is.null(currentmodel$CV.Data) && !is.null(currentmodel$all.models.for.cv)){
        return(currentmodel$params.to.assign[["output.var"]] %in% names(currentmodel$CV.Data))
      } else{
        return(FALSE)
      }
    })

    observe({
      if(Validation_Results_CV())
        showTab(inputId = "Validation", target = "Validation Results")
      if(!Validation_Results_CV()){
        hideTab(inputId = "Validation", target = "Validation Results")
        updateTabsetPanel(session, "Validation",selected = "View Data and Prediction")
      }
    })
    ##################PredictExport CV Outputs##########
    ###Show Warnings
    observe({
      input$PredictExportSubmit.CV
      if(!is.null(currentmodel$prediction.err.CV)){
        output$Prediction.err.CV <- renderUI({
          Names<-names(currentmodel$prediction.err.CV)
          plot_output_list <- lapply(Names, function(i) {
            tablename <- paste("prediction.err.CV_", i, sep="")
            DT::dataTableOutput(tablename)
          })  
          
          fluidRow(do.call(tagList, plot_output_list))
        }) 
        
        temp.table<-currentmodel$prediction.err.CV
        Names<-names(currentmodel$prediction.err.CV)
        for (i in Names){
          local({
            my_i <- i
            tablename <- paste("prediction.err.CV_", my_i, sep="")
            output[[tablename]] <- DT::renderDataTable({ 
              datatable(temp.table[[my_i]],caption=paste("Variables which do not exist in new data in",my_i),selection = "single",
                        options=list(info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE))
            },server = FALSE)
          })
        }  
      }})
  
    output$Keeping.complete.cases.in.target.CV<-renderText({
      if(!is.null(currentmodel$Keeping.complete.cases.in.target.CV)){
        currentmodel$Keeping.complete.cases.in.target.CV
      } else{
        ""
      }
    })
    
    output$Vars.with.diff.distribution.CV<-DT::renderDataTable({
      if(!is.null(currentmodel$Vars.with.diff.distribution.CV)){
        Table<-currentmodel$Vars.with.diff.distribution.CV
        datatable(Table,caption="These Variables have different distribution",selection = "single",
                  options=list(info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    output$NA.table.CV<-DT::renderDataTable({
      if(!is.null(currentmodel$NA.table.CV) && nrow(currentmodel$NA.table.CV)>0){
        datatable(currentmodel$NA.table.CV,caption="Number of NA in Prediction (Validation Data)",selection = "single",
                options=list(scrollX=TRUE,info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    ##Show Data
    output$PredictExport.Validation.Data <- DT::renderDataTable({
      input$PredictExportSubmit.CV
      if(!is.null(currentmodel$CV.Data)){
        SPY<-currentmodel$CV.Data
        ind<-which(unlist(lapply(names(SPY),function(i){is.factor(SPY[[i]]) && 
            !all(is.na(nchar(as.character(SPY[[i]])[c(1,2,3)]))) &&
            mean(nchar(as.character(SPY[[i]])[c(1,2,3)]),na.rm=TRUE)>30})))
        datatable(SPY,selection = "single",
                  options=list(
                    columnDefs = list(list(
                      targets  = ind,
                      render  = JS(
                        "function ( data, type ) {",
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(ind),
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    })
 
    ##Validation Results
    ##Plots
    output$accuracy_barplot.est.cv<-renderPlot({   
      tryCatch({
      if (!is.null(currentmodel$summary_table.est.cv) && Is.Estimation()) {  
        temp<-currentmodel$summary_table.est.cv
        if('Abs Difference' %in% names(temp))
          names(temp)[which(names(temp)=='Abs Difference')]<-'MAE'
        
        plot.features<-ggplot(temp,aes(x=temp[,'Method'],color=Criterion))+
          geom_point(aes(y=temp[,'RMSE'],col="RMSE"),stat="identity",shape=21,size=5,stroke=1)+ 
          geom_point(aes(y=temp[,'MAE'],col="MAE"),stat="identity",shape=21,size=6,stroke=1)+
          labs(x="Model",y="Value")+ggtitle("Comparison Plot")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        plot.features
      }
      },error=function(err){})
    }, bg="transparent")
    
    output$all_models_Norm_Residuals_plot.est.cv<-renderPlot({   
      if (!is.null(currentmodel$Residuals.table.cv) && Is.Estimation()) {  
        plot.features <- ggplot(currentmodel$Residuals.table.cv, aes(factor(Model), Norm_Residuals))+
          geom_violin(aes(fill = Model))+geom_point(alpha = 0.3)+
          labs(x="Model",y="Normalized Residuals")+ggtitle("Normalized Residuals")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        plot.features 
      }
    }, bg="transparent")
    
    output$accuracy_barplot.cv<-renderPlot({  
      tryCatch({
      if (!is.null(currentmodel$summary_table.cv) && Is.Classification()) {
        temp<-currentmodel$summary_table.cv
          plot.features<-ggplot(temp,aes(x=factor(temp[,"Method"]),color=Criterion))+  
            geom_point(aes(y=temp[,"Accuracy"],col="Accuracy"),stat="identity",shape=21,size=4,stroke=1)+ 
            geom_point(aes(y=temp[,"F measure"],col="F measure"),stat="identity",shape=21,size=5,stroke=1)+ 
            geom_point(aes(y=temp[,"AUC"],col="AUC"),stat="identity",shape=21,size=6,stroke=1)+ 
            geom_point(aes(y=temp[,"Gini"],col="Gini"),stat="identity",shape=21,size=7,stroke=1)+ 
            labs(x="Model",y="Value")

        plot.features+ggtitle("Comparison Plot")+ 
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
      }
      },error=function(err){})
    }, bg="transparent")
    
    output$multiple_roc.cv<-renderPlot({ 
      if(!is.null(currentmodel$Multiple.ROC.cv) && Is.Classification()){  
        tryCatch({
        temp<-currentmodel$Multiple.ROC.cv
        temp<-temp[cleaned.places(temp$target),]
        long_df <- as.data.frame(melt(as.data.table(temp), id.vars="target", variable.name="name"))
        model_df <- do.call(rbind, unname(by(long_df, long_df$name, getROC_AUC_for_multiple_models)))
        p <- ggplot(model_df, aes(x=stack_x, y=stack_y, colour=Model)) + geom_line() +
          labs(x="probability of false alarm\n(1-Specificity)",
               y="probability of detection\n(Sensitivity)")+ggtitle("ROC Curve")+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        
        p

        }, error=function(err){}) 
      }
    }, bg="transparent")
    
    ##Summary
    output$summary_table.cv <- DT::renderDataTable({
      input$PredictExportSubmit.CV
      if(!is.null(currentmodel$summary_table.cv) && Is.Classification()){
        Table<-currentmodel$summary_table.cv
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits)
        ##
        datatable(Table,caption="Summary Table",rownames=FALSE, selection = "single", 
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE))
      } else{
        NULL
      }
    },server = FALSE)  
    
    output$summary_table.est.cv <- DT::renderDataTable({
      input$PredictExportSubmit.CV
      if(!is.null(currentmodel$summary_table.est.cv) && Is.Estimation()){
        Table<-currentmodel$summary_table.est.cv
        ##Round
        numVars <- sapply(Table, is.numeric) 
        Table[numVars] <- lapply(Table[numVars], round, digits = round.digits)
        ##
        datatable(Table,caption="Summary Table",rownames=FALSE, selection = "single", 
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    
    ##Cross Validation tables
    observe({
      if(!is.null(currentmodel$confusion.table.cv) && Is.Classification()){
        output$confusion.cv <- renderUI({
          Names<-names(currentmodel$confusion.table.cv)
          plot_output_list <- lapply(Names, function(i) {
            tablename <- paste("tablename.cross.validation_", i, sep="")
            column(6,DT::dataTableOutput(tablename))
          })
          
          fluidRow(do.call(tagList, plot_output_list))
        }) 
        
        temp.table<-currentmodel$confusion.table.cv
        Names<-names(currentmodel$confusion.table.cv)
        for (i in Names){
          local({
            my_i <- i
            tablename <- paste("tablename.cross.validation_", my_i, sep="")
            output[[tablename]] <- DT::renderDataTable({ 
              Table<-temp.table[[my_i]]
              datatable(Table,caption=my_i, selection = "single",
                        options=list(info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
            },server = FALSE)
          })
        }  
      }})
    ###############Export The Data#########
    observeEvent(input$Export.button.CV,{   
        if(is.null(currentmodel$CV.Data)){
          CV.last_action <<- paste("No Data") 
        } else{ #!is.null(currentmodel$CV.Data)
          if(input$path.CV==""){
            CV.last_action <<- paste("Insert File name")
          } else{ #input$path.CV!=""
            tryCatch({
              write_excel_csv(currentmodel$CV.Data,path = paste0("C://AlgoTraceFolder.export/",input$path.CV,".csv"))
              CV.last_action<<-paste("Data was Exported to C://AlgoTraceFolder.export/")
            }, error=function(err) {
              CV.last_action <<- paste("Error occured - Data was not Exported")
            })
          }
        }
    })
         

    output$Export.CV <- renderText({
      input$PredictExportSubmit.CV
      input$Export.button.CV
      CV.last_action
    })
    ##########################################################
    ###################Accuracy Deciles Tab#############################
    ###############Creating Data for Accuracy Deciles###############
    #######Show & Hide Tabs
    Accuracy_Deciles_CV<-reactive({
      if(!is.null(currentmodel$params.to.assign) && !is.null(currentmodel$CV.Data) && !is.null(currentmodel$all.models.for.cv)){
        return((currentmodel$params.to.assign[["output.var"]] %in% names(currentmodel$CV.Data)) && Is.Classification())
      } else{
        return(FALSE)
      }
    })
    observe({
      if(Accuracy_Deciles_CV())
        showTab(inputId = "Validation", target = "Accuracy Deciles")
      if(!Accuracy_Deciles_CV()){
        hideTab(inputId = "Validation", target = "Accuracy Deciles")
        updateTabsetPanel(session, "Validation",selected = "View Data and Prediction")
      }
    })
    
    output$Model.for.deciles.accuracy.ui.cv<-renderUI({
      if(!is.null(currentmodel$CV.Data) && !is.null(currentmodel$all.models.for.cv) && Is.Classification()){
        if(isolate(input$choose_Method_for_CV)!="All Models"){
          choices<-isolate(input$choose_Method_for_CV)
        } else{
         choices<-currentmodel$all.models.for.cv
        }
        ###In case we loaded a model
        isolate({
          if(!is.null(currentmodel$Loaded.Model.for.deciles.accuracy.cv)){
            selected<-currentmodel$Loaded.Model.for.deciles.accuracy.cv
            currentmodel$Loaded.Model.for.deciles.accuracy.cv<-NULL
          } else{
            selected<-NULL
          } 
        })
        selectInput("Model.for.deciles.accuracy.cv", label="Model", choices=choices, selected=selected,width='200px')
      }
    })
    
    observeEvent(input$MainSubmit.Deciles.Accuracy.cv,{ 
      ##Reset Export text
      deciles.accuracy.cv.last_action<<-""
      ###
      if(!is.null(currentmodel$CV.Data) && !is.null(input$Model.for.deciles.accuracy.cv)){ 
        currentmodel$Accuracy.deciles.table.cv<-tryCatch({  
          SPY.Validation <- currentmodel$CV.Data 
          prob<-SPY.Validation[[paste0(gsub(" ",".",input$Model.for.deciles.accuracy.cv),"_predict_probability")]]   
          output.var<-currentmodel$params.to.assign[["output.var"]]
          classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
          Deciles.Count<-input$Deciles.Count.cv

          if(!is.na(Deciles.Count) && Deciles.Count>0){
            table.count<-vector(mode="numeric",length=Deciles.Count)
            Bins<-vector(mode="character",length=Deciles.Count)
            
            if(input$Criterion.for.deciles.accuracy.cv=="Accuracy"){
              table.accuracy<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Validation[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.accuracy[i]<- (Table[1,1]+Table[2,2])/sum(Table)
                } else{
                  table.accuracy[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.accuracy)
              colnames(table)<-c("Bins","Count in each Bin","Accuracy")
            }
            if(input$Criterion.for.deciles.accuracy.cv=="Accuracy 0"){
              table.accuracy_0<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Validation[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.accuracy_0[i]<- Table[1,1]/(Table[1,1]+Table[2,1])
                } else{
                  table.accuracy_0[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.accuracy_0)
              colnames(table)<-c("Bins","Count in each Bin","Accuracy 0")
            }
            if(input$Criterion.for.deciles.accuracy.cv=="Precision"){
              table.precision<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Validation[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.precision[i]<- Table[2,2]/(Table[1,2]+Table[2,2])
                } else{
                  table.precision[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.precision)
              colnames(table)<-c("Bins","Count in each Bin","Precision")
            }
            if(input$Criterion.for.deciles.accuracy.cv=="Recall"){
              table.recall<-vector(mode="numeric",length=Deciles.Count)
              for(i in 1:Deciles.Count){
                Bins[i]<-paste(round((1/Deciles.Count)*(i-1),3),"-",round((1/Deciles.Count)*i,3))
                ind<-intersect(which(prob>=(1/Deciles.Count)*(i-1)),which(prob<(1/Deciles.Count)*i))
                table.count[i]<-length(ind)
                if(length(ind)>0){
                  Table<-CV.Table(SPY.Validation[[output.var]][ind],ifelse(prob[ind]>classification.threshold, 1, 0))
                  table.recall[i]<-Table[2,2]/(Table[2,1]+Table[2,2])
                } else{
                  table.recall[i]<- NaN
                }
              }
              table<-data.frame(Bins,table.count,table.recall)
              colnames(table)<-c("Bins","Count in each Bin","Recall")
            }
            Accuracy.deciles.table.cv<-table
          } else{#if(Deciles.Count<=0)
            Accuracy.deciles.table.cv<-NULL
          }
          
          Accuracy.deciles.table.cv
        }, error=function(err) {
          return(NULL)
        })
      } else{
        currentmodel$Accuracy.deciles.table.cv<-NULL
      }
    }) 

    output$deciles.accuracy.table.cv<-DT::renderDataTable({ 
      if(!is.null(currentmodel$Accuracy.deciles.table.cv) && Is.Classification()){
        temp.table<-currentmodel$Accuracy.deciles.table.cv
        temp.names<-as.character(temp.table[,"Bins"])
        table<-as.data.frame(t(temp.table[,colnames(temp.table)[!colnames(temp.table) %in% "Bins"],drop=FALSE]))
        colnames(table)<-temp.names  
        ##Round
        numVars <- sapply(table, is.numeric) 
        table[numVars] <- lapply(table[numVars], round, digits = round.digits)
        ##
        datatable(table,selection = "single", class = 'cell-border stripe',
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL  
      }
    },server = FALSE)
    
    output$deciles.accuracy.count.plot.cv<-renderPlot({ 
      if(!is.null(currentmodel$Accuracy.deciles.table.cv) && Is.Classification()){
        table<-currentmodel$Accuracy.deciles.table.cv
        p<-ggplot(table,aes(x=as.factor(table[,1]),y=table[,2]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y="Count in each Bin")+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        p
      }}, bg="transparent")
    
    output$deciles.accuracy.plot.cv<-renderPlot({  
      if(!is.null(currentmodel$Accuracy.deciles.table.cv) && Is.Classification()){
        table<-currentmodel$Accuracy.deciles.table.cv
        p<-ggplot(table,aes(x=as.factor(table[,1]),y=table[,3]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y=paste(colnames(table)[3],"in each Bin"))+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        p
      }}, bg="transparent") 
    
    output$deciles_accuracy_table_cv_actionlink <- reactive({
      return(!is.null(currentmodel$Accuracy.deciles.table.cv) && Is.Classification())
    }) 
    outputOptions(output, 'deciles_accuracy_table_cv_actionlink', suspendWhenHidden=FALSE)
    
    ####Export Deciles Table
    observeEvent(input$MainSubmit.deciles.accuracy.table.cv,{
      if(!is.null(currentmodel$Accuracy.deciles.table.cv)){
        if(input$path.deciles.accuracy.table.cv!=""){
          tryCatch({
            write_excel_csv(currentmodel$Accuracy.deciles.table.cv,path = paste0("C://AlgoTraceFolder.export/",input$path.deciles.accuracy.table.cv,".csv"))
            deciles.accuracy.cv.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            deciles.accuracy.cv.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          deciles.accuracy.cv.last_action<<-paste("Insert File name")
        }
      } else{
        deciles.accuracy.cv.last_action<<-paste("No Data to export")
      }
    })
    output$deciles.accuracy.table.text.cv<-renderText({
      input$PredictExportSubmit.CV
      input$MainSubmit.Deciles.Accuracy.cv
      input$MainSubmit.deciles.accuracy.table.cv
      deciles.accuracy.cv.last_action
    })
    
    ####################AVG Deciles For Validation##################################
    AVG_Deciles_CV<-reactive({
      if(!is.null(currentmodel$params.to.assign) && !is.null(currentmodel$CV.Data) && !is.null(currentmodel$all.models.for.cv)){
        return((currentmodel$params.to.assign[["output.var"]] %in% names(currentmodel$CV.Data)) && Is.Estimation())
      } else{
        return(FALSE)
      }
    })
    
    observe({
      if(AVG_Deciles_CV())
        showTab(inputId = "Validation", target = "Avg Deciles")
      if(!AVG_Deciles_CV()){
        hideTab(inputId = "Validation", target = "Avg Deciles")
        updateTabsetPanel(session, "Validation",selected = "View Data and Prediction")
      }
    })
    
    output$Model.for.deciles.avg.ui.cv<-renderUI({
      if(!is.null(currentmodel$CV.Data) && !is.null(currentmodel$all.models.for.cv) && Is.Estimation()){
      if(isolate(input$choose_Method_for_CV)!="All Models"){
        choices<-isolate(input$choose_Method_for_CV)
      } else{
        choices<-currentmodel$all.models.for.cv
      }

        ###In case we loaded a model
        isolate({
          if(!is.null(currentmodel$Loaded.Model.for.deciles.avg.cv)){
            selected<-currentmodel$Loaded.Model.for.deciles.avg.cv
            currentmodel$Loaded.Model.for.deciles.avg.cv<-NULL
          } else{
            selected<-NULL
          } 
        })
        selectInput("Model.for.deciles.avg.cv", label="Model", choices=choices , selected=selected,width='200px')
      }
    })

    ####
    observeEvent(input$MainSubmit.Deciles.Avg.cv,{ 
      ##Reset Export Text
      deciles.avg.cv.last_action<<-""
      ########
      if(!is.null(currentmodel$CV.Data) && !is.null(input$Model.for.deciles.avg.cv)){ 
        currentmodel$Avg.deciles.table.cv<-tryCatch({  
          SPY.Validation <- currentmodel$CV.Data 
          prediction<-SPY.Validation[[paste0(gsub(" ",".",input$Model.for.deciles.avg.cv),"_predict_response")]]
          output.var<-currentmodel$params.to.assign[["output.var"]]
          Deciles.Count<-input$Deciles.Avg.Count.cv
          SPY.Validation<-SPY.Validation[cleaned.places(SPY.Validation[[output.var]]),] 
          if(!is.na(Deciles.Count) && Deciles.Count>0){
            ##Finding the mean of the prediction in each Bin
            cut_to_levels<-cut.nv(SPY.Validation[[output.var]],Deciles.Count)
            cut_to_levels.uni<-levels(cut_to_levels)
            Count<-vector(mode="numeric",length=length(cut_to_levels.uni))
            
            if(input$Criterion.for.deciles.avg.cv=="Avg Traget vs Prediction"){
              Avg_target<-vector(mode="numeric",length=length(cut_to_levels.uni))
              Avg_prediction<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Avg_target[i]<-mean(SPY.Validation[[output.var]][ind],na.rm=TRUE)
                Avg_prediction[i]<-mean(prediction[ind],na.rm=TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Avg_target,Avg_prediction)
              colnames(table)<-c("Bins","Count in each Bin","Avg Target","Avg Prediction")
            }
            
            if(input$Criterion.for.deciles.avg.cv=="MAE"){
              MAE<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                MAE[i]<-mean(abs(SPY.Validation[[output.var]][ind]-prediction[ind]),na.rm =TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,MAE)
              colnames(table)<-c("Bins","Count in each Bin","MAE")
            }
            
            if(input$Criterion.for.deciles.avg.cv=="Norm abs Difference"){
              Norm_abs_Difference<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Norm_abs_Difference[i]<-mean(abs(SPY.Validation[[output.var]][ind]-prediction[ind]),na.rm =TRUE)/mean(abs(SPY.Validation[[output.var]][ind]),na.rm =TRUE)
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Norm_abs_Difference)
              colnames(table)<-c("Bins","Count in each Bin","Norm abs Difference")
            }
            
            if(input$Criterion.for.deciles.avg.cv=="Abs Sum Difference"){
              Abs_Sum_Difference<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                Abs_Sum_Difference[i]<-abs(sum(SPY.Validation[[output.var]][ind],na.rm=TRUE)-sum(prediction[ind],na.rm=TRUE))
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,Abs_Sum_Difference)
              colnames(table)<-c("Bins","Count in each Bin","Abs Sum Difference")
            }
            
            if(input$Criterion.for.deciles.avg.cv=="RMSE"){
              RMSE<-vector(mode="numeric",length=length(cut_to_levels.uni))
              for(i in 1:length(cut_to_levels.uni)){
                ind<-which(cut_to_levels==cut_to_levels.uni[i])
                RMSE[i]<-sqrt(mean((SPY.Validation[[output.var]][ind]-prediction[ind])^2,na.rm =TRUE))
                Count[i]<-length(ind)
              }
              table<-data.frame(cut_to_levels.uni,Count,RMSE)
              colnames(table)<-c("Bins","Count in each Bin","RMSE")
            }
            
            Avg.deciles.table.cv<-table
            
          } else{#if(Deciles.Count<=0)
            Avg.deciles.table.cv<-NULL
          }
          Avg.deciles.table.cv
        }, error=function(err) {
          return(NULL)
        })
      } else{
        currentmodel$Avg.deciles.table.cv<-NULL
      }
    }) 
    
    output$Avg.deciles.table.cv<-DT::renderDataTable({
      if(!is.null(currentmodel$Avg.deciles.table.cv) && Is.Estimation()){
        temp.table<-currentmodel$Avg.deciles.table.cv
        temp.names<-as.character(temp.table[,"Bins"])
        table<-as.data.frame(t(temp.table[,colnames(temp.table)[!colnames(temp.table) %in% "Bins"],drop=FALSE]))
        colnames(table)<-temp.names 
        ##Round
        numVars <- sapply(table, is.numeric) 
        table[numVars] <- lapply(table[numVars], round, digits = round.digits)
        ##
        datatable(table,selection = "single", class = 'cell-border stripe',
                  options=list(scrollX=TRUE,info=FALSE,paging = FALSE,ordering=FALSE,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    output$Avg.deciles.count.plot.cv<-renderPlot({ 
      if(!is.null(currentmodel$Avg.deciles.table.cv) && Is.Estimation()){
        table<-currentmodel$Avg.deciles.table.cv
        p<-ggplot(table,aes(x=table[,"Bins"],y=table[,"Count in each Bin"]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Target Variable Bins",y="Count in each Bin")+scale_x_discrete(limits=table[,"Bins"])+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        p
      }}, bg="transparent")
    
    
    output$Avg.deciles.plot.cv<-renderPlot({
      if(!is.null(currentmodel$Avg.deciles.table.cv) && Is.Estimation()){
        table<-currentmodel$Avg.deciles.table.cv
        if(isolate(input$Criterion.for.deciles.avg.cv)!="Avg Traget vs Prediction"){
        p<-ggplot(table,aes(x=table[,"Bins"],y=table[,3]))+geom_bar(color="darkblue", fill="lightblue",stat="identity")+
          labs(x="Bins",y=paste(colnames(table)[3],"in each Bin"))+
          theme(
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(colour = "lightgray"),
            axis.text.x = element_text(angle = 330),
            legend.background=element_rect(fill='white'),
            plot.background = element_rect(fill="white")
          )
        } else{
          p<-ggplot(table,aes(x=table[,"Bins"]))+
            geom_point(aes(y=table[,"Avg Target"],col="Avg Target"),stat="identity",shape=21,size=5,stroke=2)+ 
            geom_point(aes(y=table[,"Avg Prediction"],col="Avg Prediction"),stat="identity",shape=21,size=5,stroke=2) + 
            labs(x="Target Variable Bins",y="value",color="variable")+scale_x_discrete(limits=table[,"Bins"])+
            theme(
              panel.background = element_rect(fill = "white"),
              panel.grid.major = element_line(colour = "lightgray"),
              axis.text.x = element_text(angle = 330),
              legend.background=element_rect(fill='white'),
              legend.position="bottom",
              plot.background = element_rect(fill="white")
            )
        }
        p
      }
    }, bg="transparent")
    
   
    output$deciles_avg_table_actionlink_cv <- reactive({
      return(!is.null(currentmodel$Avg.deciles.table.cv) && Is.Estimation())
    }) 
    outputOptions(output, 'deciles_avg_table_actionlink_cv', suspendWhenHidden=FALSE)
    
    
    ####Export Deciles Table
    observeEvent(input$MainSubmit.deciles.avg.table.cv,{
      if(!is.null(currentmodel$Avg.deciles.table.cv)){
        if(input$path.deciles.avg.table.cv!=""){
          tryCatch({
            write_excel_csv(currentmodel$Avg.deciles.table.cv,path = paste0("C://AlgoTraceFolder.export/",input$path.deciles.avg.table.cv,".csv"))
            deciles.avg.cv.last_action<<-paste("File was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err){
            deciles.avg.cv.last_action<<-paste("Error occured - Data was not Exported")
          })
        } else{
          deciles.avg.cv.last_action<<-paste("Insert File name")
        }
      } else{
        deciles.avg.cv.last_action<<-paste("No Data to export")
      }
    })
    output$deciles.avg.table.text.cv<-renderText({
      input$PredictExportSubmit.CV
      input$MainSubmit.Deciles.Avg.cv
      input$MainSubmit.deciles.avg.table.cv
      deciles.avg.cv.last_action
    })
   
     ########################################################
    ##################PredictExport.Prediction.Data##########
    ##########################################################
    ## Loading the file for prediction
    ##########################################################
    observeEvent(input$PredictExportSubmit.Pred, {
      SPY<-NULL
      tryCatch({
        if(input$DataBase.Pred.type=="Flat"){
          inFile <- input$datafilePredict
          if (is.null(inFile)) {
            SPY <- NULL
          } else {
            SPY <- import(inFile$datapath,format =split(inFile$name))
          }}
        if(input$DataBase.Pred.type=="Big Files(.csv)"){
          SPY<-fread(file.choose(),header = T, sep =',',data.table=FALSE) 
        }
        if(input$DataBase.Pred.type=="ODBC"){
          if(is.null(input$ODBC_Pred.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$ODBC_Pred.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
            close(myconn)
          }}
        if(input$DataBase.Pred.type=="MS SQL"){
          if(is.null(input$MS_sql_Pred.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$MS_sql_Pred.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
            close(myconn)
          }}
        if(input$DataBase.Pred.type=="MySQL"){
          if(is.null(input$Mysql_Pred.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Mysql_Pred.table) 
            myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                 pwd=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
            close(myconn)
          }}
        if(input$DataBase.Pred.type=="Oracle"){
          if(is.null(input$Oracle_Pred.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Oracle_Pred.table) 
            drv <-JDBC("oracle.jdbc.OracleDriver",
                       classPath=Table[Table$Parameters=="ojdbc.jar Path","Insert"]," ") 
            myconn<- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:orcl",Table[Table$Parameters=="USERID","Insert"],
                               Table[Table$Parameters=="PASSWORD","Insert"]) 
            SPY <-dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
            dbDisconnect(myconn) 
          }}
        if(input$DataBase.Pred.type=="Amazon Redshift"){
          if(is.null(input$Amazon_Redshift_Pred.table)){
            SPY <- NULL
          } else{
            Table <- hot_to_r(input$Amazon_Redshift_Pred.table) 
            drv <- dbDriver(Table[Table$Parameters=="DRIVER","Insert"])   
            myconn <- dbConnect(drv, host=Table[Table$Parameters=="Host","Insert"], 
                                dbname=Table[Table$Parameters=="DATABASE","Insert"],
                                port=Table[Table$Parameters=="PORT","Insert"],
                                user=Table[Table$Parameters=="USER","Insert"],
                                password=Table[Table$Parameters=="PASSWORD","Insert"])
            SPY <- dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])
            dbDisconnect(myconn)
          }} 
        if(input$DataBase.Pred.type=="Google Big Query"){
          if(is.null(input$Google_Big_Query_Pred.table)){
            SPY <- NULL
          } else{
            gar_auth_service(
              json_file = input$json.file.Pred$datapath,
              scope = "https://www.googleapis.com/auth/bigquery"
            )
            Table <- hot_to_r(input$Google_Big_Query_Pred.table) 
            SPY <- bqr_query(projectId = Table[Table$Parameters=="ProjectID","Insert"],query =Table[Table$Parameters=="Query","Insert"],
                             datasetId = Table[Table$Parameters=="DatasetId","Insert"],useLegacySql = input$useLegacySql.Pred) 
          }}
        
      } , error=function(err) {
        Pred.file.loading <<- paste("Error occured - File was not Loaded")
      }) 
      
      if(!is.null(SPY) && !is.character(SPY) && nrow(SPY)>0){
        Pred.file.loading<<-""
        SPY<-as.data.table(SPY)
        ##checking if we have longitude and latitude, and make from them extra variables
        if(all(c('latitude', 'longitude') %in% tolower(names(SPY)))){
          for(i in 1:8){
            SPY[,paste0("latitude_",i)]<-round(SPY[[which(tolower(names(SPY))=="latitude")]],i)
            SPY[,paste0("longitude",i)]<-round(SPY[[which(tolower(names(SPY))=="longitude")]],i)
          }
        }
        ##Converting integer64\Logical to numeric
        ind<-sapply(names(SPY), function(x) {class(SPY[[x]])[1] %in% c("logical","integer64")})
        for(i in names(SPY)[ind]){SPY[,i]<-as.numeric(SPY[[i]])}   
        
        ##Converting character to Factor
        ind<-sapply(names(SPY), function(x) {"character" %in% class(SPY[[x]])})
        for(i in names(SPY)[ind]){SPY[,i]<-as.factor(SPY[[i]])}  
        
        ####Class Date####
        ##Adding columns concerning dates
        ind1<-sapply(names(SPY), function(x) {"Date" %in% class(SPY[[x]])})
        ind2<-sapply(names(SPY), function(x) {"POSIXct" %in% class(SPY[[x]])})
        if(input$Dates.as.parts.Pred=="No"){
          for(i in names(SPY)[ind1]){SPY[,i]<-as.factor(SPY[[i]])}   
          for(i in names(SPY)[ind2]){SPY[,i]<-as.factor(SPY[[i]])}  
        } else{ 
          for(i in names(SPY)[ind1]){
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_week")]<-as.factor(format(SPY[[i]],"%a"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%b"))
            SPY[,paste0(i,"_quarter")]<-as.factor(quarters(SPY[[i]]))
            SPY[,paste0(i,"_month_day")]<-as.factor(format(SPY[[i]],"%b %d"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }
          for(i in names(SPY)[ind2]){
            SPY[,paste0(i,"_sec")]<-as.factor(format(SPY[[i]],"%S"))
            SPY[,paste0(i,"_min")]<-as.factor(format(SPY[[i]],"%M"))
            SPY[,paste0(i,"_hour")]<-as.factor(format(SPY[[i]],"%H"))
            SPY[,paste0(i,"_day")]<-as.factor(format(SPY[[i]],"%d"))
            SPY[,paste0(i,"_month")]<-as.factor(format(SPY[[i]],"%m"))
            SPY[,paste0(i,"_year")]<-as.factor(format(SPY[[i]],"%Y"))
            
            SPY[,i]<-as.factor(SPY[[i]])
          }  
        }#End of Dates.as.parts.Pred if 
        
        ##Text
        if(!is.null(textAnalysis$text.analysis.for.export.saved)){
          Pos.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Pos.Exp.table"]]
          Neg.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Neg.Exp.table"]]
          Other.Exp.table<-textAnalysis$text.analysis.for.export.saved[["Other.Exp.table"]]
          table<-textAnalysis$text.analysis.for.export.saved[["table"]]
          SPY<-turn.text.to.data(SPY,Pos.Exp.table,Neg.Exp.table,Other.Exp.table,table) 
        }
        
        colnames(SPY)<-make.names(colnames(SPY),unique=TRUE) 
      } else{#End of !is.null(SPY) && !is.character(SPY) && ncol(SPY)>0
        SPY<-NULL
        Pred.file.loading <<- paste("No Data")
      }
      currentmodel$uploaded.dataPredict <- SPY
    })
    
    ##Loading Text
    output$Pred.file.loading <- renderText({
      input$PredictExportSubmit.Pred
      Pred.file.loading
    })
    #######################################
    output$choose_Method_for_Pred.ui<-renderUI({
      if(!is.null(currentmodel$Used.Models)){
        models.used<-currentmodel$Used.Models[["models.used"]]
        best.models.used<-currentmodel$Used.Models[["best.models.used"]]
        models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
        best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
        
        choices<-models.used
        if(length(models.used)>1)
          choices<-c(choices,"Ensemble all models")  
        if(length(models.used.weights)>1)
          choices<-c(choices,"Ensemble all models weighting")  
        if(length(best.models.used)>1)
          choices<-c(choices,"Ensemble best models")
        if(length(best.models.used.weights)>1)
          choices<-c(choices,"Ensemble best models weighting")
        
        if(length(models.used)>1)
          choices<-c(choices,"All Models")  
        ###In case we loaded a model
        isolate({
          if(!is.null(currentmodel$Loaded.choose_Method_for_Pred)){
            selected<-currentmodel$Loaded.choose_Method_for_Pred
            currentmodel$Loaded.choose_Method_for_Pred<-NULL
          } else{
            selected<-NULL
          } 
        })
        selectInput("choose_Method_for_Pred", 
                    label = "Model",
                    choices=choices,selected=selected)
      }})

    
    Pred.list<-reactive({
      if(!is.null(currentmodel$uploaded.dataPredict) && !is.null(currentmodel$necessary.variables.found)){
        output.var<-currentmodel$params.to.assign[["output.var"]] 
        vars.not.include<-currentmodel$params.to.assign[["vars.not.include"]] 
        chosen.not.fix<-currentmodel$params.to.assign[["chosen.not.fix"]]
        not.fix.vars<-chosen.not.fix[["not.fix"]]
        Imputation.Value<-currentmodel$Settings_Table[["Imputation Value"]]
        if(currentmodel$Method.used=="Classification")
          classification.threshold<-currentmodel$Settings_Table[["Class Threshold"]]
        ####Keeping the original data for output
        Original.SPY.Predict<-as.data.table(currentmodel$uploaded.dataPredict)
        SPY.Predict<-as.data.table(currentmodel$uploaded.dataPredict) 
        ###Checking if the variables have the same class####
        mutual.names<-names(SPY.Predict)[names(SPY.Predict) %in% names(currentmodel$uploaded.data)] 
        if(length(mutual.names)>0){
          class.data<-sapply(mutual.names,function(x){return(class(currentmodel$uploaded.data[[x]]))})
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
        if(currentmodel$ratio.as.dummies.inside[["Add"]]=="Yes"){
          char<-currentmodel$ratio.as.dummies.inside[["char"]]
          is.interval<-currentmodel$ratio.as.dummies.inside[["is.interval"]]
          err<-New_Data_prediction_error(char,SPY.Predict)
          if(is.null(err)){
            table<-currentmodel$ratio.as.dummies.inside[["table"]]
            Data<-SPY.Predict
            SPY.Predict<-turn.ratio.combination.to.data(Data,table,is.interval)
          }} 
        #####################################
        ######Turn Factors to Numeric########
        if(!is.null(currentmodel$levels.table)){
          temp<-currentmodel$levels.table
          for(i in names(temp)){
            if(i %in% names(SPY.Predict))
              SPY.Predict[,i]<-turn.fac.to.num(SPY.Predict[[i]],temp[[i]])
          }
        }
        ######################################
        if(!is.null(currentmodel$Create.is.na.vars))
          SPY.Predict<-Create.is.na.vars.for.prediction(SPY.Predict,currentmodel$Create.is.na.vars)
        #######Clean the Data
        SPY.Predict<-Fix.Data(SPY.Predict,output.var,vars.not.include,chosen.not.fix,Imputation.Value,OutliersSettings$Outliers.settings.saved)  
        ################################################################
        Pred.list<-NULL
        Progression.Vars <- currentmodel$necessary.variables.found
        ########################################################
        if(!is.null(currentmodel$necessary.variables.found.for.first.layer))
          SPY.Predict<-First.layer.prediction(SPY.Predict,currentmodel$necessary.variables.found.for.first.layer,currentmodel$Used.Models.for.first.layer)
        
        ###performing wilcoxon test to check whether
        ###each numeric variable has the same distribution in both data 
        ### SPY.Predict && currentmodel$SPY.fixed
        Pred.list[["Vars.with.diff.distribution"]]<-tryCatch({
          diff.distribution<-sapply(names(currentmodel$SPY.fixed),function(x){
            return(wilcox.test(currentmodel$SPY.fixed[[x]],SPY.Predict[[x]],alternative ="two.sided")$p.value<0.05)
          })
          
          diff.distribution<-which(diff.distribution==TRUE)
          temp.names<-names(currentmodel$SPY.fixed)[diff.distribution]
          table<-NULL
          for(i in temp.names){
            table<-rbind(table,data.frame("Variable"=i,"data mean"=round(mean(currentmodel$SPY.fixed[[i]],na.rm=TRUE),3),
                                          "new data mean"=round(mean(SPY.Predict[[i]],na.rm=TRUE),3),check.names=F))
          }
          
          table
        }, error=function(err){
          return(NULL)
        })
        ############
        temp.data<-list()
        temp.data.b<-list()
        basic.names<-names(Original.SPY.Predict)
        NA.table.Pred<-NULL
        Pred.list[["basic.names"]]<-basic.names
        prediction.err.Pred<-list()
        models.used<-currentmodel$Used.Models[["models.used"]]
        best.models.used<-currentmodel$Used.Models[["best.models.used"]]
        models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
        best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
        ############
        for(i in models.used){
          tryCatch({
            loop.vars<-Progression.Vars$Vars[[i]] 
            basic.vars<-base.vars(loop.vars,currentmodel$SPY.fixed)
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
              if(currentmodel$Method.used=="Classification"){
                prob<-predict.model(model,myform,SPY.Predict,classification.threshold,i,"Classification")[["prob"]]  
                prob<-na.assign(prob,not.fix.ind)
                NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"=i,"Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
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
                  NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"=i,"Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
                  if(!all(is.na(prediction))){
                    temp.data[[i]]<-prediction  
                    if(i %in% best.models.used)
                      temp.data.b[[i]]<-prediction
                    Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_response")]<-prediction
                    Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_lower_limit")]<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
                    Original.SPY.Predict[,paste0(gsub(" ",".",i),"_predict_upper_limit")]<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
                  }}
            } else{
              prediction.err.Pred[[i]]<-err
            }
          }, error=function(err){})
        }#End of loop
        
        Pred.list[["prediction.err.Pred"]]<-prediction.err.Pred
        ####################
        if(length(temp.data)>1){
          if(currentmodel$Method.used=="Classification"){
            tryCatch({
              prob<-apply(as.data.frame(temp.data),1,combined_prob)   
              prediction<- ifelse(prob>classification.threshold, 1, 0)
              NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble all models","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
              Original.SPY.Predict$Ensemble.all.models_predict_probability<-prob
              Original.SPY.Predict$Ensemble.all.models_predict_response<-prediction
            }, error=function(err){})
            ##################################
            if(!is.null(models.used.weights)){
              tryCatch({  
                prob<-weighted.prob(temp.data,models.used.weights)
                prediction<- ifelse(prob>classification.threshold, 1, 0)
                NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble all models weighting","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
                Original.SPY.Predict$Ensemble.all.models.weighting_predict_probability<-prob
                Original.SPY.Predict$Ensemble.all.models.weighting_predict_response<-prediction
              }, error=function(err){})
            }
          } else{
            tryCatch({
              prediction<-apply(as.data.frame(temp.data),1,function(x){median(x,na.rm=TRUE)})
              NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble all models","Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
              Original.SPY.Predict$Ensemble.all.models_predict_response<-prediction
              Original.SPY.Predict$Ensemble.all.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
              Original.SPY.Predict$Ensemble.all.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
            }, error=function(err){})
          }
        }
        if(length(temp.data.b)>1){
          if(currentmodel$Method.used=="Classification"){
            tryCatch({
              prob<-apply(as.data.frame(temp.data.b),1,combined_prob)
              prediction<- ifelse(prob>classification.threshold, 1, 0)
              NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble best models","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
              Original.SPY.Predict$Ensemble.best.models_predict_probability<-prob
              Original.SPY.Predict$Ensemble.best.models_predict_response<-prediction
            }, error=function(err){})
            ##################################
            tryCatch({  
              prob<-weighted.prob(temp.data.b,best.models.used.weights)
              prediction<- ifelse(prob>classification.threshold, 1, 0)
              NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble best models weighting","Num of NA"=length(which(is.na(prob))),"Percentage of NA"=length(which(is.na(prob)))/length(prob),check.names=F))
              Original.SPY.Predict$Ensemble.best.models.weighting_predict_probability<-prob
              Original.SPY.Predict$Ensemble.best.models.weighting_predict_response<-prediction
            }, error=function(err){})
          } else{
            tryCatch({
              prediction<-apply(as.data.frame(temp.data.b),1,function(x){median(x,na.rm=TRUE)})
              NA.table.Pred<-rbind(NA.table.Pred,data.frame("Method"="Ensemble best models","Num of NA"=length(which(is.na(prediction))),"Percentage of NA"=length(which(is.na(prediction)))/length(prediction),check.names=F))
              Original.SPY.Predict$Ensemble.best.models_predict_response<-prediction
              Original.SPY.Predict$Ensemble.best.models_predict_lower_limit<-round(prediction-qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
              Original.SPY.Predict$Ensemble.best.models_predict_upper_limit<-round(prediction+qnorm(0.975)*sd(prediction,na.rm=TRUE)/sqrt(length(prediction)),3)
            }, error=function(err){})
          }
        }
        
        Pred.list[["NA.table.Pred"]]<-NA.table.Pred
        Pred.list[["Pred.Data"]]<-Original.SPY.Predict
        return(Pred.list)
      } else{
        return(NULL)
      }
    })
    
    observeEvent(input$PredictExportSubmit.Pred,{
      ##Reset Export text
      Pred.last_action<<-""
      if(!is.null(Pred.list())){
        Progression.Vars <- currentmodel$necessary.variables.found
        if(input$choose_Method_for_Pred!="All Models"){
          method<-input$choose_Method_for_Pred
          vars<-Progression.Vars$Vars[[method]]
          vars<-base.vars(vars,currentmodel$SPY.fixed)
          ##Vars.with.diff.distribution.Pred
          Vars.with.diff.distribution.Pred<-Pred.list()[["Vars.with.diff.distribution"]]
          temp<-Vars.with.diff.distribution.Pred[Vars.with.diff.distribution.Pred$Variable %in% vars,]
          if(is.null(temp) || nrow(temp)==0){
            currentmodel$Vars.with.diff.distribution.Pred<-NULL
          } else{
            currentmodel$Vars.with.diff.distribution.Pred<-temp
          }
          ##prediction.err.Pred
          prediction.err.Pred<-Pred.list()[["prediction.err.Pred"]]
          prediction.err.Pred[!names(prediction.err.Pred) %in% method]<-NULL
          if(is.null(prediction.err.Pred) || length(prediction.err.Pred)==0){
            currentmodel$prediction.err.Pred<-NULL
          } else{
            currentmodel$prediction.err.Pred<-prediction.err.Pred
          }
          ##NA table
          NA.table.Pred<-Pred.list()[["NA.table.Pred"]]
          currentmodel$NA.table.Pred<-NA.table.Pred[NA.table.Pred$Method==method,]
          ##Predict Data
          basic.names<-Pred.list()[["basic.names"]]
          Pred.Data<-Pred.list()[["Pred.Data"]]
          if(currentmodel$Method.used=="Classification"){
            ##Predict Data
            basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_probability"),paste0(gsub(" ",".",method),"_predict_response"))
            currentmodel$Predict.Data<-Pred.Data[,names(Pred.Data)[names(Pred.Data) %in% basic.names],with=FALSE]
          } else{ #Estimation
            ##Predict Data
            basic.names<-c(basic.names,paste0(gsub(" ",".",method),"_predict_response"),paste0(gsub(" ",".",method),"_predict_lower_limit"),
                           paste0(gsub(" ",".",method),"_predict_upper_limit"))
            currentmodel$Predict.Data<-Pred.Data[,names(Pred.Data)[names(Pred.Data) %in% basic.names],with=FALSE]
          }
        } else{ #input$choose_Method_for_Pred=="All Models"
          vars<-unique(unlist(Progression.Vars$Vars))
          vars<-base.vars(vars,currentmodel$SPY.fixed)
          ##Vars.with.diff.distribution.Pred
          Vars.with.diff.distribution.Pred<-Pred.list()[["Vars.with.diff.distribution"]]
          temp<-Vars.with.diff.distribution.Pred[Vars.with.diff.distribution.Pred$Variable %in% vars,]
          if(is.null(temp) || nrow(temp)==0){
            currentmodel$Vars.with.diff.distribution.Pred<-NULL
          } else{
            currentmodel$Vars.with.diff.distribution.Pred<-temp
          }
          ##prediction.err.Pred
          prediction.err.Pred<-Pred.list()[["prediction.err.Pred"]]
          if(is.null(prediction.err.Pred) || length(prediction.err.Pred)==0){
            currentmodel$prediction.err.Pred<-NULL
          } else{
            currentmodel$prediction.err.Pred<-prediction.err.Pred
          }
          ##NA table
          currentmodel$NA.table.Pred<-Pred.list()[["NA.table.Pred"]]
          ##Predict Data
          currentmodel$Predict.Data<-Pred.list()[["Pred.Data"]]
        }#End of #input$choose_Method_for_Pred=="All Models"
        if(!is.null(currentmodel$Vars.with.diff.distribution.Pred) ||
           !is.null(currentmodel$prediction.err.Pred))
          showModal(modalDialog(
            title = "",
            size="s",
            easyClose = TRUE,
            paste('Check Show Warnings')
          ))
        } else{#Pred.list is NULL
        currentmodel$Vars.with.diff.distribution.Pred<-NULL
        currentmodel$prediction.err.Pred<-NULL
        currentmodel$NA.table.Pred<-NULL
        currentmodel$Predict.Data<-NULL
      }
      })
    
    ##################PredictExport Outputs##########
  
    output$Memory_Size_Pred_Data<-renderUI({
      if(!is.null(currentmodel$Predict.Data)){
        HTML(paste('Training data size in RAM:<br/>',
                   format(object.size(currentmodel$Predict.Data), units = 'Mb')))
      } else{
        ""
      }
    })
    
    ##Show Warnings
    observe({
      input$PredictExportSubmit.Pred
      if(!is.null(currentmodel$prediction.err.Pred)){
        output$Prediction.err.Pred <- renderUI({
          Names<-names(currentmodel$prediction.err.Pred)
          plot_output_list <- lapply(Names, function(i) {
            tablename <- paste("prediction.err.Pred_", i, sep="")
            DT::dataTableOutput(tablename)
          })
          
          fluidRow(do.call(tagList, plot_output_list))
        }) 
        
        temp.table<-currentmodel$prediction.err.Pred
        Names<-names(currentmodel$prediction.err.Pred)
        for (i in Names){
          local({
            my_i <- i
            tablename <- paste("prediction.err.Pred_", my_i, sep="")
            output[[tablename]] <- DT::renderDataTable({ 
              datatable(temp.table[[my_i]],caption=paste("Variables which do not exist in new data in",my_i),selection = "single",
                        options=list(info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE))
            },server = FALSE)
          })
        }  
      }})

    output$Vars.with.diff.distribution.Pred<-DT::renderDataTable({
      if(!is.null(currentmodel$Vars.with.diff.distribution.Pred)){
        Table<-currentmodel$Vars.with.diff.distribution.Pred
        datatable(Table,caption="These Variables have different distribution",selection = "single",
                  options=list(info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    },server = FALSE)
    
    output$NA.table.Pred<-DT::renderDataTable({
      if(!is.null(currentmodel$NA.table.Pred) && nrow(currentmodel$NA.table.Pred)>0){
        datatable(currentmodel$NA.table.Pred,caption="Number of NA in Prediction (Prediction Data)",selection = "single",
                  options=list(info=FALSE,pageLength = 5,ordering=F,searching = FALSE,lengthChange=FALSE))   
      } else{
        NULL
      }
    },server = FALSE)
    
    ##Show Data
    output$PredictExport.Prediction.Data <- DT::renderDataTable({
      input$PredictExportSubmit.Pred
      if(!is.null(currentmodel$Predict.Data)){
        SPY<-currentmodel$Predict.Data
        ind<-which(unlist(lapply(names(SPY),function(i){is.factor(SPY[[i]]) && 
            !all(is.na(nchar(as.character(SPY[[i]])[c(1,2,3)]))) &&
            mean(nchar(as.character(SPY[[i]])[c(1,2,3)]),na.rm=TRUE)>30})))
        datatable(SPY,selection = "single",
                  options=list(
                    columnDefs = list(list(
                      targets  = ind,
                      render  = JS(
                        "function ( data, type ) {",
                        "return type === 'display' && data!==null && data!=='' && data.length > 30 ?",
                        "data.substr(0, 30) +'...' :",
                        "data;",
                        "}"))),
                    rowCallback = JS(
                      "function(nRow, aData) {",
                      string.for.tooltip(ind),
                      "}"),
                    pageLength = 100,ordering=F,searching = FALSE,lengthChange=FALSE)) 
      } else{
        NULL
      }
    })  

    ###############Export The Data#########
    observeEvent(input$Export.button.Pred,{   
      if(is.null(currentmodel$Predict.Data)){
        Pred.last_action <<- paste("No Data") 
      } else{ #!is.null(currentmodel$Predict.Data)
        if(input$path.Pred==""){
          Pred.last_action <<- paste("Insert File name")
        } else{ #input$path.Pred!=""
          tryCatch({
            write_excel_csv(currentmodel$Predict.Data,path = paste0("C://AlgoTraceFolder.export/",input$path.Pred,".csv"))
            Pred.last_action<<-paste("Data was Exported to C://AlgoTraceFolder.export/")
          }, error=function(err) {
            Pred.last_action <<- paste("Error occured - Data was not Exported")
          })
        }
      }
    })
    
     output$Export.Pred <- renderText({
       input$PredictExportSubmit.Pred
       input$Export.button.Pred
       Pred.last_action
     })
    
    ##########################################################
    ## Block for Source Listener
    ########################################################## 
     observe({
       if(!is.null(currentmodel$necessary.variables.found)){
         showTab(inputId = "Front", target = "Source Listener")
       } else{
         hideTab(inputId = "Front", target = "Source Listener")
       }
     })
     

     ##########Source Listener Folder##################
     ##Import
     models.dir.Source.listener_import<-""
     observeEvent(input$Source.listener.Dir.import, {
       temp <- choose.dir()
       if (!is.na(temp)) {
         models.dir.Source.listener_import <<- temp
       }
     })
     
     output$Source.listener.Dir.import.Text<-renderText({
       input$Source.listener.Dir.import 
       if(models.dir.Source.listener_import!=""){
         paste("Import from:",models.dir.Source.listener_import)
       } else{
         ""
       }
     })
     
     ##Export
     models.dir.Source.listener_export<-""
     observeEvent(input$Source.listener.Dir.export, {
       temp <- choose.dir()
       if (!is.na(temp)) {
         models.dir.Source.listener_export <<- temp
       }
     })
     
     output$Source.listener.Dir.export.Text<-renderText({
       input$Source.listener.Dir.export
       if(models.dir.Source.listener_export!=""){
         paste("Export to:",models.dir.Source.listener_export)
       } else{
         ""
       }
     })
     
     output$choose_Method_for_Source.listener<-renderUI({
       if(!is.null(currentmodel$Used.Models)){
         models.used<-currentmodel$Used.Models[["models.used"]]
         best.models.used<-currentmodel$Used.Models[["best.models.used"]]
         models.used.weights<-currentmodel$Used.Models[["models.used.weights"]]
         best.models.used.weights<-currentmodel$Used.Models[["best.models.used.weights"]]
         
         choices<-models.used
         if(length(models.used)>1)
           choices<-c(choices,"Ensemble all models")  
         if(length(models.used.weights)>1)
           choices<-c(choices,"Ensemble all models weighting")  
         if(length(best.models.used)>1)
           choices<-c(choices,"Ensemble best models")
         if(length(best.models.used.weights)>1)
           choices<-c(choices,"Ensemble best models weighting")
         
         if(length(models.used)>1)
           choices<-c(choices,"All Models") 

         isolate({
           if(!is.null(currentmodel$Loaded.choose_Method_Source.listener)){
             selected<-currentmodel$Loaded.choose_Method_Source.listener
             currentmodel$Loaded.choose_Method_Source.listener<-NULL
           } else{
             selected<-NULL
           } 
         })

         selectInput("choose_Method_for_Source.listener", 
                     label = "Model",
                     choices=choices,selected=selected)  
       }})
     

     continue.source.listener<-reactiveValues(val=FALSE)
     source.listener.last_action<-""
     
     observeEvent(input$MainSubmit.source.listener,{
       source.listener.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(models.dir.Source.listener_import==""){
           source.listener.last_action<<-paste("Import Directory is not chosen")
           return()
         }
         if(models.dir.Source.listener_export==""){
           source.listener.last_action<<-paste("Export Directory is not chosen")
           return()
         }
         if(is.na(input$Source.listener.time)){
           source.listener.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.last_action<<-paste("Model is not chosen")
           return()
         }
         models.to.start <- list.files(models.dir.Source.listener_import, full.names=TRUE)
         table.to.start<-data.frame(Model=models.to.start, `Last Modified`=unlist(lapply(models.to.start, function(f) format(file.info(f)$mtime, "%Y-%m-%d %H:%M:%S"))), stringsAsFactors=FALSE,check.names = F)
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         for(inFile in models.to.start){
           Users.data<-tryCatch({
             temp<-unlist(strsplit(inFile,paste0(models.dir.Source.listener_import,"/"),fixed=TRUE))[2]
             temp<-unlist(strsplit(temp,".",fixed=TRUE))
             name<-temp[1]
             format<-temp[2]
             import(inFile,format=format)
           }, error=function(err){
             return(NULL)
           })
           if(is.null(Users.data)  || is.character(Users.data) || !all(dim(Users.data)>0))
             next
           
           tryCatch({
             data.to.return<-Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                                               levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                                               uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                                               necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
             write_excel_csv(data.to.return,path = paste0(models.dir.Source.listener_export,paste0("/",name,"_Output"),".csv"))
           }, error=function(err){})
         }
         ########End of Prediction######  
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         list.source.listener<<-list("Progression.Vars"=Progression.Vars,
                                     "Method.used"=Method.used,
                                     "params.to.assign"=params.to.assign,
                                     "Settings_Table"=Settings_Table,
                                     "levels.table"=levels.table,
                                     "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                     "Create.is.na.vars"=Create.is.na.vars,
                                     "Used.Models"=Used.Models,
                                     "uploaded.data"=uploaded.data,
                                     "SPY.fixed"=SPY.fixed,
                                     "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                     "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                     "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                     "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                     "Outliers.settings.saved"=Outliers.settings.saved,
                                     "diff.time"=diff.time,
                                     "models.dir.Source.listener_import"=models.dir.Source.listener_import,
                                     "models.dir.Source.listener_export"=models.dir.Source.listener_export,
                                     "models.to.start"=models.to.start,
                                     "table.to.start"=table.to.start,
                                     "First.Prediction"=TRUE)
         
         continue.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.Text<-renderText({
       input$MainSubmit.source.listener
       input$MainSubmit.stop.source.listener
       source.listener.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener,{
       continue.source.listener$val<-FALSE
       source.listener.last_action<<-paste("Calclation stopped")
     })
     
     observe({
       if(!continue.source.listener$val)
         return()
       if(list.source.listener[["First.Prediction"]]){
         list.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.last_action<<-""
         ##Arguments for the prediction function
         Progression.Vars<-list.source.listener[["Progression.Vars"]]
         Method.used<-list.source.listener[["Method.used"]]
         params.to.assign<-list.source.listener[["params.to.assign"]]
         Settings_Table<-list.source.listener[["Settings_Table"]]
         levels.table<-list.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.source.listener[["Used.Models"]]
         uploaded.data<-list.source.listener[["uploaded.data"]]
         SPY.fixed<-list.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.source.listener[["Outliers.settings.saved"]]
         diff.time<-list.source.listener[["diff.time"]]
         models.dir.Source.listener_import<-list.source.listener[["models.dir.Source.listener_import"]]
         models.dir.Source.listener_export<-list.source.listener[["models.dir.Source.listener_export"]]
         models.to.start<-list.source.listener[["models.to.start"]]
         table.to.start<-list.source.listener[["table.to.start"]]
         ######################################################################
         models <- list.files(models.dir.Source.listener_import, full.names=TRUE)  
         table<-data.frame(Model=models, `Last Modified`=unlist(lapply(models, function(f) format(file.info(f)$mtime, "%Y-%m-%d %H:%M:%S"))), stringsAsFactors=FALSE,check.names = F)
         ####Keep only updated and new files####
         models_updated<-models[models %in% models.to.start]
         ind<-unlist(lapply(models_updated,function(x){
           table[table$Model==x,"Last Modified"]!=table.to.start[table.to.start$Model==x,"Last Modified"]
         }))
         models_updated<-models_updated[ind]
         models_left<-union(models[!models %in% models.to.start],models_updated)
         ########Prediction#####   
         for(inFile in models_left){
           Users.data<-tryCatch({
             temp<-unlist(strsplit(inFile,paste0(models.dir.Source.listener_import,"/"),fixed=TRUE))[2]
             temp<-unlist(strsplit(temp,".",fixed=TRUE))
             name<-temp[1]
             format<-temp[2]
             import(inFile,format=format)
           }, error=function(err){
             return(NULL)
           })
           if(is.null(Users.data)  || is.character(Users.data) || !all(dim(Users.data)>0))
             next
           
           tryCatch({
             data.to.return<-Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                                               levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                                               uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                                               necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
             write_excel_csv(data.to.return,path = paste0(models.dir.Source.listener_export,paste0("/",name,"_Output"),".csv"))
           }, error=function(err){})
         }
         ########End of Prediction######  
         list.source.listener[["models.to.start"]]<<-models
         list.source.listener[["table.to.start"]]<<-table
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.last_action<<-paste("Error occured")
         return()
       })
     })
     ##########Source Listener ODBC##########
     output$Source.listener.ODBC.table.Rhands <- renderRHandsontable({
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.ODBC.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.ODBC.table.Rhands
           currentmodel$Loaded.Source.listener.ODBC.table.Rhands<-NULL
         } else{
           parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query","Output Table Name")
           insert<-c("","","","Example: select * from table","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })
       
       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.odbc.source.listener<-reactiveValues(val=FALSE)
     source.listener.ODBC.table.last_action<-""
     
     
     observeEvent(input$MainSubmit.source.listener.ODBC.table,{
       source.listener.ODBC.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.ODBC.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.ODBC.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.ODBC.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.ODBC.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.ODBC.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         ###Read Data from SQL
         Users.data <-tryCatch({
           myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                pwd=Table[Table$Parameters=="PASSWORD","Insert"])
           sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.ODBC.table.last_action<<-paste("Error occured")
           close(myconn)
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.ODBC.table.last_action<<-paste("Table is empty")
           close(myconn)
           return()
         }
         if(!("ID" %in% names(Users.data))){
           source.listener.ODBC.table.last_action<<-paste("Table must have incremental column called ID")
           close(myconn)
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             sqlQuery(myconn,paste("DROP TABLE IF EXISTS",output_table_name)) 
             sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
           }, error=function(err){})
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.odbc.source.listener<<-list("old_data"=old_data,
                                          "Progression.Vars"=Progression.Vars,
                                          "Method.used"=Method.used,
                                          "params.to.assign"=params.to.assign,
                                          "Settings_Table"=Settings_Table,
                                          "levels.table"=levels.table,
                                          "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                          "Create.is.na.vars"=Create.is.na.vars,
                                          "Used.Models"=Used.Models,
                                          "uploaded.data"=uploaded.data,
                                          "SPY.fixed"=SPY.fixed,
                                          "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                          "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                          "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                          "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                          "Outliers.settings.saved"=Outliers.settings.saved,
                                          "output_data"=output_data,
                                          "output_table_name"=output_table_name,
                                          "Query"=Table[Table$Parameters=="Query","Insert"],
                                          "myconn"=myconn,
                                          "diff.time"=diff.time,
                                          "First.Prediction"=TRUE)
         
         continue.odbc.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.ODBC.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.ODBC.table.Text<-renderText({
       input$MainSubmit.source.listener.ODBC.table
       input$MainSubmit.stop.source.listener.ODBC.table
       source.listener.ODBC.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.ODBC.table,{
       tryCatch({
         myconn<-list.odbc.source.listener[["myconn"]]
         continue.odbc.source.listener$val<-FALSE
         source.listener.ODBC.table.last_action<<-paste("Calclation stopped")
         close(myconn)
       } , error=function(err) {})
     })
     
     observe({
       if(!continue.odbc.source.listener$val)
         return()
       if(list.odbc.source.listener[["First.Prediction"]]){
         list.odbc.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.odbc.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.ODBC.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.odbc.source.listener[["old_data"]]
         Progression.Vars<-list.odbc.source.listener[["Progression.Vars"]]
         Method.used<-list.odbc.source.listener[["Method.used"]]
         params.to.assign<-list.odbc.source.listener[["params.to.assign"]]
         Settings_Table<-list.odbc.source.listener[["Settings_Table"]]
         levels.table<-list.odbc.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.odbc.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.odbc.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.odbc.source.listener[["Used.Models"]]
         uploaded.data<-list.odbc.source.listener[["uploaded.data"]]
         SPY.fixed<-list.odbc.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.odbc.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.odbc.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.odbc.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.odbc.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.odbc.source.listener[["Outliers.settings.saved"]]
         output_data<-list.odbc.source.listener[["output_data"]]
         output_table_name<-list.odbc.source.listener[["output_table_name"]]
         Query<-list.odbc.source.listener[["Query"]]
         myconn<-list.odbc.source.listener[["myconn"]]
         diff.time<-list.odbc.source.listener[["diff.time"]]
         ######################################################################
         new_data <-tryCatch({
           sqlQuery(myconn,Query) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###If old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         output_data<-rbind(temp.output_data,output_data)
         tryCatch({
           sqlQuery(myconn,paste("DROP TABLE IF EXISTS",output_table_name)) 
           sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
         }, error=function(err){})
         list.odbc.source.listener[["old_data"]]<<-old_data 
         list.odbc.source.listener[["output_data"]]<<-output_data
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.ODBC.table.last_action<<-paste("Error occured")
         return()
       })
     })
     ##########Source Listener MS SQL##########
     output$Source.listener.MS.SQL.table.Rhands <- renderRHandsontable({
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.MS.SQL.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.MS.SQL.table.Rhands
           currentmodel$Loaded.Source.listener.MS.SQL.table.Rhands<-NULL
         } else{
           parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query","Output Table Name")
           insert<-c("","","","Example: select * from table","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })

       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.ms.sql.source.listener<-reactiveValues(val=FALSE)
     source.listener.MS.SQL.table.last_action<-""
     
     #####SQL
     #odbcConnect("SQL_conn", uid="", pwd="")
     #sqlQuery("select * from source_table")
     
     observeEvent(input$MainSubmit.source.listener.MS.SQL.table,{
       source.listener.MS.SQL.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.MS.SQL.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.MS.SQL.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.MS.SQL.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.MS.SQL.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.MS.SQL.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         ###Read Data from SQL
         Users.data <-tryCatch({
           myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                pwd=Table[Table$Parameters=="PASSWORD","Insert"])
           sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.MS.SQL.table.last_action<<-paste("Error occured")
           close(myconn)
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.MS.SQL.table.last_action<<-paste("Table is empty")
           close(myconn)
           return()
         }
         if(!("ID" %in% names(Users.data))){
           source.listener.MS.SQL.table.last_action<<-paste("Table must have incremental column called ID")
           close(myconn)
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             sqlQuery(myconn,paste(paste0("IF OBJECT_ID('",output_table_name,"')"),"IS NOT NULL DROP TABLE",output_table_name)) 
             sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
           }, error=function(err){})
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.ms.sql.source.listener<<-list("old_data"=old_data,
                                            "Progression.Vars"=Progression.Vars,
                                            "Method.used"=Method.used,
                                            "params.to.assign"=params.to.assign,
                                            "Settings_Table"=Settings_Table,
                                            "levels.table"=levels.table,
                                            "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                            "Create.is.na.vars"=Create.is.na.vars,
                                            "Used.Models"=Used.Models,
                                            "uploaded.data"=uploaded.data,
                                            "SPY.fixed"=SPY.fixed,
                                            "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                            "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                            "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                            "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                            "Outliers.settings.saved"=Outliers.settings.saved,
                                            "output_data"=output_data,
                                            "output_table_name"=output_table_name,
                                            "Query"=Table[Table$Parameters=="Query","Insert"],
                                            "myconn"=myconn,
                                            "diff.time"=diff.time,
                                            "First.Prediction"=TRUE)
         
         continue.ms.sql.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.MS.SQL.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.MS.SQL.table.Text<-renderText({
       input$MainSubmit.source.listener.MS.SQL.table
       input$MainSubmit.stop.source.listener.MS.SQL.table
       source.listener.MS.SQL.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.MS.SQL.table,{
       tryCatch({
         myconn<-list.ms.sql.source.listener[["myconn"]]
         continue.ms.sql.source.listener$val<-FALSE
         source.listener.MS.SQL.table.last_action<<-paste("Calclation stopped")
         close(myconn)
       } , error=function(err) {})
     })
     
     observe({
       if(!continue.ms.sql.source.listener$val)
         return()
       if(list.ms.sql.source.listener[["First.Prediction"]]){
         list.ms.sql.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.ms.sql.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.MS.SQL.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.ms.sql.source.listener[["old_data"]]
         Progression.Vars<-list.ms.sql.source.listener[["Progression.Vars"]]
         Method.used<-list.ms.sql.source.listener[["Method.used"]]
         params.to.assign<-list.ms.sql.source.listener[["params.to.assign"]]
         Settings_Table<-list.ms.sql.source.listener[["Settings_Table"]]
         levels.table<-list.ms.sql.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.ms.sql.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.ms.sql.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.ms.sql.source.listener[["Used.Models"]]
         uploaded.data<-list.ms.sql.source.listener[["uploaded.data"]]
         SPY.fixed<-list.ms.sql.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.ms.sql.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.ms.sql.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.ms.sql.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.ms.sql.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.ms.sql.source.listener[["Outliers.settings.saved"]]
         output_data<-list.ms.sql.source.listener[["output_data"]]
         output_table_name<-list.ms.sql.source.listener[["output_table_name"]]
         Query<-list.ms.sql.source.listener[["Query"]]
         myconn<-list.ms.sql.source.listener[["myconn"]]
         diff.time<-list.ms.sql.source.listener[["diff.time"]]
         ######################################################################
         new_data <-tryCatch({
           sqlQuery(myconn,Query) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###If old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         output_data<-rbind(temp.output_data,output_data)
         tryCatch({
           sqlQuery(myconn,paste(paste0("IF OBJECT_ID('",output_table_name,"')"),"IS NOT NULL DROP TABLE",output_table_name)) 
           sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
         }, error=function(err){})
         list.ms.sql.source.listener[["old_data"]]<<-old_data 
         list.ms.sql.source.listener[["output_data"]]<<-output_data
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.MS.SQL.table.last_action<<-paste("Error occured")
         return()
       })
     })
     ##########Source Listener MySQL##########
     output$Source.listener.MySQL.table.Rhands <- renderRHandsontable({
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.MySQL.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.MySQL.table.Rhands
           currentmodel$Loaded.Source.listener.MySQL.table.Rhands<-NULL
         } else{
           parameters<-c("DSN(ODBC)","USERID","PASSWORD","Query","Output Table Name")
           insert<-c("","","","Example: select * from table","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })

       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.mysql.source.listener<-reactiveValues(val=FALSE)
     source.listener.MySQL.table.last_action<-""
     
     #####MySQL
     #myconn <-odbcConnect("MySql_Conn", uid="root",pwd="Baseld1990")
     #sqlQuery(myconn,"select * from new_schema.`pima-oracle`") 
     #,Mysqlconn,127.0.0.1,uid=root,pwd=Baseld1990
     
     observeEvent(input$MainSubmit.source.listener.MySQL.table,{
       source.listener.MySQL.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.MySQL.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.MySQL.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.MySQL.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.MySQL.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.MySQL.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         ###Read Data from SQL
         Users.data <-tryCatch({
           myconn <-odbcConnect(Table[Table$Parameters=="DSN(ODBC)","Insert"], uid=Table[Table$Parameters=="USERID","Insert"], 
                                pwd=Table[Table$Parameters=="PASSWORD","Insert"])
           sqlQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.MySQL.table.last_action<<-paste("Error occured")
           close(myconn)
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.MySQL.table.last_action<<-paste("Table is empty")
           close(myconn)
           return()
         }
         if(!("ID" %in% names(Users.data))){
           source.listener.MySQL.table.last_action<<-paste("Table must have incremental column called ID")
           close(myconn)
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars 
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             sqlQuery(myconn,paste("DROP TABLE IF EXISTS",output_table_name)) 
             sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
           }, error=function(err){})
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.mysql.source.listener<<-list("old_data"=old_data,
                                           "Progression.Vars"=Progression.Vars,
                                           "Method.used"=Method.used,
                                           "params.to.assign"=params.to.assign,
                                           "Settings_Table"=Settings_Table,
                                           "levels.table"=levels.table,
                                           "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                           "Create.is.na.vars"=Create.is.na.vars,
                                           "Used.Models"=Used.Models,
                                           "uploaded.data"=uploaded.data,
                                           "SPY.fixed"=SPY.fixed,
                                           "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                           "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                           "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                           "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                           "Outliers.settings.saved"=Outliers.settings.saved,
                                           "output_data"=output_data,
                                           "output_table_name"=output_table_name,
                                           "Query"=Table[Table$Parameters=="Query","Insert"],
                                           "myconn"=myconn,
                                           "diff.time"=diff.time,
                                           "First.Prediction"=TRUE)
         
         continue.mysql.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.MySQL.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.MySQL.table.Text<-renderText({
       input$MainSubmit.source.listener.MySQL.table
       input$MainSubmit.stop.source.listener.MySQL.table
       source.listener.MySQL.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.MySQL.table,{
       tryCatch({
         myconn<-list.mysql.source.listener[["myconn"]]
         continue.mysql.source.listener$val<-FALSE
         source.listener.MySQL.table.last_action<<-paste("Calclation stopped")
         close(myconn)
       } , error=function(err) {})
     })
     
     observe({
       if(!continue.mysql.source.listener$val)
         return()
       if(list.mysql.source.listener[["First.Prediction"]]){
         list.mysql.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.mysql.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.MySQL.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.mysql.source.listener[["old_data"]]
         Progression.Vars<-list.mysql.source.listener[["Progression.Vars"]]
         Method.used<-list.mysql.source.listener[["Method.used"]]
         params.to.assign<-list.mysql.source.listener[["params.to.assign"]]
         Settings_Table<-list.mysql.source.listener[["Settings_Table"]]
         levels.table<-list.mysql.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.mysql.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.mysql.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.mysql.source.listener[["Used.Models"]]
         uploaded.data<-list.mysql.source.listener[["uploaded.data"]]
         SPY.fixed<-list.mysql.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.mysql.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.mysql.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.mysql.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.mysql.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.mysql.source.listener[["Outliers.settings.saved"]]
         output_data<-list.mysql.source.listener[["output_data"]]
         output_table_name<-list.mysql.source.listener[["output_table_name"]]
         Query<-list.mysql.source.listener[["Query"]]
         myconn<-list.mysql.source.listener[["myconn"]]
         diff.time<-list.mysql.source.listener[["diff.time"]]
         ######################################################################
         new_data <-tryCatch({
           sqlQuery(myconn,Query) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###If old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         output_data<-rbind(temp.output_data,output_data)
         tryCatch({
           sqlQuery(myconn,paste("DROP TABLE IF EXISTS",output_table_name)) 
           sqlSave(myconn,output_data,output_table_name,rownames=FALSE)
         }, error=function(err){})
         list.mysql.source.listener[["old_data"]]<<-old_data 
         list.mysql.source.listener[["output_data"]]<<-output_data
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.MySQL.table.last_action<<-paste("Error occured")
         return()
       })
     })
     ##########Source Listener Oracle##########
     output$Source.listener.Oracle.table.Rhands <- renderRHandsontable({
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.Oracle.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.Oracle.table.Rhands
           currentmodel$Loaded.Source.listener.Oracle.table.Rhands<-NULL
         } else{
           parameters<-c("ojdbc.jar Path","USERID","PASSWORD","Query","Output Table Name")
           insert<-c("","","","Example: select * from table","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })
       
       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.oracle.source.listener<-reactiveValues(val=FALSE)
     source.listener.Oracle.table.last_action<-""
     
     #Users.data <-tryCatch({
     #  drv <-JDBC("oracle.jdbc.OracleDriver",
     #                classPath="C:/Users/Admin/Downloads/ojdbc5.jar"," ") 
     #  myconn<- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:orcl","basel",
     #                     "Baseld1990") 
     #  dbGetQuery(myconn,"select * from source_table")  
     #} , error=function(err) {
     #  return(NULL)
     #})  
     #####Oracle account needed to download ojdbc.jar
     ##### user: my mail , password:Baseld1990
     
     observeEvent(input$MainSubmit.source.listener.Oracle.table,{
       source.listener.Oracle.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.Oracle.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.Oracle.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.Oracle.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.Oracle.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.Oracle.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         Users.data <-tryCatch({
           drv <-JDBC("oracle.jdbc.OracleDriver",
                      classPath=Table[Table$Parameters=="ojdbc.jar Path","Insert"]," ") 
           myconn<- dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:orcl",Table[Table$Parameters=="USERID","Insert"],
                              Table[Table$Parameters=="PASSWORD","Insert"]) 
           dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"])  
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.Oracle.table.last_action<<-paste("Error occured")
           dbDisconnect(myconn)
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.Oracle.table.last_action<<-paste("Table is empty")
           dbDisconnect(myconn)
           return()
         }
         if(!("ID" %in% names(Users.data))){
           source.listener.Oracle.table.last_action<<-paste("Table must have incremental column called ID")
           dbDisconnect(myconn)
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           temp<-Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
           ##Oracle sql developer dosent accept "." in column names
           names(temp)<-gsub(".","_",make.names(names(temp),unique=TRUE),fixed=TRUE)
           temp
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             dbWriteTable(myconn,output_table_name,output_data)
           }, error=function(err){
             return(dbWriteTable(myconn,toupper(output_table_name),output_data,append=TRUE, overwrite=TRUE))
           })
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.oracle.source.listener<<-list("old_data"=old_data,
                                            "Progression.Vars"=Progression.Vars,
                                            "Method.used"=Method.used,
                                            "params.to.assign"=params.to.assign,
                                            "Settings_Table"=Settings_Table,
                                            "levels.table"=levels.table,
                                            "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                            "Create.is.na.vars"=Create.is.na.vars,
                                            "Used.Models"=Used.Models,
                                            "uploaded.data"=uploaded.data,
                                            "SPY.fixed"=SPY.fixed,
                                            "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                            "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                            "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                            "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                            "Outliers.settings.saved"=Outliers.settings.saved,
                                            "output_data"=output_data,
                                            "output_table_name"=output_table_name,
                                            "Query"=Table[Table$Parameters=="Query","Insert"],
                                            "myconn"=myconn,
                                            "diff.time"=diff.time,
                                            "First.Prediction"=TRUE)
         
         continue.oracle.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.Oracle.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.Oracle.table.Text<-renderText({
       input$MainSubmit.source.listener.Oracle.table
       input$MainSubmit.stop.source.listener.Oracle.table
       source.listener.Oracle.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.Oracle.table,{
       tryCatch({
       myconn<-list.oracle.source.listener[["myconn"]]
       continue.oracle.source.listener$val<-FALSE
       source.listener.Oracle.table.last_action<<-paste("Calclation stopped")
       dbDisconnect(myconn)
       }, error=function(err){})
     })
     
     observe({
       if(!continue.oracle.source.listener$val)
         return()
       if(list.oracle.source.listener[["First.Prediction"]]){
         list.oracle.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.oracle.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.Oracle.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.oracle.source.listener[["old_data"]]
         Progression.Vars<-list.oracle.source.listener[["Progression.Vars"]]
         Method.used<-list.oracle.source.listener[["Method.used"]]
         params.to.assign<-list.oracle.source.listener[["params.to.assign"]]
         Settings_Table<-list.oracle.source.listener[["Settings_Table"]]
         levels.table<-list.oracle.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.oracle.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.oracle.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.oracle.source.listener[["Used.Models"]]
         uploaded.data<-list.oracle.source.listener[["uploaded.data"]]
         SPY.fixed<-list.oracle.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.oracle.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.oracle.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.oracle.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.oracle.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.oracle.source.listener[["Outliers.settings.saved"]]
         output_data<-list.oracle.source.listener[["output_data"]]
         output_table_name<-list.oracle.source.listener[["output_table_name"]]
         Query<-list.oracle.source.listener[["Query"]]
         myconn<-list.oracle.source.listener[["myconn"]]
         diff.time<-list.oracle.source.listener[["diff.time"]]
         
         ######################################################################
         new_data <-tryCatch({
           dbGetQuery(myconn,Query) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###IIf old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           temp<-Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
           names(temp)<-gsub(".","_",make.names(names(temp),unique=TRUE),fixed=TRUE)
           temp
         }, error=function(err){
           return(NULL)
         })
         
           output_data<-rbind(temp.output_data,output_data)
           tryCatch({
             dbWriteTable(myconn,output_table_name,output_data)
           }, error=function(err){
             return(dbWriteTable(myconn,toupper(output_table_name),output_data,append=TRUE, overwrite=TRUE))
           })
           list.oracle.source.listener[["old_data"]]<<-old_data 
           list.oracle.source.listener[["output_data"]]<<-output_data
 
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.Oracle.table.last_action<<-paste("Error occured")
         return()
       })
     })
     

     ##########Source Listener Amazon Redshift##########
     output$Source.listener.Amazon.Redshift.table.Rhands <- renderRHandsontable({ 
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.Amazon.Redshift.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.Amazon.Redshift.table.Rhands
           currentmodel$Loaded.Source.listener.Amazon.Redshift.table.Rhands<-NULL
         } else{
           parameters<-c("Host","DATABASE","PORT","USER","PASSWORD","DRIVER","Query","Output Table Name")
           insert<-c("","","","","","","Example: select * from table","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })

       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.amazon.redshift.source.listener<-reactiveValues(val=FALSE)
     source.listener.Amazon.Redshift.table.last_action<-""
     
     observeEvent(input$MainSubmit.source.listener.Amazon.Redshift.table,{
       source.listener.Amazon.Redshift.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.Amazon.Redshift.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         Users.data <-tryCatch({
           drv <- dbDriver(Table[Table$Parameters=="DRIVER","Insert"])   
           myconn <- dbConnect(drv, host=Table[Table$Parameters=="Host","Insert"], 
                               dbname=Table[Table$Parameters=="DATABASE","Insert"],
                               port=Table[Table$Parameters=="PORT","Insert"],
                               user=Table[Table$Parameters=="USER","Insert"],
                               password=Table[Table$Parameters=="PASSWORD","Insert"])
           dbGetQuery(myconn,Table[Table$Parameters=="Query","Insert"]) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Error occured")
           dbDisconnect(myconn)
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Table is empty")
           dbDisconnect(myconn)
           return()
         }
         if(!("ID" %in% names(Users.data))){
           source.listener.Amazon.Redshift.table.last_action<<-paste("Table must have incremental column called ID")
           dbDisconnect(myconn)
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           temp<-Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                                   levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                                   uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                                   necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
           ##Oracle sql developer dosent accept "." in column names
           names(temp)<-gsub(".","_",make.names(names(temp),unique=TRUE),fixed=TRUE)
           temp
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             dbWriteTable(myconn,output_table_name,output_data)
           }, error=function(err){
             return(dbWriteTable(myconn,toupper(output_table_name),output_data,append=TRUE, overwrite=TRUE))
           })
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.amazon.redshift.source.listener<<-list("old_data"=old_data,
                                                     "Progression.Vars"=Progression.Vars,
                                                     "Method.used"=Method.used,
                                                     "params.to.assign"=params.to.assign,
                                                     "Settings_Table"=Settings_Table,
                                                     "levels.table"=levels.table,
                                                     "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                                     "Create.is.na.vars"=Create.is.na.vars,
                                                     "Used.Models"=Used.Models,
                                                     "uploaded.data"=uploaded.data,
                                                     "SPY.fixed"=SPY.fixed,
                                                     "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                                     "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                                     "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                                     "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                                     "Outliers.settings.saved"=Outliers.settings.saved,
                                                     "output_data"=output_data,
                                                     "output_table_name"=output_table_name,
                                                     "Query"=Table[Table$Parameters=="Query","Insert"],
                                                     "myconn"=myconn,
                                                     "diff.time"=diff.time,
                                                     "First.Prediction"=TRUE)
         
         continue.amazon.redshift.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.Amazon.Redshift.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.Amazon.Redshift.table.Text<-renderText({
       input$MainSubmit.source.listener.Amazon.Redshift.table
       input$MainSubmit.stop.source.listener.Amazon.Redshift.table
       source.listener.Amazon.Redshift.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.Amazon.Redshift.table,{
       tryCatch({
         myconn<-list.amazon.redshift.source.listener[["myconn"]]
         continue.amazon.redshift.source.listener$val<-FALSE
         source.listener.Amazon.Redshift.table.last_action<<-paste("Calclation stopped")
         dbDisconnect(myconn)
       }, error=function(err){})
     })
     
     observe({
       if(!continue.amazon.redshift.source.listener$val)
         return()
       if(list.amazon.redshift.source.listener[["First.Prediction"]]){
         list.amazon.redshift.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.amazon.redshift.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.Amazon.Redshift.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.amazon.redshift.source.listener[["old_data"]]
         Progression.Vars<-list.amazon.redshift.source.listener[["Progression.Vars"]]
         Method.used<-list.amazon.redshift.source.listener[["Method.used"]]
         params.to.assign<-list.amazon.redshift.source.listener[["params.to.assign"]]
         Settings_Table<-list.amazon.redshift.source.listener[["Settings_Table"]]
         levels.table<-list.amazon.redshift.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.amazon.redshift.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.amazon.redshift.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.amazon.redshift.source.listener[["Used.Models"]]
         uploaded.data<-list.amazon.redshift.source.listener[["uploaded.data"]]
         SPY.fixed<-list.amazon.redshift.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.amazon.redshift.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.amazon.redshift.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.amazon.redshift.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.amazon.redshift.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.amazon.redshift.source.listener[["Outliers.settings.saved"]]
         output_data<-list.amazon.redshift.source.listener[["output_data"]]
         output_table_name<-list.amazon.redshift.source.listener[["output_table_name"]]
         Query<-list.amazon.redshift.source.listener[["Query"]]
         myconn<-list.amazon.redshift.source.listener[["myconn"]]
         diff.time<-list.amazon.redshift.source.listener[["diff.time"]]
         ######################################################################
         new_data <-tryCatch({
           dbGetQuery(myconn,Query) 
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###IIf old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           temp<-Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                                   levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                                   uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                                   necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
           names(temp)<-gsub(".","_",make.names(names(temp),unique=TRUE),fixed=TRUE)
           temp
         }, error=function(err){
           return(NULL)
         })
         
         output_data<-rbind(temp.output_data,output_data)
         tryCatch({
           dbWriteTable(myconn,output_table_name,output_data)
         }, error=function(err){
           return(dbWriteTable(myconn,toupper(output_table_name),output_data,append=TRUE, overwrite=TRUE))
         })
         list.amazon.redshift.source.listener[["old_data"]]<<-old_data 
         list.amazon.redshift.source.listener[["output_data"]]<<-output_data
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.Amazon.Redshift.table.last_action<<-paste("Error occured")
         return()
       })
     })
     ###############Source listener Google Big Query########
     output$Source.listener.Google.Big.Query.table.Rhands<- renderRHandsontable({  
       isolate({
         if(!is.null(currentmodel$Loaded.Source.listener.Google.Big.Query.table.Rhands)){
           temp<-currentmodel$Loaded.Source.listener.Google.Big.Query.table.Rhands
           currentmodel$Loaded.Source.listener.Google.Big.Query.table.Rhands<-NULL
         } else{
           parameters<-c("ProjectID","DatasetId","Query","Output Table Name")
           insert<-c("","Example: select * from table","","Example: table_output")
           temp <- data.frame("Parameters"=parameters, "Insert"=insert, stringsAsFactors=FALSE)
         }
       })

       rhandsontable(temp,maxRows=nrow(temp)) %>% 
         hot_col("Parameters", readOnly=TRUE) %>%
         hot_col("Insert",width=350,strict = FALSE)  
     })
     
     continue.google.big.query.source.listener<-reactiveValues(val=FALSE)
     source.listener.Google.Big.Query.table.last_action<-""
     
     
     observeEvent(input$MainSubmit.source.listener.Google.Big.Query.table,{
       source.listener.Google.Big.Query.table.last_action<<-""
       tryCatch({
         ############Check Input################################  
         if(is.na(input$Source.listener.time)){
           source.listener.Google.Big.Query.table.last_action<<-paste("Time is not chosen")
           return()
         }
         if(input$Source.listener.time<=0){
           source.listener.Google.Big.Query.table.last_action<<-paste("Time must be positive")
           return()
         }  
         if(is.null(input$choose_Method_for_Source.listener)){
           source.listener.Google.Big.Query.table.last_action<<-paste("Model is not chosen")
           return()
         }
         Table <- hot_to_r(input$Source.listener.Google.Big.Query.table.Rhands) 
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         if(output_table_name==""){
           source.listener.Google.Big.Query.table.last_action<<-paste("Output Table Name is empty")
           return()
         }  
         ###############################################  
         Users.data <-tryCatch({
           gar_auth_service(
             json_file = input$Source.listener.Google.Big.Query.json.file$datapath,   
             scope = "https://www.googleapis.com/auth/bigquery"
           )
           bqr_query(projectId = Table[Table$Parameters=="ProjectID","Insert"],query =Table[Table$Parameters=="Query","Insert"],
                     datasetId = Table[Table$Parameters=="DatasetId","Insert"],useLegacySql = input$Source.listener.Google.Big.Query.useLegacySql)
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(Users.data) || is.character(Users.data)){
           source.listener.Google.Big.Query.table.last_action<<-paste("Error occured")
           return()
         }  #is.character(Users.data) when it's an error
         if(!all(dim(Users.data)>0)){
           source.listener.Google.Big.Query.table.last_action<<-paste("Table is empty")
           return()
         }
         ###################################################
         ##Arguments for the prediction function
         Progression.Vars<-currentmodel$necessary.variables.found  
         Method.used<-currentmodel$Method.used 
         params.to.assign<-currentmodel$params.to.assign
         Settings_Table<-currentmodel$Settings_Table
         levels.table<-currentmodel$levels.table 
         ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside 
         Create.is.na.vars<-currentmodel$Create.is.na.vars
         Used.Models<-currentmodel$Used.Models 
         uploaded.data<-currentmodel$uploaded.data
         SPY.fixed<-currentmodel$SPY.fixed
         text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
         choose_Method_for_Source.listener<-input$choose_Method_for_Source.listener
         necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
         Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
         Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
         ###################################
         ########Prediction#####   
         output_data<-tryCatch({
           Source.prediction(Users.data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer,Outliers.settings.saved)
         }, error=function(err){
           return(NULL)
         })
         
         if(!is.null(output_data)){
           tryCatch({
             bqr_upload_data(projectId =Table[Table$Parameters=="ProjectID","Insert"],
                             datasetId =Table[Table$Parameters=="DatasetId","Insert"],
                             tableId=output_table_name,upload_data=output_data,create="CREATE_IF_NEEDED",overwrite=FALSE)
           }, error=function(err){
             return(bqr_upload_data(projectId =Table[Table$Parameters=="ProjectID","Insert"],
                                    datasetId =Table[Table$Parameters=="DatasetId","Insert"],
                                    tableId=output_table_name,upload_data=output_data,create="CREATE_NEVER",overwrite=TRUE))
           })
         }
         ########End of Prediction######  
         old_data<-Users.data
         diff.time<-transfer.to.secs(input$Source.listener.time,input$Source.listener.time.units) 
         
         list.google.big.query.source.listener<<-list("old_data"=old_data,
                                                      "Progression.Vars"=Progression.Vars,
                                                      "Method.used"=Method.used,
                                                      "params.to.assign"=params.to.assign,
                                                      "Settings_Table"=Settings_Table,
                                                      "levels.table"=levels.table,
                                                      "ratio.as.dummies.inside"=ratio.as.dummies.inside,
                                                      "Create.is.na.vars"=Create.is.na.vars,
                                                      "Used.Models"=Used.Models,
                                                      "uploaded.data"=uploaded.data,
                                                      "SPY.fixed"=SPY.fixed,
                                                      "text.analysis.for.export.saved"=text.analysis.for.export.saved,
                                                      "choose_Method_for_Source.listener"=choose_Method_for_Source.listener,
                                                      "necessary.variables.found.for.first.layer"=necessary.variables.found.for.first.layer,
                                                      "Used.Models.for.first.layer"=Used.Models.for.first.layer,
                                                      "Outliers.settings.saved"=Outliers.settings.saved,
                                                      "output_data"=output_data,
                                                      "Table"=Table,
                                                      "Source.listener.Google.Big.Query.useLegacySql"=input$Source.listener.Google.Big.Query.useLegacySql,
                                                      "diff.time"=diff.time,
                                                      "First.Prediction"=TRUE)
         
         continue.google.big.query.source.listener$val<-TRUE
       }, error=function(err){
         source.listener.Google.Big.Query.table.last_action<<-paste("Error occured")
         return()
       })
     })
     
     output$Source.listener.Google.Big.Query.table.Text<-renderText({
       input$MainSubmit.source.listener.Google.Big.Query.table
       input$MainSubmit.stop.source.listener.Google.Big.Query.table
       source.listener.Google.Big.Query.table.last_action
     })
     
     observeEvent(input$MainSubmit.stop.source.listener.Google.Big.Query.table,{
       continue.google.big.query.source.listener$val<-FALSE
       source.listener.Google.Big.Query.table.last_action<<-paste("Calclation stopped")
     })
     
     observe({
       if(!continue.google.big.query.source.listener$val)
         return()
       if(list.google.big.query.source.listener[["First.Prediction"]]){
         list.google.big.query.source.listener[["First.Prediction"]]<<-FALSE
         diff.time<-list.google.big.query.source.listener[["diff.time"]]
         invalidateLater(diff.time*1000, session)
         return()
       }
       tryCatch({
         source.listener.Google.Big.Query.table.last_action<<-""
         ##Arguments for the prediction function
         old_data<-list.google.big.query.source.listener[["old_data"]]
         Progression.Vars<-list.google.big.query.source.listener[["Progression.Vars"]]
         Method.used<-list.google.big.query.source.listener[["Method.used"]]
         params.to.assign<-list.google.big.query.source.listener[["params.to.assign"]]
         Settings_Table<-list.google.big.query.source.listener[["Settings_Table"]]
         levels.table<-list.google.big.query.source.listener[["levels.table"]]
         ratio.as.dummies.inside<-list.google.big.query.source.listener[["ratio.as.dummies.inside"]]
         Create.is.na.vars<-list.google.big.query.source.listener[["Create.is.na.vars"]]
         Used.Models<-list.google.big.query.source.listener[["Used.Models"]]
         uploaded.data<-list.google.big.query.source.listener[["uploaded.data"]]
         SPY.fixed<-list.google.big.query.source.listener[["SPY.fixed"]]
         text.analysis.for.export.saved<-list.google.big.query.source.listener[["text.analysis.for.export.saved"]]
         choose_Method_for_Source.listener<-list.google.big.query.source.listener[["choose_Method_for_Source.listener"]]
         necessary.variables.found.for.first.layer<-list.google.big.query.source.listener[["necessary.variables.found.for.first.layer"]]
         Used.Models.for.first.layer<-list.google.big.query.source.listener[["Used.Models.for.first.layer"]]
         Outliers.settings.saved<-list.google.big.query.source.listener[["Outliers.settings.saved"]]
         output_data<-list.google.big.query.source.listener[["output_data"]]
         Table<-list.google.big.query.source.listener[["Table"]]
         Source.listener.Google.Big.Query.useLegacySql<-list.google.big.query.source.listener[["Source.listener.Google.Big.Query.useLegacySql"]]
         diff.time<-list.google.big.query.source.listener[["diff.time"]]
         output_table_name<-Table[Table$Parameters=="Output Table Name","Insert"]
         ######################################################################
         new_data <-tryCatch({
           bqr_query(projectId = Table[Table$Parameters=="ProjectID","Insert"],query =Table[Table$Parameters=="Query","Insert"],
                     datasetId = Table[Table$Parameters=="DatasetId","Insert"],useLegacySql = Source.listener.Google.Big.Query.useLegacySql)
         } , error=function(err) {
           return(NULL)
         })
         
         ################Check imported Table####################################
         if(is.null(new_data) || is.character(new_data)){
           invalidateLater(diff.time*1000, session)
           return()  #is.character(Users.data) when it's an error
         }
         if(!all(dim(new_data)>0)){
           invalidateLater(diff.time*1000, session)
           return()
         }
         if(!("ID" %in% names(new_data))){
           invalidateLater(diff.time*1000, session)
           return()
         }
         #################################################################
         ###Check difference between this table and the last one
         ###IIf old.data is NULL max(old_data$ID,na.rm=TRUE)=-Inf
         ind.new.id<-which(new_data$ID>max(old_data$ID,na.rm=TRUE))
         diff_data<-new_data[ind.new.id,] 
         if(nrow(diff_data)==0){
           invalidateLater(diff.time*1000, session)
           return()
         }
         old_data<-new_data 
         #########Perfom Prediction on diff.data and add to the output.data we got####################### 
         temp.output_data<-tryCatch({
           Source.prediction(diff_data,Progression.Vars,Method.used,params.to.assign,Settings_Table,
                             levels.table,ratio.as.dummies.inside,Create.is.na.vars,Used.Models,
                             uploaded.data,SPY.fixed,text.analysis.for.export.saved,choose_Method_for_Source.listener,
                             necessary.variables.found.for.first.layer,Used.Models.for.first.layer)
         }, error=function(err){
           return(NULL)
         })
         
         output_data<-rbind(temp.output_data,output_data)
         tryCatch({
           bqr_upload_data(projectId =Table[Table$Parameters=="ProjectID","Insert"],
                           datasetId =Table[Table$Parameters=="DatasetId","Insert"],
                           tableId=output_table_name,upload_data=output_data,create="CREATE_IF_NEEDED",overwrite=FALSE)
         }, error=function(err){
           return(bqr_upload_data(projectId =Table[Table$Parameters=="ProjectID","Insert"],
                                  datasetId =Table[Table$Parameters=="DatasetId","Insert"],
                                  tableId=output_table_name,upload_data=output_data,create="CREATE_NEVER",overwrite=TRUE))
         })
         list.google.big.query.source.listener[["old_data"]]<<-old_data 
         list.google.big.query.source.listener[["output_data"]]<<-output_data
         
         invalidateLater(diff.time*1000, session)
         
       }, error=function(err){
         source.listener.Google.Big.Query.table.last_action<<-paste("Error occured")
         return()
       })
     })
    ##########################################################
    ## Block for saving/loading models
    ##########################################################
    
    observeEvent(input$ChooseDirButton, {
      temp <- choose.dir()
      if (!is.na(temp)) {
        models_dir <<- temp
      }
    })
    
    output$ModelToChoose = DT::renderDataTable({
      input$ChooseDirButton
      input$SaveButton
      input$DeleteButton
      
      models <- list.files(models_dir, ".model", full.names=TRUE, recursive=TRUE)
      if (length(models)>0) {
        model_files <<- models
        models_found <<- paste(length(models), "models found in the directory")
        table<-data.frame(Model=models, `Last Modified`=unlist(lapply(models, function(f) format(file.info(f)$mtime, "%Y-%m-%d %H:%M:%S"))), stringsAsFactors=FALSE,check.names = F)
        datatable(table, selection = 'single',
                  options=list(pageLength = 10))
      } else {
        model_files <<- c()
        models_found <<- "No models found in the directory"
        NULL
      }
    }, server = FALSE)
                 
    
    observeEvent(input$SaveButton, {
      ##Main ###Train
      DataBase.Train.type<-input$DataBase.Train.type
      Shuffle.Train<-input$Shuffle.Train
      Dates.as.parts.Train<-input$Dates.as.parts.Train
      division.rate.slider<-input$division.rate.slider
      division.rate<-currentmodel$division.rate
      uploaded.data <- currentmodel$uploaded.data
      Basic.uploaded.data<-currentmodel$Basic.uploaded.data
      ####DBA
      ODBC_Train.table<-tryCatch({hot_to_r(input$ODBC_Train.table)},error=function(err){return(NULL)})
      ODBC_CV.table<-tryCatch({hot_to_r(input$ODBC_CV.table)},error=function(err){return(NULL)})
      ODBC_Pred.table<-tryCatch({hot_to_r(input$ODBC_Pred.table)},error=function(err){return(NULL)})
      ##
      MS_sql_Train.table<-tryCatch({hot_to_r(input$MS_sql_Train.table)},error=function(err){return(NULL)})
      MS_sql_CV.table<-tryCatch({hot_to_r(input$MS_sql_CV.table)},error=function(err){return(NULL)})
      MS_sql_Pred.table<-tryCatch({hot_to_r(input$MS_sql_Pred.table)},error=function(err){return(NULL)})
      ##
      Mysql_Train.table<-tryCatch({hot_to_r(input$Mysql_Train.table)},error=function(err){return(NULL)})
      Mysql_CV.table<-tryCatch({hot_to_r(input$Mysql_CV.table)},error=function(err){return(NULL)})
      Mysql_Pred.table<-tryCatch({hot_to_r(input$Mysql_Pred.table)},error=function(err){return(NULL)})
      ##
      Oracle_Train.table<-tryCatch({hot_to_r(input$Oracle_Train.table)},error=function(err){return(NULL)})
      Oracle_CV.table<-tryCatch({hot_to_r(input$Oracle_CV.table)},error=function(err){return(NULL)})
      Oracle_Pred.table<-tryCatch({hot_to_r(input$Oracle_Pred.table)},error=function(err){return(NULL)})
      ##
      Amazon_Redshift_Train.table<-tryCatch({hot_to_r(input$Amazon_Redshift_Train.table)},error=function(err){return(NULL)})
      Amazon_Redshift_CV.table<-tryCatch({hot_to_r(input$Amazon_Redshift_CV.table)},error=function(err){return(NULL)})
      Amazon_Redshift_Pred.table<-tryCatch({hot_to_r(input$Amazon_Redshift_Pred.table)},error=function(err){return(NULL)})
      ##
      Google_Big_Query_Train.table<-tryCatch({hot_to_r(input$Google_Big_Query_Train.table)},error=function(err){return(NULL)})
      Google_Big_Query_CV.table<-tryCatch({hot_to_r(input$Google_Big_Query_CV.table)},error=function(err){return(NULL)})
      Google_Big_Query_Pred.table<-tryCatch({hot_to_r(input$Google_Big_Query_Pred.table)},error=function(err){return(NULL)})
      useLegacySql.Train<-input$useLegacySql.Train
      useLegacySql.CV<-input$useLegacySql.CV
      useLegacySql.Pred<-input$useLegacySql.Pred
      ###
      ##Text Analysis
      text.analysis.split<-input$text.analysis.split
      Words_split<-input$Words_split
      Show.only.Exp<-input$Show.only.Exp
      Exclude.stopwords<-input$Exclude.stopwords
      Our.Pos.Exp<-input$Our.Pos.Exp
      Our.Neg.Exp<-input$Our.Neg.Exp
      Our.StopWords<-input$Our.StopWords
      Neg.Exp.table<-textAnalysis$Neg.Exp.table
      Pos.Exp.table<-textAnalysis$Pos.Exp.table
      Other.Exp.table<-textAnalysis$Other.Exp.table
      StopWords.table<-textAnalysis$StopWords.table
      ##Fill Tables
      text.analysis.fill.table.count<-textAnalysis$text.analysis.fill.table.count
      text.analysis.fill.table.regex<-textAnalysis$text.analysis.fill.table.regex
      text.analysis.fill.table.replace.regex<-textAnalysis$text.analysis.fill.table.replace.regex
      text.analysis.fill.table.as.variable<-textAnalysis$text.analysis.fill.table.as.variable
      text.analysis.fill.table.as.variable.using.regex<-textAnalysis$text.analysis.fill.table.as.variable.using.regex
      ###
      text.analysis.for.export<-textAnalysis$text.analysis.for.export
      text.analysis.for.export.saved<-textAnalysis$text.analysis.for.export.saved
      text.analysis_chosen.var<-textAnalysis$text.analysis_chosen.var
      transfer.string.to.dummy<-input$transfer.string.to.dummy
      text.analysis.phrases.plot.export.extension<-input$text.analysis.phrases.plot.export.extension
      ##Configure
      Method<-input$Method
      Method.saved<-currentmodel$Method.saved
      Column.to.check.configure<-input$Column.to.check.configure
      Target.Vars <- currentmodel$Target.Vars
      vars.to.exclude <- currentmodel$vars.to.exclude
      not.to.fix<-currentmodel$not.to.fix
      not.to.fix.outliers<-currentmodel$not.to.fix.outliers
      Vars.Summary<-tryCatch({hot_to_r(input$Vars.Summary)},error=function(err){return(NULL)})
      ##Filter Data
      Calculate.filter.data<-currentmodel$Calculate.filter.data
      Data.Filter.Index<-currentmodel$Data.Filter.Index
      Numeric.data.filter.table<-currentmodel$Numeric.data.filter.table
      Factor.data.filter.table<-currentmodel$Factor.data.filter.table
      Filter.by.coding.text<-input$Filter.by.coding.text
      ##Ratio Combination
      Interval.for.ratio.combination<-input$Interval.for.ratio.combination
      ratio.comb.mean<-input$ratio.comb.mean
      ratio.comb.count<-input$ratio.comb.count
      ratio.comb.for.couples<-input$ratio.comb.for.couples
      ratio.comb.table.to.show<-currentmodel$ratio.comb.table.to.show
      ratio.comb.table.to.show.saved<-currentmodel$ratio.comb.table.to.show.saved
      ratio.comb.info<-currentmodel$ratio.comb.info
      ##is NA
      Create.is.na.vars<-currentmodel$Create.is.na.vars
      ##Outliers Settings
      Outliers.settings.saved<-OutliersSettings$Outliers.settings.saved
      ##Map
      AllowSampling.GeographyData<-input$AllowSampling.GeographyData
      Percent.GeographyData<-input$Percent.GeographyData
      Map.Filter.Index<-currentmodel$Map.Filter.Index
      Numeric.Geographydata.filter.table<-currentmodel$Numeric.Geographydata.filter.table
      Factor.Geographydata.filter.table<-currentmodel$Factor.Geographydata.filter.table
      ##Prediction Analysis
      AllowSampling<-input$AllowSampling
      percent<-input$percent
      ChooseAlgorithms <- input$ChooseAlgorithms
      Stratified.Shuffle<-input$Stratified.Shuffle
      Use.CV<-input$Use.CV   
      Imputation.Value<-input$Imputation.Value
      Auto.classification.threshold<-input$Auto.classification.threshold
      classification.threshold<-input$classification.threshold
      accuracy.tolerance<-input$accuracy.tolerance
      stability.threshold<-input$stability.threshold
      ChooseCriterion<-input$ChooseCriterion
      Preserve.Accuracy_01<-input$Preserve.Accuracy_01
      Differential.Matrix<-input$Differential.Matrix
      RareEvents<-input$RareEvents
      ChooseCriterion.Estimation<-input$ChooseCriterion.Estimation
      difference.tolerance<-input$difference.tolerance
      analysisMethod<-input$analysisMethod
      Xgb.best.parameters<-input$Xgb.best.parameters
      Xgb.classification.eval.metric<-input$Xgb.classification.eval.metric
      ##Before calculating
      params.to.assign<-currentmodel$params.to.assign
      levels.table<-currentmodel$levels.table
      ratio.as.dummies.inside<-currentmodel$ratio.as.dummies.inside
      Settings_Table<-currentmodel$Settings_Table
      WL.params<-currentmodel$WL.params
      Rpart.params<-currentmodel$Rpart.params
      Xgb.params<-currentmodel$Xgb.params
      Method.used<-currentmodel$Method.used
      SPY.fixed<-currentmodel$SPY.fixed
      SPY.Test.fixed<-currentmodel$SPY.Test.fixed
      ##After calculating
      ##for reducing the size
      temp.necessary.variables.found<-currentmodel$necessary.variables.found
      temp.necessary.variables.found[["Myform"]]<-NULL
      temp.necessary.variables.found[["Models"]]<-NULL
      ##
      Used.Models<-currentmodel$Used.Models
      ##First Layer
      temp.necessary.variables.found.for.first.layer<-currentmodel$necessary.variables.found.for.first.layer
      temp.necessary.variables.found.for.first.layer[["Myform"]]<-NULL
      temp.necessary.variables.found.for.first.layer[["Models"]]<-NULL
      Used.Models.for.first.layer<-currentmodel$Used.Models.for.first.layer
      Two.layers.prediction<-input$Two.layers.prediction
      ChooseAlgorithms.for.first.layer<-input$ChooseAlgorithms.for.first.layer
      WL.params.for.first.layer<-currentmodel$WL.params.for.first.layer
      Rpart.params.for.first.layer<-currentmodel$Rpart.params.for.first.layer
      Xgb.params.for.first.layer<-currentmodel$Xgb.params.for.first.layer
      ##Outputs
      Multiple.ROC<-currentmodel$Multiple.ROC
      summary_table <- currentmodel$summary_table
      Residuals_table<-currentmodel$Residuals_table
      confusion.table<-currentmodel$confusion.table
      summary_trainning_table <- currentmodel$summary_trainning_table
      summary_trainning_table.est <- currentmodel$summary_trainning_table.est
      ##Deciles Accuracy
      Model.for.deciles.accuracy<-input$Model.for.deciles.accuracy
      Criterion.for.deciles.accuracy<-input$Criterion.for.deciles.accuracy
      Deciles.Count<-input$Deciles.Count
      Accuracy.deciles.table<-currentmodel$Accuracy.deciles.table
      ##Deciles Average
      Model.for.deciles.avg<-input$Model.for.deciles.avg
      Criterion.for.deciles.avg<-input$Criterion.for.deciles.avg
      Deciles.Avg.Count<-input$Deciles.Avg.Count
      Avg.deciles.table<-currentmodel$Avg.deciles.table
      ##
      Analysis.DataSet <- currentmodel$Analysis.DataSet
      ##Formula
      Choose.formula.lang<-input$Choose.formula.lang
      #MS SQL
      ViewModelForm.ms.sql <- currentmodel$ViewModelForm.ms.sql
      Make.ViewModelForm.ms.sql<-currentmodel$Make.ViewModelForm.ms.sql
      #Oracle
      ViewModelForm.oracle <- currentmodel$ViewModelForm.oracle
      Make.ViewModelForm.oracle<-currentmodel$Make.ViewModelForm.oracle
      ##
      time.taken<-currentmodel$time.taken
      Run.all.options.time.taken<-currentmodel$Run.all.options.time.taken
      ##CV Data
      DataBase.CV.type<-input$DataBase.CV.type
      text.analysis.CV<-input$text.analysis.CV
      Dates.as.parts.CV<-input$Dates.as.parts.CV
      choose_Method_for_CV<-input$choose_Method_for_CV
      uploaded.dataCrossValid <- currentmodel$uploaded.dataCrossValid
      CV.Data <- currentmodel$CV.Data
      ##show Warnings
      Keeping.complete.cases.in.target.CV<-currentmodel$Keeping.complete.cases.in.target.CV
      prediction.err.CV<-currentmodel$prediction.err.CV  
      Vars.with.diff.distribution.CV<-currentmodel$Vars.with.diff.distribution.CV
      NA.table.CV<-currentmodel$NA.table.CV
      ##Pred Data
      DataBase.Pred.type<-input$DataBase.Pred.type
      text.analysis.Pred<-input$text.analysis.Pred
      Dates.as.parts.Pred<-input$Dates.as.parts.Pred
      choose_Method_for_Pred<-input$choose_Method_for_Pred
      uploaded.dataPredict <- currentmodel$uploaded.dataPredict
      Predict.Data <- currentmodel$Predict.Data
      ##show Warnings
      prediction.err.Pred<-currentmodel$prediction.err.Pred
      Vars.with.diff.distribution.Pred<-currentmodel$Vars.with.diff.distribution.Pred
      NA.table.Pred<-currentmodel$NA.table.Pred
      ##Validation Results
      summary_table.cv <- currentmodel$summary_table.cv
      summary_table.est.cv<-currentmodel$summary_table.est.cv
      confusion.table.cv <- currentmodel$confusion.table.cv
      Multiple.ROC.cv<-currentmodel$Multiple.ROC.cv
      Residuals.table.cv<-currentmodel$Residuals.table.cv
      ##Accuracy Deciles for CV
      Model.for.deciles.accuracy.cv<-input$Model.for.deciles.accuracy.cv
      Criterion.for.deciles.accuracy.cv<-input$Criterion.for.deciles.accuracy.cv
      Deciles.Count.cv<-input$Deciles.Count.cv
      Accuracy.deciles.table.cv<-currentmodel$Accuracy.deciles.table.cv
      ##
      all.models.for.cv<-currentmodel$all.models.for.cv
      ##Deciles Accuracy for CV
      Model.for.deciles.avg.cv<-input$Model.for.deciles.avg.cv
      Criterion.for.deciles.avg.cv<-input$Criterion.for.deciles.avg.cv
      Deciles.Avg.Count.cv<-input$Deciles.Avg.Count.cv
      Avg.deciles.table.cv<-currentmodel$Avg.deciles.table.cv
      ##Source Listener
      Source.listener.time.units<-input$Source.listener.time.units
      Source.listener.time<-input$Source.listener.time
      choose_Method_Source.listener<-input$choose_Method_for_Source.listener
      Item.listener<-input$Item.listener
      Source.listener.ODBC.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.ODBC.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.MS.SQL.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.MS.SQL.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.MySQL.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.MySQL.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.Oracle.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.Oracle.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.Amazon.Redshift.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.Amazon.Redshift.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.Google.Big.Query.table.Rhands<-tryCatch({hot_to_r(input$Source.listener.Google.Big.Query.table.Rhands)},error=function(err){return(NULL)})
      Source.listener.Google.Big.Query.useLegacySql<-input$Source.listener.Google.Big.Query.useLegacySql
      ##
      tryCatch({ 
        save(
          DataBase.Train.type,Shuffle.Train,Dates.as.parts.Train,division.rate.slider,division.rate,
          uploaded.data,Basic.uploaded.data,
          ##DBA
          ODBC_Train.table,ODBC_CV.table,ODBC_Pred.table,
          MS_sql_Train.table,MS_sql_CV.table,MS_sql_Pred.table,
          Mysql_Train.table,Mysql_CV.table,Mysql_Pred.table,
          Oracle_Train.table,Oracle_CV.table,Oracle_Pred.table,
          Amazon_Redshift_Train.table,Amazon_Redshift_CV.table,Amazon_Redshift_Pred.table,
          Google_Big_Query_Train.table,Google_Big_Query_CV.table,Google_Big_Query_Pred.table,
          useLegacySql.Train,useLegacySql.CV,useLegacySql.Pred,
          ##
          text.analysis.split,Words_split,Show.only.Exp,Exclude.stopwords,
          Our.Pos.Exp,Our.Neg.Exp,Our.StopWords,Neg.Exp.table,Pos.Exp.table,Other.Exp.table,StopWords.table,
          text.analysis.fill.table.count,text.analysis.fill.table.regex,text.analysis.fill.table.replace.regex,
          text.analysis.fill.table.as.variable,text.analysis.fill.table.as.variable.using.regex,
          text.analysis.for.export,text.analysis.for.export.saved,
          text.analysis_chosen.var,transfer.string.to.dummy,text.analysis.phrases.plot.export.extension,Method,Method.saved,Column.to.check.configure,Target.Vars,vars.to.exclude,not.to.fix,not.to.fix.outliers,
          Vars.Summary,Calculate.filter.data,Data.Filter.Index,Numeric.data.filter.table,Factor.data.filter.table,Filter.by.coding.text,Interval.for.ratio.combination,ratio.comb.mean,ratio.comb.count,
          ratio.comb.for.couples,ratio.comb.table.to.show,ratio.comb.table.to.show.saved,ratio.comb.info,Create.is.na.vars,Outliers.settings.saved,AllowSampling.GeographyData,Percent.GeographyData,Map.Filter.Index,Numeric.Geographydata.filter.table,Factor.Geographydata.filter.table,
          AllowSampling,percent,ChooseAlgorithms,Stratified.Shuffle,Use.CV,Imputation.Value,Auto.classification.threshold,classification.threshold,
          accuracy.tolerance,stability.threshold,ChooseCriterion,Preserve.Accuracy_01,Differential.Matrix,
          RareEvents,ChooseCriterion.Estimation,difference.tolerance,analysisMethod,Xgb.best.parameters,Xgb.classification.eval.metric,
          params.to.assign,levels.table,ratio.as.dummies.inside,Settings_Table,WL.params,Rpart.params,Xgb.params,Method.used,
          SPY.fixed,SPY.Test.fixed,temp.necessary.variables.found,Used.Models,
          #First Layer
          temp.necessary.variables.found.for.first.layer,Used.Models.for.first.layer,
          Two.layers.prediction,ChooseAlgorithms.for.first.layer,
          WL.params.for.first.layer,Rpart.params.for.first.layer,Xgb.params.for.first.layer,
          ##
          Multiple.ROC,
          summary_table,Residuals_table,confusion.table,summary_trainning_table,summary_trainning_table.est,Model.for.deciles.accuracy,Criterion.for.deciles.accuracy,Deciles.Count,Accuracy.deciles.table,
          Model.for.deciles.avg,Criterion.for.deciles.avg,Deciles.Avg.Count,Avg.deciles.table,
          Analysis.DataSet,Choose.formula.lang,ViewModelForm.ms.sql,Make.ViewModelForm.ms.sql,ViewModelForm.oracle,Make.ViewModelForm.oracle,time.taken,Run.all.options.time.taken,DataBase.CV.type,text.analysis.CV,Dates.as.parts.CV,choose_Method_for_CV,uploaded.dataCrossValid,
          CV.Data,Keeping.complete.cases.in.target.CV,prediction.err.CV,Vars.with.diff.distribution.CV,NA.table.CV,DataBase.Pred.type,text.analysis.Pred,Dates.as.parts.Pred,
          choose_Method_for_Pred,uploaded.dataPredict,Predict.Data,prediction.err.Pred,Vars.with.diff.distribution.Pred,NA.table.Pred,
          summary_table.cv,summary_table.est.cv,confusion.table.cv,Multiple.ROC.cv,Residuals.table.cv,
          Model.for.deciles.accuracy.cv,Criterion.for.deciles.accuracy.cv,Deciles.Count.cv,Accuracy.deciles.table.cv,all.models.for.cv,
          Model.for.deciles.avg.cv,Criterion.for.deciles.avg.cv,Deciles.Avg.Count.cv,Avg.deciles.table.cv,
          ##Source Listener
          Source.listener.time.units,Source.listener.time,choose_Method_Source.listener,Item.listener,
          Source.listener.ODBC.table.Rhands,Source.listener.MS.SQL.table.Rhands,Source.listener.MySQL.table.Rhands,Source.listener.Oracle.table.Rhands,
          Source.listener.Amazon.Redshift.table.Rhands,Source.listener.Google.Big.Query.table.Rhands,Source.listener.Google.Big.Query.useLegacySql,
          ##
          file=file.path(models_dir, paste0(input$ModelName, ".model")))
        last_action <<- paste("File named", paste0(input$ModelName, ".model"), "was saved to", models_dir, "folder")
      }, error=function(err) {
        last_action <<- paste("Error occured - file was not saved")
      })
    })
    observeEvent(input$LoadButton, { 
      if (!is.null(input$ModelToChoose_rows_selected)) {
         tryCatch({ 
          load(file=model_files[input$ModelToChoose_rows_selected])
          ######Reset Current model#########
          for(i in names(currentmodel))
            currentmodel[[i]]<-NULL
          ###Reset Text Analysis
          for(i in names(textAnalysis))
            textAnalysis[[i]]<-NULL
          ###Reset Variables Correlation
          for(i in names(variablesCorrelation))
            variablesCorrelation[[i]]<-NULL
          ###Reset Data Analysis
          for(i in names(dataAnalysis))
            dataAnalysis[[i]]<-NULL
          ###Reset Var vs Target model
          for(i in names(var.vs.target.Model))
            var.vs.target.Model[[i]]<-NULL
          ###Reset Model Insights
          for(i in names(modelInsights))
            modelInsights[[i]]<-NULL
          ##Main ###Train
          tryCatch({updateSelectInput(session, "DataBase.Train.type", selected=DataBase.Train.type)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Shuffle.Train", selected=Shuffle.Train)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Dates.as.parts.Train", selected=Dates.as.parts.Train)},error=function(err){})
          tryCatch({updateSliderInput(session,"division.rate.slider",value=division.rate.slider)},error=function(err){})
          currentmodel$division.rate <-tryCatch({division.rate},error=function(err){return(0.8)})
          currentmodel$uploaded.data <-tryCatch({
            if(is.null(uploaded.data)){
              temp<-NULL
            } else{
              temp<-as.data.table(uploaded.data)
            }
            temp
          },error=function(err){return(NULL)})
          currentmodel$Basic.uploaded.data <-tryCatch({
            if(is.null(Basic.uploaded.data)){
              temp<-NULL
            } else{
              temp<-as.data.table(Basic.uploaded.data)
            }
            temp
          },error=function(err){return(NULL)})
          ##DBA
          currentmodel$Loaded.ODBC_Train.table <-tryCatch({ODBC_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.ODBC_CV.table <-tryCatch({ODBC_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.ODBC_Pred.table <-tryCatch({ODBC_Pred.table},error=function(err){return(NULL)})
          ##
          currentmodel$Loaded.MS_sql_Train.table <-tryCatch({MS_sql_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.MS_sql_CV.table <-tryCatch({MS_sql_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.MS_sql_Pred.table <-tryCatch({MS_sql_Pred.table},error=function(err){return(NULL)})
          ##
          currentmodel$Loaded.Mysql_Train.table <-tryCatch({Mysql_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Mysql_CV.table <-tryCatch({Mysql_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Mysql_Pred.table <-tryCatch({Mysql_Pred.table},error=function(err){return(NULL)})
          ##
          currentmodel$Loaded.Oracle_Train.table <-tryCatch({Oracle_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Oracle_CV.table <-tryCatch({Oracle_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Oracle_Pred.table <-tryCatch({Oracle_Pred.table},error=function(err){return(NULL)})
          ##
          currentmodel$Loaded.Amazon_Redshift_Train.table <-tryCatch({Amazon_Redshift_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Amazon_Redshift_CV.table <-tryCatch({Amazon_Redshift_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Amazon_Redshift_Pred.table <-tryCatch({Amazon_Redshift_Pred.table},error=function(err){return(NULL)})
          ##
          currentmodel$Loaded.Google_Big_Query_Train.table <-tryCatch({Google_Big_Query_Train.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Google_Big_Query_CV.table <-tryCatch({Google_Big_Query_CV.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Google_Big_Query_Pred.table <-tryCatch({Google_Big_Query_Pred.table},error=function(err){return(NULL)})
          tryCatch({updateRadioButtons(session, "useLegacySql.Train", selected=useLegacySql.Train)},error=function(err){})
          tryCatch({updateRadioButtons(session, "useLegacySql.CV", selected=useLegacySql.CV)},error=function(err){})
          tryCatch({updateRadioButtons(session, "useLegacySql.Pred", selected=useLegacySql.Pred)},error=function(err){})
          ##Text Analysis
          tryCatch({updateSelectInput(session, "text.analysis.split", selected=text.analysis.split)},error=function(err){})
          tryCatch({updateNumericInput(session, "Words_split", value=Words_split)},error=function(err){})
          tryCatch({updateSelectInput(session, "Show.only.Exp", selected=Show.only.Exp)},error=function(err){})
          tryCatch({updateSelectInput(session, "Exclude.stopwords", selected=Exclude.stopwords)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Our.Pos.Exp", selected=Our.Pos.Exp)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Our.Neg.Exp", selected=Our.Neg.Exp)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Our.StopWords", selected=Our.StopWords)},error=function(err){})
          textAnalysis$Neg.Exp.table<-tryCatch({Neg.Exp.table},error=function(err){return(NULL)})
          textAnalysis$Pos.Exp.table<-tryCatch({Pos.Exp.table},error=function(err){return(NULL)})
          textAnalysis$Other.Exp.table<-tryCatch({Other.Exp.table},error=function(err){return(NULL)})
          textAnalysis$StopWords.table<-tryCatch({StopWords.table},error=function(err){return(NULL)})
          ##Fill Tables
          textAnalysis$text.analysis.fill.table.count<-tryCatch({text.analysis.fill.table.count},error=function(err){return(NULL)})
          textAnalysis$text.analysis.fill.table.regex<-tryCatch({text.analysis.fill.table.regex},error=function(err){return(NULL)})
          textAnalysis$text.analysis.fill.table.replace.regex<-tryCatch({text.analysis.fill.table.replace.regex},error=function(err){return(NULL)})
          textAnalysis$text.analysis.fill.table.as.variable<-tryCatch({text.analysis.fill.table.as.variable},error=function(err){return(NULL)})
          textAnalysis$text.analysis.fill.table.as.variable.using.regex<-tryCatch({text.analysis.fill.table.as.variable.using.regex},error=function(err){return(NULL)})
          ###
          textAnalysis$text.analysis.for.export<-tryCatch({text.analysis.for.export},error=function(err){return(NULL)})
          textAnalysis$text.analysis.for.export.saved<-tryCatch({text.analysis.for.export.saved},error=function(err){return(NULL)})
          textAnalysis$text.analysis_chosen.var<- tryCatch({text.analysis_chosen.var},error=function(err){return(NULL)})
          tryCatch({updateRadioButtons(session, "transfer.string.to.dummy", selected=transfer.string.to.dummy)},error=function(err){})
          tryCatch({updateRadioButtons(session, "text.analysis.phrases.plot.export.extension", selected=text.analysis.phrases.plot.export.extension)},error=function(err){})
          ##Configure
          tryCatch({updateSelectInput(session, "Method", selected=Method)},error=function(err){})
          currentmodel$Method.saved <- tryCatch({Method.saved},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session, "Column.to.check.configure", selected=Column.to.check.configure)},error=function(err){}) 
          currentmodel$Target.Vars<-tryCatch({Target.Vars},error=function(err){return(NULL)})
          currentmodel$vars.to.exclude<- tryCatch({vars.to.exclude},error=function(err){return(NULL)})
          currentmodel$not.to.fix<- tryCatch({not.to.fix},error=function(err){return(NULL)})
          currentmodel$not.to.fix.outliers <-tryCatch({not.to.fix.outliers},error=function(err){return(NULL)})
          currentmodel$Vars.Summary<-tryCatch({Vars.Summary},error=function(err){return(NULL)})
          ##Filter Data
          currentmodel$Calculate.filter.data<-tryCatch({Calculate.filter.data},error=function(err){return(NULL)})
          currentmodel$Data.Filter.Index<-tryCatch({Data.Filter.Index},error=function(err){return(NULL)})
          currentmodel$Loaded.Numeric.data.filter.table<- tryCatch({Numeric.data.filter.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Factor.data.filter.table<- tryCatch({Factor.data.filter.table},error=function(err){return(NULL)})
          tryCatch({updateTextAreaInput(session,"Filter.by.coding.text",value=Filter.by.coding.text)},error=function(err){})
          ##Ratio Combination
          currentmodel$Loaded.Interval.for.ratio.combination<-tryCatch({Interval.for.ratio.combination},error=function(err){return(NULL)})
          tryCatch({updateSliderInput(session,"ratio.comb.mean",value=ratio.comb.mean)},error=function(err){})
          tryCatch({updateNumericInput(session, "ratio.comb.count", value = ratio.comb.count)},error=function(err){})
          tryCatch({updateRadioButtons(session, "ratio.comb.for.couples", selected=ratio.comb.for.couples)},error=function(err){})
          currentmodel$ratio.comb.table.to.show<-tryCatch({ratio.comb.table.to.show},error=function(err){return(NULL)})
          currentmodel$ratio.comb.table.to.show.saved<-tryCatch({ratio.comb.table.to.show.saved},error=function(err){return(NULL)})
          currentmodel$ratio.comb.info<-tryCatch({ratio.comb.info},error=function(err){return(NULL)})
          ##is NA
          currentmodel$Create.is.na.vars<-tryCatch({Create.is.na.vars},error=function(err){return(NULL)})
          ##Outliers Settings
          OutliersSettings$Outliers.settings.saved<-tryCatch({Outliers.settings.saved},error=function(err){return(NULL)})
          ##Map
          tryCatch({updateSelectInput(session,"AllowSampling.GeographyData",selected=AllowSampling.GeographyData)},error=function(err){})
          tryCatch({updateSliderInput(session,"Percent.GeographyData",value=Percent.GeographyData)},error=function(err){})
          currentmodel$Map.Filter.Index <- tryCatch({Map.Filter.Index},error=function(err){return(NULL)})
          currentmodel$Loaded.Numeric.Geographydata.filter.table<- tryCatch({Numeric.Geographydata.filter.table},error=function(err){return(NULL)})
          currentmodel$Loaded.Factor.Geographydata.filter.table<- tryCatch({Factor.Geographydata.filter.table},error=function(err){return(NULL)})
          ##Prediction Analysis
          tryCatch({updateSelectInput(session,"AllowSampling",selected=AllowSampling)},error=function(err){})
          tryCatch({updateSliderInput(session,"percent",value=percent)},error=function(err){})
          currentmodel$ChooseAlgorithms<-tryCatch({ChooseAlgorithms},error=function(err){return(NULL)})
          tryCatch({updateRadioButtons(session, "Stratified.Shuffle", selected=Stratified.Shuffle)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Use.CV", selected=Use.CV)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Imputation.Value", selected=Imputation.Value)},error=function(err){})
          tryCatch({updateRadioButtons(session,"Auto.classification.threshold",selected=Auto.classification.threshold)},error=function(err){})
          tryCatch({updateSliderInput(session,"classification.threshold",value=classification.threshold)},error=function(err){})
          tryCatch({updateSliderInput(session,"accuracy.tolerance",value=accuracy.tolerance)},error=function(err){})
          tryCatch({updateSliderInput(session,"stability.threshold",value=stability.threshold)},error=function(err){})
          #tryCatch({updateRadioButtons(session, "ChooseCriterion", selected=ChooseCriterion)},error=function(err){})
          #####MAE instead Abs Difference
          if(ChooseCriterion=='Abs Difference')
            ChooseCriterion<-'MAE'
          if(ChooseCriterion=='Accuracy 1')
            ChooseCriterion<-'Precision'
          tryCatch({updateRadioButtons(session, "ChooseCriterion", selected=ChooseCriterion)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Preserve.Accuracy_01", selected=Preserve.Accuracy_01)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Differential.Matrix", selected=Differential.Matrix)},error=function(err){})
          tryCatch({updateRadioButtons(session, "RareEvents", selected=RareEvents)},error=function(err){})
          tryCatch({updateRadioButtons(session, "ChooseCriterion.Estimation", selected=ChooseCriterion.Estimation)},error=function(err){})
          tryCatch({updateSliderInput(session,"difference.tolerance",value=difference.tolerance)},error=function(err){})
          tryCatch({updateRadioButtons(session, "analysisMethod", selected=analysisMethod)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Xgb.best.parameters", selected=Xgb.best.parameters)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Xgb.classification.eval.metric", selected=Xgb.classification.eval.metric)},error=function(err){})
          ##Before calculating
          currentmodel$params.to.assign<-tryCatch({params.to.assign},error=function(err){return(NULL)})
          currentmodel$levels.table<-tryCatch({levels.table},error=function(err){return(NULL)})
          currentmodel$ratio.as.dummies.inside<-tryCatch({ratio.as.dummies.inside},error=function(err){return(NULL)})
          currentmodel$Settings_Table<-tryCatch({Settings_Table},error=function(err){return(NULL)})
          #####MAE instead Abs Difference
          if(currentmodel$Settings_Table[,"Criterion"]=='Abs Difference')
            currentmodel$Settings_Table[,"Criterion"]<-"MAE"
          if(currentmodel$Settings_Table[,"Criterion"]=='Accuracy 1')
            currentmodel$Settings_Table[,"Criterion"]<-"Precision"
          currentmodel$WL.params<-tryCatch({WL.params},error=function(err){return(NULL)})
          currentmodel$Rpart.params<-tryCatch({Rpart.params},error=function(err){return(NULL)})
          currentmodel$Xgb.params<-tryCatch({Xgb.params},error=function(err){return(NULL)})
          currentmodel$Method.used<-tryCatch({Method.used},error=function(err){return(NULL)})
          currentmodel$SPY.fixed <-tryCatch({
            if(is.null(SPY.fixed)){
              temp<-NULL
            } else{
              temp<-as.data.table(SPY.fixed)
            }
            temp
          },error=function(err){return(NULL)})
          currentmodel$SPY.Test.fixed <-tryCatch({
            if(is.null(SPY.Test.fixed)){
              temp<-NULL
            } else{
              temp<-as.data.table(SPY.Test.fixed)
            }
            temp
          },error=function(err){return(NULL)})
          ##After calculating
          ##getting currentmodel$necessary.variables.found from 
          ## temp.necessary.variables.found
          currentmodel$necessary.variables.found<-tryCatch({
            Retrieving.necessary.variables.found(temp.necessary.variables.found,currentmodel$SPY.fixed,
                                                 currentmodel$params.to.assign[["output.var"]],currentmodel$WL.params,
                                                 currentmodel$Rpart.params,currentmodel$Xgb.params,currentmodel$Method.used)
          },error=function(err){return(NULL)})
          currentmodel$Used.Models<-tryCatch({Used.Models},error=function(err){return(NULL)})
          ##First Layer
          currentmodel$Used.Models.for.first.layer<-tryCatch({Used.Models.for.first.layer},error=function(err){return(NULL)})
          tryCatch({updateRadioButtons(session, "Two.layers.prediction", selected=Two.layers.prediction)},error=function(err){})
          currentmodel$ChooseAlgorithms.for.first.layer<-tryCatch({ChooseAlgorithms.for.first.layer},error=function(err){return(NULL)})  
          currentmodel$WL.params.for.first.layer<-tryCatch({WL.params.for.first.layer},error=function(err){return(NULL)})
          currentmodel$Rpart.params.for.first.layer<-tryCatch({Rpart.params.for.first.layer},error=function(err){return(NULL)})
          currentmodel$Xgb.params.for.first.layer<-tryCatch({Xgb.params.for.first.layer},error=function(err){return(NULL)})
          currentmodel$necessary.variables.found.for.first.layer<-tryCatch({
            Retrieving.necessary.variables.found(temp.necessary.variables.found.for.first.layer,currentmodel$SPY.fixed,
                                                 currentmodel$params.to.assign[["output.var"]],currentmodel$WL.params.for.first.layer,
                                                 currentmodel$Rpart.params.for.first.layer,currentmodel$Xgb.params.for.first.layer,"Estimation")
          },error=function(err){return(NULL)})
          ##Outputs
          currentmodel$Multiple.ROC<-tryCatch({Multiple.ROC},error=function(err){return(NULL)})
          currentmodel$summary_table<-tryCatch({summary_table},error=function(err){return(NULL)})
          currentmodel$Residuals_table<-tryCatch({Residuals_table},error=function(err){return(NULL)})
          currentmodel$confusion.table<-tryCatch({confusion.table},error=function(err){return(NULL)})
          currentmodel$summary_trainning_table<-tryCatch({summary_trainning_table},error=function(err){return(NULL)})
          currentmodel$summary_trainning_table.est<-tryCatch({summary_trainning_table.est},error=function(err){return(NULL)})
          ##Deciles Accuracy
          currentmodel$Loaded.Model.for.deciles.accuracy<-tryCatch({Model.for.deciles.accuracy},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session,"Criterion.for.deciles.accuracy",selected=Criterion.for.deciles.accuracy)},error=function(err){})
          tryCatch({updateNumericInput(session, "Deciles.Count", value = Deciles.Count)},error=function(err){})
          currentmodel$Accuracy.deciles.table<-tryCatch({Accuracy.deciles.table},error=function(err){return(NULL)})
          ##Deciles Average
          currentmodel$Loaded.Model.for.deciles.avg<-tryCatch({Model.for.deciles.avg},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session,"Criterion.for.deciles.avg",selected=Criterion.for.deciles.avg)},error=function(err){})
          tryCatch({updateNumericInput(session, "Deciles.Avg.Count", value = Deciles.Avg.Count)},error=function(err){})
          currentmodel$Avg.deciles.table<-tryCatch({Avg.deciles.table},error=function(err){return(NULL)})
          ##
          currentmodel$Analysis.DataSet <-tryCatch({
            if(is.null(Analysis.DataSet)){
              temp<-NULL
            } else{
              temp<-as.data.table(Analysis.DataSet)
            }
            temp
          },error=function(err){return(NULL)})
          ##Formula
          tryCatch({updateSelectInput(session, "Choose.formula.lang", selected=Choose.formula.lang)},error=function(err){})
          #MS SQL
          currentmodel$ViewModelForm.ms.sql<-tryCatch({ViewModelForm.ms.sql},error=function(err){return(NULL)})
          currentmodel$Make.ViewModelForm.ms.sql<-tryCatch({Make.ViewModelForm.ms.sql},error=function(err){return(NULL)})
          #Oracle
          currentmodel$ViewModelForm.oracle<-tryCatch({ViewModelForm.oracle},error=function(err){return(NULL)})
          currentmodel$Make.ViewModelForm.oracle<-tryCatch({Make.ViewModelForm.oracle},error=function(err){return(NULL)})
          
          ##
          currentmodel$time.taken<-tryCatch({time.taken},error=function(err){return(NULL)})
          currentmodel$Run.all.options.time.taken<-tryCatch({Run.all.options.time.taken},error=function(err){return(NULL)})
          ##CV Data
          tryCatch({updateSelectInput(session, "DataBase.CV.type", selected=DataBase.CV.type)},error=function(err){})
          tryCatch({updateRadioButtons(session, "text.analysis.CV", selected=text.analysis.CV)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Dates.as.parts.CV", selected=Dates.as.parts.CV)},error=function(err){})
          currentmodel$Loaded.choose_Method_for_CV<-tryCatch({choose_Method_for_CV},error=function(err){return(NULL)})
          currentmodel$uploaded.dataCrossValid <-tryCatch({
            if(is.null(uploaded.dataCrossValid)){
              temp<-NULL
            } else{
              temp<-as.data.table(uploaded.dataCrossValid)
            }
            temp
          },error=function(err){return(NULL)})
          currentmodel$CV.Data <-tryCatch({
            if(is.null(CV.Data)){
              temp<-NULL
            } else{
              temp<-as.data.table(CV.Data)
            }
            temp
          },error=function(err){return(NULL)})
          ##show Warnings
          currentmodel$Keeping.complete.cases.in.target.CV<-tryCatch({Keeping.complete.cases.in.target.CV},error=function(err){return(NULL)})
          currentmodel$prediction.err.CV<-tryCatch({prediction.err.CV},error=function(err){return(NULL)})
          currentmodel$Vars.with.diff.distribution.CV<-tryCatch({Vars.with.diff.distribution.CV},error=function(err){return(NULL)})
          currentmodel$NA.table.CV<-tryCatch({NA.table.CV},error=function(err){return(NULL)})
          ##Pred Data
          tryCatch({updateSelectInput(session, "DataBase.Pred.type", selected=DataBase.Pred.type)},error=function(err){})
          tryCatch({updateRadioButtons(session, "text.analysis.Pred", selected=text.analysis.Pred)},error=function(err){})
          tryCatch({updateRadioButtons(session, "Dates.as.parts.Pred", selected=Dates.as.parts.Pred)},error=function(err){})
          currentmodel$Loaded.choose_Method_for_Pred<-tryCatch({choose_Method_for_Pred},error=function(err){return(NULL)})
          currentmodel$uploaded.dataPredict <-tryCatch({
            if(is.null(uploaded.dataPredict)){
              temp<-NULL
            } else{
              temp<-as.data.table(uploaded.dataPredict)
            }
            temp
          },error=function(err){return(NULL)})
          currentmodel$Predict.Data <-tryCatch({
            if(is.null(Predict.Data)){
              temp<-NULL
            } else{
              temp<-as.data.table(Predict.Data)
            }
            temp
          },error=function(err){return(NULL)})
          ##show Warnings
          currentmodel$prediction.err.Pred<-tryCatch({prediction.err.Pred},error=function(err){return(NULL)})
          currentmodel$Vars.with.diff.distribution.Pred<-tryCatch({Vars.with.diff.distribution.Pred},error=function(err){return(NULL)})
          currentmodel$NA.table.Pred<-tryCatch({NA.table.Pred},error=function(err){return(NULL)})
          ##Validation Results
          currentmodel$summary_table.cv<-tryCatch({summary_table.cv},error=function(err){return(NULL)})
          currentmodel$summary_table.est.cv<-tryCatch({summary_table.est.cv},error=function(err){return(NULL)})
          currentmodel$confusion.table.cv<-tryCatch({confusion.table.cv},error=function(err){return(NULL)})
          currentmodel$Multiple.ROC.cv<-tryCatch({Multiple.ROC.cv},error=function(err){return(NULL)})
          currentmodel$Residuals.table.cv<-tryCatch({Residuals.table.cv},error=function(err){return(NULL)})
          ##Deciles Accuracy for CV
          currentmodel$Loaded.Model.for.deciles.accuracy.cv<-tryCatch({Model.for.deciles.accuracy.cv},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session,"Criterion.for.deciles.accuracy.cv",selected=Criterion.for.deciles.accuracy.cv)},error=function(err){})
          tryCatch({updateNumericInput(session, "Deciles.Count.cv", value = Deciles.Count.cv)},error=function(err){})
          currentmodel$Accuracy.deciles.table.cv<-tryCatch({Accuracy.deciles.table.cv},error=function(err){return(NULL)})
          ##
          currentmodel$all.models.for.cv<-tryCatch({all.models.for.cv},error=function(err){return(NULL)})
          ##Deciles Average for CV
          currentmodel$Loaded.Model.for.deciles.avg.cv<-tryCatch({Model.for.deciles.avg.cv},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session,"Criterion.for.deciles.avg.cv",selected=Criterion.for.deciles.avg.cv)},error=function(err){})
          tryCatch({updateNumericInput(session, "Deciles.Avg.Count.cv", value = Deciles.Avg.Count.cv)},error=function(err){})
          currentmodel$Avg.deciles.table.cv<-tryCatch({Avg.deciles.table.cv},error=function(err){return(NULL)})
          ##Source Listener
          tryCatch({updateSelectInput(session, "Source.listener.time.units", selected=Source.listener.time.units)},error=function(err){})
          tryCatch({updateNumericInput(session, "Source.listener.time", value = Source.listener.time)},error=function(err){})
          currentmodel$Loaded.choose_Method_Source.listener<-tryCatch({choose_Method_Source.listener},error=function(err){return(NULL)})
          tryCatch({updateSelectInput(session, "Item.listener", selected=Item.listener)},error=function(err){})
          currentmodel$Loaded.Source.listener.ODBC.table.Rhands<-tryCatch({Source.listener.ODBC.table.Rhands},error=function(err){return(NULL)})
          currentmodel$Loaded.Source.listener.MS.SQL.table.Rhands<-tryCatch({Source.listener.MS.SQL.table.Rhands},error=function(err){return(NULL)})
          currentmodel$Loaded.Source.listener.MySQL.table.Rhands<-tryCatch({Source.listener.MySQL.table.Rhands},error=function(err){return(NULL)})
          currentmodel$Loaded.Source.listener.Oracle.table.Rhands<-tryCatch({Source.listener.Oracle.table.Rhands},error=function(err){return(NULL)})
          currentmodel$Loaded.Source.listener.Amazon.Redshift.table.Rhands<-tryCatch({Source.listener.Amazon.Redshift.table.Rhands},error=function(err){return(NULL)})
          currentmodel$Loaded.Source.listener.Google.Big.Query.table.Rhands<-tryCatch({Source.listener.Google.Big.Query.table.Rhands},error=function(err){return(NULL)})
          tryCatch({updateRadioButtons(session, "Source.listener.Google.Big.Query.useLegacySql", selected=Source.listener.Google.Big.Query.useLegacySql)},error=function(err){})
          ##
         last_action <<- paste("Model was loaded from file named", unlist(strsplit(model_files[input$ModelToChoose_rows_selected],paste0(models_dir,"/"),fixed=TRUE))[2],"from",models_dir)
        }, error=function(err) {
          last_action <<- paste("Error occured - model was not loaded")
        })
      } else {
        last_action <<- paste("No model was chosen")
      }
    })
    
    observeEvent(input$DeleteButton,{
      if (!is.null(input$ModelToChoose_rows_selected)) {
        tryCatch({
          file.remove(file=model_files[input$ModelToChoose_rows_selected])
          last_action <<- paste("File", unlist(strsplit(model_files[input$ModelToChoose_rows_selected],paste0(models_dir,"/"),fixed=TRUE))[2],"from",models_dir, "was deleted")
        }, error=function(err) {
          last_action <<- paste("Error occured - model was not deleted")
        })
      } else {
        last_action <<- paste("No model was chosen")
      }
    })
    
    output$FoundModelsLabel <- renderText({
      input$ChooseDirButton
      input$SaveButton
      input$DeleteButton
      models_found
    })
    
    output$LoadSaveInformation <- renderText({
      input$SaveButton
      input$LoadButton
      input$DeleteButton
      last_action
    })

    
    session$onSessionEnded(function() { 
      stopApp()
      q("no") 
    }) 
     
  })


