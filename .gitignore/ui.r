shinyUI( 
  fluidPage(theme = shinytheme("sandstone"),
            useShinyjs(),
            includeCSS("style.css"),
            tags$style(HTML("
          .navbar .navbar-header {float: right;margin-right: 30px}
                            ")),
            tags$head(tags$style(HTML('
                                      .content-wrapper,
                                      .right-side {background-color: white;
                                      }'))),
          tags$head(tags$style(HTML('
                                    .modal-sm {
                                    width: 650px;
                                    }
                                    '))),
          tags$head(tags$style(HTML('
                                    .modal-lg {
                                    width: 1200px;
                                    }
                                    '))),
          div(id = "admin_page",
              div(id= "white_page",
                  div(id="loading_position", 
                      div(id="blue_text",h4('Processing...')),
                      div(class="loader")
                  )#End of loading_position
              ),#End of white_page
              hidden(
                div(id= "not_white_page",
                    div(id = "login",
                        wellPanel(
                          textInput("userName", "Username"),
                          passwordInput("passwd", "Password"),
                          actionButton("Login", "Log in"),
                          h6('Algotrace LTD. Version 1.5.0.4'),
                          br(),verbatimTextOutput("Wrong_details")
                        ))
                ))#End of hidden of not_white_page
          ),#End of admin_page
          
          hidden(
            div(id = "main_content",
                ## Main Tab
                bsModal("importdata_Train", "Fill the following", "go_importdata_train", size = "small",
                        selectInput("DataBase.Train.type","Data Base Type",c("Flat","Load","Big Files(.csv)","ODBC","MS SQL","MySQL","Oracle","Amazon Redshift","Google Big Query"),selected="Flat"),
                        conditionalPanel("input['DataBase.Train.type']=='Flat'",
                                         fileInput('datafile', h4('Import Data'),
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='ODBC'",
                                         rHandsontableOutput("ODBC_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='MS SQL'",
                                         rHandsontableOutput("MS_sql_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='MySQL'", 
                                         rHandsontableOutput("Mysql_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='Oracle'",
                                         rHandsontableOutput("Oracle_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='Amazon Redshift'",
                                         rHandsontableOutput("Amazon_Redshift_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']=='Google Big Query'",
                                         fileInput('json.file.Train', h4('Import json File')),
                                         radioButtons("useLegacySql.Train","Use LegacySql",c("No"="FALSE","Yes"="TRUE"),selected="FALSE",inline=TRUE),
                                         rHandsontableOutput("Google_Big_Query_Train.table")
                        ),
                        conditionalPanel("input['DataBase.Train.type']!='Load'",
                                         actionLink("go_importdata_settings_train","Settings",style="color: darkblue"),
                                         br(),
                                         br(),
                                         actionButton("MainSubmit","Submit")
                        )
                        
                ),
                bsModal("importdata_settings_train","Settings","go_importdata_settings_train",size="small",
                        radioButtons("Shuffle.Train","Shuffle Uploaded Data",c("No","Yes"),selected="No",inline=TRUE),
                        radioButtons("Dates.as.parts.Train","Dates as Parts",c("No","Yes"),selected="No",inline=TRUE)
                ),
                ## Data Analysis
                bsModal("text_analysis_phrases_table_export","","go_text_analysis_phrases_table_export",size="small",
                        textInput("path.text.analysis.phrases.table.export", "File:"),  
                        actionButton("MainSubmit.text.analysis.phrases.table.export","Export"), 
                        textOutput("text.analysis.phrases.table.export.text")),
                bsModal("text_analysis_phrases_plot_export","","go_text_analysis_phrases_plot_export",size="small",
                        radioButtons("text.analysis.phrases.plot.export.extension", "Which Extension:",c("png","pdf"),inline = TRUE), 
                        textInput("path.text.analysis.phrases.plot.export", "File:"),
                        actionButton("MainSubmit.text.analysis.phrases.plot.export","Export"), 
                        textOutput("text.analysis.phrases.plot.export.text")),
                bsModal("Choose_Exp","Show Table","go_Choose_Exp",size="small",
                        fluidPage(
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Positive Expressions", 
                                       radioButtons("Our.Pos.Exp", "Use Built In Expressions", inline=TRUE,
                                                    c("No","Yes"),selected="Yes"),
                                       conditionalPanel("input['Our.Pos.Exp']=='No'",
                                                        fileInput('datafile.for.pos.exp', h4('Import Data'),
                                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
                                       actionButton("MainSubmit.for.pos.exp","Submit"),
                                       br(),
                                       verbatimTextOutput("Pos.Exp.error.text"),
                                       rHandsontableOutput("Pos.Exp")
                              ),##End of Pos Exp
                              tabPanel("Negative Expressions", 
                                       radioButtons("Our.Neg.Exp", "Use Built In Expressions", inline=TRUE,
                                                    c("No","Yes"),selected="Yes"),
                                       conditionalPanel("input['Our.Neg.Exp']=='No'",
                                                        fileInput('datafile.for.neg.exp', h4('Import Data'),
                                                                  accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
                                       actionButton("MainSubmit.for.neg.exp","Submit"),
                                       br(),
                                       verbatimTextOutput("Neg.Exp.error.text"),
                                       rHandsontableOutput("Neg.Exp")
                              ),##End of Neg Exp
                              tabPanel("Other Expressions", 
                                       fileInput('datafile.for.other.exp', h4('Import Data'),
                                                 accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                                       actionButton("MainSubmit.for.other.exp","Submit"),
                                       br(),
                                       verbatimTextOutput("Other.Exp.error.text"),
                                       rHandsontableOutput("Other.Exp")
                              )##End of Other Exp
                            )
                          )) 
                ),
                bsModal("Choose_StopWords","Show Table","go_Choose_StopWords",size="small",
                        radioButtons("Our.StopWords", "Use Built In Stopwords", inline=TRUE,
                                     c("No","Yes"),selected="Yes"),
                        conditionalPanel("input['Our.StopWords']=='No'",
                                         fileInput('datafile.for.stopwords', h4('Import Data'),
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain'))),
                        actionButton("MainSubmit.for.stopwords","Submit"),
                        br(),
                        verbatimTextOutput("StopWords.error.text"),
                        rHandsontableOutput("StopWords")),

                ## Data Analysis Tab ##
                ##Distribution tab
                #Frequency plot
                bsModal("Distributionfrequencyplot", "Your plot", "go_Distributionfrequencyplot", size = "large",plotOutput("Distribution.frequency.plot.zoom"),
                        actionLink("go_Distributionfrequencyplot_export", "Export Plot",style="color: darkblue")),
                bsModal("Distributionfrequencyplot_export", "Export plot file", "go_Distributionfrequencyplot_export", size = "small",radioButtons("Distributionfrequencyplot_export.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Distributionfrequencyplot_export", "File:"),actionButton("MainSubmit.Distributionfrequencyplot_export","Export"),
                        textOutput("Distributionfrequencyplot_export.text")),
                #Frequency table
                bsModal("Distributionfrequencytable", "Your table", "go_Distributionfrequencytable", size = "small",DT::dataTableOutput("Distribution.frequency.table.zoom"),
                        actionLink("go_Distributionfrequencytable_export", "Export Table",style="color: darkblue")),
                bsModal("Distributionfrequencytable_export", "Export table file", "go_Distributionfrequencytable_export", size = "small",
                        textInput("path.Distributionfrequencytable_export", "File:"),actionButton("MainSubmit.Distributionfrequencytable_export","Export"),
                        textOutput("Distributionfrequencytable_export.text")),
                #Distribution plot
                bsModal("Distributionoccuranceplot", "Your plot", "go_Distributionoccuranceplot", size = "large",plotOutput("Distribution.occurance.plot.zoom"),
                        actionLink("go_Distributionoccuranceplot_export", "Export Plot",style="color: darkblue")), 
                bsModal("Distributionoccuranceplot_export", "Export plot file", "go_Distributionoccuranceplot_export", size = "small",radioButtons("Distributionoccuranceplot_export.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Distributionoccuranceplot_export", "File:"),actionButton("MainSubmit.Distributionoccuranceplot_export","Export"),
                        textOutput("Distributionoccuranceplot_export.text")),
                #Distribution table    
                bsModal("Distributionoccurancetable", "Your table", "go_Distributionoccurancetable", size = "small",DT::dataTableOutput("Distribution.occurance.table.zoom"),
                        actionLink("go_Distributionoccurancetable_export", "Export Table",style="color: darkblue")), 
                bsModal("Distributionoccurancetable_export", "Export table file", "go_Distributionoccurancetable_export", size = "small",
                        textInput("path.Distributionoccurancetable_export", "File:"),actionButton("MainSubmit.Distributionoccurancetable_export","Export"),
                        textOutput("Distributionoccurancetable_export.text")),
                #Distribution tab Grid plot Frequency & Distribution
                bsModal("Distribution_gridplot", "Export plot file", "go_Distribution_gridplot", size = "small",radioButtons("Distribution_gridplot.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Distribution_gridplot", "File:"),actionButton("MainSubmit.Distribution_gridplot","Export"),
                        textOutput("Distribution_gridplot.text"),
                        conditionalPanel("$('html').hasClass('shiny-busy')",  
                                         class="busy_loader")),
                ##Missing Values
                bsModal("Missing_Values_table_export", "Export table file", "go_Missing_Values_table_export", size = "small",
                        textInput("path.Missing.Values.table.export", "File:"),actionButton("MainSubmit.Missing.Values.table.export","Export"),
                        textOutput("Missing.Values.table.export.text")),
                ##Outliers tab
                bsModal("Outliers_table","Your table", "go_Outliers_table", size = "small",
                        DT::dataTableOutput("Outliers_table.zoom"),
                        actionLink("go_Outliers_table_export", "Export Table",style="color: darkblue")),
                bsModal("Outliers_table_export","Export table file", "go_Outliers_table_export", size = "small",textInput("path.Outliers.table.export", "File:"),
                        actionButton("MainSubmit.Outliers.table.export","Export"),textOutput("Outliers.table.export.text")),
                bsModal("Outliers_settings","Settings","go_Outliers_settings",size="small",
                        helpText(HTML(paste("App's Default<br/>",
                                            "Criterion 1: value > Quantile(0.99) Or value < Quantile(0.01)<br/>",
                                            "Criterion 2: ABS(value-Median(X))/Mad(X) > 3<br/>",
                                            "Criterion 3: ABS(value-Mean(X))/sd(X) > 3<br/>"))),
                        sliderInput("Outliers.criterion1", 
                                    label = "Criterion 1", 
                                    min = 0, max = 1.00, value = c(0.01,0.99)),
                        sliderInput("Outliers.criterion2", 
                                    label = "Criterion 2", 
                                    min = 1, max = 50, value = 3,step=0.25),
                        sliderInput("Outliers.criterion3", 
                                    label = "Criterion 3", 
                                    min = 1, max = 50, value = 3,step=0.25),
                        div(style="display:inline-block",actionButton("MainSubmit.Outliers.settings.save","Save",width='88px')),
                        div(style="display:inline-block",actionButton("MainSubmit.Outliers.settings.reset","Reset",width='88px')),
                        conditionalPanel("$('html').hasClass('shiny-busy')",  
                                         class="busy_loader")
                ),  
                ##Scatter tab
                bsModal("scatter_table","Your table", "go_scatter_table", size = "small",
                        DT::dataTableOutput("scatter_table.zoom"),
                        actionLink("go_scatter_table_export", "Export Table",style="color: darkblue")),
                bsModal("scatter_table_export","Export table file", "go_scatter_table_export", size = "small",textInput("path.scatter.table.export", "File:"),
                        actionButton("MainSubmit.scatter.table.export","Export"),textOutput("scatter.table.export.text")),
                ##Heatmap tab
                #Heatmap Frequency plot
                bsModal("Combfrequencymap", "Your plot", "go_Combfrequencymap", size = "large",plotOutput("Combination.frequency.map.zoom"),
                        actionLink("go_Combfrequencymap_export", "Export Plot",style="color: darkblue")),
                bsModal("Combfrequencymap_export", "Export plot file", "go_Combfrequencymap_export", size = "small",radioButtons("Combfrequencymap_export.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Combfrequencymap.export", "File:"),actionButton("MainSubmit.Combfrequencymap.export","Export"),
                        textOutput("Combfrequencymap.export.text")),
                #Heatmap Frequency table
                bsModal("Combfrequencymaptable", "Your table", "go_Combfrequencymaptable", size = "small",DT::dataTableOutput("Combination.frequency.map.table.zoom"),
                        actionLink("go_Combfrequencymaptable_export", "Export Table",style="color: darkblue")),
                bsModal("Combfrequencymaptable_export", "Export table file", "go_Combfrequencymaptable_export", size = "small",
                        textInput("path.Combfrequencymaptable.export", "File:"),actionButton("MainSubmit.Combfrequencymaptable.export","Export"),
                        textOutput("Combfrequencymaptable.export.text")),
                #Heatmap plot
                bsModal("Combmap", "Your plot", "go_Combmap", size = "large",plotOutput("Combination.map.zoom"),
                        actionLink("go_Combmap_export", "Export Plot",style="color: darkblue")),
                bsModal("Combmap_export", "Export plot file", "go_Combmap_export", size = "small",radioButtons("Combmap_export.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Combmap.export", "File:"),actionButton("MainSubmit.Combmap.export","Export"),
                        textOutput("Combmap.export.text")),
                #Heatmap table
                bsModal("Combmaptable", "Your table", "go_Combmaptable", size = "small",DT::dataTableOutput("Combination.map.table.zoom"),
                        actionLink("go_Combmaptable_export", "Export Table",style="color: darkblue")),
                bsModal("Combmaptable_export", "Export table file", "go_Combmaptable_export", size = "small",
                        textInput("path.Combmaptable.export", "File:"),actionButton("MainSubmit.Combmaptable.export","Export"),
                        textOutput("Combmaptable.export.text")),
                #Heatmap tab Grid plot Frequency & Distribution
                bsModal("Combmap_gridplot","Export plot file", "go_Combmap_gridplot", size = "small",radioButtons("Combination.map.gridplot.extension", "Which Extension:",c("png","pdf"),inline = TRUE),
                        textInput("path.Combination.map.gridplot", "File:"),actionButton("MainSubmit.Combination.map.gridplot","Export"),
                        textOutput("Combination.map.Gridplot.text"),
                        conditionalPanel("$('html').hasClass('shiny-busy')",  
                                         class="busy_loader")),     
                
                ## Prediction Analysis Tab
                bsModal("Settings", "Change the Settings", "go_settings", size = "small",
                        radioButtons("Stratified.Shuffle", "Stratified Shuffle", inline=TRUE,
                                     c("No","Yes"),selected="No"),
                        radioButtons("create.is.NA.vars", "Create is.na Variables", inline=TRUE,
                                     c("No","Yes"),selected="No"),
                        radioButtons("Use.CV", "Use CV", inline=TRUE,
                                     c("No","Yes"),selected="No"),
                        radioButtons("Imputation.Value", "Imputation Value", inline=TRUE,
                                     c("Mean","Median"),selected="Mean"),  #,"Regression","Decission Tree"
                        conditionalPanel("output.Classification_chosen",
                                         radioButtons("Auto.classification.threshold", "Classification threshold Automatic", inline=TRUE,
                                                      c("No","Yes"),selected="No"),
                                         conditionalPanel("output.Classification_chosen && input['Auto.classification.threshold']=='No'",
                                                          sliderInput("classification.threshold", 
                                                                      label = "Classification threshold", 
                                                                      min = 0.05, max = 1.00, value = 0.5)),
                                         sliderInput("accuracy.tolerance", 
                                                     label = "Accuracy tolerance", 
                                                     min = 0.005, max = 0.3, value = 0.01),
                                         sliderInput("stability.threshold", 
                                                     label = "Stability threshold", 
                                                     min = 0.05, max = 0.3, value = 0.1),
                                         radioButtons("ChooseCriterion", "Criterion", inline=TRUE,
                                                      c("Accuracy","Accuracy 0","Precision","F measure","AUC"),selected="Accuracy"),
                                         conditionalPanel("output.Classification_chosen && input['ChooseCriterion']=='Accuracy'",
                                                          radioButtons("Preserve.Accuracy_01", "Preserve Accuracy 0 & 1", inline=TRUE,
                                                                       c("No","Yes"),selected="No")
                                         ),
                                         radioButtons("Differential.Matrix", "Differential Matrix", inline=TRUE,
                                                      c("No","Yes"),selected="No"),
                                         radioButtons("RareEvents", "Rare Events", inline=TRUE,
                                                      c("No","Undersample","Hybrid"),selected="No")
                        ),  
                        conditionalPanel("output.Estimation_chosen",
                                         radioButtons("ChooseCriterion.Estimation", "Criterion", inline=TRUE,
                                                      c("MAE","Norm abs Difference","Abs Sum Difference","RMSE","R2","R2 Adj"),selected="MAE"),
                                         sliderInput("difference.tolerance", 
                                                     label = "Difference tolerance", 
                                                     min = 0.0005, max = 0.3, value = 0.001)
                        ),
                        radioButtons("analysisMethod", "Analysis Method:",inline = TRUE,
                                     c("Native Variables","Combinations frequencies",
                                       "Light Crunching","Deep Crunching","All methods"),
                                     selected="Native Variables"),
                        conditionalPanel("output.Rpart_Algorithms",
                                         radioButtons("Rpart.best.parameters", "Model Best Parameters (Recursive Partitioning Tree)", inline=TRUE,
                                                      c("No","Yes"),selected="No")
                                         ),
                        conditionalPanel("output.Xgboost_Algorithms",
                                         radioButtons("Xgb.best.parameters", "Model Best Parameters (xgboost)", inline=TRUE,
                                                      c("No","Yes"),selected="No"),
                                         conditionalPanel("output.Xgboost_best_params_classification",
                                                          radioButtons("Xgb.classification.eval.metric", "Eval metric", inline=TRUE,
                                                                       c("Logloss","Error","AUC"),selected="Logloss")
                                                          )
                        )
                ),
                bsModal("Run_all_options","All Options","go_Run_all_options",size="small",
                        textInput("path.Run.all.options", "File:"),
                        actionButton("MainSubmit.Run.all.options","Submit"),
                        textOutput("Run.all.options.text"),
                        textOutput("Run.all.options.Time_Taken"),
                        conditionalPanel("$('html').hasClass('shiny-busy')",  
                                         class="busy_loader")),
                ##Models Summary
                bsModal("model_summary", "", "go_model_summary", size = "small",
                        uiOutput("Model.for.Summary.ui"), 
                        br(),
                        verbatimTextOutput("Show.Summary")),
                ##Accuracy Deciles 
                bsModal("deciles_accuracy_table","Export Deciles Table","go_deciles_accuracy_table",size="small",
                        textInput("path.deciles.accuracy.table", "File name:"),actionButton("MainSubmit.deciles.accuracy.table","Submit"),
                        textOutput("deciles.accuracy.table.text")),
                ##AVG Deciles 
                bsModal("deciles_avg_table","Export Deciles Table","go_deciles_avg_table",size="small",
                        textInput("path.deciles.avg.table", "File name:"),actionButton("MainSubmit.deciles.avg.table","Submit"),
                        textOutput("deciles.avg.table.text")),
                ## Insights Tab
                bsModal("Variables_Importance", "Your plot", "go_Variables_Importance_plot", size = "large",div(style='overflow-y: scroll',plotOutput("Variables.Importance.zoom"))),
                bsModal("Variables_Importance_Table", "Your Table", "go_Variables_Importance_table", size = "small",DT::dataTableOutput("Variables.Importance.table.zoom"),
                        actionLink("go_Variables_Importance_table_export", "Export Table",style="color: darkblue")),
                bsModal("Variables_Importance_table_export", "Export Table", "go_Variables_Importance_table_export",size="small",
                        textInput("path.Variables.Importance.table", "File name:"),actionButton("MainSubmit.Variables.Importance.table","Submit"),
                        textOutput("Variables.Importance.table.text")),
                bsModal("Dashboard_Est_Act_Avg_Table", "Your Table", "go_Dashboard_Est_Act_Avg_Table", size = "small",rHandsontableOutput('Dashboard.Est.Act_Avg.Table')),
                bsModal("Dashboard_CV_Table", "Your Table", "go_Dashboard_CV_Table", size = "small",rHandsontableOutput('Dashboard.CV.Table')),
                bsModal("Vars_vs_target_Correlation_plot", "Your plot", "go_Vars_vs_target_Correlation_plot", size = "large",div(style='overflow-y: scroll',plotOutput("Vars.vs.target.Correlation.plot.zoom"))), 
                ## Predict and Export Tab
                bsModal("importdata_CV", "Fill the following", "go_importdata_cv", size = "small",
                        selectInput("DataBase.CV.type","Data Base Type",c("Flat","Big Files(.csv)","ODBC","MS SQL","MySQL","Oracle","Amazon Redshift","Google Big Query"),selected="Flat"),
                        conditionalPanel("input['DataBase.CV.type']=='Flat'",
                                         fileInput('datafileCrossValid', h4('Data set for validation'),
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='ODBC'",
                                         rHandsontableOutput("ODBC_CV.table")
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='MS SQL'",
                                         rHandsontableOutput("MS_sql_CV.table")
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='MySQL'",
                                         rHandsontableOutput("Mysql_CV.table")
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='Oracle'",
                                         rHandsontableOutput("Oracle_CV.table")
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='Amazon Redshift'",
                                         rHandsontableOutput("Amazon_Redshift_CV.table")
                        ),
                        conditionalPanel("input['DataBase.CV.type']=='Google Big Query'",
                                         fileInput('json.file.CV', h4('Import json File')),
                                         radioButtons("useLegacySql.CV","Use LegacySql",c("No"="FALSE","Yes"="TRUE"),selected="FALSE",inline=TRUE),
                                         rHandsontableOutput("Google_Big_Query_CV.table")
                        ),
                        br(),
                        radioButtons("Dates.as.parts.CV","Dates as Parts",c("No","Yes"),selected="No",inline=TRUE)
                ),
                bsModal("CV_Warnings","Warnings","go_CV_Warnings", size = "small",
                        verbatimTextOutput("Keeping.complete.cases.in.target.CV"),
                        uiOutput("Prediction.err.CV"),
                        DT::dataTableOutput("Vars.with.diff.distribution.CV"),
                        DT::dataTableOutput("NA.table.CV")
                ),
                ##Accuracy Deciles CV
                bsModal("deciles_accuracy_table_cv","Export Deciles Table","go_deciles_accuracy_table_cv",size="small",
                        textInput("path.deciles.accuracy.table.cv", "File name:"),actionButton("MainSubmit.deciles.accuracy.table.cv","Submit"),
                        textOutput("deciles.accuracy.table.text.cv")
                ),
                ##AVG Deciles CV
                bsModal("deciles_avg_table_cv","Export Deciles Table","go_deciles_avg_table_cv",size="small",
                        textInput("path.deciles.avg.table.cv", "File name:"),actionButton("MainSubmit.deciles.avg.table.cv","Submit"),
                        textOutput("deciles.avg.table.text.cv")
                ),
                bsModal("importdata_Pred", "Fill the following", "go_importdata_pred", size = "small",
                        selectInput("DataBase.Pred.type","Data Base Type",c("Flat","Big Files(.csv)","ODBC","MS SQL","MySQL","Oracle","Amazon Redshift","Google Big Query"),selected="Flat"),
                        conditionalPanel("input['DataBase.Pred.type']=='Flat'",
                                         fileInput('datafilePredict', h4('Data set for Prediction'),
                                                   accept=c('text/csv', 'text/comma-separated-values,text/plain'))
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='ODBC'",
                                         rHandsontableOutput("ODBC_Pred.table")
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='MS SQL'",
                                         rHandsontableOutput("MS_sql_Pred.table")
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='MySQL'",
                                         rHandsontableOutput("Mysql_Pred.table")
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='Oracle'",
                                         rHandsontableOutput("Oracle_Pred.table")
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='Amazon Redshift'",
                                         rHandsontableOutput("Amazon_Redshift_Pred.table")
                        ),
                        conditionalPanel("input['DataBase.Pred.type']=='Google Big Query'",
                                         fileInput('json.file.Pred', h4('Import json File')),
                                         radioButtons("useLegacySql.Pred","Use LegacySql",c("No"="FALSE","Yes"="TRUE"),selected="FALSE",inline=TRUE),
                                         rHandsontableOutput("Google_Big_Query_Pred.table")
                        ),
                        br(),
                        radioButtons("Dates.as.parts.Pred","Dates as Parts",c("No","Yes"),selected="No",inline=TRUE)
                ),
                bsModal("Pred_Warnings","Warnings","go_Pred_Warnings", size = "small",
                        uiOutput("Prediction.err.Pred"),
                        DT::dataTableOutput("Vars.with.diff.distribution.Pred"),
                        DT::dataTableOutput("NA.table.Pred")
                ),
                
                navbarPage("AlgoTrace",id="Front",  #"Momenta Guided Data Science"
                           tabPanel("Main", 
                                    tabsetPanel(id="Main",
                                                tabPanel("Import Data",   
                                                         tabsetPanel(
                                                           tabPanel("View Train Data",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Import Data"),
                                                                      dashboardSidebar(    
                                                                        div(id="Margin",h4('Import Data')),
                                                                        actionButton("go_importdata_train","Import",width='90px',style="background-color: #367fa9"),
                                                                        sliderInput("division.rate.slider", 
                                                                                    label = h4('Training Set'),
                                                                                    min = 0.05, max = 0.95, value = 0.8),
                                                                        actionButton("MainSubmit.division.rate","Submit",width='90px',style="background-color: #367fa9"),
                                                                        conditionalPanel("output.DataIsChosen",
                                                                                         br(),
                                                                                         textInput("path.uploaded.data.export","File:"),
                                                                                         actionButton("MainSubmit.uploaded.data.export","Export",width='90px',style="background-color: #367fa9"),
                                                                                         div(id="Margin",textOutput("uploaded.data.export.text")),
                                                                                         br(),
                                                                                         div(id="Margin",htmlOutput("Memory_Size_Train_Data"))
                                                                        ),
                                                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                         class="busy_loader"))
                                                                      ),
                                                                      dashboardBody(
                                                                        textOutput("Train.file.loading"),
                                                                        conditionalPanel("output.DataIsChosen",
                                                                                         textOutput("showDataDimensions.Train"),
                                                                                         div(style='height:600px; overflow-x: scroll', 
                                                                                             DT::dataTableOutput('Main.View.Train')
                                                                                         ))
                                                                      ))
                                                           ),
                                                           tabPanel("View Test Data",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Import Data"),
                                                                      dashboardSidebar(
                                                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                         class="busy_loader"))
                                                                      ),
                                                                      dashboardBody(
                                                                        conditionalPanel("output.DataIsChosen",
                                                                                         textOutput("showDataDimensions.Test"),
                                                                                         div(style='height:600px; overflow-x: scroll',  
                                                                                             DT::dataTableOutput('Main.View.Test')))
                                                                      ))
                                                           )
                                                         )),  
                                                tabPanel("Filter Data",
                                                         dashboardPage(
                                                           dashboardHeader(title = "Filter Data"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.DataIsChosen",
                                                                              div(id="Margin",h3("Filter")),
                                                                              sidebarMenu(id="Filter",
                                                                                          menuItem("Filter By Coding", tabName = "Filter_By_Coding", icon = icon("th"),selected = TRUE),
                                                                                          menuItem("Filter", tabName = "Filter", icon = icon("th"))
                                                                              ),
                                                                              conditionalPanel("input['Filter']=='Filter_By_Coding' || output.CalculateFilter",
                                                                              div(style="display:inline-block",actionButton("MainSubmit.Data.Filter.Save","Save",width='88px',style="background-color: #367fa9")),
                                                                              div(style="display:inline-block",actionButton("MainSubmit.Data.Filter.Reset","Reset",width='88px',style="background-color: #367fa9"))
                                                                              ),
                                                                              div(id="Margin",textOutput("showDataDimensions.Filter")),
                                                                              div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                               class="busy_loader"))
                                                             )),   
                                                           dashboardBody(   
                                                             tabItems(
                                                               tabItem(tabName = "Filter_By_Coding",
                                                                       conditionalPanel("output.DataIsChosen",
                                                                                        helpText(HTML(paste("Here you can write R command<br/>",
                                                                                                            "Example: Var1>=5 & (Var2 %in% c('A','B','C'))"))),
                                                                                        textAreaInput("Filter.by.coding.text","Insert Code",rows=5),
                                                                                        actionButton("MainSubmit.Filter.by.coding","Submit"),
                                                                                        textOutput("Filter.by.coding.text.error")     
                                                                       )),
                                                               tabItem(tabName = "Filter",
                                                                       conditionalPanel("output.CalculateFilter",  
                                                                                        uiOutput("Data.Filter.numeric"),
                                                                                        uiOutput("Data.Filter.factor")
                                                                       ))
                                                             ))
                                                         ))
                                    )),
                           tabPanel("Text Analysis",
                                    conditionalPanel("output.DataIsChosen",
                                                     dashboardPage(
                                                       dashboardHeader(title = "Settings"),
                                                       dashboardSidebar(
                                                         selectInput("text.analysis.split",label="Split Text",choices=c("Words","Sentences","No"),selected="Words"),
                                                         conditionalPanel("input['text.analysis.split']=='Words'",
                                                                          numericInput("Words_split", "Number of Words", 1,min=1),
                                                                          
                                                                          conditionalPanel("input['Words_split']>0",#"output.One_word_split",
                                                                                           selectInput("Show.only.Exp",label="Show Pos/Neg/Other Expressions",
                                                                                                       choices=c("No","Neg","Pos","Other")),  
                                                                                           selectInput("Exclude.stopwords","Exclude Stop words",
                                                                                                       choices=c("No"="No","Yes"="Yes")),
                                                                                           div(style="display:inline-block",actionButton("go_Choose_Exp","Expressions",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px")), 
                                                                                           div(style="display:inline-block",actionButton("go_Choose_StopWords","Stop Words",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px"))
                                                                          )#End of "input['Words_split']>0"
                                                         ),##End of "input['text.analysis.split']=='Words'"
                                                         br(),
                                                         div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                          class="busy_loader"))
                                                       ),
                                                       dashboardBody(
                                                         fluidRow(
                                                           column(3,
                                                                  div(style='height:300px; overflow-y: scroll', 
                                                                  DT::dataTableOutput("Choose.variable.for.text.analysis")),
                                                                  br(),
                                                                  actionButton("MainSubmit.Choose.variable.for.text.analysis","Submit",width='150px')
                                                           ),
                                                           column(4,
                                                                  conditionalPanel("output.text_Analysis_char_is_chosen", 
                                                                                   div(style='height:300px; overflow-y: scroll', 
                                                                                       DT::dataTableOutput("text.analysis.variable.table"))
                                                                  )),
                                                           column(4,
                                                                  conditionalPanel("output.text_Analysis_char_is_chosen", 
                                                                                   div(style='height:300px; overflow-y: scroll', 
                                                                                       DT::dataTableOutput("text.analysis.phrases.table")), 
                                                                                   #radioButtons("transfer.string.to.dummy","Transfer Strings to dummy variables",choices = c("No","Yes"),selected = "No",inline = TRUE),
                                                                                   actionLink("go_text_analysis_phrases_table_export","Export Table",style="color: darkblue")
                                                                  ))
                                                         ),
                                                         br(),
                                                         ###############
                                                         tabsetPanel(
                                                           tabPanel("Transfer data to variables",
                                                                    fluidPage(
                                                                      conditionalPanel("output.text_Analysis_char_is_chosen",
                                                                                       br(),
                                                                                       radioButtons("transfer.string.to.dummy","Content Breakdown to variables",choices = c("No","Yes"),selected = "No",inline = TRUE)
                                                                      ),
                                                                      br(),
                                                                      helpText("Fill Table Manually"),
                                                                      tabsetPanel(
                                                                        tabPanel("Count",
                                                                        rHandsontableOutput("text.analysis.fill.table.count")  
                                                                        ),
                                                                        tabPanel("Count by Regex",
                                                                        rHandsontableOutput("text.analysis.fill.table.regex")  
                                                                        ),
                                                                        tabPanel("Replace Text",
                                                                        rHandsontableOutput("text.analysis.fill.table.replace.regex")  
                                                                        ),
                                                                        tabPanel("Create text variable",
                                                                        rHandsontableOutput("text.analysis.fill.table.as.variable")
                                                                        ),
                                                                        tabPanel("Create text variable using regex",
                                                                        rHandsontableOutput("text.analysis.fill.table.as.variable.using.regex")
                                                                        )
                                                                      ),
                                                                      br(),
                                                                      actionButton("MainSubmit.text.analysis.fill.table.save","Save Changes",width='200px'),
                                                                      br(),
                                                                      actionLink("View.Data.Text_Analysis","View Data",style="color: darkblue")
                                                                    )),  
                                                           tabPanel("Frequency Chart",
                                                                    br(),
                                                                    fluidPage(
                                                                      conditionalPanel("output.text_Analysis_char_is_chosen",
                                                                                       div(style='overflow-y: scroll',plotOutput("text.analysis.phrases.plot")),
                                                                                       div(id="Margin_actionLinks",actionLink("go_text_analysis_phrases_plot_export","Export Plot",style="color: darkblue")))
                                                                    ))
                                                         )
                                                        ################## 
                                                       )))
                           ),           
                           tabPanel("Configure", 
                                    dashboardPage(
                                      dashboardHeader(title = "Configure"),
                                      dashboardSidebar( 
                                        selectInput("Method",label="Method",
                                                    choices=c("Classification","Estimation"),
                                                    selected ="Classification"),
                                        selectInput("Column.to.check.configure",label="Choose Column",
                                                    choices=c("Exclude","Unimputed","Outliers"),
                                                    selected="Exclude"),
                                        div(style="display:inline-block",actionButton("Checkall","Check",width='88px',style="background-color: #367fa9")),
                                        div(style="display:inline-block",actionButton("UnCheckall","UnCheck",width='88px',style="background-color: #367fa9")),
                                        actionButton("MainSubmit.Vars.Summary","Save Changes",width='200px',style="background-color: #367fa9"),
                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                         class="busy_loader"))
                                      ),
                                      dashboardBody(
                                        rHandsontableOutput("Vars.Summary"),
                                        htmlOutput("ExcludedVariables.text"),  
                                        htmlOutput("not.to.fix.Variables.text"), 
                                        htmlOutput("not.to.fix.outliers.Variables.text"), 
                                        textOutput("TargetVariable.text")  
                                      ))
                           ),
                           tabPanel("Explore",
                                    tabsetPanel(id="Explore",
                                                tabPanel("Variables Correlation",   
                                                         dashboardPage(
                                                           dashboardHeader(title = "Variables Correlation"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.DataIsChosen",
                                                                              selectInput("Select.Choose.Data.Variables.Correlation", 
                                                                                          label = "View Data",
                                                                                          choices = c("Data", "Train","Test"),
                                                                                          selected = "Data"),
                                                                              div(id="Margin_and_Red_text",textOutput("Choose.Data.Variables.Correlation.text")),
                                                                              selectInput("AllowSampling.Variables.Correlation", 
                                                                                          label = "Sample",
                                                                                          choices = c("No", "Yes"),
                                                                                          selected = "No"),
                                                                              conditionalPanel("input['AllowSampling.Variables.Correlation']=='Yes'",
                                                                                               sliderInput("Percent.Variables.Correlation", 
                                                                                                           label = "Percentage of Data", 
                                                                                                           min = 0.05, max = 1.00, value = 0.10),
                                                                                               h6("Source:Sample data",style="margin-left: 15px;color:Red;")
                                                                              ),   
                                                                              actionButton("MainSubmit.Variables.Correlation","Allow Computing",icon = icon("play-circle"),width='200px',style="background-color: #367fa9"),  
                                                                              br(), 
                                                                              textInput("path.Variables.Correlation", "File:"),
                                                                              actionButton("MainSubmit.Export.button.Variables.Correlation","Export",width='90px',style="background-color: #367fa9"),
                                                                              div(id="Margin",textOutput("Variables.Correlation.Export.text")),
                                                                              div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                               class="busy_loader"))
                                                             )),
                                                           dashboardBody(
                                                             verbatimTextOutput("WarningTextForTarget.variables.correlation"), 
                                                             conditionalPanel("output.DataIsChosen",
                                                                              DT::dataTableOutput('Variables.Correlation')
                                                             )))),
                                                tabPanel("Data Analysis",
                                                         dashboardPage(
                                                           dashboardHeader(title = "Data Analysis"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.Have_Target_and_Vars", 
                                                                              sidebarMenu(id = "Data_analysis_sidebarmenu",
                                                                                          menuItem("Distribution", icon = icon("th"), tabName = "distribution",selected=TRUE),
                                                                                          menuItem("Missing Values", icon = icon("th"), tabName = "Missing_Values"),
                                                                                          menuItem("Outliers Graph", icon = icon("th"), tabName = "Outliers_graph"),
                                                                                          menuItem("Scatter", icon = icon("th"), tabName = "scatter"),
                                                                                          menuItem("Heat map", icon = icon("th"), tabName = "heat_map")
                                                                              ),
                                                                              selectInput("Select.Choose.Data.Data.Analysis", 
                                                                                          label = "View Data",
                                                                                          choices = c("Data", "Train","Test"),
                                                                                          selected = "Data"),
                                                                              div(id="Margin_and_Red_text",textOutput("Choose.Data.Data.Analysis.text")),
                                                                              actionButton("MainSubmit.Choose.Data.Data.Analysis","Submit",width='90px',style="background-color: #367fa9"),
                                                                              selectInput("AllowSampling.Data.Analysis", 
                                                                                          label = "Sample",
                                                                                          choices = c("No", "Yes"),
                                                                                          selected = "No"),
                                                                              conditionalPanel("input['AllowSampling.Data.Analysis']=='Yes'",
                                                                                               sliderInput("Percent_Data_Analysis", 
                                                                                                           label = "Percentage of Data", 
                                                                                                           min = 0.05, max = 1.00, value = 0.10),                
                                                                                               h6("Source:Sample data",style="margin-left: 15px;color:Red;")
                                                                              ),
                                                                              ##Outliers
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'Outliers_graph'",
                                                                                               div(id="Margin_and_Red_text",textOutput("Slow.Computation.Outliers.text"))
                                                                              ),
                                                                              ##Scatter
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'scatter'",
                                                                                               div(id="Margin_and_Red_text",textOutput("Slow.Computation.Scatter.text"))
                                                                              ),
                                                                              ##distribution $ Missing Values
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'distribution' || input['Data_analysis_sidebarmenu'] == 'Missing_Values'",
                                                                                               radioButtons("AddTextToFreq", "Add Text",
                                                                                                            c("No","Count","Percent"),selected = "No",inline = TRUE)
                                                                              ),
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'distribution'",
                                                                                               radioButtons("UseLog", "Log Distribution",
                                                                                                            c("No","Yes"),inline = TRUE)
                                                                              ),
                                                                              ##distribution & heat_map
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'distribution' || input['Data_analysis_sidebarmenu'] == 'heat_map'",
                                                                                               radioButtons("CutTheData", "Groups",
                                                                                                            c("No","Yes"),selected = "Yes",inline = TRUE),
                                                                                               conditionalPanel("input['CutTheData'] == 'Yes'",
                                                                                                                numericInput("CutTheDataSize","Number of Groups",10,min=2))
                                                                              ),
                                                                              conditionalPanel("output.Estimation_chosen",
                                                                                               conditionalPanel("input['Data_analysis_sidebarmenu'] == 'distribution'",                  
                                                                                                                selectInput("Est.explore.data.criterion.distribution", 
                                                                                                                            label = "Criterion",
                                                                                                                            choices = c("Mean","Median","Min","Max","Q1","Q3","Box.plot"),
                                                                                                                            selected = "Mean")),
                                                                                               conditionalPanel("input['Data_analysis_sidebarmenu'] == 'heat_map'",                  
                                                                                                                selectInput("Est.explore.data.criterion.heat_map", 
                                                                                                                            label = "Criterion",
                                                                                                                            choices = c("Mean","Median","Min","Max","Q1","Q3"),
                                                                                                                            selected = "Mean"))
                                                                              ),#End of "input['Method']=='Estimation'"
                                                                              
                                                                              ##heat_map
                                                                              conditionalPanel("input['Data_analysis_sidebarmenu'] == 'heat_map'",
                                                                                               selectInput("HeatMap", 
                                                                                                           label = "Heat Map count",
                                                                                                           choices = c("count","percent","row percent","column percent","row mean","column mean"),
                                                                                                           selected = "count")
                                                                              ),
                                                                              div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                               class="busy_loader"))
                                                             )),
                                                           dashboardBody(  
                                                             verbatimTextOutput("Inter.data.analysis.text"), 
                                                             conditionalPanel("output.Have_Target_and_Vars", 
                                                                              tabItems(
                                                                                tabItem(tabName = "distribution",
                                                                                        fluidRow(  
                                                                                          column(3,rHandsontableOutput("Distribution.Rhands"),
                                                                                                 actionLink("go_Distribution_gridplot", "Export All Plots",style="color: darkblue"),
                                                                                                 br(),actionButton("MainSubmit.Distribution","Submit")), 
                                                                                          column(9,
                                                                                                 verbatimTextOutput("Distribution.frequency.plot.warning.text"),
                                                                                                 plotOutput("Distribution.frequency.plot"),
                                                                                                 conditionalPanel("output.Distribution_frequency_actionlink",
                                                                                                                  div(id="Margin_actionLinks",
                                                                                                                      actionLink("go_Distributionfrequencyplot", "Expand Plot",style="color: darkblue"),
                                                                                                                      actionLink("go_Distributionfrequencytable", "/Expand Table",style="color: darkblue"))),
                                                                                                 br(),
                                                                                                 verbatimTextOutput("Distribution.occurance.plot.warning.text"),
                                                                                                 plotOutput("Distribution.occurance.plot"),
                                                                                                 conditionalPanel("output.Distribution_occurance_actionlink",
                                                                                                                  div(id="Margin_actionLinks",
                                                                                                                      actionLink("go_Distributionoccuranceplot", "Expand Plot",style="color: darkblue"),
                                                                                                                      actionLink("go_Distributionoccurancetable", "/Expand Table",style="color: darkblue")))
                                                                                          )
                                                                                        )),
                                                                                tabItem(tabName = "Missing_Values",
                                                                                        fluidRow(  
                                                                                          column(3,rHandsontableOutput("Missing.Values.Rhands"),
                                                                                                 conditionalPanel("output.Missing_Values_actionlink",
                                                                                                                  actionLink("go_Missing_Values_table_export", "Export Table",style="color: darkblue")),
                                                                                                 br(),actionButton("MainSubmit.Missing.Values","Submit")
                                                                                          ), 
                                                                                          column(9,plotOutput("Missing.Values.plot"))
                                                                                        )),
                                                                                tabItem(tabName = "Outliers_graph",
                                                                                        fluidRow(  
                                                                                          column(3,rHandsontableOutput("Outliers.Rhands"),
                                                                                                 actionLink("go_Outliers_settings", "Outliers Settings",style="color: darkblue"),
                                                                                                   br(),actionButton("MainSubmit.Outliers","Submit")
                                                                                          ), 
                                                                                          column(9,conditionalPanel("output.Outliers_actionlink",
                                                                                                                    ggvisOutput("Outliers_plot"),  
                                                                                                                    div(id="Margin_actionLinks",actionLink("go_Outliers_table", "Expand Table",style="color: darkblue"))
                                                                                                                    ))
                                                                                        )),
                                                                                tabItem(tabName = "scatter",
                                                                                        fluidRow(   
                                                                                          column(3,rHandsontableOutput("Scatter.Rhands"),
                                                                                                  br(),actionButton("MainSubmit.scatter","Submit")
                                                                                          ), 
                                                                                          column(9,conditionalPanel("output.Scatter_actionlink",
                                                                                                                    ggvisOutput("scatter_plot"),
                                                                                                                    div(id="Margin_actionLinks",actionLink("go_scatter_table", "Expand Table",style="color: darkblue"))
                                                                                          ))
                                                                                        )),
                                                                                tabItem(tabName = "heat_map",
                                                                                        fluidRow( 
                                                                                          column(3,rHandsontableOutput("Heat.map.Rhands"),
                                                                                                 actionLink("go_Combmap_gridplot", "Export ALL Plots",style="color: darkblue"),
                                                                                                 br(),actionButton("MainSubmit.vars.for.comb.map","Submit")), 
                                                                                          column(9,
                                                                                                 verbatimTextOutput("Combination.frequency.map.warning"),
                                                                                                 plotOutput("Combination.frequency.map"),
                                                                                                 conditionalPanel("output.Combination_frequency_map_actionlink",
                                                                                                                  div(id="Margin_actionLinks",                 
                                                                                                                      actionLink("go_Combfrequencymap", "Expand Plot",style="color: darkblue"),
                                                                                                                      actionLink("go_Combfrequencymaptable", "/Expand Table",style="color: darkblue"))),
                                                                                                 br(),
                                                                                                 verbatimTextOutput("Combination.map.warning"),
                                                                                                 plotOutput("Combination.map"),
                                                                                                 conditionalPanel("output.Combination_map_actionlink",
                                                                                                                  div(id="Margin_actionLinks",
                                                                                                                      actionLink("go_Combmap", "Expand Plot",style="color: darkblue"),
                                                                                                                      actionLink("go_Combmaptable", "/Expand Table",style="color: darkblue")))
                                                                                          ))
                                                                                )
                                                                              )
                                                             )))),
                                                tabPanel("Variable vs Target Model",   
                                                         dashboardPage(
                                                           dashboardHeader(title = "Variable vs Target Model"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.Have_Target_and_Vars",
                                                                              selectInput("AllowSampling.Var.vs.target.model", 
                                                                                          label = "Sample",
                                                                                          choices = c("No", "Yes"),
                                                                                          selected = "No"),
                                                                              conditionalPanel("input['AllowSampling.Var.vs.target.model']=='Yes'",
                                                                                               sliderInput("Percent.Var.vs.target.model", 
                                                                                                           label = "Percentage of Data", 
                                                                                                           min = 0.05, max = 1.00, value = 0.10),
                                                                                               h6("Source:Sample data",style="margin-left: 15px;color:Red;")
                                                                              ),
                                                             div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                              class="busy_loader"))
                                                             )
                                                           ),
                                                           dashboardBody(
                                                             verbatimTextOutput("Inter.Var.vs.target.model.text"), 
                                                             conditionalPanel("output.Have_Target_and_Vars",
                                                                              fluidRow(
                                                                                column(3,rHandsontableOutput("Var.vs.target.model_rhTable"),br(),actionButton("MainSubmit_Var.vs.target.model","Submit")),
                                                                                column(9,plotOutput("Var.vs.target.model_Plot.count"),br(),plotOutput("Var.vs.target.model_Plot.percent"))
                                                                              )                
                                                             )))),
                                                tabPanel("Ratio Combinations",
                                                         dashboardPage(
                                                           dashboardHeader(title = "Ratio Combinations"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.Have_Target_and_Vars",
                                                                              conditionalPanel("output.Estimation_chosen",
                                                                                               uiOutput("Interval.for.ratio.combination")),
                                                                              sliderInput("ratio.comb.mean", 
                                                                                          label = "Ratio levels", 
                                                                                          min = 0.05, max = 1, value = 0.75),
                                                                              numericInput("ratio.comb.count", "Minimum count:", 0,min=0),
                                                                              radioButtons("ratio.comb.for.couples", "Two Variables Combinations", inline=TRUE, c("No","Yes")),
                                                                              actionButton("Allow.Computing.ratio.comb","Allow Computing",icon = icon("play-circle"),width='200px',style="background-color: #367fa9"),
                                                                              div(style="display:inline-block",actionButton("transfer.ratio.to.dummies","Transfer",width='88px',style="background-color: #367fa9")),
                                                                              div(style="display:inline-block",actionButton("reset.ratio.to.dummies","Reset",width='88px',style="background-color: #367fa9")),
                                                                              div(id="Margin",textOutput("Transfer.ratio.to.dummies.text")),
                                                                              br(),
                                                                              textInput("path.ratio.comb.export", "File:"),
                                                                              radioButtons("What.to.export.ratio.comb", "Export Data",
                                                                                           c("Table Export"="Table","File Export"="File"),
                                                                                           inline = TRUE),
                                                                              actionButton("MainSubmit.Export.button.ratio.comb","Export",width='90px',style="background-color: #367fa9"),
                                                                              div(id="Margin",textOutput("Export.ratio.comb.text")),
                                                                              div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                               class="busy_loader"))
                                                             )),
                                                           dashboardBody(
                                                             verbatimTextOutput("Inter.ratio.comb.text"), 
                                                             conditionalPanel("output.Have_Target_and_Vars",
                                                                              DT::dataTableOutput("Ratio.comb.table.to.show")
                                                                              
                                                             ))
                                                         )),
                                                tabPanel("Geography",
                                                         dashboardPage(
                                                           dashboardHeader(title = "Geography"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.Geography",
                                                                              div(id="Margin",h3("Filter")),
                                                                              selectInput("AllowSampling.GeographyData", 
                                                                                          label = "Sample",
                                                                                          choices = c("No", "Yes"),
                                                                                          selected = "No"),
                                                                              conditionalPanel("input['AllowSampling.GeographyData']=='Yes'",
                                                                                               sliderInput("Percent.GeographyData", 
                                                                                                           label = "Percentage of Data", 
                                                                                                           min = 0.05, max = 1.00, value = 0.10),
                                                                                               h6("Source:Sample data",style="margin-left: 15px;color:Red;")
                                                                              ),
                                                                              div(style="display:inline-block",actionButton("MainSubmit.GeographyData.Filter.Save","Save",width='88px',style="background-color: #367fa9")),
                                                                              div(style="display:inline-block",actionButton("MainSubmit.GeographyData.Filter.Reset","Reset",width='88px',style="background-color: #367fa9")),
                                                                              div(id="Margin",textOutput("showDataDimensions.GeographyData.Filter")),
                                                                              div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                               class="busy_loader"))
                                                             )),
                                                           dashboardBody(
                                                             conditionalPanel("output.Geography",
                                                                              div(id="map_black_text",textOutput("Geography.map.text")), 
                                                                              fluidRow(
                                                                                column(4,
                                                                                       div(style='height:800px; overflow-y: scroll',
                                                                                           wellPanel(
                                                                                             uiOutput("GeographyData.Filter.numeric"),
                                                                                             uiOutput("GeographyData.Filter.factor")
                                                                                           ))
                                                                                ),
                                                                                column(8,
                                                                                       conditionalPanel("$('html').hasClass('shiny-busy')",
                                                                                           div(id="loading_position",div(class="loader_plot"))             
                                                                                                        ),
                                                                                leafletOutput("Geography.map",height =800)
                                                                                
                                                                                )
                                                                              )
                                                             ))))
                                    )),
                           tabPanel("Prediction Analysis",
                                    tabsetPanel(id="Outer_Prediction_Analysis",
                                                tabPanel("Prediction Analysis",
                                                         tabsetPanel(id="Prediction_Analysis",
                                                                     tabPanel("Prediction Table", 
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  selectInput("AllowSampling", 
                                                                                              label = "Sample Trainning Data",    
                                                                                              choices = c("No","Just Shuffle", "Yes"),
                                                                                              selected = "No"),
                                                                                  conditionalPanel("input['AllowSampling']=='Yes'",
                                                                                                   sliderInput("percent", 
                                                                                                               label = "Percentage of Data", 
                                                                                                               min = 0.05, max = 1.00, value = 0.10),
                                                                                                   div(id="Margin",h6("Source:Sample data",style="color:Red;"))),
                                                                                  radioButtons("Two.layers.prediction","Two layers prediction",c("No","Yes"),selected="No",inline=TRUE),
                                                                                  conditionalPanel("input['Two.layers.prediction']=='Yes'",
                                                                                                   selectInput("ChooseAlgorithms.for.first.layer", 
                                                                                                               label = "Algorithms for first layer",
                                                                                                               choices = c("Linear","Weighted Linear","Naive Linear","Naive Weighted Linear","Negative Binomial","Naive Negative Binomial",
                                                                                                                           "Quantile","Naive Quantile","Xgboost","Naive Xgboost","Recursive Partitioning Tree","Naive Recursive Partitioning Tree","Rforest","Naive Rforest",
                                                                                                                           "Neural Network","Naive Neural Network","All Models"), 
                                                                                                               selected = "All Models",multiple = TRUE)
                                                                                  ),
                                                                                  selectInput("ChooseAlgorithms", 
                                                                                              label = "Algorithms",
                                                                                              choices = c("Logistic","Weighted Logistic","Naive Logistic","Naive Weighted Logistic","Xgboost",
                                                                                                          "Naive Xgboost","Recursive Partitioning Tree","Naive Recursive Partitioning Tree","Rforest",
                                                                                                          "Naive Rforest","Neural Network","Naive Neural Network","All Models"),  
                                                                                              selected = "All Models",multiple = TRUE),
                                                                                  div(id="Margin_and_Red_text",textOutput("Slow.Algorithms.text")),
                                                                                  div(style="display:inline-block",actionButton("go_settings", "Settings",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px")),
                                                                                  div(style="display:inline-block",actionButton("go_Run_all_options", "All options",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px")),
                                                                                  actionButton("AllowComputingAnalysis", "Allow Computing",icon = icon("play-circle"),width='200px',style="background-color: #367fa9"),
                                                                                  div(id="Excluded_red_text",textOutput("CheckExcludedTab")),
                                                                                  div(id="Margin",textOutput("Time_Taken")),
                                                                                  br(),
                                                                                  div(id="Margin",htmlOutput("Memory_Size_Models")),
                                                                                  div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                   class="busy_loader"))
                                                                                ),
                                                                                dashboardBody(
                                                                                  verbatimTextOutput("not.allow.computing.text"),
                                                                                  conditionalPanel("output.Estimation && output.HaveResults",
                                                                                                   fluidRow(
                                                                                                     splitLayout(cellWidths = c("50%", "50%"),plotOutput("accuracy_barplot.est"),plotOutput('all_models_Norm_Residuals_plot.est'))
                                                                                                   ),                 
                                                                                                   DT::dataTableOutput("Settings_table.est"),
                                                                                                   br(),
                                                                                                   DT::dataTableOutput("summary_table.est")
                                                                                  ),
                                                                                  conditionalPanel("output.Classification && output.HaveResults",
                                                                                                   fluidRow(
                                                                                                     splitLayout(cellWidths = c("50%", "50%"),plotOutput('accuracy_barplot'),plotOutput('multiple_roc'))
                                                                                                   ),
                                                                                                   DT::dataTableOutput("Settings_table"),
                                                                                                   br(),
                                                                                                   DT::dataTableOutput("summary_table"),
                                                                                                   br(),
                                                                                                   uiOutput("confusion")
                                                                                  )
                                                                                ))
                                                                     ),
                                                                     tabPanel("Trainning Results", 
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                   class="busy_loader"))
                                                                                ),
                                                                                dashboardBody(
                                                                                  conditionalPanel("output.Estimation && output.HaveResults",
                                                                                                   DT::dataTableOutput("summary_trainning_table.est")
                                                                                  ),
                                                                                  conditionalPanel("output.Classification && output.HaveResults",
                                                                                                   DT::dataTableOutput("summary_trainning_table")               
                                                                                  ),
                                                                                  #######Summary###
                                                                                  conditionalPanel("output.At_least_one_Summary_exist",
                                                                                                   actionLink("go_model_summary", "Show Model Summary",style="color: darkblue")
                                                                                  )
                                                                                ))
                                                                     ),
                                                                     tabPanel("Accuracy Deciles",
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  conditionalPanel("output.Classification && output.HaveResults",
                                                                                                   uiOutput("Model.for.deciles.accuracy.ui"),
                                                                                                   selectInput("Criterion.for.deciles.accuracy", label="Criterion", choices=c("Accuracy",
                                                                                                                                                                                     "Accuracy 0","Precision","Recall"),selected="Accuracy",width='200px'),
                                                                                                   numericInput("Deciles.Count", "Number of Bins", 10,min=1,width='200px'),
                                                                                                   actionButton("MainSubmit.Deciles.Accuracy","Submit",style="background-color: #367fa9"),
                                                                                                   div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                                    class="busy_loader"))
                                                                                  )),
                                                                                dashboardBody(
                                                                                  conditionalPanel("output.Classification && output.HaveResults",
                                                                                                   fluidRow(
                                                                                                     column(12,DT::dataTableOutput("deciles.accuracy.table"),
                                                                                                            conditionalPanel("output.deciles_accuracy_table_actionlink",
                                                                                                                             actionLink("go_deciles_accuracy_table","Export Table",style="color: darkblue")))
                                                                                                   ),
                                                                                                   fluidRow(
                                                                                                     column(6,plotOutput("deciles.accuracy.count.plot")),
                                                                                                     column(6,plotOutput("deciles.accuracy.plot"))
                                                                                                   )
                                                                                  )))
                                                                     ),
                                                                     tabPanel("Avg Deciles",
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  conditionalPanel("output.Estimation && output.HaveResults",
                                                                                                   uiOutput("Model.for.deciles.avg.ui"),
                                                                                                   selectInput("Criterion.for.deciles.avg", label="Criterion", choices=c("Avg Traget vs Prediction","MAE","Norm abs Difference",
                                                                                                                                                                         "Abs Sum Difference","RMSE"),selected="Avg Traget vs Prediction",width='200px'),
                                                                                                   numericInput("Deciles.Avg.Count", "Number of Bins", 5,min=1,width='200px'),
                                                                                                   actionButton("MainSubmit.Deciles.Avg","Submit",style="background-color: #367fa9"),
                                                                                                   div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                                    class="busy_loader"))
                                                                                  )),
                                                                                dashboardBody(
                                                                                  conditionalPanel("output.Estimation && output.HaveResults",
                                                                                                   fluidRow(
                                                                                                     column(12,DT::dataTableOutput("Avg.deciles.table"),
                                                                                                            conditionalPanel("output.deciles_avg_table_actionlink",
                                                                                                                             actionLink("go_deciles_avg_table","Export Table",style="color: darkblue")))
                                                                                                   ),
                                                                                                   fluidRow(
                                                                                                     column(6,plotOutput("Avg.deciles.count.plot")),
                                                                                                     column(6,plotOutput("Avg.deciles.plot"))
                                                                                                   )
                                                                                  )))
                                                                     ),
                                                                     tabPanel("Data set", 
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                   class="busy_loader"))
                                                                                ),
                                                                                dashboardBody(
                                                                                  conditionalPanel("output.Analysis_DataSet_exist",
                                                                                                   div(style='height:600px; overflow-x: scroll',  
                                                                                                       DT::dataTableOutput('Analysis.DataSet')))
                                                                                ))
                                                                     ),
                                                                     tabPanel("View Model", 
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  div(id="Margin",h4('Show Formula')),
                                                                                  selectInput("Choose.formula.lang", label="Formula", choices=c("SQL","Oracle")),
                                                                                  conditionalPanel("input['Choose.formula.lang']=='SQL'",
                                                                                                   conditionalPanel("output.At_least_one_formula_exist_ms_sql",
                                                                                                                    uiOutput("Model.for.Formula.ui.ms.sql"), 
                                                                                                                    actionButton("MainSubmit.Model.for.Formula.ms.sql","Submit",style="background-color: #367fa9"),
                                                                                                                    br(), 
                                                                                                                    textInput("path.Formula.export.ms.sql","File:"),
                                                                                                                    actionButton("MainSubmit.Formula.export.ms.sql","Export",style="background-color: #367fa9"),
                                                                                                                    div(id="Margin",textOutput("Formula.export.text.ms.sql"))
                                                                                                   )),
                                                                                  conditionalPanel("input['Choose.formula.lang']=='Oracle'",
                                                                                                   conditionalPanel("output.At_least_one_formula_exist_oracle",
                                                                                                                    uiOutput("Model.for.Formula.ui.oracle"), 
                                                                                                                    actionButton("MainSubmit.Model.for.Formula.oracle","Submit",style="background-color: #367fa9"),
                                                                                                                    br(), 
                                                                                                                    textInput("path.Formula.export.oracle","File:"),
                                                                                                                    actionButton("MainSubmit.Formula.export.oracle","Export",style="background-color: #367fa9"),
                                                                                                                    div(id="Margin",textOutput("Formula.export.text.oracle"))
                                                                                                   )),     
                                                                                                   
                                                                                  br(), 
                                                                                  div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                   class="busy_loader"))
                                                                                ),
                                                                                dashboardBody(
                                                                                  conditionalPanel("input['Choose.formula.lang']=='SQL'",
                                                                                  verbatimTextOutput("Show.Formula.ms.sql")),
                                                                                  conditionalPanel("input['Choose.formula.lang']=='Oracle'",
                                                                                  verbatimTextOutput("Show.Formula.oracle"))
                                                                                ))),
                                                                     ##############
                                                                     tabPanel("Variables Excluded",
                                                                              dashboardPage(
                                                                                dashboardHeader(title = "Prediction Analysis"),
                                                                                dashboardSidebar(
                                                                                  div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                   class="busy_loader"))
                                                                                ),
                                                                                dashboardBody(
                                                                                  DT::dataTableOutput("Lot.Of.NA.Variables.Table"),
                                                                                  DT::dataTableOutput("Const.Variables.Table")                  
                                                                                ))
                                                                     )
                                                         )),
                                                tabPanel("Insights",
                                                         dashboardPage(
                                                           dashboardHeader(title = "Insights"),
                                                           dashboardSidebar(
                                                             conditionalPanel("output.HaveResults",  
                                                                              uiOutput("Model.for.dashboard.ui"),
                                                                              sidebarMenu(
                                                                                menuItem("Dashboard", tabName = "dashboard", icon = icon("th"),selected = TRUE),  
                                                                                menuItem("Variables vs Target", icon = icon("th"), tabName = "vars_vs_target"),
                                                                                div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                 class="busy_loader"))
                                                                              )
                                                             )),
                                                           dashboardBody(
                                                             conditionalPanel("output.HaveResults",  
                                                                              tabItems(
                                                                                tabItem(tabName = "dashboard",
                                                                                        fluidRow(
                                                                                          column(3,box(plotOutput("Variables.Importance"), 
                                                                                                       height = 450,width = NULL,title ="Variables Importance"),  
                                                                                                 conditionalPanel("output.Variables_Importance",
                                                                                                                  actionLink("go_Variables_Importance_plot", "Expand Plot/",style="color: darkblue"),
                                                                                                                  actionLink("go_Variables_Importance_table", "Expand Table",style="color: darkblue")
                                                                                                 )), 
                                                                                          conditionalPanel("output.Classification",
                                                                                                           column(4,box(plotOutput("Dashboard.CV.plot"),
                                                                                                                        conditionalPanel("output.Dashboard_CV_Table",actionLink("go_Dashboard_CV_Table","Expand Table",style="color: darkblue")),
                                                                                                                        height = 450,width = NULL,title="Confusion Matrix"))
                                                                                          ),
                                                                                          conditionalPanel("output.Estimation",
                                                                                                           column(4,box(plotOutput('Dashboard.Est.Act_Avg'),
                                                                                                                        conditionalPanel("output.Dashboard_Est_Act_Avg",actionLink("go_Dashboard_Est_Act_Avg_Table","Expand Table",style="color: darkblue")),
                                                                                                                        height = 450,width = NULL,title="Model Estimation vs Target"))
                                                                                          ),
                                                                                          column(3,box(rHandsontableOutput("input.what.if"),height = 450,width = NULL,title="What If")),  
                                                                                          column(2,box(plotOutput("What.if.prediction.result", height="100%"),height = 450,width = NULL ,title="Prediction Result"))
                                                                                        ),
                                                                                        br(),
                                                                                        fluidRow(
                                                                                          column(6,box(rHandsontableOutput("Seq.table"),height = 450,width = NULL,title="Sequence Table")), 
                                                                                          column(6,div(style='overflow-y: scroll',box(plotOutput("Seq.table.plot"),height = 450,width = NULL,title="Prediction Results Series")))
                                                                                        )),
                                                                                tabItem("vars_vs_target",
                                                                                        fluidRow(
                                                                                          column(3,rHandsontableOutput("Vars.vs.target.Correlation.table")),
                                                                                          column(9,div(style='overflow-y: scroll',box(plotOutput("Vars.vs.target.Correlation.plot"),height = 450,width = NULL)),
                                                                                                 conditionalPanel("output.Vars_vs_target_Correlation_plot_actionlink",
                                                                                                                  actionLink("go_Vars_vs_target_Correlation_plot", "Expand Plot",style="color: darkblue")))
                                                                                        ))
                                                                              )
                                                             ))))  
                                    )),
                           tabPanel("Predict and Export",
                                    tabsetPanel(
                                      tabPanel("Validation",
                                               tabsetPanel(id="Validation", 
                                                           tabPanel("View Data and Prediction",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Validation"),
                                                                      dashboardSidebar(
                                                                        div(id="Margin",h4('Validation')),
                                                                        actionButton("go_importdata_cv","Import",width='90px',style="background-color: #367fa9"),
                                                                        uiOutput("choose_Method_for_CV.ui"),
                                                                        actionButton("PredictExportSubmit.CV","Submit",width='90px',style="background-color: #367fa9"),
                                                                        actionLink("go_CV_Warnings","Show Warnings",style="color: white"),
                                                                        br(),
                                                                        textInput("path.CV", "File:"),
                                                                        actionButton("Export.button.CV","Export",width='90px',style="background-color: #367fa9"),
                                                                        div(id="Margin",textOutput("Export.CV")),
                                                                        br(),
                                                                        div(id="Margin",htmlOutput("Memory_Size_CV_Data")),
                                                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                         class="busy_loader"))
                                                                      ),
                                                                      dashboardBody(
                                                                        textOutput("CV.file.loading"),
                                                                        conditionalPanel("output.CV_Data_exist",
                                                                                         div(style='height:600px; overflow-x: scroll',  
                                                                                             DT::dataTableOutput('PredictExport.Validation.Data'))
                                                                        )))
                                                           ),
                                                           tabPanel("Validation Results",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Validation"),
                                                                      dashboardSidebar(
                                                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                         class="busy_loader"))
                                                                      ),
                                                                      dashboardBody(
                                                                        conditionalPanel("output.Estimation",
                                                                                         fluidRow(
                                                                                           splitLayout(cellWidths = c("50%", "50%"),plotOutput("accuracy_barplot.est.cv"),plotOutput('all_models_Norm_Residuals_plot.est.cv'))
                                                                                         ),
                                                                                         DT::dataTableOutput("Settings_table.est.cv"),
                                                                                         br(),
                                                                                         DT::dataTableOutput('summary_table.est.cv')  
                                                                        ),
                                                                        conditionalPanel("output.Classification",
                                                                                         fluidRow(
                                                                                           splitLayout(cellWidths = c("50%", "50%"),plotOutput('accuracy_barplot.cv'),plotOutput('multiple_roc.cv'))
                                                                                         ),
                                                                                         DT::dataTableOutput("Settings_table.cv"),
                                                                                         br(),
                                                                                         DT::dataTableOutput('summary_table.cv'),  
                                                                                         br(),
                                                                                         uiOutput('confusion.cv')  
                                                                        )))
                                                           ),
                                                           tabPanel("Accuracy Deciles",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Validation"),
                                                                      dashboardSidebar(
                                                                        conditionalPanel("output.Classification && output.HaveResults",
                                                                                         uiOutput("Model.for.deciles.accuracy.ui.cv"),
                                                                                         selectInput("Criterion.for.deciles.accuracy.cv", label="Criterion", choices=c("Accuracy",
                                                                                                                                                                              "Accuracy 0","Precision","Recall"),selected="Accuracy",width='200px'),
                                                                                         numericInput("Deciles.Count.cv", "Number of Bins", 10,min=1,width='200px'),
                                                                                         actionButton("MainSubmit.Deciles.Accuracy.cv","Submit",style="background-color: #367fa9"),
                                                                                         div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                          class="busy_loader"))
                                                                        )),
                                                                      dashboardBody(
                                                                        conditionalPanel("output.Classification && output.HaveResults",
                                                                                         fluidRow(
                                                                                           column(12,DT::dataTableOutput("deciles.accuracy.table.cv"),
                                                                                                  conditionalPanel("output.deciles_accuracy_table_cv_actionlink",
                                                                                                                   actionLink("go_deciles_accuracy_table_cv","Export Table",style="color: darkblue")))
                                                                                         ),
                                                                                         fluidRow(
                                                                                           column(6,plotOutput("deciles.accuracy.count.plot.cv")),
                                                                                           column(6,plotOutput("deciles.accuracy.plot.cv"))
                                                                                         )
                                                                        )))
                                                           ),
                                                           tabPanel("Avg Deciles",
                                                                    dashboardPage(
                                                                      dashboardHeader(title = "Validation"),
                                                                      dashboardSidebar(
                                                                        conditionalPanel("output.Estimation && output.HaveResults",
                                                                                         uiOutput("Model.for.deciles.avg.ui.cv"),
                                                                                         selectInput("Criterion.for.deciles.avg.cv", label="Criterion", choices=c("Avg Traget vs Prediction","MAE","Norm abs Difference",
                                                                                                                                                                "Abs Sum Difference","RMSE"),selected="Avg Traget vs Prediction",width='200px'),
                                                                                         numericInput("Deciles.Avg.Count.cv", "Number of Bins", 5,min=1,width='200px'),
                                                                                         actionButton("MainSubmit.Deciles.Avg.cv","Submit",style="background-color: #367fa9"),
                                                                                         div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                                                          class="busy_loader"))
                                                                        )),
                                                                      dashboardBody(
                                                                        conditionalPanel("output.Estimation && output.HaveResults",
                                                                                         fluidRow(
                                                                                           column(12,DT::dataTableOutput("Avg.deciles.table.cv"),
                                                                                                  conditionalPanel("output.deciles_avg_table_actionlink_cv",
                                                                                                                   actionLink("go_deciles_avg_table_cv","Export Table",style="color: darkblue")))
                                                                                         ),
                                                                                         fluidRow(
                                                                                           column(6,plotOutput("Avg.deciles.count.plot.cv")),
                                                                                           column(6,plotOutput("Avg.deciles.plot.cv"))
                                                                                         )
                                                                        )))
                                                           )
                                               )),#End of Validation Tab
                                      tabPanel("Prediction",
                                               dashboardPage(
                                                 dashboardHeader(title = "Prediction"),
                                                 dashboardSidebar(
                                                   div(id="Margin",h4('Prediction')),
                                                   actionButton("go_importdata_pred","Import",width='90px',style="background-color: #367fa9"),
                                                   uiOutput("choose_Method_for_Pred.ui"),
                                                   actionButton("PredictExportSubmit.Pred","Submit",width='90px',style="background-color: #367fa9"),
                                                   actionLink("go_Pred_Warnings","Show Warnings",style="color: white"),
                                                   br(),
                                                   textInput("path.Pred", "File:"),
                                                   actionButton("Export.button.Pred", "Export",width='90px',style="background-color: #367fa9"),
                                                   div(id="Margin",textOutput("Export.Pred")),
                                                   br(),
                                                   div(id="Margin",htmlOutput("Memory_Size_Pred_Data")),
                                                   div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                                    class="busy_loader"))
                                                 ),
                                                 dashboardBody(
                                                   textOutput("Pred.file.loading"),
                                                   conditionalPanel("output.Predict_Data_exist", 
                                                                    div(style='height:600px; overflow-x: scroll',  
                                                                        DT::dataTableOutput('PredictExport.Prediction.Data')))
                                                 ))
                                      ) #End of Prediction Tab
                                    )),
                           tabPanel("Source Listener",  
                                    dashboardPage(
                                      dashboardHeader(title = "Source Listener"),
                                      dashboardSidebar(
                                        selectInput("Source.listener.time.units","Units:",c("Secs"="secs","Mins"="mins","Hours"="hours","Days"="days"),selected="secs"),
                                        numericInput("Source.listener.time","Time:",1,min=1),
                                        uiOutput("choose_Method_for_Source.listener"),
                                        selectInput("Item.listener","Source",c("Folder","ODBC","MS SQL","MySQL","Oracle","Amazon Redshift","Google Big Query"),selected="Folder"),
                                        ###
                                        conditionalPanel("input[['Item.listener']]=='Folder'",
                                        actionButton("Source.listener.Dir.import", "Directory name for Import",style="background-color: #367fa9"),
                                        br(),
                                        actionButton("Source.listener.Dir.export", "Directory name for Export",style="background-color: #367fa9"),
                                        br(),
                                        div(style="display:inline-block",actionButton("MainSubmit.source.listener","Start",width='88px',style="background-color: #367fa9")),
                                        div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener","Stop",width='88px',style="background-color: #367fa9"))
                                        ),
                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                         class="busy_loader"))
                                      ),
                                      dashboardBody(
                                        conditionalPanel("input[['Item.listener']]=='Folder'",
                                        verbatimTextOutput("Source.listener.Dir.import.Text"),
                                        br(),
                                        verbatimTextOutput("Source.listener.Dir.export.Text"),
                                        br(),
                                        verbatimTextOutput("Source.listener.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='ODBC'",
                                                         rHandsontableOutput("Source.listener.ODBC.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.ODBC.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.ODBC.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.ODBC.table.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='MS SQL'",
                                                         rHandsontableOutput("Source.listener.MS.SQL.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.MS.SQL.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.MS.SQL.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.MS.SQL.table.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='MySQL'",
                                                         rHandsontableOutput("Source.listener.MySQL.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.MySQL.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.MySQL.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.MySQL.table.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='Oracle'",
                                                         rHandsontableOutput("Source.listener.Oracle.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.Oracle.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.Oracle.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.Oracle.table.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='Amazon Redshift'",
                                                         rHandsontableOutput("Source.listener.Amazon.Redshift.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.Amazon.Redshift.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.Amazon.Redshift.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.Amazon.Redshift.table.Text")
                                        ),
                                        conditionalPanel("input[['Item.listener']]=='Google Big Query'",
                                                         fileInput('Source.listener.Google.Big.Query.json.file', h4('Import json File')),
                                                         radioButtons("Source.listener.Google.Big.Query.useLegacySql","Use LegacySql",c("No"="FALSE","Yes"="TRUE"),selected="FALSE",inline=TRUE),
                                                         rHandsontableOutput("Source.listener.Google.Big.Query.table.Rhands"),
                                                         br(),
                                                         div(style="display:inline-block",actionButton("MainSubmit.source.listener.Google.Big.Query.table","Start listening")),
                                                         div(style="display:inline-block",actionButton("MainSubmit.stop.source.listener.Google.Big.Query.table","Stop listening")),
                                                         verbatimTextOutput("Source.listener.Google.Big.Query.table.Text")
                                        )
                                      ))
                           ),
                           tabPanel("Load & Save",  
                                    dashboardPage(
                                      dashboardHeader(title = "Load & Save"),
                                      dashboardSidebar(
                                        textInput("ModelName", "Model Name:"),
                                        div(style="display:inline-block",actionButton("ChooseDirButton","Directory",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px")), 
                                        div(style="display:inline-block",actionButton("SaveButton","Save",width='88px',style="background-color: #367fa9;padding-left:4px;padding-right:4px")),
                                        div(id="Margin",textOutput("FoundModelsLabel")),
                                        div(id="Margin",textOutput("LoadSaveInformation")),
                                        div(id="Margin",conditionalPanel("$('html').hasClass('shiny-busy')",  
                                                                         class="busy_loader"))
                                      ),
                                      dashboardBody(
                                        DT::dataTableOutput("ModelToChoose"),
                                        actionButton("LoadButton","Load"),
                                        actionButton("DeleteButton","Delete")
                                      ))
                           )

                ))
                )))
