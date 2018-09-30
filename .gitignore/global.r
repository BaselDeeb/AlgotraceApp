library(shiny)

####
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages)) 
  {
    install.packages(Remaining_Packages, repos='http://cran.us.r-project.org');
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
}
####

Required_Packages_for_ui<-c("shinythemes","shinyjs","shinyBS","shinydashboard","rhandsontable","ggvis",
                            "leaflet","DT","htmltools")
#shinytheme,#useShinyjs,#bsModal,#rhandsontableOutput,#Dashboardpage,#ggvisoutput,#leafletOutput,#DT::dataTableOutput

Install_And_Load(Required_Packages_for_ui)



# Specify the list of required packages to be installed and load    
Required_Packages=c("speedglm","quantreg","ggplot2","RODBC","Matrix","xgboost",
                    "stringr","rio","rpart","readr","caret",
                    "sp","KernSmooth","RPostgreSQL","unbalanced","data.table",
                    "googleAuthR","bigQueryR","ranger","RJDBC","DBI","mltools"); 

###For mxnet
if(!"mxnet" %in% installed.packages()[,"Package"])
install.packages("mxnet",repos="https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/")
library(mxnet) 
############


if(dir.exists("C://AlgoTraceFolder.export") == FALSE)
  dir.create("C://AlgoTraceFolder.export")

if(dir.exists("C://AlgoTraceFolder.save") == FALSE)
  dir.create("C://AlgoTraceFolder.save")

#####
