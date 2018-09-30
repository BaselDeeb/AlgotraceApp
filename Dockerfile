## install R package dependencies (and clean up)
RUN apt-get update && apt-get install -y gnupg2 \
    libssl-dev \
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
## install packages from CRAN (and clean up)
RUN Rscript -e "install.packages(c("devtools","shinythemes","shinyjs","shinyBS","shinydashboard","rhandsontable","ggvis",
  "leaflet","DT","htmltools","speedglm","quantreg","ggplot2","RODBC","Matrix","xgboost",
  "stringr","rio","rpart","readr","caret",
  "sp","KernSmooth","RPostgreSQL","unbalanced","data.table",
  "googleAuthR","bigQueryR","ranger","RJDBC","DBI","mltools"), repos='https://cran.rstudio.com/')" \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages("mxnet",repos="https://apache-mxnet.s3-accelerate.dualstack.amazonaws.com/R/CRAN/")"\
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


## assume shiny app is in build folder /.gitignore
COPY ./.gitignore /srv/shiny-server/Algotrace/
