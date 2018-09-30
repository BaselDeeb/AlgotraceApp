##########Identifying the functions##########################
###I
functions.sql<-paste(###I
                "--Functions",
                "IF OBJECT_ID('dbo.I') IS NOT NULL\n",
                 "DROP FUNCTION I\n",
                 "GO\n",
                 "CREATE FUNCTION [dbo].[I]\n",
                 "(\n",
                 "@x real\n",
                 ")\n",
                 "RETURNS real\n",
                 "AS\n",
                 "BEGIN\n",
                 "return @x\n",
                 "END\n",
                 "GO\n\n",
                ###Cut mean
                "IF OBJECT_ID('dbo.cut_mean') IS NOT NULL\n",
                "DROP FUNCTION cut_mean\n",
                "GO\n",
                "CREATE FUNCTION [dbo].[cut_mean]\n",
                "(\n",
                "@x real, @mean real\n",
                ")\n",
                "RETURNS real\n",
                "AS\n",
                "BEGIN\n",
                "if @x is NULL return NULL\n",
                "DECLARE @ret real;\n",
                "if(@x>@mean)\n",
                "set @ret=1\n",
                "else\n",
                "set @ret=0\n",
                "return @ret\n",
                "END\n",
                "GO\n\n",
                ###Cut 4 pieces
                "IF OBJECT_ID('dbo.cut_4_pieces') IS NOT NULL\n",
                "DROP FUNCTION cut_4_pieces\n",
                "GO\n",
                "CREATE FUNCTION [dbo].[cut_4_pieces]\n",
                "(\n",
                "@x real, @mean real\n",
                ")\n",
                "RETURNS real\n",
                "AS\n",
                "BEGIN\n",
                "DECLARE @ret real;\n",
                "if(@x>@mean)\n",
                "if(@x>1.5*@mean)\n",
                "set @ret=4\n", 
                "else\n",
                "set @ret=3\n",
                "else\n",
                "if(@x>0.5*@mean)\n",
                "set @ret=2\n",
                "else\n",
                "set @ret=1\n",
                "return @ret\n",
                "END\n",
                "GO\n\n",
                ###Scale
                "IF OBJECT_ID('dbo.Scale') IS NOT NULL\n",
                "DROP FUNCTION Scale\n",
                "GO\n",
                "CREATE FUNCTION [dbo].[Scale]\n",
                "(\n",
                "@x real, @mean real, @sd real\n",
                ")\n",
                "RETURNS real\n",
                "AS\n",
                "BEGIN\n",
                "if @x is NULL return NULL\n",
                "if @sd = 0 return @x\n",
                "DECLARE @ret real;\n", 
                "set @ret= (@x - @mean)/@sd\n",
                "return @ret\n",
                "END\n",
                "GO\n\n")
                


sql.fix<-function(str){
  if(grepl("^2",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    var<-unlist(strsplit(temp,"^", fixed = TRUE))[1]
    return(paste0("dbo.I(power(",var,",2))"))  
  }
  if(grepl("^0.3",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("dbo.I(power(ABS(",var,"),0.3))"))
  }
  if(grepl("^0.5",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("dbo.I(power(ABS(",var,"),0.5))"))
  }
  if(grepl("I(log(abs(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(log(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("dbo.I(log(ABS(",var,")+0.001))"))
  }
  if(grepl("/log(abs(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    i<-unlist(strsplit(temp,"/", fixed = TRUE))[1]
    temp<-unlist(strsplit(str,"/log(abs(", fixed = TRUE))[2]
    j<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("dbo.I(",i,"/log(ABS(",j,")+1.001))"))
  }
  if(grepl("I(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("dbo.I(",temp,")"))
  }
  if(grepl("cut.4.pieces(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"cut.4.pieces(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("dbo.cut_4_pieces(",temp,",",paste0("@mean_",temp),")"))
  }
  if(grepl("cut.mean(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"cut.mean(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("dbo.cut_mean(",temp,",",paste0("@mean_",temp),")"))
  }
  if(grepl("Scale(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"Scale(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("dbo.Scale(",temp,",",paste0("@mean_",temp),",",paste0("@sd_",temp),")"))
  }
  if(grepl(":",str,fixed=TRUE)){
    i<-unlist(strsplit(str,":", fixed = TRUE))[1]
    j<-unlist(strsplit(str,":", fixed = TRUE))[2]
    return(paste0(i,"*",j))
  }
  return(str)
}

################################
Find_outliers_sql<-function(var){
  string<-character(0)
  string<-c(string,paste("--outliers_q1\n",
                         "declare", paste0("@outliers_q1_",var),"real;\n",
                         "select", paste0("@outliers_q1_",var),"=(\n",
                         "SELECT MIN(",var,")\n", 
                         "FROM\n",
                         "(\n",
                         "SELECT TOP 2.5 PERCENT",var,"\n",#WITH TIES
                         "FROM algotrace_table where",var,"is not NULL\n",
                         "ORDER BY",var,"DESC\n",
                         ") AS temp)\n\n",
                         "--outliers_q2\n",
                         "declare", paste0("@outliers_q2_",var),"real;\n",
                         "select", paste0("@outliers_q2_",var),"=(\n",
                         "SELECT MIN(",var,")\n", 
                         "FROM\n",
                         "(\n",
                         "SELECT TOP 97.5 PERCENT",var,"\n",  # WITH TIES
                         "FROM algotrace_table where",var,"is not NULL\n",
                         "ORDER BY",var,"DESC\n",
                         ") AS temp)\n\n",
                         "--outliers_median\n",
                         "declare",paste0("@outliers_med1_",var),"real;\n",
                         "select",paste0("@outliers_med1_",var),"=(\n",
                          "SELECT MIN(",var,")\n", 
                           "FROM\n",
                           "(\n",
                             "SELECT TOP 50 PERCENT WITH TIES",var,"\n",
                             "FROM algotrace_table where",var,"is not NULL\n",
                             "ORDER BY",var,"DESC\n",
                           ") AS temp1)\n",
                         "declare", paste0("@outliers_med2_",var),"real;\n",
                         "select", paste0("@outliers_med2_",var),"=(\n",
                           "SELECT MAX(",var,")\n", 
                           "FROM\n",
                           "(\n",
                             "SELECT TOP 50 PERCENT WITH TIES",var,"\n",
                             "FROM algotrace_table where",var,"is not NULL\n",
                             "ORDER BY",var,"\n", 
                           ") AS temp2)\n",
                         "declare",paste0("@outliers_med_",var),"real;\n",
                         "select", paste0("@outliers_med_",var),"=(",paste0("@outliers_med1_",var),"+",paste0("@outliers_med2_",var),")/2\n\n",
                         "--outliers_mean\n",
                         "declare",paste0("@outliers_mean_",var),"real;\n",
                         "select", paste0("@outliers_mean_",var),"=AVG(",var,") from algotrace_table where",var,"is not NULL\n\n",
                         "--outliers_sd\n",
                         "declare",paste0("@outliers_sd_",var),"real;\n",
                         "select", paste0("@outliers_sd_",var),"=STDEV(",var,") from algotrace_table where",var,"is not NULL\n\n",
                         "--outliers_mad\n",
                         "UPDATE algotrace_table\n",
                         "set temp_mad_col_algotrace=ABS(",var,"-",paste0("@outliers_med_",var),")\n",
                         "from algotrace_table\n",
                         "declare",paste0("@outliers_mad1_",var),"real;\n",
                         "select",paste0("@outliers_mad1_",var),"=(\n",
                         "SELECT MIN(temp_mad_col_algotrace)\n", 
                         "FROM\n",
                         "(\n",
                         "SELECT TOP 50 PERCENT WITH TIES temp_mad_col_algotrace\n",
                         "FROM algotrace_table where temp_mad_col_algotrace is not NULL\n",
                         "ORDER BY temp_mad_col_algotrace DESC\n",
                         ") AS temp1)\n",
                         "declare", paste0("@outliers_mad2_",var),"real;\n",
                         "select", paste0("@outliers_mad2_",var),"=(\n",
                         "SELECT MAX(temp_mad_col_algotrace)\n", 
                         "FROM\n",
                         "(\n",
                         "SELECT TOP 50 PERCENT WITH TIES temp_mad_col_algotrace\n",
                         "FROM algotrace_table where temp_mad_col_algotrace is not NULL\n",
                         "ORDER BY temp_mad_col_algotrace\n", 
                         ") AS temp2)\n",
                         "declare",paste0("@outliers_mad_",var),"real;\n",
                         "select", paste0("@outliers_mad_",var),"=1.4826 *(",paste0("@outliers_mad1_",var),"+",paste0("@outliers_mad2_",var),")/2\n\n",
                         "--UPDATE Table\n",
                         "--condition 1\n",
                         "UPDATE algotrace_table\n",
                         "SET cond1_algotrace=\n",
                         "CASE\n",
                         "when",var,"is NULL then NULL\n",
                         "when",var,">",paste0("@outliers_q1_",var),"then 1\n",
                         "when",var,"<",paste0("@outliers_q2_",var),"then 1\n",
                         "when",var,"<=",paste0("@outliers_q1_",var),"AND",var,">=",paste0("@outliers_q2_",var),"then 0\n",
                         "END\n",
                         "--condition 2\n",
                         "UPDATE algotrace_table\n",
                         "SET cond2_algotrace=\n",
                         "CASE\n",
                         "when",var,"is NULL then NULL\n",
                         "when ABS(",var,"-",paste0("@outliers_med_",var),")/(",paste0("@outliers_mad_",var),"+0.001)>2 then 1\n",
                         "when ABS(",var,"-",paste0("@outliers_med_",var),")/(",paste0("@outliers_mad_",var),"+0.001)<=2 then 0\n",
                         "END\n",
                         "--condition 3\n",
                         "UPDATE algotrace_table\n",
                         "SET cond3_algotrace=\n",
                         "CASE\n",
                         "when",var,"is NULL then NULL\n",
                         "when ABS(",var,"-",paste0("@outliers_mean_",var),")/(",paste0("@outliers_sd_",var),"+0.001)>3 then 1\n",
                         "when ABS(",var,"-",paste0("@outliers_mean_",var),")/(",paste0("@outliers_sd_",var),"+0.001)<=3 then 0\n",
                         "END\n",
                         "UPDATE algotrace_table\n",
                         "SET sum_cond_algotrace = cond1_algotrace+cond2_algotrace+cond3_algotrace\n",
                         "UPDATE algotrace_table\n",
                         "SET",var,"=\n",
                         "CASE\n",
                         "when",var,"is NULL then NULL\n",
                         "when sum_cond_algotrace>1 then", paste0("@outliers_med_",var),"\n",
                         "when sum_cond_algotrace<=1 then",var,"\n",
                         "END\n"
                         ))
  return(string)
}

######Convert Logistic to sql rules###############
sql.rules.Logistic<-function(model,vars,vars_not.fix.outliers,vars_not.fix,method,levels.table,Imputation.Value){
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.sql)
  
  #############################################################
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                               "Go\n\n"))
  #Copy source_table 
  rules_out<-c(rules_out,paste("IF OBJECT_ID('algotrace_table') IS NOT NULL\n",     
                              "DROP TABLE algotrace_table\n",
                              "Go\n"))
  rules_out<-c(rules_out,paste("select *\n",
                                "into algotrace_table\n",
                               "from source_table\n\n"))
  #add prediction_algotrace column to the algotrace_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','prediction_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add prediction_algotrace float\n",
                               "Go\n\n"))
  #add cond1_algotrace cond2_algotrace cond3_algotrace
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond1_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond1_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond2_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond2_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond3_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond3_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','sum_cond_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add sum_cond_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','temp_mad_col_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add temp_mad_col_algotrace float\n",
                               "Go\n\n"))
  
  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var,"=\n",
                                 "(case\n",
                                 "When",var,"='NA' or ",var,"='NULL' Then NULL\n"))
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(var,"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))
    rules_out<-c(rules_out,paste(
      "else",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END)\n"))
    
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))
  }
  

  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var," =NULL where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf';\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))

  for(var in vars)
    rules_out<-c(rules_out,
                 "DECLARE",paste0("@mean_",var),"real;\n",
                 "select",paste0("@mean_",var),"= AVG(",var,") from algotrace_table\n",
                 "DECLARE",paste0("@sd_",var),"real;\n",
                 "select",paste0("@sd_",var),"= STDEV(",var,") from algotrace_table\n\n")
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,
                   "DECLARE",paste0("@median1_",var),"real;\n",
                   "select",paste0("@median1_",var),"=(\n", 
                   "SELECT MAX(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,") AS temp1)\n",
                   "DECLARE",paste0("@median2_",var),"real;\n",
                   "select",paste0("@median2_",var),"=(\n", 
                   "SELECT MIN(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,"DESC) AS temp2)\n",
                   "DECLARE",paste0("@median_",var),"real;\n",
                   "select",paste0("@median_",var),"=(", paste0("@median1_",var),"+",paste0("@median2_",var),")/2\n\n") 
    
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                   "\nSET",var,"= \n",
                                   "CASE WHEN", var, "is NULL THEN", paste0("@median_",var),"\n",
                                   "ELSE",var,"END\n\n"))
  } else{ #Imputation.Value=="Mean"
  for(var in vars[!vars %in% vars_not.fix])
    rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                 "\nSET",var,"= \n",
                                 "CASE WHEN", var, "is NULL THEN", paste0("@mean_",var),"\n",
                                 "ELSE",var,"END\n\n"))
  }
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
  rules_out<-c(rules_out,Find_outliers_sql(var))
  
  
  ######The Formula#########
  coef.glm<-model$coefficients
  coef<-coef.glm[-1][!is.na(coef.glm[-1])]
  coef_names<-sapply(names(coef),function(x){sql.fix(x)})
  names(coef_names)<-NULL
  
  if(method=="Classification")
    Myform<-paste("1/(1+EXP(-(",paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+"),")))") 
  if(method=="Estimation")
    Myform<-paste(paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+")) 
  ##########################
  rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                               "\nSET prediction_algotrace = \n",
                               Myform,"\n\n"))
  ##add prediction_algotrace to source_table and drop algotrace_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','prediction_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add prediction_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("UPDATE source_table\n",
                               "Set prediction_algotrace=(select prediction_algotrace\n",
                               "from algotrace_table\n",
                               "where algotrace_table.id_algotrace=source_table.id_algotrace)\n\n"))
  rules_out<-c(rules_out,paste("drop table algotrace_table\n\n"))
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste("UPDATE source_table",
                                 "\nSET prediction_algotrace = NULL\n",
                                  "where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf'\n\n"))

  rules_out<-c(rules_out,paste("\nIF COL_LENGTH('source_table','id_algotrace') IS not NULL\n",
                               "ALTER TABLE source_table Drop COLUMN id_algotrace\n",
                               "Go\n\n"))
  
  sql_out <- paste(rules_out, collapse=" ")
  sql_out
}
######Convert Logistic to sql rules###############
sql.rules.Negative.Binomial<-function(model,vars,vars_not.fix.outliers,vars_not.fix,method,levels.table,Imputation.Value){
  
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  
  
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.sql)
  
  #############################################################
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                               "Go\n\n"))
  #Copy source_table 
  rules_out<-c(rules_out,paste("IF OBJECT_ID('algotrace_table') IS NOT NULL\n",     
                               "DROP TABLE algotrace_table\n",
                               "Go\n"))
  rules_out<-c(rules_out,paste("select *\n",
                               "into algotrace_table\n",
                               "from source_table\n\n"))
  #add prediction_algotrace column to the algotrace_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','prediction_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add prediction_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond1_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond1_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond2_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond2_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond3_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond3_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','sum_cond_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add sum_cond_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','temp_mad_col_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add temp_mad_col_algotrace float\n",
                               "Go\n\n"))
  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var,"=\n",
                                 "(case\n",
                                 "When",var,"='NA' or ",var,"='NULL' Then NULL\n"))
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(var,"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))
    rules_out<-c(rules_out,paste(
      "else",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END)\n"))
    
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))
  }
  
  
  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var," =NULL where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf';\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))
  
  
  for(var in vars)
    rules_out<-c(rules_out,
                 "DECLARE",paste0("@mean_",var),"real;\n",
                 "select",paste0("@mean_",var),"= AVG(",var,") from algotrace_table\n",
                 "DECLARE",paste0("@sd_",var),"real;\n",
                 "select",paste0("@sd_",var),"= STDEV(",var,") from algotrace_table\n\n")
  
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,
                   "DECLARE",paste0("@median1_",var),"real;\n",
                   "select",paste0("@median1_",var),"=(\n", 
                   "SELECT MAX(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,") AS temp1)\n",
                   "DECLARE",paste0("@median2_",var),"real;\n",
                   "select",paste0("@median2_",var),"=(\n", 
                   "SELECT MIN(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,"DESC) AS temp2)\n",
                   "DECLARE",paste0("@median_",var),"real;\n",
                   "select",paste0("@median_",var),"=(", paste0("@median1_",var),"+",paste0("@median2_",var),")/2\n\n") 
    
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                   "\nSET",var,"= \n",
                                   "CASE WHEN", var, "is NULL THEN", paste0("@median_",var),"\n",
                                   "ELSE",var,"END\n\n"))
  } else{ #Imputation.Value=="Mean"
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                   "\nSET",var,"= \n",
                                   "CASE WHEN", var, "is NULL THEN", paste0("@mean_",var),"\n",
                                   "ELSE",var,"END\n\n"))
  }
  
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
    rules_out<-c(rules_out,Find_outliers_sql(var))
  
 
  
  ######The Formula#########
  coef.glm<-model$coefficients
  coef<-coef.glm[-1][!is.na(coef.glm[-1])]
  coef_names<-sapply(names(coef),function(x){sql.fix(x)})
  names(coef_names)<-NULL
  
   if(method=="Estimation")
    Myform<-paste("EXP(",paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+"),")") 
  ##########################
  rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                               "\nSET prediction_algotrace = \n",
                               Myform,"\n\n"))
  ##add prediction_algotrace to source_table and drop algotrace_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','prediction_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add prediction_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("UPDATE source_table\n",
                               "Set prediction_algotrace=(select prediction_algotrace\n",
                               "from algotrace_table\n",
                               "where algotrace_table.id_algotrace=source_table.id_algotrace)\n\n"))
  rules_out<-c(rules_out,paste("drop table algotrace_table\n\n"))
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste("UPDATE source_table",
                                 "\nSET prediction_algotrace = NULL\n",
                                 "where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf'\n\n"))

  rules_out<-c(rules_out,paste("\nIF COL_LENGTH('source_table','id_algotrace') IS not NULL\n",
                               "ALTER TABLE source_table Drop COLUMN id_algotrace\n",
                               "Go\n\n"))
  
  sql_out <- paste(rules_out, collapse=" ")
  sql_out
}

#####Convert Rpart model to sql rules#############
#https://gist.github.com/tomasgreif/6038822
sql_parse_tree <- function (df=NULL,model=NULL,vars,
                        vars_not.fix.outliers,vars_not.fix,levels.table,Imputation.Value) {
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.sql)
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','id_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add id_algotrace INT IDENTITY(1,1)\n",
                               "Go\n\n"))
  #Copy source_table 
  rules_out<-c(rules_out,paste("IF OBJECT_ID('algotrace_table') IS NOT NULL\n",     
                               "DROP TABLE algotrace_table\n",
                               "Go\n"))
  rules_out<-c(rules_out,paste("select *\n",
                               "into algotrace_table\n",
                               "from source_table\n\n"))
  
  #############################################################
  #add prediction_algotrace column to the algotrace_table
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','prediction_algotrace') IS NULL\n"))
  rules_out<-c(rules_out,paste("ALTER TABLE algotrace_table add prediction_algotrace float\n",
                               "Go\n\n")) 
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond1_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond1_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond2_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond2_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','cond3_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add cond3_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','sum_cond_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add sum_cond_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("IF COL_LENGTH('algotrace_table','temp_mad_col_algotrace') IS NULL\n",
                               "ALTER TABLE algotrace_table add temp_mad_col_algotrace float\n",
                               "Go\n\n"))
  
  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var,"=\n",
                                 "(case\n",
                                 "When",var,"='NA' or ",var,"='NULL' Then NULL\n"))
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(var,"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))
    rules_out<-c(rules_out,paste(
      "else",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END)\n"))
    
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))
  }
  
  
  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste("if EXISTS (select DATA_TYPE from INFORMATION_SCHEMA.COLUMNS IC where TABLE_NAME = 'algotrace_table' and COLUMN_NAME = ",paste0("'",var,"'"),"and  DATA_TYPE <> 'float')\n",
                                 "UPDATE algotrace_table SET", var," =NULL where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf';\n",
                                 "ALTER TABLE algotrace_table ALTER COLUMN", var,"float\n",
                                 "Go\n"))
  
  for(var in vars)
    rules_out<-c(rules_out,
                 "DECLARE",paste0("@mean_",var),"real;\n",
                 "select",paste0("@mean_",var),"= AVG(",var,") from algotrace_table\n",
                 "DECLARE",paste0("@sd_",var),"real;\n",
                 "select",paste0("@sd_",var),"= STDEV(",var,") from algotrace_table\n\n")
  
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,
                   "DECLARE",paste0("@median1_",var),"real;\n",
                   "select",paste0("@median1_",var),"=(\n", 
                   "SELECT MAX(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,") AS temp1)\n",
                   "DECLARE",paste0("@median2_",var),"real;\n",
                   "select",paste0("@median2_",var),"=(\n", 
                   "SELECT MIN(",var,") FROM\n",
                   "(SELECT TOP 50 PERCENT WITH TIES",var,"\n", 
                   "FROM algotrace_table where",var,"is not NULL\n",
                   "ORDER BY", var,"DESC) AS temp2)\n",
                   "DECLARE",paste0("@median_",var),"real;\n",
                   "select",paste0("@median_",var),"=(", paste0("@median1_",var),"+",paste0("@median2_",var),")/2\n\n") 
    
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                   "\nSET",var,"= \n",
                                   "CASE WHEN", var, "is NULL THEN", paste0("@median_",var),"\n",
                                   "ELSE",var,"END\n\n"))
  } else{ #Imputation.Value=="Mean"
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste("\nUPDATE algotrace_table",
                                   "\nSET",var,"= \n",
                                   "CASE WHEN", var, "is NULL THEN", paste0("@mean_",var),"\n",
                                   "ELSE",var,"END\n\n"))
  }
  
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
    rules_out<-c(rules_out,Find_outliers_sql(var))
  

  log <- capture.output({
    rpart.rules <- path.rpart(model,rownames(model$frame)[model$frame$var=="<leaf>"])
  }) 
  rules_out <- c(rules_out,"\nUPDATE algotrace_table\n")
  if(length(rpart.rules)>1){
    rules_out <- c(rules_out,"SET prediction_algotrace = case\n")
    
    args <- c("<=",">=","<",">","=")
    
    i <- 1
    
    for (rule in rpart.rules) {  
      rule_out <- character(0)
      for (component in rule) {
        sep <- lapply(args, function(x) length(unlist(strsplit(component,x)))) > 1
        elements <- unlist(strsplit(component,(args[sep])[1]))
        if(!(elements[1]=="root")) {
          if (is.numeric(eval(parse(text=elements[1]), df))) {
            rule_out <- c(rule_out,paste(sql.fix(elements[1]),(args[sep])[1],elements[2]))
          } else {
            rule_out <- c(rule_out,paste0(sql.fix(elements[1])," in (",paste0("'",unlist(strsplit(elements[2],",")),"'",collapse=","),")"))
          }
        }
      }
      rules_out <- c(rules_out, paste0("\nwhen ", paste(rule_out,collapse=" AND ")," then 'node_" ,names(rpart.rules)[i],"'"))
      if(i==length(rpart.rules)) rules_out <- c(rules_out,"\nend")
      i <- i +1
    }} else{ #only one path
      rules_out <- c(rules_out,paste0("SET prediction_algotrace = 'node_",names(rpart.rules)[1],"'\n"))
    }
 
  ##add prediction_algotrace to source_table and drop algotrace_table
  rules_out<-c(rules_out,paste("\nIF COL_LENGTH('source_table','prediction_algotrace') IS NULL\n",
                               "ALTER TABLE source_table add prediction_algotrace float\n",
                               "Go\n\n"))
  rules_out<-c(rules_out,paste("UPDATE source_table\n",
                               "Set prediction_algotrace=(select prediction_algotrace\n",
                               "from algotrace_table\n",
                               "where algotrace_table.id_algotrace=source_table.id_algotrace)\n\n"))
  rules_out<-c(rules_out,paste("drop table algotrace_table\n\n"))
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste("UPDATE source_table",
                                 "\nSET prediction_algotrace = NULL\n",
                                 "where", var,"='NA' or ",var,"='NaN' or ",var,"='NULL' or ",var," ='Inf' or ",var," ='-Inf'\n\n"))

  rules_out<-c(rules_out,paste("IF COL_LENGTH('source_table','id_algotrace') IS not NULL\n",
                               "ALTER TABLE source_table Drop COLUMN id_algotrace\n",
                               "Go\n\n"))
  
  sql_out <- paste(rules_out, collapse=" ")
  return(sql_out)
}

replace.node<-function(word,model,method){
  if(method=="Estimation")
    nodes.val<-summary(model)$frame$yval
  if(method=="Classification")
    nodes.val<-summary(model)$frame$yval2[,5]
  nodes.val<-nodes.val
  temp<-strsplit(word,"_")[[1]][2]
  temp<-gsub("'","",temp)
  node.num<-as.numeric(temp)
  all.nodes.num<-as.numeric(row.names(summary(model)$frame))
  node.place<-which(all.nodes.num==node.num)
  return(nodes.val[node.place])
}
replace<-function(old.rules,model,method){
  temp1<-old.rules#gsub("'","",old.rules)
  temp2<-strsplit(temp1," ")[[1]]
  text.as.words<-temp2[temp2!=""]
  node.words<-text.as.words[grep("^'node_", text.as.words)]
  node.val<-sapply(node.words,function(x){replace.node(x,model,method)})
  text.as.words[grep("^'node_", text.as.words)]<-node.val
  new.rules<-paste(text.as.words,collapse=" ")
  return(new.rules)
}
