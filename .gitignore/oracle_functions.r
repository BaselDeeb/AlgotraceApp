##########Identifying the functions.oracle##########################
functions.oracle<-paste(
  "--Functions\n",
  ###I
  "CREATE OR REPLACE FUNCTION I(x IN real)\n",
  "RETURN REAL IS\n",
  "BEGIN\n",
  "RETURN round(x,15);\n",
  "END I;\n",
  "/\n",
  ###Cut mean
  "CREATE OR REPLACE FUNCTION cut_mean(arg1 IN real, arg2 IN real)\n",
  "RETURN NUMBER AS\n",
  "x real;\n",
  "mean real;\n",
  "BEGIN\n",
  "x := round (arg1,15);\n",
  "mean := round (arg2,15);\n",
  "if x is NULL THEN return NULL;\n",
  "END IF;\n",
  "if (x>mean) THEN return 1;\n",
  "else return 0;\n",
  "END IF;\n",
  "END cut_mean;\n",
  "/\n",
  ###Cut 4 pieces
  "CREATE OR REPLACE FUNCTION cut_4_pieces (arg1 in real, arg2 in real)\n",
  "RETURN real AS\n",
  "x real;\n",
  "mean real;\n",
  "ret real;\n",
  "BEGIN\n",
  "x := round (arg1,15);\n",
  "mean := round (arg2,15);\n",
  "if(x>mean) then\n",
  "if(x>1.5*mean) then ret:=4;\n",
  "else ret:=3;\n",
  "end if;\n",
    "else\n",
        "if(x>0.5*mean) then ret:=2;\n",
  "else ret:=1;\n",
  "end if;\n",
  "end if;\n",
  "return ret;\n",
  "END cut_4_pieces;\n",
  "/\n",
  ###Scale
  "CREATE OR REPLACE FUNCTION Scale  (arg1 real, arg2 real, arg3 real )\n",
  "RETURN REAL AS\n",
  "x real;\n",
  "mean real;\n",
  "sd real;\n",
  "BEGIN\n",
  "x := round (arg1,15);\n",
  "mean := round (arg2,15);\n",
  "sd := round (arg3,15);\n",
  "IF x IS NULL THEN RETURN NULL;\n",
  "END IF;\n",
  "IF sd = 0 THEN RETURN x;\n",
  "END IF;\n",
  "RETURN ((x - mean)/sd);\n",
  "END Scale;\n",
  "/\n")


oracle.fix<-function(str){
  if(grepl("^2",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    var<-unlist(strsplit(temp,"^", fixed = TRUE))[1]
    return(paste0("I(power(",var,",2))"))  
  }
  if(grepl("^0.3",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("I(power(ABS(",var,"),0.3))"))
  }
  if(grepl("^0.5",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("I(power(ABS(",var,"),0.5))"))
  }
  if(grepl("I(log(abs(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(log(abs(", fixed = TRUE))[2]
    var<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("I(log(exp(1),ABS(",var,")+0.001))"))
  }
  if(grepl("/log(abs(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    i<-unlist(strsplit(temp,"/", fixed = TRUE))[1]
    temp<-unlist(strsplit(str,"/log(abs(", fixed = TRUE))[2]
    j<-unlist(strsplit(temp,")", fixed = TRUE))[1]
    return(paste0("I(",i,"/log(exp(1),ABS(",j,")+1.001))"))
  }
  if(grepl("I(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"I(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("I(",temp,")"))
  }
  if(grepl("cut.4.pieces(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"cut.4.pieces(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("cut_4_pieces(",temp,",",paste0("mean_",temp),")"))
  }
  if(grepl("cut.mean(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"cut.mean(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("cut_mean(",temp,",",paste0("mean_",temp),")"))
  }
  if(grepl("Scale(",str,fixed=TRUE)){
    temp<-unlist(strsplit(str,"Scale(", fixed = TRUE))[2]
    temp<-gsub("[)]","",temp)
    return(paste0("Scale(",temp,",",paste0("mean_",temp),",",paste0("sd_",temp),")"))
  }
  if(grepl(":",str,fixed=TRUE)){
    i<-unlist(strsplit(str,":", fixed = TRUE))[1]
    j<-unlist(strsplit(str,":", fixed = TRUE))[2]
    return(paste0(i,"*",j))
  }
  return(str)
}

Find_outliers_oracle<-function(var){
  string<-character(0)
  string<-c(string,paste(
  "DECLARE\n",
  paste0("outliers_q1_",var),"REAL;\n",
  paste0("outliers_q2_",var),"REAL;\n",
  paste0("outliers_med_",var),"REAL;\n",
  paste0("outliers_mean_",var),"REAL;\n",
  paste0("outliers_sd_",var),"REAL;\n",
  paste0("outliers_mad_",var),"REAL;\n",
  "BEGIN\n",
  "SELECT PERCENTILE_CONT(0.025) WITHIN GROUP (ORDER BY",var,"DESC) INTO",paste0("outliers_q1_",var),"FROM algotrace_table;\n",
  "SELECT PERCENTILE_CONT(0.975) WITHIN GROUP (ORDER BY",var,"DESC) INTO",paste0("outliers_q2_",var),"FROM algotrace_table;\n",   
  "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY",var,"DESC) INTO", paste0("outliers_med_",var),"FROM algotrace_table;\n",
  "SELECT",paste0("AVG(",var,")"),"INTO",paste0("outliers_mean_",var),"FROM algotrace_table;\n",
  "SELECT",paste0("STDDEV(",var,")"),"INTO",paste0("outliers_sd_",var),"FROM algotrace_table;\n",
  "UPDATE algotrace_table SET temp_mad_col_algotrace=ABS(",var,"-",paste0("outliers_med_",var),");\n",
  "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY temp_mad_col_algotrace DESC) INTO",paste0("outliers_mad_",var),"FROM algotrace_table;\n",
  paste0("outliers_mad_",var),":=1.4826 *",paste0("outliers_mad_",var),";\n",
  "--UPDATE Table\n",
  "--condition 1\n",
  "UPDATE algotrace_table SET cond1_algotrace =\n", 
  "CASE\n",  
  "WHEN",var,"= NULL THEN NULL\n",
  "WHEN",var,"<", paste0("outliers_q2_",var),"THEN 1\n",
  "WHEN",var,">", paste0("outliers_q1_",var),"THEN 1\n",
  "ELSE 0\n",
  "END;\n",
  "--condition 2\n",
  "UPDATE algotrace_table\n",
  "SET cond2_algotrace=\n",
  "CASE\n",
  "WHEN",var," = NULL THEN NULL\n",
  "WHEN ABS(",var," - ",paste0("outliers_med_",var),")/(",paste0("outliers_mad_",var)," +0.001)>2 THEN 1\n",
  "ELSE 0\n",
  "END;\n",
  "--condition 3\n",
  "UPDATE algotrace_table\n",
  "SET cond3_algotrace=\n",
  "CASE\n",
  "WHEN",var," = NULL THEN NULL\n",
  "WHEN ABS(",var," - ",paste0("outliers_mean_",var),")/(",paste0("outliers_sd_",var),"+0.001)>3 THEN 1\n",
  "ELSE 0\n",
  "END;\n",
  "--sum up the condirtions\n",
  "UPDATE algotrace_table\n",
  "SET sum_cond_algotrace = cond1_algotrace+cond2_algotrace+cond3_algotrace;\n",
  "--replace the median of the  column on the values corresponding with 2 conditions at least\n",
  "UPDATE algotrace_table\n",
  "SET",var,"=\n",
  "CASE\n",
  "WHEN",var,"= NULL THEN NULL\n",
  "WHEN sum_cond_algotrace>1 THEN",paste0("outliers_med_",var),"\n",
  "ELSE",var,"\n", 
  "END;\n",
  "END;\n"))
  return(string)
}


###################################################################
######Convert Logistic to oracle rules###############
oracle.rules.Logistic<-function(model,vars,vars_not.fix.outliers,vars_not.fix,method,levels.table,Imputation.Value){
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.oracle)
  
  #############################################################
  rules_out<-c(rules_out,paste(
    "DECLARE\n",
    "not_exist EXCEPTION;\n",
    "PRAGMA EXCEPTION_INIT( not_exist,-942); --a directive to initiate the exception we wish to catch\n",
    "v_PredictionAlgotrace_exists number := 0;\n", 
    "v_Cond1Algotrace_exists number := 0;\n",  
    "v_Cond2Algotrace_exists number := 0;\n",  
    "v_Cond3Algotrace_exists number := 0;\n",  
    "v_SumCondAlgotrace_exists number := 0;\n",  
    "v_TempMadColAlgotrace_exists number := 0;\n",  
    "BEGIN\n\n"))
  
  #Create algotrace_table
  rules_out<-c(rules_out,paste(
    "BEGIN\n",
    "EXECUTE IMMEDIATE 'DROP TABLE algotrace_table';\n",
    "EXCEPTION WHEN not_exist THEN NULL;\n",
    "END;\n\n"))
  
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("EXECUTE IMMEDIATE 'ALTER TABLE source_table ADD (id_algotrace  NUMBER(10))';\n",
                               "EXECUTE IMMEDIATE 'UPDATE source_table SET id_algotrace =ROWNUM';\n",
                               "EXECUTE IMMEDIATE 'CREATE TABLE algotrace_table AS SELECT * FROM SOURCE_TABLE';\n",
                               "\n\n"))
  ##add prediction_algotrace column to algotrace_table
  rules_out<-c(rules_out,paste(
  "Select count(*) into v_PredictionAlgotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'prediction_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_PredictionAlgotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (prediction_algotrace float)';\n",
  "end if;\n\n",
  ##add cond1Algotrace column to algotrace_table
  "Select count(*) into v_Cond1Algotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'cond1_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_Cond1Algotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (cond1_algotrace float)';\n",
  "end if;\n\n",
  ##add cond2lgotrace column to algotrace_table
  "Select count(*) into v_Cond2Algotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'cond2_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_Cond2Algotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (cond2_algotrace float)';\n",
  "end if;\n\n",
  ##add cond3Algotrace column to algotrace_table
  "Select count(*) into v_Cond3Algotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'cond3_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_Cond3Algotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (cond3_algotrace float)';\n",
  "end if;\n\n",
  ##add SumCondAlgotrace column to algotrace_table
  "Select count(*) into v_SumCondAlgotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'sum_cond_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_SumCondAlgotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (sum_cond_algotrace float)';\n",
  "end if;\n\n",
  ##add TempMadColAlgotrace column to algotrace_table
  "Select count(*) into v_TempMadColAlgotrace_exists\n",
  "from user_tab_columns\n",
  "where column_name = 'temp_mad_col_algotrace'\n",
  "and table_name = 'algotrace_table';\n",
  "if (v_TempMadColAlgotrace_exists = 0) then\n",
  "execute immediate 'alter table algotrace_table add (temp_mad_col_algotrace float)';\n",
  "end if;\n",
  "END;\n",
  "/\n\n"))
  

  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste(
   "ALTER TABLE ALGOTRACE_TABLE\n",
   "RENAME COLUMN",var, "TO", paste0(var,"_temp"),";\n",
   "ALTER TABLE ALGOTRACE_TABLE\n",
   "ADD",var,"FLOAT;\n",
   "UPDATE ALGOTRACE_TABLE\n",
    "SET",var,"= CASE\n"))
    
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    rules_out<-c(rules_out,paste(
      "When",paste0(var,"_temp"),"is null then null\n"
    ))
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(paste0(var,"_temp"),"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))

    rules_out<-c(rules_out,paste(
      "ELSE",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END;\n"))
    
    rules_out<-c(rules_out,paste(
    "ALTER TABLE ALGOTRACE_TABLE\n",
    "DROP COLUMN",paste0(var,"_temp"),";\n"))
  }
  
  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste(
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NA', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NaN', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NULL', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'Inf', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", '-Inf', NULL);\n",
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "RENAME COLUMN",var,"TO",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "ADD",var,"FLOAT;\n",
      
      "UPDATE ALGOTRACE_TABLE\n",
      "SET",var,"=",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "DROP COLUMN", paste0(var,"_temp"),";\n\n"
    ))
  
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("PERCENTILE_CONT(",var,",0.5)"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n")) 
  } else{ #Mean
    for(var in vars)
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("AVG(",var,")"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n"))
  }
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
    rules_out<-c(rules_out,
                 Find_outliers_oracle(var),"/\n\n")
                 
  
  
  ######The Formula#########
  coef.glm<-model$coefficients
  coef<-coef.glm[-1][!is.na(coef.glm[-1])]
  coef_names<-sapply(names(coef),function(x){oracle.fix(x)})
  names(coef_names)<-NULL
  
  if(method=="Classification")
    Myform<-paste("1/(1+EXP(-(",paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+"),")))") 
  if(method=="Estimation")
    Myform<-paste(paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+")) 
  ##########################
  rules_out<-c(rules_out,paste("DECLARE\n",
    "v_PredictionAlgotrace_exists NUMBER :=0;\n"))
  for(var in vars){
    rules_out<-c(rules_out,paste(
      paste0("mean_",var),"REAL;\n",
      paste0("sd_",var),"REAL;\n"))
  }
    
     rules_out<-c(rules_out,paste("BEGIN\n"))
     for(var in vars){
       rules_out<-c(rules_out,paste(
         "SELECT",paste0("AVG(",var,")"),"INTO",paste0("mean_",var),"FROM algotrace_table;\n",
         "SELECT",paste0("STDDEV(",var,")"),"INTO",paste0("sd_",var),"FROM algotrace_table;\n"))
     }  
     rules_out<-c(rules_out,paste(
    "UPDATE algotrace_table\n",
    "SET prediction_algotrace =\n",
      Myform,";\n\n"))
  
  
  rules_out<-c(rules_out,paste(
    "Select count(*) into v_PredictionAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'prediction_algotrace' and table_name = 'source_table';\n",
    "if (v_PredictionAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table source_table add (prediction_algotrace float)';\n",
    "end if;\n",
    "END;\n", 
    "/\n\n"))
  ######

  rules_out<-c(rules_out,paste(
    "--now we match the predictions by the ID of the row in the temporary table and then discard it\n",
    "DECLARE\n",
    "v_id_algotrace_exists NUMBER;\n",
    "BEGIN\n",
    "UPDATE source_table\n",
    "SET prediction_algotrace=\n",
    "(select prediction_algotrace FROM algotrace_table\n",
    "WHERE algotrace_table.id_algotrace=source_table.id_algotrace);\n",
    "execute immediate 'DROP TABLE algotrace_table';\n",
    "Select count(*) into v_id_algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'PREDICTION_ALGOTRACE' and table_name = 'SOURCE_TABLE';\n",
    "if (v_id_algotrace_exists = 1) then\n",
    "execute immediate 'ALTER TABLE source_table DROP COLUMN id_algotrace';\n",
    "end if;\n"))
  
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste(
      "UPDATE source_table\n",
      "SET prediction_algotrace= NULL\n",
      "where",var,"is null;\n"
    ))
  
  rules_out<-c(rules_out,paste("END;\n"))
    

  oracle_out <- paste(rules_out, collapse=" ")
  return(oracle_out)
}


oracle.rules.Negative.Binomial<-function(model,vars,vars_not.fix.outliers,vars_not.fix,method,levels.table,Imputation.Value){
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.oracle)
  
  #############################################################
  rules_out<-c(rules_out,paste(
    "DECLARE\n",
    "not_exist EXCEPTION;\n",
    "PRAGMA EXCEPTION_INIT( not_exist,-942); --a directive to initiate the exception we wish to catch\n",
    "v_PredictionAlgotrace_exists number := 0;\n", 
    "v_Cond1Algotrace_exists number := 0;\n",  
    "v_Cond2Algotrace_exists number := 0;\n",  
    "v_Cond3Algotrace_exists number := 0;\n",  
    "v_SumCondAlgotrace_exists number := 0;\n",  
    "v_TempMadColAlgotrace_exists number := 0;\n",  
    "BEGIN\n\n"))
  
  #Create algotrace_table
  rules_out<-c(rules_out,paste(
    "BEGIN\n",
    "EXECUTE IMMEDIATE 'DROP TABLE algotrace_table';\n",
    "EXCEPTION WHEN not_exist THEN NULL;\n",
    "END;\n\n"))
  
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("EXECUTE IMMEDIATE 'ALTER TABLE source_table ADD (id_algotrace  NUMBER(10))';\n",
                               "EXECUTE IMMEDIATE 'UPDATE source_table SET id_algotrace =ROWNUM';\n",
                               "EXECUTE IMMEDIATE 'CREATE TABLE algotrace_table AS SELECT * FROM SOURCE_TABLE';\n",
                               "\n\n"))
  ##add prediction_algotrace column to algotrace_table
  rules_out<-c(rules_out,paste(
    "Select count(*) into v_PredictionAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'prediction_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_PredictionAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (prediction_algotrace float)';\n",
    "end if;\n\n",
    ##add cond1Algotrace column to algotrace_table
    "Select count(*) into v_Cond1Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond1_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond1Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond1_algotrace float)';\n",
    "end if;\n\n",
    ##add cond2lgotrace column to algotrace_table
    "Select count(*) into v_Cond2Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond2_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond2Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond2_algotrace float)';\n",
    "end if;\n\n",
    ##add cond3Algotrace column to algotrace_table
    "Select count(*) into v_Cond3Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond3_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond3Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond3_algotrace float)';\n",
    "end if;\n\n",
    ##add SumCondAlgotrace column to algotrace_table
    "Select count(*) into v_SumCondAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'sum_cond_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_SumCondAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (sum_cond_algotrace float)';\n",
    "end if;\n\n",
    ##add TempMadColAlgotrace column to algotrace_table
    "Select count(*) into v_TempMadColAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'temp_mad_col_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_TempMadColAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (temp_mad_col_algotrace float)';\n",
    "end if;\n",
    "END;\n",
    "/\n\n"))
  
  
  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste( 
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "RENAME COLUMN",var, "TO", paste0(var,"_temp"),";\n",
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "ADD",var,"FLOAT;\n",
      "UPDATE ALGOTRACE_TABLE\n",
      "SET",var,"= CASE\n")) 
    
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    rules_out<-c(rules_out,paste(
      "When",paste0(var,"_temp"),"is null then null\n"
    ))
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(paste0(var,"_temp"),"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))
    
    rules_out<-c(rules_out,paste(
      "ELSE",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END;\n"))
    
    rules_out<-c(rules_out,paste(
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "DROP COLUMN",paste0(var,"_temp"),";\n"))
  }
  
  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste(
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NA', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NaN', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NULL', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'Inf', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", '-Inf', NULL);\n",
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "RENAME COLUMN",var,"TO",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "ADD",var,"FLOAT;\n",
      
      "UPDATE ALGOTRACE_TABLE\n",
      "SET",var,"=",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "DROP COLUMN", paste0(var,"_temp"),";\n\n"
    ))
  
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("PERCENTILE_CONT(",var,",0.5)"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n")) 
  } else{ #Mean
    for(var in vars)
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("AVG(",var,")"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n"))
  }
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
    rules_out<-c(rules_out,
                 Find_outliers_oracle(var),"/\n\n")
  
  
  
  ######The Formula#########
  coef.glm<-model$coefficients
  coef<-coef.glm[-1][!is.na(coef.glm[-1])]
  coef_names<-sapply(names(coef),function(x){oracle.fix(x)})
  names(coef_names)<-NULL
  
  if(method=="Estimation")
    Myform<-paste("EXP(",paste(coef.glm[1]),"+",paste0(coef,"*",coef_names,collapse="+"),")") 
  ##########################
  rules_out<-c(rules_out,paste("DECLARE\n",
                               "v_PredictionAlgotrace_exists NUMBER :=0;\n"))
  for(var in vars){
    rules_out<-c(rules_out,paste(
      paste0("mean_",var),"REAL;\n",
      paste0("sd_",var),"REAL;\n"))
  }
  
  rules_out<-c(rules_out,paste("BEGIN\n"))
  for(var in vars){
    rules_out<-c(rules_out,paste(
      "SELECT",paste0("AVG(",var,")"),"INTO",paste0("mean_",var),"FROM algotrace_table;\n",
      "SELECT",paste0("STDDEV(",var,")"),"INTO",paste0("sd_",var),"FROM algotrace_table;\n"))
  }  
  rules_out<-c(rules_out,paste(
    "UPDATE algotrace_table\n",
    "SET prediction_algotrace =\n",
    Myform,";\n\n"))
  
  
  rules_out<-c(rules_out,paste(
    "Select count(*) into v_PredictionAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'prediction_algotrace' and table_name = 'source_table';\n",
    "if (v_PredictionAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table source_table add (prediction_algotrace float)';\n",
    "end if;\n",
    "END;\n", 
    "/\n\n"))
  ######
  
  rules_out<-c(rules_out,paste(
    "--now we match the predictions by the ID of the row in the temporary table and then discard it\n",
    "DECLARE\n",
    "v_id_algotrace_exists NUMBER;\n",
    "BEGIN\n",
    "UPDATE source_table\n",
    "SET prediction_algotrace=\n",
    "(select prediction_algotrace FROM algotrace_table\n",
    "WHERE algotrace_table.id_algotrace=source_table.id_algotrace);\n",
    "execute immediate 'DROP TABLE algotrace_table';\n",
    "Select count(*) into v_id_algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'PREDICTION_ALGOTRACE' and table_name = 'SOURCE_TABLE';\n",
    "if (v_id_algotrace_exists = 1) then\n",
    "execute immediate 'ALTER TABLE source_table DROP COLUMN id_algotrace';\n",
    "end if;\n"))
  
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste(
      "UPDATE source_table\n",
      "SET prediction_algotrace= NULL\n",
      "where",var,"is null;\n"
    ))
  
  rules_out<-c(rules_out,paste("END;\n"))
  
  oracle_out <- paste(rules_out, collapse=" ")
  return(oracle_out)
}

#####Convert Rpart model to oracle rules#############
#https://gist.github.com/tomasgreif/6038822
oracle_parse_tree <- function (df=NULL,model=NULL,vars,
                        vars_not.fix.outliers,vars_not.fix,levels.table,Imputation.Value) {
  rules_out<-character(0)
  rules_out<-c(rules_out,"--Change your Data table's name to source_table\n\n")
  
  ##########Identifying the functions##########################
  rules_out<-c(rules_out,functions.oracle)
  
  #############################################################
  rules_out<-c(rules_out,paste(
    "DECLARE\n",
    "not_exist EXCEPTION;\n",
    "PRAGMA EXCEPTION_INIT( not_exist,-942); --a directive to initiate the exception we wish to catch\n",
    "v_PredictionAlgotrace_exists number := 0;\n", 
    "v_Cond1Algotrace_exists number := 0;\n",  
    "v_Cond2Algotrace_exists number := 0;\n",  
    "v_Cond3Algotrace_exists number := 0;\n",  
    "v_SumCondAlgotrace_exists number := 0;\n",  
    "v_TempMadColAlgotrace_exists number := 0;\n",  
    "BEGIN\n\n"))
  
  #Create algotrace_table
  rules_out<-c(rules_out,paste(
    "BEGIN\n",
    "EXECUTE IMMEDIATE 'DROP TABLE algotrace_table';\n",
    "EXCEPTION WHEN not_exist THEN NULL;\n",
    "END;\n\n"))
  
  #add id_algotrace column to source_table
  rules_out<-c(rules_out,paste("EXECUTE IMMEDIATE 'ALTER TABLE source_table ADD (id_algotrace  NUMBER(10))';\n",
                               "EXECUTE IMMEDIATE 'UPDATE source_table SET id_algotrace =ROWNUM';\n",
                               "EXECUTE IMMEDIATE 'CREATE TABLE algotrace_table AS SELECT * FROM SOURCE_TABLE';\n",
                               "\n\n"))
  ##add prediction_algotrace column to algotrace_table
  rules_out<-c(rules_out,paste(
    "Select count(*) into v_PredictionAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'prediction_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_PredictionAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (prediction_algotrace float)';\n",
    "end if;\n\n",
    ##add cond1Algotrace column to algotrace_table
    "Select count(*) into v_Cond1Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond1_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond1Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond1_algotrace float)';\n",
    "end if;\n\n",
    ##add cond2lgotrace column to algotrace_table
    "Select count(*) into v_Cond2Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond2_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond2Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond2_algotrace float)';\n",
    "end if;\n\n",
    ##add cond3Algotrace column to algotrace_table
    "Select count(*) into v_Cond3Algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'cond3_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_Cond3Algotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (cond3_algotrace float)';\n",
    "end if;\n\n",
    ##add SumCondAlgotrace column to algotrace_table
    "Select count(*) into v_SumCondAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'sum_cond_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_SumCondAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (sum_cond_algotrace float)';\n",
    "end if;\n\n",
    ##add TempMadColAlgotrace column to algotrace_table
    "Select count(*) into v_TempMadColAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'temp_mad_col_algotrace'\n",
    "and table_name = 'algotrace_table';\n",
    "if (v_TempMadColAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table algotrace_table add (temp_mad_col_algotrace float)';\n",
    "end if;\n",
    "END;\n",
    "/\n\n"))
  
  
  ##Change Factor to Numeric
  for(var in names(levels.table)){  
    rules_out<-c(rules_out,paste(
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "RENAME COLUMN",var, "TO", paste0(var,"_temp"),";\n",
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "ADD",var,"FLOAT;\n",
      "UPDATE ALGOTRACE_TABLE\n",
      "SET",var,"= CASE\n"))
    
    table<-levels.table[[var]]
    levels<-as.character(table$level)
    rules_out<-c(rules_out,paste(
      "When",paste0(var,"_temp"),"is null then null\n"
    ))
    for(i in levels[-length(levels)])
      rules_out<-c(rules_out,paste(
        "When",paste0(paste0(var,"_temp"),"=","'",i,"'"),"Then",table[which(table$level==i),"num"],"\n"))
    
    rules_out<-c(rules_out,paste(
      "ELSE",table[which(table$level=="new_level_algotrace"),"num"],"\n",
      "END;\n"))
    
    rules_out<-c(rules_out,paste(
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "DROP COLUMN",paste0(var,"_temp"),";\n"))
  }
  
  ##we change 'NA'/'NaN'/'NULL'/'Inf'/'-Inf' in NULL so we can identify the variable as float
  for(var in vars[!vars %in% names(levels.table)])  
    rules_out<-c(rules_out,paste(
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NA', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NaN', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'NULL', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", 'Inf', NULL);\n",
      "UPDATE ALGOTRACE_TABLE SET",var," = REPLACE(",paste0("ALGOTRACE_TABLE.",var),", '-Inf', NULL);\n",
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "RENAME COLUMN",var,"TO",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "ADD",var,"FLOAT;\n",
      
      "UPDATE ALGOTRACE_TABLE\n",
      "SET",var,"=",paste0(var,"_temp"),";\n",
      
      "ALTER TABLE ALGOTRACE_TABLE\n",
      "DROP COLUMN", paste0(var,"_temp"),";\n\n"
    ))
  
  
  if(Imputation.Value=="Median"){
    for(var in vars[!vars %in% vars_not.fix])
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("PERCENTILE_CONT(",var,",0.5)"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n")) 
  } else{ #Mean
    for(var in vars)
      rules_out<-c(rules_out,paste(
        "UPDATE algotrace_table\n", 
        "SET", var,"= CASE\n", 
        "WHEN",var,"is NULL THEN (SELECT",paste0("AVG(",var,")"),"FROM algotrace_table)\n",
        "ELSE",var,"\n", 
        "END;\n"))
  }
  
  for(var in vars[!vars %in% vars_not.fix.outliers])
    rules_out<-c(rules_out,
                 Find_outliers_oracle(var),"/\n\n")
  
  rules_out<-c(rules_out,paste("DECLARE\n",
                               "v_PredictionAlgotrace_exists NUMBER :=0;\n"))
  
  for(var in vars){
    rules_out<-c(rules_out,paste(
      paste0("mean_",var),"REAL;\n",
      paste0("sd_",var),"REAL;\n"))
  }
  
  rules_out<-c(rules_out,paste("BEGIN\n"))
  for(var in vars){
    rules_out<-c(rules_out,paste(
      "SELECT",paste0("AVG(",var,")"),"INTO",paste0("mean_",var),"FROM algotrace_table;\n",
      "SELECT",paste0("STDDEV(",var,")"),"INTO",paste0("sd_",var),"FROM algotrace_table;\n"))
  }  
  
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
            rule_out <- c(rule_out,paste(oracle.fix(elements[1]),(args[sep])[1],elements[2]))
          } else {
            rule_out <- c(rule_out,paste0(oracle.fix(elements[1])," in (",paste0("'",unlist(strsplit(elements[2],",")),"'",collapse=","),")"))
          }
        }
      }
      rules_out <- c(rules_out, paste0("\nwhen ", paste(rule_out,collapse=" AND ")," then 'node_" ,names(rpart.rules)[i],"'"))
      if(i==length(rpart.rules)) rules_out <- c(rules_out,"\nend\n;")
      i <- i +1
    }} else{ #only one path
      rules_out <- c(rules_out,paste0("SET prediction_algotrace = 'node_",names(rpart.rules)[1],"'\n"))
    }
  
  ##add prediction_algotrace to source_table and drop algotrace_table
  rules_out<-c(rules_out,paste(
    "Select count(*) into v_PredictionAlgotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'prediction_algotrace' and table_name = 'source_table';\n",
    "if (v_PredictionAlgotrace_exists = 0) then\n",
    "execute immediate 'alter table source_table add (prediction_algotrace float)';\n",
    "end if;\n",
    "END;\n", 
    "/\n\n"))
  ######
  
  rules_out<-c(rules_out,paste(
    "--now we match the predictions by the ID of the row in the temporary table and then discard it\n",
    "DECLARE\n",
    "v_id_algotrace_exists NUMBER;\n",
    "BEGIN\n",
    "UPDATE source_table\n",
    "SET prediction_algotrace=\n",
    "(select prediction_algotrace FROM algotrace_table\n",
    "WHERE algotrace_table.id_algotrace=source_table.id_algotrace);\n",
    "execute immediate 'DROP TABLE algotrace_table';\n",
    "Select count(*) into v_id_algotrace_exists\n",
    "from user_tab_columns\n",
    "where column_name = 'PREDICTION_ALGOTRACE' and table_name = 'SOURCE_TABLE';\n",
    "if (v_id_algotrace_exists = 1) then\n",
    "execute immediate 'ALTER TABLE source_table DROP COLUMN id_algotrace';\n",
    "end if;\n",
    "END;\n"))
  
  ##Update NULL where numeric vars_not.fix is NULL 
  for(var in vars_not.fix)
    rules_out<-c(rules_out,paste(
      "UPDATE source_table\n",
      "SET prediction_algotrace= NULL\n",
      "where",var,"is null\n"
    ))
  
  
  oracle_out <- paste(rules_out, collapse=" ")
  return(oracle_out)
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
