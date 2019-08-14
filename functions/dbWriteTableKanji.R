dbWriteTableKanji <-
function(conn,datname,df,overwrite =FALSE,...){
  #### 在数据库里写入含中文的表
  # conn: 数据库连接
  # datname: 打算在数据库里创建的新表的名称，注意不可以和已有的数据库中的表重名
  # df：打算保存的表，需要是data.frame格式
  
  if(is.data.frame(df)){
    if(!require(stringi)){install.packages('stringi')}
    if(!require(data.table)){install.packages('data.table')}
    
    toutf8 <- function(x) {
      if (is.factor(x)) {
        x <- as.character(x)
      }
      if (is.character(x)) {
        if (!all(stri_enc_isutf8(x))) {
          x <- stri_encode(x, "", "UTF-8")
        }
        Encoding(x) <- "utf8"
      }
      return(x)
    }
    df<-as.data.table(df)
    if(Sys.info()[["sysname"]]=="Windows"){
      names(df)<-toutf8(names(df))
      df<-df[ , lapply(.SD, toutf8)]
      dbGetQuery(conn, "set NAMES utf8")# 使保存时中文不乱码
      dbWriteTable(conn,datname,df,overwrite = overwrite,...)
      dbSendQuery(conn, "SET NAMES GBK")# 使读取时中文不乱码
    }
  }else{
    warning('df need to be a data.frame')
  }
}
