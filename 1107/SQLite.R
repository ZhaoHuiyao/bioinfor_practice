install.packages("RSQLite",dependencies=T)
library(RSQLite)
setwd("d:/bioinfor_practice/1107/")
db = dbConnect(SQLite(), dbname="baseball.db")
dbSendQuery(conn = db,"CREATE TABLE BASEBALL
       (Team_ID INTEGER,Team_Name TEXT,League TEXT,Payroll REAL,Wins INTEGER)")
dbSendQuery(conn = db,"INSERT INTO BASEBALL
         VALUES (1, 'Twins', 'American League', '54641175','1020')")
dbSendQuery(conn = db,"INSERT INTO BASEBALL
         VALUES (2, 'Giants', 'American League', '82288960','1033')")
url_path<-"https://raw.githubusercontent.com/ysquared2/RSQLiteTutorial/master/bball.csv"
file<-"d:/bioinfor_practice/1107/bball.csv"
download.file(url_path,file)
bball = read.csv("bball.csv")
#若不存在.db文件,则使用命令db2= dbConnect(SQLite(),dbname="BBALL.db),生成一个新的.db文件
db2= dbConnect(SQLite())
db2= dbConnect(SQLite(), dbname="bball.db")
dbWriteTable(conn = db2, name = "BBALL", bball,
             overwrite=T,row.names=FALSE)
dbListTables(db) 
dbListFields(db, "BASEBALL") 
head(dbReadTable(db, "BASEBALL"))
dbListTables(db2) 
dbListFields(db2, "BBALL") 
head(dbReadTable(db2, "BBALL"))
dbGetQuery(db2, "select * from BBALL WHERE LEAGUE='AL'")[1:3,]
dbGetQuery(db2, "select * from BBALL WHERE LEAGUE='AL' LIMIT 3")
dbGetQuery(db2, "select Team,Payroll,RSWP from BBALL WHERE LEAGUE='AL' AND RSWP>0.5")
dbGetQuery(db2, "select Team,Payroll,PSW from BBALL WHERE Team LIKE 'M%' AND PSW IN (6,8,11)")
dbGetQuery(db2, "select Division,AVG(RSW) AS 'average_RSW' from BBALL GROUP BY Division")
dbGetQuery(db2, "select * from BBALL ORDER BY RSW")
dbInquire<-function(database,requirement,namecol,limit){
  a<-dbListTables(database) 
  b<-paste(namecol,collapse = ",")
  if(limit=="all"){query<-paste("select",b,"from",a,"WHERE",requirement,collapse = " ")}
  else{query<-paste("select",b,"from",a,"WHERE",requirement,"LIMIT",limit,collapse = " ")}
  result<-dbGetQuery(database,query)
  return(result)
}
a<-db2
c<-c("Team","Payroll","RSWP")
b<-"LEAGUE='AL' AND RSWP>0.5"
d<-3
dbInquire(a,b,c,d)
dbInquire(a,b,c,'all')
dbExecute(db2, "DELETE  from BBALL WHERE LEAGUE='AL'")
dbExecute(db2, "INSERT  INTO BBALL VALUES ('NL','East','Mars',45782939,1000,1030,0.501,8,2,0.678)")
dbDisconnect(db)
dbDisconnect(db2)

