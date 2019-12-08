#genbank文件提取基因ID与序列信息，并分别生成ID_V1.fasta(单行)与ID_V2.fasta(多行)文件
#下载AB115403.gb文件
url_path<-"https://raw.githubusercontent.com/GuangchuangYu/course_bioinfo_training/master/data/AB115403.gb"
in_file<-"d:/bioinfor_practice/1115/AB115403.gb"
download.file(url_path,in_file)

genbank<-readLines("d:/bioinfor_practice/1115/AB115403.gb")
b<-strsplit(genbank[1]," +")      #根据任意空白数进行分割
id<-b[[1]][2]     #第二个为序列id                 
ID<-paste(">",id,sep="")
seq<-""    #空白序列
i<-1
for(i in 1:length(genbank)){
  b<-strsplit(genbank[i]," +")
  if("ORIGIN"==b[[1]][1]) {break}      
}          #确定ORIGIN所在行，下一行则是序列起始行
for(k in (i+1):length(genbank)){
  b<-strsplit(genbank[k]," +")
  c<-as.vector(b[[1]])
  if(c[1]!="") {break}
  d<-paste(c[3:length(c)],collapse = "")
  seq<-paste(seq,d,seq = "")
}         #序列前面存在数字，因此删除


#输出的是一行序列
seq_V1<-gsub("\\s","",seq)
SEQ_V1<-toupper(seq_V1)    #变成大写
result_V1<-paste(ID,SEQ_V1,sep = "\n")
cat(result_V1,file = "d:/bioinfor_practice/1115/AB115403_V1.fasta")


#输出的是多行序列
seq_V2<-gsub("  ","\n",seq)
seq_V2<-gsub(" ","",seq_V2)
seq_V2<-paste(seq_V2,"\n",sep = "")  #最后一行添加换行符
SEQ_V2<-toupper(seq_V2)    #变成大写
result_V2<-paste(ID,SEQ_V2,sep = "\n")
cat(result_V2,file = "d:/bioinfor_practice/1115/AB115403_V2.fasta")



