#The line of "#" is the code of multiline sequence 
library(bioinforzhy)	
setwd("d:/bioinfor_practice/1202/file/")
accession<-paste("AJ5345",c(26:30),sep = "")
Download_genbank(accession)
temp<-list.files(path="d:/bioinfor_practice/1202/file/",pattern="*.gb")
seqset<-c()
i<-1
for(i in 1:length(temp)){
  genbank<-readLines(temp[i])
  fasta<-Gen_to_fas(genbank,seq_type=2) #signle;seq_type=2:multiple
  seqset<-c(seqset,fasta)
}
fre_set<-c();seq_set<-c();ID_set<-c()	
i<-1
for(i in 1:length(seqset)){
  seq<-strsplit(seqset[i],"\n")
  ID<-strsplit(seq[[1]][1],">")
  ID_set<-c(ID_set,ID[[1]][2])
  seq<-seq[[1]][2]
  seq_set<-c(seq_set,seq)
  a<-Base_fre(seq) 
  fre_set<-c(fre_set,a)
}	
# for(i in 1:length(seqset)){
#  seq<-strsplit(seqset[i],"\n")
#  ID<-strsplit(seq[[1]][1],">")
#  ID_set<-c(ID_set,ID[[1]][2])
#  seq<-paste(seq[[1]][2:length(seq[[1]])],collapse="")	
#  seq_set<-c(seq_set,seq)
#  a<-Base_fre(seq) 
#  fre_set<-c(fre_set,a)
#}	
FRE<-matrix(fre_set,ncol=4,byrow = TRUE)
colnames(FRE)<-c("A","T","C","G")
rownames(FRE)<-ID_set
for(i in 1:length(seqset)){
  name<-paste(ID_set[i],"_V1",".fasta",sep = "")
  file_path=paste("d:/bioinfor_practice/1202/file",name,sep ="/")
  cat(seqset[i],file=file_path)}
# for(i in 1:length(seqset)){
#  name<-paste(ID_set[i],"_V2",".fasta",sep = "")
#  file_path=paste("d:/bioinfor_practice/1202/file/",name,sep ="/")
#  cat(seqset[i],file=file_path)}
install.packages("pheatmap")
install.packages("rlang")
library(pheatmap)
pheatmap(FRE,scale = "none",color = colorRampPalette(colors = c("blue","white","red"))(100))
seq1<-seq_set[1]
seq1_ID<-ID_set[1]
seq2<-seq_set[2]
seq2_ID<-ID_set[2]
s_rule<-c(5,-2,-6)
result_align<-Global_align(seq1,seq2,s_rule)	
print("The dynamic programming matrix:")
print(result_align[[1]])
print(paste("The dynamic programming score:",result_align[[2]],sep=""))
print(paste("aligement seq1","seq1_ID","result:",result_align[[3]][1],sep=""))
print(paste("aligement seq2","seq2_ID"," result:",result_align[[3]][2],sep=""))



