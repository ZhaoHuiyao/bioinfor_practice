#extract ID and sequence in genbank file,and output them
#function named Gen_to_fas
#which print ID and sequence information
#parameter genbank: .gb file
#parameter seq_type=1:sigle sequence;seq_type=2:multiple sequence;

Gen_to_fas<-function(genbank,seq_type){
  b<-strsplit(genbank[1]," +")
  id<-b[[1]][2]
  ID<-paste(">",id,sep="")
  seq<-""
  i<-1
  for(i in 1:length(genbank)){
    b<-strsplit(genbank[i]," +")
    if("ORIGIN"==b[[1]][1]) {break}}
  for(k in (i+1):length(genbank)){
    b<-strsplit(genbank[k]," +")
    c<-as.vector(b[[1]])
    if(c[1]!="") {break}
    d<-paste(c[3:length(c)],collapse = "")
    seq<-paste(seq,d,seq = "")}
  seq_V1<-gsub("\\s","",seq)
  SEQ_V1<-toupper(seq_V1)
  result_V1<-paste(ID,SEQ_V1,sep = "\n")
  seq_V2<-gsub("  ","\n",seq)
  seq_V2<-gsub(" ","",seq_V2)
  seq_V2<-paste(seq_V2,"\n",sep = "")
  SEQ_V2<-toupper(seq_V2)
  result_V2<-paste(ID,SEQ_V2,sep = "\n")
  if(seq_type==1){
    return(result_V1)}
  if(seq_type==2){
    return(result_V2)}
}



