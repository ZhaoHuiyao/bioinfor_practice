#download genbank file in Batch
#function named Download_genbank
#which prints .gb file
#parameter acc:gene ID vector

Download_genbank<-function(acc){
 Base_url<-"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id="
 b<-"&rettype=gb&retmode=text"
 i<-1
 for(i in 1:length(acc)){
  url_path<-paste(Base_url,acc[i],b,sep = "")
  in_file<-paste(acc[i],".gb",sep = "")
  download.file(url_path,in_file)
 }
}
