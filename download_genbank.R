#NCBI批量下载genbank文件，基因ID为AJ534526~AJ534550

download_genbank<-function(acc){
Base_url<-"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nucleotide&id="
b<-"&rettype=gb&retmode=text"
i<-1
for(i in 1:length(acc)){
  url_path<-paste(Base_url,acc[i],b,sep = "")
  in_file<-paste(acc[i],".gb",sep = "")
  download.file(url_path,in_file)
}
}
accession<-paste("AJ5345",c(26:50),sep = "")
setwd("d:/bioinfor_practice/1118/")
download_genbank(accession)