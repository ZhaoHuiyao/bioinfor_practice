#calculate base frequency
#function named Base_fre
#which prints base frequency
#parameter sequence:sequence information
#class(sequence)=character

Base_fre<-function(sequence){
  a<-strsplit(sequence,"")
  fre_A<-(length(grep("A",a[[1]]))/length(a[[1]]))
  fre_T<-(length(grep("T",a[[1]]))/length(a[[1]]))
  fre_C<-(length(grep("C",a[[1]]))/length(a[[1]]))
  fre_G<-(length(grep("G",a[[1]]))/length(a[[1]]))
  fre_base<-c(fre_A,fre_T,fre_C,fre_G)
  return(fre_base)
}
