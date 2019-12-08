#pairwise alignment 
#function named Global_align
#which prints result_set(Dynamic programming matrix;alignment score;alignment result)
#parameter seq1,seq2:two sequence information
#parameter score_rule:match,dismatch,gap

Global_align<-function(seq1,seq2,score_rule){
  X<-strsplit(seq1,"");X<-X[[1]]
  Y<-strsplit(seq2,"");Y<-Y[[1]]
  location<-score<-matrix(0,nrow=(length(X)+1),ncol=(length(Y)+1))
  for(i in 1:nrow(score)){
    for(j in 1:ncol(score)){
      if(i==1){if(j>1){score[i,j]<-(score_rule[3]*(j-1));location[i,j]<-1};next}
      if(j==1){if(i>1){score[i,j]<-(score_rule[3]*(i-1));location[i,j]<-2};next}
      a<-score[i-1,j]+score_rule[3];
      b<-score[i,j-1]+score_rule[3];
      if(X[i-1]==Y[j-1]){c<-score[i-1,j-1]+score_rule[1]}
      else{c<-score[i-1,j-1]+score_rule[2]}
      d<-which.max(c(a,b,c))
      if(d==1){score[i,j]<-a;location[i,j]<-2}
      if(d==2){score[i,j]<-b;location[i,j]<-1}
      if(d==3){score[i,j]<-c;location[i,j]<-0}
    }
  }
  new_site<-function(site,row_value,col_value){
    if(site==0){site<-location[row_value-1,col_value-1]
    end_value<-c(site,row_value-1,col_value-1)}
    else if(site==1){site<-location[row_value,col_value-1]
    end_value<-c(site,row_value,col_value-1)}
    else if(site==2){site<-location[row_value-1,col_value]
    end_value<-c(site,row_value-1,col_value)}
    return(end_value)
  }
  a<-location[nrow(location),ncol(location)]
  end_value<-c(a,nrow(location),ncol(location))
  result<-c()
  while (end_value[2]>0) {
    result<-c(result,end_value[2:3])
    end_value<-new_site(end_value[1],end_value[2],end_value[3])
  }
  AlignResult<-matrix(result,ncol=2,by=TRUE)
  seq<-list(X,Y)
  seq_set<-list(NEW_seq1<-c(),NEW_seq2<-c())
  for(j in 1:2){
    for(i in nrow(AlignResult):1){
      if(i==nrow(AlignResult)){
        if((AlignResult[i,j]-1)==0){next;}
        else {seq_set[[j]]<-paste(seq_set[[j]],"-",sep="")}
      }
      else {
        if(AlignResult[i,j]==AlignResult[i+1,j]){seq_set[[j]]<-paste(seq_set[[j]],"-",sep="")}
        else{Num<-AlignResult[i,j]-1;seq_set[[j]]<-paste(seq_set[[j]],seq[[j]][Num],sep="");}
      }
    }
  }
  result_set<-list(score,score[nrow(score),ncol(score)],seq_set)
  return(result_set)
}