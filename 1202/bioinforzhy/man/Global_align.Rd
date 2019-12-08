\name{Global_align}
\alias{Global_align}
\title{Global_align}
\usage{
Global_align(seq1,seq2,score_rule)
}
\description{
Prints result_set(Dynamic programming matrix;alignment score;alignment result)
}
\arguments{
seq1: one sequence information;class(seq1)=character
seq2: another sequence information;class(seq1)=character
score_rule: Base alignment score(c(match=,dismatch=,gap=))
}
\value{
return a list(three items:matrix,score,c(align_seq1,align_seq2))
}
