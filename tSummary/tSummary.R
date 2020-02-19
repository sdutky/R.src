#$Log: tSummary.R,v $
#Revision 1.1  2020/02/16 17:52:47  dutky
#Initial revision
#

tSummary<-function(obj,columnNamePrefix="") {
  # returns a dataframe of summary(obj) transposed and 
  # rownames<-names(obj), names<-the name of the summary
  # statistic prepended by columnNamePrefix
  
  # coerce the transpose of the table returned by summary() to a data frame
  a<-as.data.frame.matrix(t(summary(obj)))
  
  # for each summary statistic
  for ( i in 1:ncol(a)) {
    # extract names and values from text of summary:
    statNames<-sub("^([^:]*):.*$","\\1", a[,i] )
    statNames<-sub("^([^ ]+|[^ ]+ [^ ]+) *$","\\1", statNames )
    a[,i]<- as.numeric(sub("^[^:]*: *([^ ]*) *$","\\1", a[,i] ))
    
    # choose the most frequent name of the column
    # specifically to exclude character columns
    choiceName<-names(sort(table(statNames),decreasing = TRUE))[1]
    names(a)[i]<-paste(columnNamePrefix,choiceName,sep="")
    
    # NA out any values that do not have the chosen statistic
    a[!grepl(choiceName,statNames),i]<-NA
  }
  a
}
