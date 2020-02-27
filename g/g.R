 g<-function(regEx,vec,v="v",i=FALSE) {
#
# convenient call to the forms of grep that minimizes typing
# also works in dplyr pipeline:
#   obj %>% g(regEx,.)
 if (v=="v") a<-grep(regEx,vec,value = TRUE,ignore.case=i)
 if (v=="i") a<-grep(regEx,vec,ignore.case=i)
 if (v=="l") a<-grepl(regEx,vec,ignore.case=i)
 a
 }
