# return values and frequencies in descending order
f<-function(a) plyr::count(a) %>% arrange(desc(freq))
