camelCase <- function(x) {
# remove all non-alphanerics, make each right adjacent alphanumeric upper case
    x<-gsub("(^|[^[:alnum:]])([[:alnum:]])", "\\U\\2", x, perl = TRUE)
# make leading alpanumeric lowercase
    sub("(^[[:alnum:]])", "\\L\\1", x, perl = TRUE)

}
