#$Log: qw.R,v $
#Revision 1.6  2020/01/12 03:26:12  dutky
#simple stupid parsing of list names & values
#
#Revision 1.5  2020/01/12 02:21:07  dutky
#valiant but failed attempt to handle list names and values
#
#Revision 1.4  2020/01/06 22:12:22  dutky
#makeList now casts list elements according to recyled classes of what
#
#Revision 1.3  2020/01/05 17:16:20  dutky
#Accept regex as sep
#
#Revision 1.2  2020/01/02 23:05:52  dutky
#revised to handle lists with non variable names
#
#Revision 1.1  2020/01/02 21:44:15  dutky
#Initial revision
#
qw<-function(string,what=character,dim=FALSE,sep="",quiet=FALSE,qwCode=FALSE) {

	warn<-function(msg) if (!quiet) warn(msg)

	quotedNAtoNA<-function(string) gsub("\x22NA\x22","NA",string)

	genSep<-function(string,reSep) {
	# the value of sep passed to qw is two or more characters long:
	# we interpret this as a regular expression which we replace
	# with a new single byte seperator ( \x01 -\x7f )
	# skipping over characters "\()[]{}|
	seps<-sapply(c(1:33,35:39,42:90,94:122),FUN=function(a) rawToChar(as.raw(a)) )
	sepGrep<-sapply(seps, FUN=function(a) grepl(a,string))
	if ( all(sepGrep)) stop("No possible substitute separator for ",reSep)

	# choose the first character not contained in string as the new seperator
	newSep=seps[!sepGrep][1]
	# remove trailing reSeps at the end of the string
	newString<-gsub(paste("(",reSep,")*$",sep=""),"",string)
	return( list(newSep=newSep,newString=gsub(reSep,newSep,newString)) )

	}

	hexQuote<-function(string) {
	# replace each embedded accent gras, single and double quotes with hex codes:
		string<-gsub('"',"\\\\x22", string)
		string<-gsub('`',"\\\\x60", string)
		gsub("'","\\\\x27", string)
	}


	genVecCode<-function(v) {
		if (class(v) !="character" ) {
			v<-paste(v,collapse=",")
			func<-paste("function() c(",v,")")
			return(eval(parse(text=func)))
		}
		v<-hexQuote(v)
		v<-paste(v,sep="",collapse="\x22,\x22")
		func<-paste("function() c(\x22",v,"\x22)",sep="")
		func<-quotedNAtoNA(func)
		return(eval(parse(text=func)))
	}
		

	genArrayCode<-function(v,dim) {
		dim<-paste(dim,collapse=",")
		if ( class(v) != "character" ) {
			v<-paste(v,collapse=",")
			func<-paste("function() array( c(",v,"),c(",dim,"))")
		} else {
			v<-hexQuote(v)
			v<-paste(v,sep="",collapse="\x22,\x22")
			func<-paste("function() array( c(\x22",v,"\x22),c(",dim,"))",sep="")
			func<-quotedNAtoNA(func)
		}
		return(eval(parse(text=func)))
	}

	makeArray<-function(v) {
		v<-as(v,class(what()))
		if (length(v)>prod(dim)) warn("the length of the vector is greater than the product of the dimensions")
		else if ( prod(dim) %% length(v) !=0 ) warn("the product of the dimensions is not a multiple of the length of vector")
		if (!qwCode) return(array(v,dim=dim))
		return( genArrayCode(v,dim))
	}

	genListCode<-function(listNames,vList) {
		listNames<-hexQuote(listNames)
		quotedNames<-!grepl("^[[:alpha:]][[:alnum:]]*$",listNames)

		# enclose non variable names with "`"
		listNames[quotedNames]<-paste("`",listNames[quotedNames],"`",sep="")
		listNames<-paste(listNames,"=c(" ,sep="")
		listNames[grepl("``",listNames)]<-"c("

		isChar<-sapply(vList,FUN=function(a) class(a)=="character" )

		if (any(!isChar)) {
			vList[!isChar]<-lapply(vList[!isChar],FUN=function(a) paste(a,collapse=",") )
			vList[!isChar]<-paste(vList[!isChar],")",sep="")
		} 
		if (any(isChar)) {
			vList[isChar]<-lapply(vList[isChar],FUN=function(a) hexQuote(a))
			vList[isChar]<-lapply(vList[isChar],FUN=function(a) paste(a,collapse='","') )
			vList[isChar]<-paste('"',vList[isChar],'")', sep="")
		}
		listArgs<-paste(listNames,vList,sep="",collapse=",")
		func<-paste("function() list(",listArgs,")", collapse="" )
		func<-quotedNAtoNA(func)
		
		return(eval(parse(text=func)))
	}

	fixNamedElements<-function(v) {
	# input text containing "... <name><white space>=<white space><value>
	# yields v[.]=<name>, v[.+1]="=", v[.+2] = <value>
	# this needs to be fix as v[.]=<name>=value and v[.+1],v[.+2] removed
	# there are exceptions:
	#  v[1]=="=.*", v[.-1]==".*=.*",v[.]="=.*] ; etc.

	prev<-""
	i<-1
	cuts<-c()

	while ( i < length(v)) {
		tok<-v[i]
		nextTok<-v[i+1]
		skipPrev<-FALSE

		if ( any(grepl("^=",tok))) {
			tok<-paste(prev,tok,sep="")
			v[i]<-tok
			skipPrev<-TRUE
			cuts<-c(cuts,i-1)
		}

		if ( any(grepl("^[^=]*=$",tok))) {
			tok<-paste(tok,nextTok,sep="")
			v[i]<-tok
			skipPrev<-TRUE
			cuts<-c(cuts,i+1)
			i<-i+1
		}

		i<-i+1
		prev<-tok
		if (skipPrev) prev<-""
			
	}
	v<-v[-cuts]

	return(v)
}		
	

	makeList<-function(v) {
		v<-fixNamedElements(v)
		listIndx<-grep("=",v)

		listNames<-sub("^([^=]*)=.*$","\\1",v[listIndx])
		v<-sub("^[^=]*=","",v)
		if (listIndx[1]!=1) {
			listNames<-c("",listNames)
			listIndx<-c(1,listIndx)
		}
		if (length(listIndx)==1) listEnd<-length(v)
		else listEnd<-c(listIndx[2:length(listIndx)]-1,length(v))

		vList<-mapply(function(a,b) v[a:b], listIndx, listEnd,SIMPLIFY=FALSE)
		what<-rep(c(what),length(vList))[1:length(vList)]
		for ( i in 1:length(vList)) vList[[i]]<-as(vList[[i]],class(what[[i]]()))
		if (qwCode) return(genListCode(listNames,vList))
		names(vList)<-listNames
		return(vList)
	}

# main qw code follows:
	if (nchar(sep)>1) { #treat sep as regular expression:
	   # generate a new single character seperator,
	   # and replace all occurencess of the regex in string with it
		sep<-genSep(string,sep)
		string=sep$newString
		sep<-sep$newSep
	}

	v<-scan(text=string,what=character(),sep=sep,quiet=quiet)

	if (any(grep("=",v)))  v<-makeList(v)
	else if(length(dim)>1) v<-makeArray(v)
	else {
		v<-as(v,class(what()))
		if (qwCode) v<-genVecCode(v)
	}
	return(v)
}
