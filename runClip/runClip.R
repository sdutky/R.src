runClip<-function(markdown=TRUE,local=FALSE,...) {
# run Rmd chunks or R code contained in clipboard
#   markdown -> logical: markdown code in clipboard
#   local -> environment: false: global environment, true: calling environment
#    ... -> passed to source(), see >source for details.

if (!require(clipr)) stop("must install clipr package")

	fixUpRmd<-function(chunksAndAll) {
		chunkStart<-grep("^```[{]r",chunksAndAll)
		if(!length(chunkStart)) return(chunksAndAll) # not Rmd
		chunkEnd<-grep("^```([^{]|$)",chunksAndAll)

		if ( length(chunkStart)!=length(chunkEnd) |
			!all(chunkStart<chunkEnd)) {
				stop("Unmatched chunks in clipboard...")
		}

		#prefix each line as a comment
		chunksAndAll<-sub("^","## ",chunksAndAll)
		chunkLines<-unlist(mapply(FUN<-function(a,b) (a+1):(b-1),chunkStart,chunkEnd))
		chunksAndAll[chunkLines]<-sub("^## ","",chunksAndAll[chunkLines])
		return( chunksAndAll)
	}

#get clipboard contents
code<-read_clip()

if (markdown) code<-fixUpRmd(code)

tx<-textConnection(code)

source(tx,local=local,...)

}

