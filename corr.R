corr<-function(directory,threshold=0)  {
	files<-list.files(path=directory)
	cr<-c()
	for(f in 1:length(files)){
		data<-read.csv(paste(directory,'/',files[f],sep=''))
		data<-data[complete.cases(data),]
		if(nrow(data)>threshold) {
			cr<-c(cr,cor(data$sulfate,data$nitrate))
		}
	}
	return(cr)
}