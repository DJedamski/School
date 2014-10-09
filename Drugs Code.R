datalist<-list()
files<-list.files(pattern='\\.csv$')

for (file in files)
{
	stem<-gsub('\\.csv$','',file)
	datalist[[stem]]<-read.csv(file, sep='',header=FALSE)
}