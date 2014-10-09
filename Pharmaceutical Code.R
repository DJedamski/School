#

#    source('Pharmaceutical-Problem.R')

#

#Read in the data - Set directory to folder

    datalist<-list()

#Set 'files' to be everything in the directory that ends with .csv

    files<-list.files(pattern='\\.csv$')

#Create a variable N, which will be the number of files. Initialize it to 0.

   N<-0

# Run a loop to pick each individual file ending with .csv

    for (file in files)

    {

    stem<-gsub('\\.csv$','',file)

    datalist[[stem]]<-read.csv(file, sep=',',skip=7, header=FALSE )

    colnames(datalist[[stem]])<-c("Column", "CSD Code", "Time", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")

   

    cat('\n ',file, file="FileNames.txt", append=TRUE)

    N<-1+N

   

    }

#Show the dimensions, check the files, check what was read in

    dim(datalist[[1]])

    FileNames<-read.table("FileNames.txt", header=F)

    FileNames

#Calculate the ratios


    for(i in 1:N)

    {

          measure<-datalist[[i]][,-c(1,2,3)]

          for(j in 1:8)

              {

                  ratio<-measure[,(2*j-1)]/measure[,(2*j)]

                  measure[,j]<-ratio

              }    

       brratio <- measure[, -c(9:16)]

       datalist[[i]] <- brratio

       datalist[[i]]<-datalist[[i]][-c(441:480),]

       datalist[[i]]<-datalist[[i]][-c(1:40),]

    }

#Graph the results

    Time <- 1:40

    for (v in 1:N)

    {

    yrange <- range(c(datalist[[v]]))

    x11()

    par(mfrow=c(4,2))

#Plot scatterplots for Drug 1 and 2

    for (w in 1:8)

    {

   for(i in 1:10)

   {

       plot(Time, datalist[[v]][c((40*(i-1)+1):(40*i)), w], ylim=yrange, col=i, ylab='Blue/Red Ratio', xlab='Time', main=paste("B/R Ratio vs. Time - Row #",w, "- Day #",v))

          par(new=T)

   

   }

       par(new=F)

    }

 }

#Calculate the max/min ratios

MMRatio<-matrix(data=NA, nrow=8, ncol=10)

for (v in 1:N)

   {

       for (w in 1:8)

       {

           for(i in 1:10)

           {

           

           MMRatio[w,i]<-mean(sort(datalist[[v]][c((40*(i-1)+1):(40*i)),w])[37:40])/mean(sort(datalist[[v]][c((40*(i-1)+1):(40*i)),w])[2:5])

           #cat('\n The ratio is ',max(datalist[[1]][c((40*(i-1)+1):(40*i)),w])/(min(datalist[[1]][c((40*(i-1)+1):(40*i)),w])))

           }

       }

   datalist[[v]]<-MMRatio

   }
datalist[[1]]

#Plot the max/min ratios
Dose<-c(1,3.16,10,31.6,100,316,1000,3160,10000,1000000)
MMRatio_column<-datalist[[1]][,c(1:10)]

x11()

par(mfrow=c(3,3))

for(v in 1:N)

{ 
	
	x11()
	par(mfrow=c(2,2))
	
	for(k in 1:4)

{

   plot(log(Dose), datalist[[v]][k,], col=c(1,2,3,4,5,6,7,8,9,10,'dark green','dark green','dark green','dark green','gold','gold','gold','gold'), lwd=3, xlab='Dosage', ylab='Max over Min Ratio', main=paste("Titration",k,"/Drug A/Day ",v),ylim=c(1.0,3.5))
   #plot(Time, datalist[[v]][1:4,], col=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,'dark green','dark green','dark green','dark green','gold','gold','gold','gold'), lwd=3, xlab='Dosage', ylab='Max over Min Ratio', main=paste("Drug A on Day ",v),ylim=c(1.0,3.5))
  

}

}

x11()

par(mfrow=c(3,3))

for(v in 1:N)

{
	
	x11()
	par(mfrow=c(2,2))
	
	for(k in 5:8)

{

   plot(log(Dose), datalist[[v]][k,], col=c(1,2,3,4,5,6,7,8,9,10,'dark green','dark green','dark green','dark green','gold','gold','gold','gold'), lwd=3, xlab='Dosage', ylab='Max over Min Ratio', main=paste("Titration",k-4,"/Drug B/Day ",v),ylim=c(1.0,3.5))
   #plot(Time, datalist[[v]][5:8,], col=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,'dark green','dark green','dark green','dark green','gold','gold','gold','gold'), lwd=3, xlab='Dosage', ylab='Max over Min Ratio', main=paste("Drug B on Day ",v),ylim=c(1.0,3.5))   
    
}

}