## this is the path of the subset data
    X.file<- c("UCI HAR Dataset/test/X_test.txt","UCI HAR Dataset/train/X_train.txt")
    Y.file<- c("UCI HAR Dataset/test/y_test.txt","UCI HAR Dataset/train/y_train.txt")
    subject.file<- c("UCI HAR Dataset/test/subject_test.txt","UCI HAR Dataset/train/subject_train.txt")
    feature.file<- "UCI HAR Dataset/features.txt"
    Activity.file<- "UCI HAR Dataset/activity_labels.txt"
  
# Create a Dataset (Step 1,3,4)
    FF<- read.table(feature.file,sep = " ", colClasses = c("numeric","character")) #var names
    Activity<- read.table(Activity.file,sep = " ",colClasses = c("numeric","character"))[,2]
    for (ii in 1:length(X.file)) {
      if (ii==1) {
        x<- read.table(X.file[ii],sep = "",dec = ".",col.names = FF[,2])
        y<- read.table(Y.file[ii],colClasses = "factor",col.names = "Activity")
        subject<- read.table(subject.file[ii],colClasses = "numeric")
        X<- cbind(subject,X=x)
        Y<- cbind(Y=y)
      } else {
        x<- read.table(X.file[ii],sep = "",dec = ".",col.names = FF[,2])
        y<- read.table(Y.file[ii],colClasses = "factor",col.names = "Activity")
        subject<- read.table(subject.file[ii],colClasses = "numeric")
        X<- rbind(X,cbind(subject,X=x))
        Y<- rbind(Y,y)
      }
      rm(list = c("x","y","subject"))
    }
    levels(Y$Activity)<- Activity
    rm(list = c("FF","Activity","X","Y"))
    Dati<- cbind(Y,X)

## Extract mean and standard deviation for each measurement (Step 2,5)    
    Search.String <- function(Data.string,String= stop("need a string character")) {
      n<- length(String)
      m<- length(Data.string)
      pos<- rep(FALSE,m)
      .Combacia<-function(x){if(length(x)==0){return(FALSE)}else{return(TRUE)}}
      
      for (ii in 1:n) {
        pos1<- sapply(stringr::str_extract_all(Data.string,String[ii]),.Combacia)
        pos<- pos|pos1
        rm("pos1")
      }
      return(pos)
    }
    
    Dati[,Search.String(colnames(Dati),c("mean","std"))] # this is Step 2
    
    Dati.Mean<- Dati[,Search.String(colnames(Dati),c("mean"))] # this is Step 5

