merge_and_label<-function(){
#  get listing of integer - activity label mappings
  activities<-read.table("activity_labels.txt")
# get listing of integer/position - feature label  mappings
  featureLabels<-read.table("features.txt")
# get the training set:
  trainSet<-do_cbind("train")
  print(trainSet)
# get the test set  
  testSet<-do_cbind("test")
  if((trainSet==-1) || (testSet ==-1)) return("the contents are NOT matched by number of rows")
# 
}

do_cbind<-function(d){
# activity type record file name
  Act<-paste(d,"/y_",d,".txt",sep="")
# subject id record file name  
  subjID<-paste(d,"/subject_",d,".txt",sep="")
# feature vector filename
  vectors<-paste(d,"/X_",d,".txt",sep="")
  print(Act)
  print(subjID)
  print(vectors)
  Act<-read.table(Act)
  Act<-sapply(Act,as.numeric)
  # subjectID
  subjID<-read.table(subjID)
  subjID<-sapply(subjID,as.numeric)
  # feature vector values
  vectors<-read.table(vectors)
  vectors<-sapply(vectors,as.numeric)
print(class(Act))
print(class(subjID))
print(class(vectors))
  if((nrow(vectors)!=nrow(subjID)) || (nrow(vectors)!=nrow(Act))){return(-1)}
  cbind(subjID,Act,vectors)
}