merge_and_label<-function(){
#  get listing of integer - activity label mappings
  activities<-read.table("activity_labels.txt")
# get listing of integer/position - feature label  mappings
  featureLabels<-read.table("features.txt")
# get the training set:
  trainSet<-do_rbind("train")
  print(trainSet)
# get the test set  
  testSet<-do_rbind("test")
print(testSet)
# 
}

do_rbind<-function(d){
  # activity type record:
  Act<-paste(d,"/y_",d,".txt",sep="")
  subjID<-paste(d,"/subject_",d,".txt",sep="")
  vectors<-paste(d,"/X_",d,".txt",sep="")
  print(Act)
  print(subjID)
  print(vectors)
  Act<-read.table(Act)
  # subjectID
  subjID<-read.table(subjID)
  # feature vector values
  vectors<-read.table(vectors) 
  if((nrow(vectors)!=nrow(subjID)) || (nrow(vectors)!=nrow(Act))){return(-1)}
  1
  
}