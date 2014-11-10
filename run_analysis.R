merge_and_label<-function(){
#  get listing of integer - activity label mappings
  activities<-read.table("activity_labels.txt")
# get listing of integer/position - feature label  mappings as strings NOT factors
  featureLabels<-read.table("features.txt",stringsAsFactors=F)
# extract the feature names as a character vector
  featureLabels<-featureLabels[,2]
# extract the indices of all feature names containing the strings mean or std (case insensitive)
  requiredFeats<-c(grep("mean",featureLabels,ignore.case=T),grep("std",featureLabels,ignore.case=T))
# sort to restore the order found in the original table
  requiredFeats<-sort(requiredFeats)
# add the subject and activity values to the lhs of the training data
  trainSet<-do_cbind("train")
# add the subject and activity values to the lhs of the test data
  testSet<-do_cbind("test")
# concatenate the test and data set
  totalSet<-rbind(trainSet,testSet)
# prepend the extra subject and activity labels to the list of feature labels  
  featureLabels<-c(c("Subject","Activity"),featureLabels)
# adjust the indexing of the required indexes to account for these additions
  requiredFeats<-requiredFeats+2
# and add in the indices of these additions
  requiredFeats<-c(1:2,requiredFeats)
#  print(paste("++++++++++++++++++++",requiredFeats))
#  print(featureLabels[requiredFeats])      
# get the training set:

#  print(trainSet)
# get the test set  
#  print(dim(trainSet));print(dim(testSet))
# the three files MUST , return an error if not contain identical numbers of rows
  if((trainSet==-1) || (testSet ==-1)) return("the contents are NOT matched by number of rows")
#  totalSet<-rbind(trainSet,testSet)
#  print(dim(totalSet))
# d
# subset the full feature table to only the required features 
  reqTable<-totalSet[,requiredFeats]
  print(dim(reqTable))
# convert to data.frame
colnames(reqTable)<-featureLabels[requiredFeats]

reqTable<-as.data.frame(reqTable)

head(reqTable)

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