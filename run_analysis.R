##\#############################[ merge_subset_and_label() ]##########################################
##\Function to :
##\==(1) read the files summarising the numerical/positional codes and explicit labels for the 
##\   features recorded in these datasets 
##\==(2) from the feature labels identify the column indices of ALL labels containing "mean" or
##\   "std" (case insensitive) maintaining relative order of labels
##\==(3) make two calls to do_cbind(), with the tokens "train" and "test" to obtain matrices 
##\   containing the factor codes and feature values for both data sets (check for equality of
##\   the number of observation within each terminate with error state if necessary)
##\==(4) merge the training and test data sets into one matrix
##\==(5) extract the factor columns and required feature columns fron the matrix
##\==(6) give the columns the explicit labels as column names, convert to a data frames and convert 
##\   the numeric activity factors into explicit labels
##\==(7) return the data frame
##\#############################################################################################
merge_subset_and_label<-function(){
##\==(1)  
#\  get listing of integer - activity label mappings
    activities<-read.table("activity_labels.txt")
#\ get listing of integer/position - feature label  mappings as strings NOT factors
    featureLabels<-read.table("features.txt",stringsAsFactors=F)
#\ extract the feature names as a character vector
    featureLabels<-featureLabels[,2]
##\==(2)
#\ extract the indices of all feature names containing the strings mean or std (case insensitive)
    requiredFeats<-c(grep("mean",featureLabels,ignore.case=T),grep("std",featureLabels,ignore.case=T))
#\ sort to restore the relative order found in the   original table
    requiredFeats<-sort(requiredFeats)
##\==(3)
#\ get the training data from file and add the subject and activity values to the lhs of the matrix
    trainSet<-do_cbind("train")
#\ get the test data from file and add the subject and activity values to the lhs of the matrix
    testSet<-do_cbind("test")
#\ the three files MUST , return an error if not contain identical numbers of rows
    if((trainSet==-1) || (testSet ==-1)) return("the contents are NOT matched by number of rows")
##\==(4)
#\ concatenate the test and data set
    totalSet<-rbind(trainSet,testSet)
#print(dim(trainSet));print(dim(testSet));print(dim(totalSet))
##\==(5)
#\ prepend the extra subject and activity labels to the list of feature labels  
    featureLabels<-c(c("Subject","Activity"),featureLabels)
#\ adjust the indexing of the required indexes to account for these additions
    requiredFeats<-requiredFeats+2
#\ and add in the indices of these additions
    requiredFeats<-c(1:2,requiredFeats)
##\==(6)
#\ subset the full feature table to only the required features 
    reqTable<-totalSet[,requiredFeats]
    print(dim(reqTable))
#\ attach the vector of required feature labels as column names
    colnames(reqTable)<-featureLabels[requiredFeats]
#\ convert the matrix to a data.frame
    reqTable<-as.data.frame(reqTable)
#\ assign the explicit activity strings to the numerical factors maintained in the activities column
    reqTable$Activity<-ordered(reqTable$Activity,  levels = activities[,1],  labels = activities[,2])
#\ take a peek (comment out after development)
   print(head(reqTable,50))
##\==(7)
#\ return the filtered, parsed and relabelled dataframe
   reqTable
}


##\################################[ do_cbind<-function(d) ]#####################################
##\ Function to :
##\ ==(1) accept a token ("test" or "train") to enable the explicit generation of valid
##\    filenames for i) the codes for the activity associated with each observation ii) the subject
##\    performing the activity for each observation iii) the 561 feature numerical vector generated
##\    from the particular observation.
##\ ==(2) read the data from files into dataframes check number of observations in each and return an
##\    error state if they are NOT equal
##\ ==(3) If equal,convert all data structures to matrices and prepend the subjectIDs and activity
##\    codes to the left hand side (columns 1 and 2) of the matrix which.
##\ ==(4) return the constructed matrix
##\##############################################################################################

do_cbind<-function(d){
#\ construct the activity type record file name
  Act<-paste(d,"/y_",d,".txt",sep="")
#\ construct thesubject id record file name  
  subjID<-paste(d,"/subject_",d,".txt",sep="")
#\ construct the feature vector filename
  vectors<-paste(d,"/X_",d,".txt",sep="")
#\  read activity ids file  into a data frame
   Act<-read.table(Act)
#\  extract the activity ids as a numeric vector 
   Act<-sapply(Act,as.numeric)
#\  read subject ids file  into a data frame
  subjID<-read.table(subjID)
#\  extract the activity ids as a numeric vector 
  subjID<-sapply(subjID,as.numeric)
#\  read feature values file into a data frame
  vectors<-read.table(vectors)
#\ extract the features into an nx516 matric 
  vectors<-sapply(vectors,as.numeric)
#\ check the number of observations equal in all three objects
   if((nrow(vectors)!=nrow(subjID)) || (nrow(vectors)!=nrow(Act))){return(-1)}
#\ if passes check, column bind the subjectID and activity codes to the feature values
   ret<-cbind(subjID,Act,vectors)
#\ return assembled matrix
   ret
}

##\############################[ summary_mean(df) ]##################################
##\ Function to:
##\ ==(1)take a tidied data set in data frame format
##\ ==(2) discard all columns where the label indicates the data consists of standard 
##\   deviations ( ie contains the string "std")
##\ ==(3) use aggregate() to subset the data according to the factors recorded in the 
##\   first two columns and calculates the mean value of the subset 
##\ ==(4) edit the column labels for the two factors to meaningful values 
##\ ==(5) Output the summary to a specified text file  
####################################################################################
summary_mean<-function(df){
  print(dim(df))
  varNames<-colnames(df)
  df_mvals<-df[,-(grep("std",varNames,ignore.case=T))]  
  agg_mvals <-aggregate.data.frame(df_mvals[,-(1:2)], by=list(df_mvals$Subject,df_mvals$Activity),FUN=mean, na.rm=TRUE)
   
  nuLabs<-colnames(agg_mvals)
  nuLabs<-paste("Average<",nuLabs,">",sep="")
  nuLabs<-nuLabs[-(1:2)]
  nuLabs<-c("Subject","Activity",nuLabs)
  colnames(agg_mvals)<-nuLabs
  print(head(agg_mvals,15)); print(dim(agg_mvals))
  write.table(agg_mvals,"data_mean_summ.txt",row.name=F)
}