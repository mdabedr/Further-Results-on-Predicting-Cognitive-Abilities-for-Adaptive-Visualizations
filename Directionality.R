#Author: Md Abed Rahman abed90@cs.ubc.ca
#This file gives the direction for the top 10 features (Eye-tracking and interaction) for each user characteristic at each window 

library(caret)
library(doSNOW)
library(infotheo)
library(FSelector)

cl<-makeCluster(8) #number of CPU cores to be used
registerDoSNOW(cl)

results.df <- data.frame(slice=integer(),
                         dependent=character(),
                          feature=character(),
                         directionality=character())
#cat(..., file="log.txt", append=TRUE)

#out='C:/Users/abed90/Desktop/R code/Directionality Mixed.txt'
for (slice in seq(from=10, to=100, by=10)){
  
dir = 'C:/Users/abed90/Desktop/R_test/Restart/IJCAI/Input/Dataset/Mixed/'
#slice = 10


slicename = paste('DataSetMixed_', toString(slice), "_percent", ".csv", sep =
                    "")
#print(slicename) #  'Data_Screen3_0.65_60_percent.csv'
filename = paste(dir, slicename, sep = "")#by default it is blank, sep
data = read.table(filename, header = T, sep = ",")
data = na.omit(data) #filter missing data

#varimp-userchar_result_timeslice__10_all.csv
dir = 'C:/Users/abed90/Desktop/R_test/Restart/IJCAI/Output/Mixed Output/'

slicename = paste('varimp-userchar_result_timeslice__',toString(slice),"_all",".csv",sep = "")
#print(slicename) 

filename = paste(dir, slicename, sep = "")

csv_con = read.csv(filename)


#nrow(csv_con)
#if(length(grep("ab","abcd"))>0)


data_final = data.frame(matrix(nrow = nrow(data)))

#get the dependent variables from data
dependent_data = subset(data, select = c('VisWM', 'SpatialMemory','PS', 'VisualScan'))
dependent = c('VisWM', 'SpatialMemory','PS', 'VisualScan')
# for(u in 1: ncol(dependent)){
#
# }


#discretize the variables to be predicted using a median split
for (i in 1:length(dependent)) {
  if(dependent[i]=='VisWM'){ next }
  if (is.numeric(dependent_data[, dependent[i]])) {
    dep1 = discretize(dependent_data[, dependent[i]], "equalwidth", 2)
    dep2 = discretize(dependent_data[, dependent[i]], "equalfreq", 2)
    
    base1 = max(length(dep1$X[dep1$X == 1]), length(dep1$X[dep1$X == 2])) / length(dep1$X)
    base2 = max(length(dep2$X[dep2$X == 1]), length(dep2$X[dep2$X == 2])) / length(dep2$X)
    
    if (base1 < base2) {
      #find the better "median" split
      tempdep = dep1
    } else{
      tempdep = dep2
    }
    
    tempdep$X[tempdep$X == 1] = "Low"
    tempdep$X[tempdep$X == 2] = "High"
    dependent_data[, dependent[i]] = NULL
    dependent_data[, dependent[i]] = as.factor(tempdep$X)
  }
}

l=0;
#Needs changing later
for (i in 1:nrow(csv_con))
{
  if (length(grep('Overall', toString(csv_con[i, 1]))) > 0) {
    l=l+1
    if(l==5) break
   # print(i)
    for (j in (i + 1):(i + 10))  {
     # print(j)
      feat_name = gsub(" .*$", "", toString(csv_con[j, 1]))
     # print(feat_name)
      data_whatever = subset(data, select = c(feat_name))
      data_final = cbind(data_final, data_whatever)
      
      data_final = data_final[-c(1)]
      #data_final=cbind(data_final,subset(data,select = c('PS','VisWM','SpatialMemory','VisualScan')))
     # for (l in 1:ncol(dependent_data))
     # {
        for (k in 1:ncol(data_final)) {
          data_int = cbind(data_final[k], dependent_data[l])
          data_high = data.frame(data_int[dependent_data[l] == 'High', 1]) 
          data_low = data.frame(data_int[dependent_data[l] == 'Low', 1])
          
          if (mean(data_high[,1]) > mean(data_low[,1])) {
            #print(paste(mean(data_high[,1]),mean(data_low[,1]),sep=' '))
            #sink(out)
            temprow.df <- data.frame(slice=slice, dependent=colnames(data_int)[2], feature=colnames(data_int)[1],  directionality='+')
            
            results.df <- rbind(results.df, temprow.df)
            # slice=integer(),
            # feature=character(),
            # dependent=character(),
            # directionality=character()
            
            print(paste(colnames(data_int)[1], colnames(data_int)[2], '+', sep = ' '))
            #catf(paste(colnames(data_int)[1], colnames(data_int)[2], '+', sep = ' '))
            #sink()
          }
          else{
            #sink(out)
            temprow.df <- data.frame(slice=slice, dependent=colnames(data_int)[2],feature=colnames(data_int)[1],  directionality='-')
            
            results.df <- rbind(results.df, temprow.df)
           print(paste(colnames(data_int)[1], colnames(data_int)[2], '-', sep = ' '))
           # catf(paste(colnames(data_int)[1], colnames(data_int)[2], '-', sep = ' '))
            #sink()
          }
          #print('+')
          
        }
      
      #}
    }
    #l=l+1;
    i = i + 10
    
   # print(i)
  }
  

}

}
write.table(results.df, file="C:/Users/abed90/Desktop/R code/Directionality Mixed.txt", quote=FALSE, sep='  ', col.names = NA)

stopCluster(cl)


# for (j in (i + 1) : (i + 10)) {
#   print(j)
# }
