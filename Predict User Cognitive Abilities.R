#Author 1: Sebastien Lalle lalles@cs.ubc.ca
#Author 2: Md Abed Rahman abed90@cs.ubc.ca
#This file is the code used for the papers listed in the repository. This code selects features manually and based on some feature selection
#methods and runs a Random Forest and LogitBoost to predict user cognitive abilities. 
library(caret)
library(doSNOW)
library(infotheo)
library(FSelector)

cl<-makeCluster(4) #number of CPU cores to be used
registerDoSNOW(cl)


filter_features_sets <- function(data, featset, dependent)  {   #"length" #Can't find anything
  
  aoinames = c("Scr3_deviation_chart", "Scr3_description", "Scr3_map", "Scr3_legend")
  aoinames_toremove=c('Help', 'Progress_popup', 'Scr1_instructions', 'Scr2_list_priorities', 'Scr2_description_priorities', 'Scr2_popup_comment', 'Scr2_popup_suggestion', 'Scr2_instruction', 'Scr3_instruction', 'Scr3_description', 'Scr3_deviation_chart', 'Scr3_map', 'Scr3_legend', 'Scr3_comment', 'Scr3_input', 'Scr3_tabs', 'Scr3_button_legend', 'Progress_bar' )
  aoinames_toremove=aoinames_toremove[!(aoinames_toremove %in% aoinames)]

  #features to be removed
  #toremove = c("Uid", "SessionID", "Layout", "UserConfused", "ConfusionClics", "Sc_type", "Confusion_in_Sc", "Code_confusion", "Sc_id", "length", "numfixations", "numsegments", 
	#						"InterceptError", "SlopeError", "InterceptErrorScaled", "SlopeErrorScaled", "InterceptTime", "SlopeTime","InterceptTimeScaled", "SlopeTimeScaled",
	#						"sumabspathangles" ,"sumfixationduration" ,"sumpathdistance" ,"sumrelpathangles",
	#						"numevents" ,"numleftclic" ,"numrightclic" ,"numdoubleclic" ,"numkeypressed",
	#						"nbactions", "nb_details.on.demand", "nb_swap", "nb_sorting", "nb_pump.increase","nb_pump.decrease","nb_resize", "TimeNeeded")
 toremove = c("PID","PS","VerWM","VisualScan","VisualLit","Locus","NeedCog","sumabspathangles","sumfixationduration","sumpathdistance","sumrelpathangles","numsegments","numfixations")
  toremove = c(toremove, aoinames_toremove)
  
  aoigeneralfeat = c('fixationrate','numfixations','totaltimespent','proportionnum','proportiontime','longestfixation', 'timetofirstfixation','timetolastfixation',
                    'numevents', 'numleftclic', 'numrightclic', 'numdoubleclic', 'leftclicrate', 'rightclicrate', 'doubleclicrate',
                    'timetofirstleftclic', 'timetofirstrightclic', 'timetofirstdoubleclic')
  aoigeneralfeat_toremove=c('numfixations','totaltimespent','numevents', 'numleftclic', 'numrightclic', 'numdoubleclic', 'leftclicrate', 'rightclicrate', 'doubleclicrate','timetofirstleftclic', 'timetofirstrightclic', 'timetofirstdoubleclic')
  for(i in 1:length(aoinames_toremove)){
    toremove = c(toremove, paste(aoinames_toremove[i], aoigeneralfeat, sep="_"))
    toremove = c(toremove, paste(aoinames_toremove[i], "numtransfrom",aoigeneralfeat, sep="_"))
    toremove = c(toremove, paste(aoinames_toremove[i], "proptransfrom",aoigeneralfeat, sep="_"))
    toremove = c(toremove, paste(aoinames_toremove[i], "proptransfrom",aoinames, sep="_"))
    toremove = c(toremove, paste(aoinames_toremove[i], "numtransfrom",aoinames, sep="_"))
  }
  
  for(i in 1:length(aoinames_toremove)){
    toremove=c(toremove, paste(aoinames_toremove[i], "numtransfrom",aoinames_toremove, sep="_"))
    toremove=c(toremove, paste(aoinames_toremove[i], "proptransfrom",aoinames_toremove, sep="_"))
    
  }
  
  for(i in 1:length(aoinames)){
    
    toremove = c(toremove, paste(aoinames[i], "numtransfrom",aoigeneralfeat, sep="_"))
    toremove = c(toremove, paste(aoinames[i], aoigeneralfeat_toremove, sep="_"))
    toremove = c(toremove, paste(aoinames[i], "numtransfrom",aoigeneralfeat_toremove, sep="_"))
    toremove = c(toremove, paste(aoinames[i], "numtransfrom",aoinames, sep="_"))
    toremove = c(toremove, paste(aoinames[i], "proptransfrom",aoinames_toremove, sep="_"))
    toremove = c(toremove, paste(aoinames[i], "numtransfrom",aoinames_toremove, sep="_"))
  }
  # print(toremove)
  # aoifeats = c()
  # for(i in 1:length(aoinames)){
  #   aoifeats = c(aoifeats, paste(aoinames[i], aoigeneralfeat, sep="_"))
  #   aoifeats = c(aoifeats, paste(aoinames[i], "numtransfrom",aoigeneralfeat, sep="_"))
  #   aoifeats = c(aoifeats, paste(aoifeats[i], "proptransfrom",aoigeneralfeat, sep="_"))
  # }
  
   #AOI	features to be removed as well
#    for(i in 1:length(aoinames)){
# 		toremove = c(toremove, paste(aoinames, "numevents", sep="_"))
# 		toremove = c(toremove, paste(aoinames, "numleftclic", sep="_"))
# 		toremove = c(toremove, paste(aoinames, "numrightclic", sep="_"))
# 		toremove = c(toremove, paste(aoinames, "numdoubleclic", sep="_"))
# 		toremove = c(toremove, paste(aoinames, "rightclicrate", sep="_"))
# 		toremove = c(toremove, paste(aoinames, "timetofirstrightclic", sep="_"))
# 	}
	toremove = toremove[!(toremove %in% dependent)] #keep dependent variables!
  pupilfeat = c("endpupilsize", "maxpupilsize", "minpupilsize", "startpupilsize", "meanpupilsize", "stddevpupilsize")#pupilok and distancefeatok	
  distancefeat = c("maxdistance", "meandistance", "mindistance", "startdistance", "enddistance", "stddevdistance")
  
  eventsfeat = c('numevents', 'numleftclic', 'numrightclic', 'numdoubleclic', 'numkeypressed', 'leftclicrate', 'rightclicrate', 'doubleclicrate', 'keypressedrate', 
                 'timetofirstleftclic', 'timetofirstrightclic', 'timetofirstdoubleclic', 'timetofirstkeypressed')
#   gazetimefeat = c()
#   for(i in 1:length(aoinames)){
# 		#events in AOIs
# 		eventsfeat = c(eventsfeat, paste(aoinames[i], "leftclicrate", sep="_"))
# 		eventsfeat = c(eventsfeat, paste(aoinames[i], "doubleclicrate", sep="_"))
# 		eventsfeat = c(eventsfeat, paste(aoinames[i], "timetofirstleftclic", sep="_"))
# 		eventsfeat = c(eventsfeat, paste(aoinames[i], "timetofirstdoubleclic", sep="_"))
# 		
# 		#time gaze feats in AOI
# 		gazetimefeat = c(gazetimefeat, paste(aoinames[i], "numfixations", sep="_"))
# 		gazetimefeat = c(gazetimefeat, paste(aoinames[i], "timetolastfixation", sep="_"))
# 		gazetimefeat = c(gazetimefeat, paste(aoinames[i], "totaltimespent", sep="_"))
# 		for(j in 1:length(aoinames)){
# 			gazetimefeat = c(gazetimefeat, paste(aoinames[i], "numtransfrom", aoinames[j], sep="_"))
# 		}
# 	}
  
  #actionsfeat = c("actionrate","timetofirstaction","details.on.demand_rate","timetofirst_details.on.demand","swap_rate", "timetofirst_swap",
	#						"sorting_rate","timetofirst_sorting","pump.increase_rate","timetofirst_pump.increase","pump.decrease_rate",
	#						"timetofirst_pump.decrease","resize_rate","timetofirst_resize")
  
  
   # data = data[, !(colnames(data) %in% toremove)]#Remove the columns specified in to remove??!!!!
  
  #Filter features based on the feature set to be used
  if(featset == "userchar"){
    data = data[, (colnames(data) %in% c(dependent))]
  }
  else if(featset == "distance"){
    data = data[, (colnames(data) %in% c(dependent, distancefeat))]
  }
  else if(featset == "events"){
    data = data[, (colnames(data) %in% c(dependent, eventsfeat))]
  }
  else if(featset == "actions"){
    data = data[, (colnames(data) %in% c(dependent, actionsfeat))]
  }
  else if(featset == "pupil"){
    data = data[, (colnames(data) %in% c(dependent, pupilfeat))]
  }
  # else if(featset == "gaze"){
  #   data = data[, !(colnames(data) %in% c(toremove, pupilfeat, distancefeat, eventsfeat, actionsfeat))]
  # }
  else if(featset == "gaze"){
    data = data[, !(colnames(data) %in% c(toremove, pupilfeat, distancefeat, eventsfeat))]
  }
  else if(featset == "gazenotime"){
    data = data[, !(colnames(data) %in% c(toremove, pupilfeat, distancefeat, eventsfeat, actionsfeat, gazetimefeat))]
  }
  else if(featset == "gaze+pupil"){
    data = data[, !(colnames(data) %in% c(toremove, distancefeat, eventsfeat, actionsfeat))]
  }
  else if(featset == "all"){
    data = data[, !(colnames(data) %in% c(toremove))]
  }
  else if(featset == "allnotime"){
    data = data[, !(colnames(data) %in% c(toremove, gazetimefeat))]
  }
  else if(featset == "nodistance"){
    data = data[, !(colnames(data) %in% c(toremove, distancefeat))]
  }

  return(data)
}


#==========================================
dir = "/Users/Hunter/Desktop/R_test/Input/"
dependent = c("VisWM","SpatialMemory") #Variables to be predicted
# dependent = c("PS", "VisWM") #Variables to be predicted
feat_set = "gaze" #feature set for the prediction
feat_selection = FALSE #TRUE to Apply feature selection  ; FALSE otherwise
#aoiset = "grid2x2" #AOI to be used for gaze #change for aoiset used anywhere
start_slice = 10 #start slice
# end_slice = 10 # end slice 
end_slice = 100 # end slice 
#GOTTA CHANGE end slice
thres=c(0.5,0.65,0.75)
slicename='Data_Screen3_0.65_60_percent.csv'
dirout = "/Users/Hunter/Desktop/R_test/Output/"

for(threshold in thres){
remove_completed_trials = FALSE #remove users who already finished the task at the current slice
#output result files  'Data_Screen3_0.5_10_percent.csv
for(slice in seq(start_slice, end_slice, 10)){
out = paste(dirout, "predict-userchar_result_full_v2_",toString(threshold),"_",toString(slice),".csv",sep="")
sink(out)
print("Userchar,Slice,Features,Logit_Accuracy,Logit_AccuracySD,Logit_Kappa,RF_Accuracy,RF_AccuracySD,RF_Kappa,Baseline")
sink()

#Output features importance
outvarimp = paste(dirout, "varimp-userchar_result_timeslice_",toString(threshold),"_",toString(slice),"_", feat_set, ".csv", sep="")
sink(outvarimp)
print("")
sink()

#Output confusion matrix
outconfmat = paste(dirout, "confmatrix-userchar_result_timeslice_", toString(threshold),"_",toString(slice),"_",feat_set, ".csv", sep="")
sink(outconfmat)
print("")
sink()

slicename=paste('Data_Screen3_',toString(threshold),"_",toString(slice),"_percent",".csv",sep="") 
print(slicename)#  'Data_Screen3_0.65_60_percent.csv'
filename=paste(dir,slicename,sep="")#by default it is blank, sep 
	data = read.table(filename, header=T, sep=",")
	data=na.omit(data) #filter missing data
	
	if(remove_completed_trials){
		data = subset( data,  length>=slice*1000-25) #why this particular value??!!!
	}

	#==========================================
	#discretize the variables to be predicted using a median split
	for(i in 1:length(dependent)){
		if(is.numeric(data[, dependent[i]])){
			dep1 = discretize(data[, dependent[i]], "equalwidth", 2)
			dep2 = discretize(data[, dependent[i]], "equalfreq", 2)
				
			base1 = max(length(dep1$X[dep1$X==1]), length(dep1$X[dep1$X==2])) / length(dep1$X)
			base2 = max(length(dep2$X[dep2$X==1]), length(dep2$X[dep2$X==2])) / length(dep2$X)
			
			if(base1 < base2){ #find the better "median" split
				tempdep = dep1
			}else{
				tempdep = dep2
			}
			
			tempdep$X[tempdep$X==1] = "low"
			tempdep$X[tempdep$X==2] = "high"
			data[, dependent[i]] = NULL
			data[, dependent[i]] = as.factor(tempdep$X)
		}
	}
	#Not Sure as what happened here
	
	data = filter_features_sets(data, feat_set,dependent)
	summary(data)
	
	#for all user characteristics
	for(dep in 1:length(dependent))
	{
		dependent2 = dependent[dependent != dependent[dep]] 
		data2 = data[, !(colnames(data) %in% dependent2)] #keep only one dependent variable for the prediction
		
		#baseline
		base = max(summary(data[, dependent[dep]])[1], summary(data[, dependent[dep]])[2]) / length(data[, dependent[dep]])
		traindata=data2
		#feat selection
		if(feat_selection){
			#METHOD 1: CFS
			#fmla <- as.formula(paste("data2$", dependent[dep],"~", paste("data2$", colnames(data2[, colnames(data2) != dependent[dep]]), collapse= "+", sep=""), sep=""))
			#subset = cfs(fmla, data2)
			#fmla <- as.simple.formula(  paste(subset),   paste("data2$", dependent[dep], sep=""))
			
			#METHOD 2: Correl
			data_cor = cor(traindata[, colnames(traindata) != dependent[dep]])
			data_cor[is.na(data_cor)] = 1
			corfeat = findCorrelation(data_cor, cutoff = .8)
			if(length(corfeat) > 0){
				traindata =  traindata[,-corfeat]
			}
			#fmla <- as.formula(paste("traindata2$", dependent[dep],"~", paste("traindata2$", colnames(traindata2[, colnames(traindata2) != dependent[dep]]), collapse= "+", sep=""), sep=""))
			fmla <- as.formula(paste("traindata$", dependent[dep],"~.", sep=""))
		}else{
			fmla <- as.formula(paste(dependent[dep],"~", paste(colnames(traindata[, colnames(traindata) != dependent[dep]]), collapse= "+", sep=""), sep=""))
		}
		
		#==========================================
#summary(traindata)
		#ML design: 
		 # ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 15, allowParallel=T) #20-runs 10-folds CV 
		  ctrl <- trainControl(method = "cv", number = 10, allowParallel=T) #10-folds CV (no repeat)
		# ctrl <- trainControl(method = "LOOCV", allowParallel=T) #LOOCV 
		
		# #SVM with Linear Kernel
		# svmGrid <-  expand.grid(C = c(1))
		# fitSvm <- train(fmla,
		# 			 data = traindata,
		# 			 method = "svmLinear",
		# 			 trControl = ctrl,
		# 			 tuneGrid = svmGrid,
		# 			 metric="Accuracy")
		# 
		# resSvm = confusionMatrix(fitSvm)
		# accSvm = fitSvm$results[2]
		# sdSvm = fitSvm$results[4]
		# kappaSvm = fitSvm$results[3]

		
		#LogicBoost
		tuneparam	 <-  expand.grid( nIter=c(50))#????
		fitLog <- train(fmla,
						 data = traindata,
						 method = "LogitBoost",
						 trControl = ctrl,
						 tuneGrid = tuneparam,
						 metric="Accuracy")
						 
		resLog = confusionMatrix(fitLog)
		accLog = fitLog$results[2]
		sdLog = fitLog$results[4]
		kappaLog = fitLog$results[3]	 
		

		#Random Forest
		# tuneparam	 <-  expand.grid(mtry=c(100))
		tuneparam	 <-  expand.grid(mtry=c(8))
		fitRf <- train(fmla,
							 data = traindata,
							 method = "rf",
							 trControl = ctrl,
							 tuneGrid = tuneparam,
							 metric="Accuracy")

		resRf = confusionMatrix(fitRf)
		accRf = fitRf$results[2]
		sdRf = fitRf$results[4]
		kappaRf = fitRf$results[3]
							 					
												
		# #Stochastic Gradient Boosting
		# tuneparam <-  expand.grid(interaction.depth = c(2),    n.trees = c(100),    shrinkage = 0.1, n.minobsinnode=10)
		# fitSgb <- train(fmla,
		# 			 data = traindata,
		# 			 method = "gbm",
		# 			 trControl = ctrl,
		# 			 tuneGrid = tuneparam,
		# 			 metric="Accuracy")
		# 
		# resSgb = confusionMatrix(fitSgb)
		# accSgb = fitSgb$results[2]
		# sdSgb = fitSgb$results[4]
		# kappaSgb = fitSgb$results[3]
		
		
		#Export results
		#Export accuracies
		sink(out, append = TRUE)
		print(paste(dependent[dep], slicename, feat_set, accLog, sdLog, kappaLog, accRf, sdRf, kappaRf, base, sep=",") )
		# print(paste(dependent[dep], slicename, feat_set, accSvm, sdSvm, kappaSvm, accLog, sdLog, kappaLog, accRf, sdRf, kappaRf, accSgb, sdSgb, kappaSgb, base, sep=",") )
		sink()
		
		#Export feature importance
		sink(outvarimp, append = TRUE)
		# print( paste("===", dependent[dep], "SVM", slicename, sep=",") )
		# print(varImp(fitSvm))
		print( paste("===", dependent[dep], "Logit", slicename, sep=",") )
		print(varImp(fitLog))
		print( paste("===", dependent[dep], "RF", slicename, sep=",") )
		print(varImp(fitRf))
		# print( paste("===", dependent[dep], "SGB", slicename, sep=",") )
		# print(varImp(fitSgb))
		 print("")
		sink()
		
		#Export confusion matrix
		sink(outconfmat, append = TRUE)
		# print( paste("===", dependent[dep], "SVM", slicename, sep=",") )
		# print(resSvm)
		print( paste("===", dependent[dep], "Logit", slicename, sep=",") )
		print(resLog)
		print( paste("===", dependent[dep], "RF", slicename, sep=",") )
		print(resRf)
		# print( paste("===", dependent[dep], "SGB", slicename, sep=",") )
		# print(resSgb)
		print("")
		sink()
		# print("Everything done alright")
	}#end user char

}#end slice

}#end threshold


stopCluster(cl)