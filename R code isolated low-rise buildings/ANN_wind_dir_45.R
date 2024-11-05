##### KEY & INSTRUCTIONS ####

Matlab_data <- "C:/Users/xeb14148/Dropbox/Cp Paper/R code/Matlab file_wind_45"

#Packages to be installed

#install.packages("R.matlab")
#install.packages("neuralnet")
#install.packages("reshape2")
#install.packages("stringr")
#install.packages("ggplot2")
#install.packages("directlabels")

#Start
wind_direction <- c(45) #Wind Direction
azimuth <- c(0, 270, 180, 90, 270) #Set the Azimuth for all surfaces (surface 1,2,3,4,5)

library(R.matlab) #Load Matlab Package

#Attach data from Matlab
setwd(Matlab_data) #directory where matalab files are
Matlab_filenames = list.files(pattern="*.mat") #identify filenames with .mat
myfiles = lapply(Matlab_filenames, readMat) #read Matlab files

#Wind attack Angle Calculation
wind_attack_angle <- c()
angle=0
for(i in 1:length(azimuth)){
  for(j in 1:length(wind_direction)){
    
    ifelse(azimuth[i]+wind_direction[j]>=360, angle[i]<-azimuth[i]+wind_direction[j]-360, angle[i]<-azimuth[i]+wind_direction[j])
    wind_attack_angle<-c(wind_attack_angle,angle)
  }
}

wind_attack_angle <-wind_attack_angle[c(1,3,6,10,15)]

#Identify no. of columns for each model 
ncolumns=0 #initialize variable

for (i in 1:length(Matlab_filenames)){
  data_i <- myfiles[[i]]
  ncolumns[i] <- ncol(data_i$Wind.pressure.coefficients) #identify no. of columns of Cp data for each data 
}

#Calculate cp from timeseries of [i] models on TPU database
cp_final<-0
for(i in 1:length(ncolumns)){
  ncolumns_i <- ncolumns[i]
  data_i <- myfiles[[i]]
  cp <- colMeans(data_i$Wind.pressure.coefficients) #calculate the mean values of all columns 
  cp_in<-data.frame(cp) #create a dataframe
  cp_final<-c(cp_final,cp_in) #create an array with all meancp values 
}
cp_final<-cp_final[-1] #delete the first list


############################################# 
#Plot of TPU Cp Vs Point ID for all models
#############################################
setwd("C:/Users/xeb14148/Dropbox/Cp Paper/R code/Plots/Models") #Change the directory for saving plots

for (i in 1:length(Matlab_filenames)){
  a <- Matlab_filenames[i]
  b <- paste(a,".png" ,sep = "")
  plot(cp_final[[i]],  xlab = "Point ID",type = "l",
       ylab = "Time-averaged TPU Cp", main =c(Matlab_filenames[i], "All Surfaces"))
  dev.copy(png,filename=b)
  dev.off()
}

#Combine data of meancp and location of points in a dataframe for all models
data.location.and.cp.all<-data.frame()
for (i in 1:length(Matlab_filenames)){
  data_i <- myfiles[[i]]
  cp<-cp_final[[i]]
  rownames(data_i$Location.of.measured.points) <- c("x", "y", "Point.id", "surface") #assign names to rows of data  
  data.location.and.cp <- data.frame(t(data_i$Location.of.measured.points), cp, Matlab_filenames[i]) #transpose of data and include mean cp in data 
  
  assign(paste0("data.location.and.cp_", i), 
         data.location.and.cp<- data.location.and.cp[order(data.location.and.cp$surface),]) #sorting data according to surface number (ascending)
  
  data.location.and.cp.all <-rbind.data.frame(data.location.and.cp.all, data.location.and.cp) #creation of one dataframe with all models
}

surfaces<- unique(data.location.and.cp$surface) #identify the number of surfaces 

###################################################
#Plot of TPU Mean Cp Vs Point ID for surface [i] 
###################################################
setwd("C:/Users/xeb14148/Dropbox/Cp Paper/R code/Plots/Surfaces") #Change the directory for saving plots

for(j in 1:length(Matlab_filenames)){
  for(i in 1:length(surfaces)) {
    object=get(paste0('data.location.and.cp_', j))
    surface_only <- subset(object, surface == i)
    a <- Matlab_filenames[j]
    b <- i
    c <- 'wind'
    d <- wind_direction
    e <- paste( a,"surface" ,b,c,d,".png", sep = "_")
    plot(x = surface_only$Point.id, y = surface_only$cp, 
         type = "l",xlab = "Point number ID",ylab = "TPU time-averaged Cp", main =e)
    dev.copy(png, filename=e)
    dev.off() # closes png device
  }}

#Separation of models and surfaces 
for(i in 1:length(Matlab_filenames)){
  for(j in 1:length(surfaces)) {
    object=get(paste0('data.location.and.cp_', i)) 
    assign(paste0("cp_",i,"_surface_",j), data.frame(object[object$surface==j,c('x','y','cp','surface')])) #creation of dataframe for each model and surface
  }
}

#Calculation of aspect Ratios and wind attack angle of Surfaces for model [i]
for (i in 1:length(Matlab_filenames)){
  #Surface 1
  aspect_ratio1 <- c(myfiles[[i]]$Building.height/myfiles[[i]]$Building.breadth)
  object=get(paste0('cp_', i, '_surface_1'))
  object <- data.frame(object,
                       aspect_ratio1,
                       wind_attack_angle[1])
  colnames(object)<- c('x','y','cp','surface','aspect_ratio', 'wind_attack')
  assign(paste0("cp_",i,"_surface_1"), data.frame(object))
  
  #Surface 2
  aspect_ratio2 <- c(myfiles[[i]]$Building.height/myfiles[[i]]$Building.depth)
  object=get(paste0('cp_', i, '_surface_2'))
  object<- data.frame(object,
                      aspect_ratio2,
                      wind_attack_angle[2])
  colnames(object)<- c('x','y','cp','surface','aspect_ratio', 'wind_attack')
  assign(paste0("cp_",i,"_surface_2"), data.frame(object))
  
  #Surface 3
  aspect_ratio3 <- c(myfiles[[i]]$Building.height/myfiles[[i]]$Building.breadth)
  object=get(paste0('cp_', i, '_surface_3'))
  object <- data.frame(object, 
                       aspect_ratio3,
                       wind_attack_angle[3])
  colnames(object)<- c('x','y','cp','surface','aspect_ratio', 'wind_attack')
  assign(paste0("cp_",i,"_surface_3"), data.frame(object))
  
  
  #Surface 4
  aspect_ratio4<- c(myfiles[[i]]$Building.height/myfiles[[i]]$Building.depth)
  object=get(paste0('cp_', i, '_surface_4'))
  object <- data.frame(object, 
                       aspect_ratio4,
                       wind_attack_angle[4])
  colnames(object)<- c('x','y','cp','surface','aspect_ratio', 'wind_attack')
  assign(paste0("cp_",i,"_surface_4"), data.frame(object))
  
  
  #Surface 5
  aspect_ratio5 <- c(myfiles[[i]]$Building.breadth/myfiles[[i]]$Building.depth)
  object=get(paste0('cp_', i, '_surface_5'))
  object <- data.frame(object, 
                       aspect_ratio5,
                       wind_attack_angle[5])
  colnames(object)<- c('x','y','cp','surface','aspect_ratio', 'wind_attack')
  assign(paste0("cp_",i,"_surface_5"), data.frame(object))
  
}

#Normalisation of x.points and y.points (normalise each surface for each data)
all_data_scaled_surface<-data.frame() #initialize dataframe for all data 
for (i in 1:length(Matlab_filenames)){
  for(j in 1:length(surfaces)){
    object=get(paste0("cp_",i,"_surface_",j))
    keeps <- c("x","y") #keep only the wanted variables 
    object<- object[keeps] #amendment of dataframe according to wanted variables 
    surface <- get(paste0("cp_",i,"_surface_",j))$surface #array with only the surface numbers, to be added further down
    wind_attack <- get(paste0("cp_",i,"_surface_",j))$wind_attack 
    maxs <- apply(object, 2, max) #find the max of each variable 
    mins <- apply(object, 2, min) #find the min of each variable 
    scaled <- as.data.frame(scale(object, center = mins, scale = maxs - mins)) #scale the data 
    scaled <- data.frame(scaled,surface, wind_attack)#, aspect_ratio, wind_attack) #include surface numbers to scaled data
    #assign(paste0('cp_',i,"_scaled_surface_", j), scaled) #creation of dataframe for each model and surface_scaled data
    all_data_scaled_surface <-rbind.data.frame(all_data_scaled_surface, scaled)#creation of one dataframe with all models
  }
}

#Normalisation of aspect_ratio and time-averaged Cp
for(i in 1:length(Matlab_filenames)){
  for(j in 1:length(surfaces)){
    object3=get(paste0('cp_',i,'_surface_',j))
    keeps<- c("aspect_ratio")
    object3<-object3[keeps]
    assign(paste0('aspect',i,'sur',j),object3)
  }
}
allaspectratio<-data.frame()
for(i in 1:length(Matlab_filenames)){
  object1=get(paste0('aspect',i,'sur1'))
  object2=get(paste0('aspect',i,'sur2'))
  object3=get(paste0('aspect',i,'sur3'))
  object4=get(paste0('aspect',i,'sur4'))
  object5=get(paste0('aspect',i,'sur5'))
  u<-rbind.data.frame(object1,  #assign(paste0('model',i, 'aspect ratio'), 
                      object2,
                      object3,
                      object4, 
                      object5)
  allaspectratio<-rbind.data.frame(allaspectratio,u)
}
aspect_ratio<-allaspectratio  
data.location.and.cp.all<-data.frame(data.location.and.cp.all,
                                     aspect_ratio)  

#Normalisation per whole data
objecty<-data.location.and.cp.all
keeps<- c("cp", "aspect_ratio")
objecty<-objecty[keeps]
maxs <- apply(objecty, 2, max) #find the max of each variable 
mins <- apply(objecty, 2, min) #find the min of each variable 
scaled <- as.data.frame(scale(objecty, center = mins, scale = maxs - mins)) #scale the data 
allscaleddata <-data.frame(scaled)#creation of one dataframe with all models
all_data_scaled_surface<-data.frame(all_data_scaled_surface,
                                    allscaleddata)

#Rearrangement of columns before ANN
data.location.and.cp.all <- data.frame(data.location.and.cp.all,
                                       all_data_scaled_surface$wind_attack)
colnames(data.location.and.cp.all)<- c('x','y','point.id','surface','cp','Matlab filename','aspect_ratio','wind_attack')
data.location.and.cp.all<-data.location.and.cp.all[,c(1,2,7,3,4,5,8,6)]
all_data_scaled_surface<-all_data_scaled_surface[,c(1,2,6,4,3,5)]

#Export Results in CSV
setwd("C:/Users/xeb14148/Dropbox/Cp Paper/R code") #Change the directory for saving plots
a <- 'wind direction'
b <- wind_direction
filename <- paste(a,b,'.csv', sep = " ")
write.csv(data.location.and.cp.all, file=filename)

a <- 'scaled wind direction'
b <- wind_direction
filename <- paste(a,b, '.csv', sep =" ")
write.csv(all_data_scaled_surface, file=filename)


#######################################################
#Basic contour graph for Model [i] and surface [j] 
#######################################################
setwd("C:/Users/xeb14148/Dropbox/Cp Paper/R code/Plots/Contour graphs") #Change the directory for saving plots

for (i in 1 : length(Matlab_filenames)){
  for (j in 1 : length(surfaces)){
    object1= get(paste0('cp_',i,'_surface_',j))
    Matlab_filenames_i <- Matlab_filenames[i] 
    surfaces_j<-surfaces[j]
    data.loess <- loess(cp ~ x * y, data = object1)
    xgrid <-  seq(min(object1$x), max(object1$x), 1)# Create a sequence of incrementally increasing (by 1 units) values for both x and y
    ygrid <-  seq(min(object1$y), max(object1$y), 1)
    data.fit <-  expand.grid(x = xgrid, y = ygrid) # Generate a dataframe with every possible combination of x and y
    mtrx3d <-  predict(data.loess, newdata = data.fit) # Feed the dataframe into the loess model and receive a matrix output with estimates of Mean Cp for each combination of x and y
    a <- Matlab_filenames_i
    b <- 'surface'
    c <- surfaces_j
    d <- "contour"
    #e <- round(runif(1), digits = 3)
    f <- paste(a,b,c,d,".png", sep = "_")
    g <- paste(a,b,c, sep='_')
    contour(x = xgrid, y = ygrid, z = mtrx3d, main=g)
    dev.copy(png,filename=f)
    dev.off()
   dev.off()
  }
}

#Artificial Neural Networks for surface [i]
setwd("C:/Users/xeb14148/Dropbox/Cp Paper/R code/Plots/ANNs") #Change the directory for saving plots

library(neuralnet) #load neuralnet package

hidden<- c(7,5,3)
RMSE_error <- c() #creation of vector
Surface_no.<- c() #creation of vector
RMSE_loop_error<-0 #initialise variable
threshold_final <- 0 #initialise variable
threshold_values <- c(0.001)
allsurfaces <- unique(all_data_scaled_surface$surface) #retrieving surface no. from dataframe

#Creation of ANN loop for surface[i] 

for(j in 1:length(threshold_values)){
  for(i in 1:length(surfaces)) {
    surface.i <- allsurfaces[i]
    surface_loop<-data.frame(all_data_scaled_surface[all_data_scaled_surface$surface==i,c('x','y','aspect_ratio','cp')]) #dataframe with scaled values for surface [i]
    
    surface_loop_unscaled <- 
      data.frame(data.location.and.cp.all[data.location.and.cp.all$surface==i,c('x','y', 'aspect_ratio','cp')]) #dataframe with unscaled values for surface [i]
    
    threshold<-threshold_values[j]
    
    #Splitting the dataset to training and validation data_unscaled 
    index <- sample(1:nrow(surface_loop),round(0.8*nrow(surface_loop))) # 0.8 is the separation percentage of training and validation data
    
    #Splitting the dataset to training and validation data_scaled
    train_ <- surface_loop[index,] #scaled training data 
    test_ <- surface_loop[-index,] #scaled validation data
    
    #Fitting of neural network
    input<-names(train_) #export the name of the columns
    f <- as.formula(paste("cp ~", paste(input[!input %in% "cp"], collapse = " + ")))
    nn <- neuralnet(f,train_,hidden=hidden,threshold_values[j],1e9, 2)  # training of neural networks
    
    #Saving nn training in an array
    a <- 'wind direction'
    b <- wind_direction
    c <- '_surface'
    d <- i
    e <- paste(a,b,c,d,'.rds')
    saveRDS(nn, file=e) #saving the training in order to use it later
    
    #Predict new output(mean Cp) with scaled validation data 
    pr.nn_ <- compute(nn,test_[,1:3])  ##[,1:2] is there because we choose only x and y not mean cp (input variables)
    
    #Scaling back the data   
    pr.nn <- pr.nn_$net.result*(max(surface_loop_unscaled$cp)-min(surface_loop_unscaled$cp))+min(surface_loop_unscaled$cp)
    
    test.r <- (test_$cp)*(max(surface_loop_unscaled$cp)-min(surface_loop_unscaled$cp))+min(surface_loop_unscaled$cp)
    
    #Saving validation results
    validation <- data.frame(pr.nn, test.r)
    a<- "Validation_"
    b<- "Surface_"
    c<- i
    d<- ".csv"
    e<- paste(a,b,c,d, sep = "")
    write.csv(validation, file=e)
    
    #Calculation of RMSE of neural network
    RMSE_loop_error[i] <- sqrt(sum((test.r - pr.nn)^2)/nrow(test_)) # mean square error of validation results
    
    #Calculation of scaled ANN Cp
    Data_new=data.frame(surface_loop$x, #creation of new dataframe of with input variables to calculate new Cp 
                        surface_loop$y,
                        surface_loop$aspect_ratio) 
    ANN_Cp_scaled <- compute(nn, Data_new)  #calculation of new Cp using the new dataframe, normalized results 
    
    #Scaling back the ANN Cp
    ANN_Cp <- (ANN_Cp_scaled$net.result)*(max(data.location.and.cp.all$cp)-min(data.location.and.cp.all$cp))+min(data.location.and.cp.all$cp)
    A<-"WD_0_surface_"
    B<-i
    C<- ".csv"
    E<- paste(A,B,C, sep = "")
    results <- data.frame(surface_loop_unscaled,ANN_Cp)
    write.csv(results, file=E)
    
    ###########################
    #Plot ANN of surface [i]
    ###########################
    
    a <- 'Wind'
    b <- wind_direction
    c <- "ANN_Plot_surface" 
    d <- i
    e <- round(runif(1), digits = 3)
    filename <- paste (a,b,c,d,e,"threshold",threshold_values[j],".png", sep = "_")
    plot(nn)
    dev.copy(png,filename=filename)
    dev.off() # closes png device
    dev.off() # closes RStudioGD device
    
    #################################################################### 
    #Plot of validation resutls (Predicted mean Cp Vs Test Mean Cp) 
    ####################################################################
    
    a <- 'Wind'
    b <- wind_direction
    c <- 'Validation_Results_surface'
    d <- i
    e <- round(runif(1), digits = 3)
    filename <- paste(a,b,c,d,e,"threshold",threshold_values[j], ".png", sep = "_")
    title <- paste(c,d)
    plot(test.r, pr.nn, xlab = "Test Cp", ylab = "Predicted Cp", main = title)
    abline(0,1, lwd=2)
    dev.copy(png,filename=filename)
    dev.off() #closes png device
    dev.off() #closes RStudioGD device
    
    #############################################
    #Plot of ANN Cp Vs TPU Cp for surface [i]
    ############################################# 
    
    a <- 'Wind'
    b <- wind_direction
    c <- "Results_ANN_Surface" 
    d <- i
    e <- round(runif(1), digits = 3)
    filename <- paste (a,b,c,d,e,"threshold",threshold_values[j],".png", sep = "_")
    title <- paste(c, d)
    plot(ANN_Cp, surface_loop_unscaled$cp,xlab='ANN Cp', ylab = 'TPU Cp' , main=title)  
    abline(0,1, lwd=2, col='red')
    dev.copy(png,filename=filename)
    dev.off()
    dev.off()
    
    #Cration of summary table 
    RMSE_error <- c(RMSE_error, RMSE_loop_error)
    Surface_no. <- c(Surface_no.,surface.i)
    threshold_final <- c(threshold_final, threshold)
    
  } 
} 

RMSE_error<- RMSE_error[c(1,3,6,10,15)]#,16,22,28,34,40,41,47,53,59,65,66,72,78,84,90)]
RMSE_error
threshold_final<-threshold_final[-1]
threshold_final
Summary_table_title <- paste('Flat Roof, Wind Direction 0, Hdden = 7,5,3')
Summary_table <- data.frame(date(),Surface_no., RMSE_error, threshold_final, Summary_table_title)
Summary_table

#write.table(Summary_table, "Summary_table.csv", append = T, sep = ",", row.names = F)
