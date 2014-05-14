setwd("J:/Subsurface/Reservoir Engineer Workspace/RE_Darat/DARAT_NEW_DEV1/Leytzher/DaratGasIPSM/IPSM_Models/MasterModel/DG3Model/scripts/mods")
library('fields')  #load library to use splint function for spline interpolation
library('zoo')
library('chron')

fname="Run_9"
filename<-paste(fname,".pxr", sep="")

################ functions ####################################

#Function to calculate cumulative production
CumGas <- function(gasRate, deltaTime)
{
  return(cumsum(gasRate*deltaTime)/10**6)
}

# Apply rate constraint
# this is point-wise

RateConstraint<- function(maxRate, lookupCum, lookupGasRate, deltaTime)
{
  newRate<-vector()  #create empty vector
  cumGasNew<-vector()
  if (lookupGasRate[1]>1000){
  newRate[1] <- maxRate  #initialize vector with *maxRate* 
  

  cumGasNew[1] <-0      #initialize vector with zero cum gas (time zero)
  
  
  for (i in 2:length(deltaTime))
  {
    cumGasNew[i]<- cumGasNew[i-1]+(newRate[i-1]*deltaTime[i-1])/10**6
    
    #interpolate in table and find new rate
    interpRate<-splint(lookupCum,lookupGasRate,cumGasNew[i])
    
    if (interpRate > maxRate){
      newRate[i]<-maxRate
    } 
    else{ newRate[i]<-interpRate}
  }
  }
  else{
    newRate<-lookupGasRate}
  return(newRate)
}

###########################################################################################

#Load Data
data <- read.table(filename, sep="\t", header=TRUE)

# Convert DATE column to Date
data$DATE<-as.Date(data$DATE, '%m/%d/%Y')

#Calculate time difference
deltaTime<-append(0,diff(data$DATE))

#get gas rates
TBRate<-data$TB_GAS_KM3D
KLRate<-data$KL_GAS_KM3D
DHRate<-data$DH_GAS_KM3D
MKRate<-data$MK_GAS_KM3D



#Calculate cumGas for each well:
TBCumGas <- CumGas(TBRate, deltaTime)
KLCumGas <- CumGas(KLRate, deltaTime)
DHCumGas <- CumGas(DHRate, deltaTime)
MKCumGas <- CumGas(MKRate, deltaTime)

#create a dataframe with 4 columns: DATE, deltaTime, GasRate, CumGas
TBData<-data.frame(data$DATE, deltaTime, TBRate, TBCumGas)
KLData<-data.frame(data$DATE, deltaTime, KLRate, KLCumGas)
DHData<-data.frame(data$DATE, deltaTime, DHRate, DHCumGas)
MKData<-data.frame(data$DATE, deltaTime, MKRate, MKCumGas)

#Subset data to points where the rate is higher than zero
TBData<-TBData[TBData$TBRate>0,]
KLData<-KLData[KLData$KLRate>0,]
DHData<-DHData[DHData$DHRate>0,]
MKData<-MKData[MKData$MKRate>0,]

#Getting constrained rates
TBRateConstr<-RateConstraint(1000,TBData$TBCumGas,TBData$TBRate,TBData$deltaTime)
KLRateConstr<-RateConstraint(1000,KLData$KLCumGas,KLData$KLRate,KLData$deltaTime)
DHRateConstr<-RateConstraint(1000,DHData$DHCumGas,DHData$DHRate,DHData$deltaTime)
MKRateConstr<-RateConstraint(1000,MKData$MKCumGas,MKData$MKRate,MKData$deltaTime)

#create zoo objects with pairs of DATE and Constrained rate 

TBOut<-zoo(TBRateConstr,TBData$data.DATE )
KLOut<-zoo(KLRateConstr,KLData$data.DATE )
DHOut<-zoo(DHRateConstr,DHData$data.DATE )
MKOut<-zoo(MKRateConstr,MKData$data.DATE )

# monthly aggregation
TB.monthly<- aggregate(TBOut, as.yearmon, mean)
KL.monthly<- aggregate(KLOut, as.yearmon, mean)
DH.monthly<- aggregate(DHOut, as.yearmon, mean)
MK.monthly<- aggregate(MKOut, as.yearmon, mean)

#yearly aggregation
TB.yr<-aggregate(TB.monthly, floor, mean)
KL.yr<-aggregate(KL.monthly, floor, mean)
DH.yr<-aggregate(DH.monthly, floor, mean)
MK.yr<-aggregate(MK.monthly, floor, mean)

#create dataframe with all aggregated values
final<- data.frame(TB.yr,KL.yr, DH.yr, MK.yr)
#create an extra column with totals
final["DaratGas_Gas"]=final$TB.yr+final$KL.yr+final$DH.yr+final$MK.yr

#get CGR from original forecast and apply to new transformed forecast
DGGas<-data$DG_TOT_GAS_KM3D
DGCond<-data$DG_TOT_COND_M3D
Dates<-data$DATE
DaratGas<-data.frame(Dates, DGGas,DGCond)
DaratGas["CGR"]<-DGCond/DGGas
DaratGas<-DaratGas[DaratGas$DGGas>0,]

#create a zoo object for CGR
CGR<-zoo(DaratGas$CGR,DaratGas$Dates)
CGR.monthly<-aggregate(CGR, as.yearmon,mean)
CGR.yr<-aggregate(CGR.monthly, floor, mean)

final["DaratGas_Cond"]= final$DaratGas_Gas*CGR.yr
outname<-paste("J:/Subsurface/Reservoir Engineer Workspace/RE_Darat/DARAT_NEW_DEV1/Leytzher/DaratGasIPSM/IPSM_Models/MasterModel/DG3Model/scripts/mods/",fname,"_out.txt")
write.table(final, outname, sep='\t')
