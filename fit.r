setwd("D:\\weather\\tn\\signaltime")
da=read.csv("expanded.csv")
colnames(da)=c('name','yr','hrinyr','mth','day','hr','lat','lon','pres','str','s0','sm6','sm12','sm24','sp6','sp12','sp24','latm6','lonm6','strm6','dist','bear','radspd')

head(da)

#Do for signal 1 or above
#Retain important columns - 6hr
dat=da[da$sm6<9990 & da$hr%in%c(0,6,12,18), c(1,2,3,4,5,6,7,8,10,11,12,18,19,20,21,22,23)]
dat$s0=(dat$s0>=1)+0; dat$sm6=(dat$sm6>=1)+0
dat$s0 = as.factor(dat$s0); dat$sm6 = as.factor(dat$sm6); 
dat$dstr=dat$str-dat$strm6
dat[dat$name=='RAMMASUN' & dat$day==18 & dat$hr==6,'str']=130
dat=dat[!dat$name=='RANCISCO',]
dat$bear=as.numeric(as.character(dat$bear))

#Potential covariates: current position + str, rad spd, sm6, change in str past 6 hr?
# obj=glm(s0~lat+lon+str+radspd+sm6+dstr,dat=dat,family="binomial")
# summary(obj)
# table(dat$s0,predict(obj,type="response")>0.5)

#Effect of signal 6 hours ago?
# obj=glm(s0~lat+lon+str+radspd+dstr,dat=dat,family="binomial")
# summary(obj)
# table(dat$s0,predict(obj,type="response")>0.5)

# summary(glm(s0~lat+lon+str+radspd+sm6+dstr,dat=dat,family="binomial"))
# summary(glm(s0~lat+I(lat^2)+lon+I(lon^2)+str+radspd+sm6+dstr,dat=dat,family="binomial"))
# summary(glm(s0~dist+I(dist^2)+cos(bear)+sin(bear)+str+radspd+sm6+dstr,dat=dat,family="binomial"))

#gam?
library(mgcv)
obj=bam(s0~te(lat,lon,str,k=c(5,5,3))+s(radspd,bs="cr",k=10)+sm6,dat=dat,family="binomial")
summary(obj); obj$aic #T1 2343 / T3 1718
#save(obj,file="T8.RData")


# obj=gam(s0~te(lat,lon,str,k=4)+radspd+sm6+dstr,dat=dat,family="binomial") #2384
# obj=bam(s0~te(lat,lon,str,k=5)+radspd+sm6+dstr,dat=dat,family="binomial")
# summary(obj); obj$aic #2367
# obj=bam(s0~te(lat,lon,str,k=6)+radspd+sm6+dstr,dat=dat,family="binomial")
# summary(obj); obj$aic #2350
# table(dat$s0,predict(obj,type="response")>0.5)
# obj=bam(s0~te(lat,lon,str,k=5)+s(radspd,bs="cr",k=10)+sm6+dstr,dat=dat,family="binomial")
# summary(obj); obj$aic #2334
# obj=bam(s0~te(lat,lon,str,k=5)+s(radspd,bs="cr",k=10)+sm6,dat=dat,family="binomial")
# summary(obj); obj$aic #2341
# obj=bam(s0~ti(lat,k=15)+ti(lon,k=15)+ti(str,k=10)+ti(lat,lon,k=15)+ti(lat,str,k=10)+ti(lon,str,k=10)+ti(lat,lon,str,k=c(10,10,5))+s(radspd,bs="cr",k=10)+sm6,dat=dat,family="binomial")
# summary(obj); obj$aic #2303

#Predictive ability
preds=as.numeric(predict(obj,type="response")>0.5)
rows=(1:nrow(dat))[dat$s0!=dat$sm6]; rows=sort(unique(c(rows,rows-1,rows+1)))
ndf=data.frame(dat$name,dat$lat,dat$lon,dat$dist,dat$sm6,dat$s0,preds)[rows,]
ndf
table(ndf$dat.s0,ndf$preds)
data.frame(dat[6214:6258,],preds=preds[6214:6258])

# #Predict on real data (this is on server.r now)

# #Parameters
# latlim=c(145,275); lonlim=c(1055,1225); strlim=c(22,130); radspdlim=c(-263,268)
# t0=c(180,1181,65)
# t12=c(187,1148,70)
# t24=c(198,1116,75)
# t36=c(208,1083,70)
# t48=c(216,1055,45)
# t72=c(226,1005,20)
# cursig1=1
# cursig3=0
# cursig8=0
# #Populate prediction data frame
# newdata=data.frame()
# for(i in seq(0,48,by=6)){
	# if(i %% 12 == 0){
		# vv=get(paste0('t',i))
		# if(!is.null(vv)) newdata=rbind(newdata,c(i,vv))
	# }else{
		# vv=get(paste0('t',i+6))
		# if(!is.null(vv)){
			# ww=get(paste0('t',i-6)); newdata=rbind(newdata,c(i,(vv+ww)/2))
		# }
	# }
# }
# for(i in seq(54,72,by=6)){
	# if(i == 72){
		# vv=get(paste0('t',i))
		# if(!is.null(vv)) newdata=rbind(newdata,c(i,vv))
	# }else{
		# vv=t72; ww=t48
		# if(!is.null(vv) & !is.null(ww)){
			# frac=(i-48)/24;	newdata=rbind(newdata,c(i,vv*frac+ww*(1-frac)))
		# }
	# }
# }
# colnames(newdata)=c('tt','lat','lon','str')
# newdata$radspd=NA
# for(i in 2:nrow(newdata)){
	# lat1=newdata$lat[i]; lon1=newdata$lon[i]
	# lat0=newdata$lat[i-1]; lon0=newdata$lon[i-1]
	# rs=sqrt((223-lat1)^2+(1142-lon1)^2*cos((223+lat1)/2/10/180*3.14159)^2)*11.13-sqrt((223-lat0)^2+(1142-lon0)^2*cos((223+lat0)/2/10/180*3.14159)^2)*11.13
	# rs=max(min(rs,radspdlim[2]),radspdlim[1])
	# newdata$radspd[i] = rs
# }

# #Do the prediction
# nd0 <- newdata; nd0$sm6=0
# nd1 <- newdata; nd1$sm6=1
# load('fits.rdata')
# nd0$T1p1=predict(objt1,newdata=nd0,type="response")
# nd1$T1p1=predict(objt1,newdata=nd1,type="response")
# nd0$T3p1=predict(objt3,newdata=nd0,type="response")
# nd1$T3p1=predict(objt3,newdata=nd1,type="response")
# nd0$T8p1=predict(objt8,newdata=nd0,type="response")
# nd1$T8p1=predict(objt8,newdata=nd1,type="response")

# for(i in 2:nrow(nd0)){
	# if(nd0$lat[i] > latlim[2] | nd0$lat[i] < latlim[1] | nd0$lon[i] > lonlim[2] | nd0$lon[i] < lonlim[1]){
		# nd0$T1p1[i]=0; nd1$T1p1[i]=0;
		# nd0$T3p1[i]=0; nd1$T3p1[i]=0;
		# nd0$T8p1[i]=0; nd1$T8p1[i]=0;
	# }
# }
# nd0$T1p0=1-nd0$T1p1; nd1$T1p0=1-nd1$T1p1;
# nd0$T3p0=1-nd0$T3p1; nd1$T3p0=1-nd1$T3p1;
# nd0$T8p0=1-nd0$T8p1; nd1$T8p0=1-nd1$T8p1;

# #Create output data frame
# for(i in 1:nrow(nd0)){
	# if(i==1){
		# outdf=data.frame(tt=nd0$tt[i],T1at=NA,T1iss=NA,T1cnl=NA,T3at=NA,T3iss=NA,T3cnl=NA,T8at=NA,T8iss=NA,T8cnl=NA); next
	# }
	# outdf[i,]=NA
	# if(i==2){
		# outdf$tt[i]=nd0$tt[i]
		# if(cursig1==0){outdf$T1at[i]=nd0$T1p1[i];}else{outdf$T1at[i]=nd1$T1p1[i];}
		# if(cursig3==0){outdf$T3at[i]=nd0$T3p1[i];}else{outdf$T3at[i]=nd1$T3p1[i];}
		# if(cursig8==0){outdf$T8at[i]=nd0$T8p1[i];}else{outdf$T8at[i]=nd1$T8p1[i];}
		# outdf$T1iss[i]=nd0$T1p1[i];	outdf$T1cnl[i]=nd1$T1p0[i]
		# outdf$T3iss[i]=nd0$T3p1[i];	outdf$T3cnl[i]=nd1$T3p0[i]
		# outdf$T8iss[i]=nd0$T8p1[i];	outdf$T8cnl[i]=nd1$T8p0[i]
	# } else {
		# outdf$tt[i]=nd0$tt[i]
		# outdf$T1at[i]=outdf$T1at[i-1]*nd1$T1p1[i] + (1-outdf$T1at[i-1])*nd0$T1p1[i]
		# outdf$T1iss[i]=prod(nd0$T1p0[2:(i-1)])*nd0$T1p1[i];	outdf$T1cnl[i]=prod(nd1$T1p1[2:(i-1)])*nd1$T1p0[i]
		# outdf$T3at[i]=outdf$T3at[i-1]*nd1$T3p1[i] + (1-outdf$T3at[i-1])*nd0$T3p1[i]
		# outdf$T3iss[i]=prod(nd0$T3p0[2:(i-1)])*nd0$T3p1[i];	outdf$T3cnl[i]=prod(nd1$T3p1[2:(i-1)])*nd1$T3p0[i]
		# outdf$T8at[i]=outdf$T8at[i-1]*nd1$T8p1[i] + (1-outdf$T8at[i-1])*nd0$T8p1[i]
		# outdf$T8iss[i]=prod(nd0$T8p0[2:(i-1)])*nd0$T8p1[i];	outdf$T8cnl[i]=prod(nd1$T8p1[2:(i-1)])*nd1$T8p0[i]
	# }
# }
# T1noiss = 1-sum(outdf$T1iss,na.rm=T); T1nocnl = 1-sum(outdf$T1cnl,na.rm=T)
# T3noiss = 1-sum(outdf$T3iss,na.rm=T); T3nocnl = 1-sum(outdf$T3cnl,na.rm=T)
# T8noiss = 1-sum(outdf$T8iss,na.rm=T); T8nocnl = 1-sum(outdf$T8cnl,na.rm=T)
# options(scipen=100);outdf;c(T1noiss,T1nocnl);c(T3noiss,T3nocnl);c(T8noiss,T8nocnl)


# # newdata=data.frame(lat=c(180,183.5,187,192.5,198,203,208,212,216,218.5,221,223.5,226),lon=c(1181,1164.5,1148,1132,1116,1099.5,1083,1069,1055,1042.5,1030,1017.5,1005),str=c(65,67.5,70,72.5,75,72.5,70,57.5,45,38.75,32.5,26.25,20),radspd=c(NA,-130.1520689,-92.88986176,-50.47581835,32.70591856,105.9765896,139.4338001,131.3416715,136.9754069,125.9828515,126.8945236,127.4639003,127.8131964))
# # newdata$fcstpt = 1:nrow(newdata)
# # newdata=newdata[rep(1:nrow(newdata),each=2),]; newdata$sm6=rep(0:1)
# # newdata$preds=predict(obj,newdata=newdata,type="response")


#Produce prediction matrix to port to Excel
radspdlength=45
frame1=list(lat=seq(min(dat$lat),max(dat$lat),by=5),
lon=seq(min(dat$lon),max(dat$lon),by=5),
str=seq(min(dat$str),max(dat$str),by=6))
nd=expand.grid(frame1[[1]],frame1[[2]],frame1[[3]]); colnames(nd)=c('lat','lon','str')
nd$radspd=seq(min(dat$radspd),max(dat$radspd),length=radspdlength); nd$sm6=0

Xp0=predict(obj,nd,type="lpmatrix")
write.table(frame1[[1]],file="lat_grid.csv",row.names=F,col.names=F,sep=",")
write.table(frame1[[2]],file="lon_grid.csv",row.names=F,col.names=F,sep=",")
write.table(frame1[[3]],file="str_grid.csv",row.names=F,col.names=F,sep=",")
write.table(nd[,1:3],file="latlonstr_expanded.csv",row.names=F,col.names=F,sep=",")
write.table(nd$radspd[1:radspdlength],file="radspd_grid.csv",row.names=F,col.names=F,sep=",")
write.table(coef(obj),file="coeff.csv",row.names=F,col.names=F,sep=",")
write.table(Xp0,file="Xp0.csv",row.names=F,sep=",")
