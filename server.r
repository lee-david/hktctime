library(shiny)
library(mgcv)
load('fits.rdata')
latlim=c(145,275); lonlim=c(1055,1225); strlim=c(22,130); radspdlim=c(-263,268)

shinyServer(function(input, output) {
	datasetInput <- reactive({
		for(i in c(0,12,24,36,48,72)){
			assign(paste0('t',i,'lat'),as.numeric(input[[paste0('t',i,'lat')]]))
			assign(paste0('t',i,'lon'),as.numeric(input[[paste0('t',i,'lon')]]))
			assign(paste0('t',i,'str'),as.numeric(input[[paste0('t',i,'str')]]))
		}
		if(is.na(t0lat) | is.na(t0lon) | is.na(t0str) | is.na(t12lat) | is.na(t12lon) | is.na(t12str)){
			return(NULL)
		}else{
			t0=c(t0lat,t0lon,t0str)
			t12=c(t12lat,t12lon,t12str)
		}
		if(is.na(t72lat) | is.na(t72lon) | is.na(t72str)){t72=NULL;}else{t72=c(t72lat,t72lon,t72str);}
		if(is.na(t48lat) | is.na(t48lon) | is.na(t48str)){t48=NULL;t72=NULL;}else{t48=c(t48lat,t48lon,t48str);}
		if(is.na(t36lat) | is.na(t36lon) | is.na(t36str)){t36=NULL;t48=NULL;t72=NULL;}else{t36=c(t36lat,t36lon,t36str);}
		if(is.na(t24lat) | is.na(t24lon) | is.na(t24str)){t24=NULL;t36=NULL;t48=NULL;t72=NULL;}else{t24=c(t24lat,t24lon,t24str);}
		if(input$cursig1==FALSE){cursig1=0;}else{cursig1=1;}
		if(input$cursig3==FALSE){cursig3=0;}else{cursig3=1;}
		if(input$cursig8==FALSE){cursig8=0;}else{cursig8=1;}
		#Populate prediction data frame
		newdata=data.frame()
		for(i in seq(0,48,by=6)){
			if(i %% 12 == 0){
				vv=get(paste0('t',i))
				if(!is.null(vv)) newdata=rbind(newdata,c(i,vv))
			}else{
				vv=get(paste0('t',i+6))
				if(!is.null(vv)){
					ww=get(paste0('t',i-6)); newdata=rbind(newdata,c(i,(vv+ww)/2))
				}
			}
		}
		for(i in seq(54,72,by=6)){
			if(i == 72){
				vv=get(paste0('t',i))
				if(!is.null(vv)) newdata=rbind(newdata,c(i,vv))
			}else{
				vv=t72; ww=t48
				if(!is.null(vv) & !is.null(ww)){
					frac=(i-48)/24;	newdata=rbind(newdata,c(i,vv*frac+ww*(1-frac)))
				}
			}
		}
		colnames(newdata)=c('tt','lat','lon','str')
		newdata$radspd=NA
		for(i in 2:nrow(newdata)){
			lat1=newdata$lat[i]; lon1=newdata$lon[i]
			lat0=newdata$lat[i-1]; lon0=newdata$lon[i-1]
			rs=sqrt((223-lat1)^2+(1142-lon1)^2*cos((223+lat1)/2/10/180*3.14159)^2)*11.13-sqrt((223-lat0)^2+(1142-lon0)^2*cos((223+lat0)/2/10/180*3.14159)^2)*11.13
			rs=max(min(rs,radspdlim[2]),radspdlim[1])
			newdata$radspd[i] = rs
		}

		#Do the prediction
		nd0 <- newdata; nd0$sm6=0
		nd1 <- newdata; nd1$sm6=1
		nd0$T1p1=predict(objt1,newdata=nd0,type="response")
		nd1$T1p1=predict(objt1,newdata=nd1,type="response")
		nd0$T3p1=predict(objt3,newdata=nd0,type="response")
		nd1$T3p1=predict(objt3,newdata=nd1,type="response")
		nd0$T8p1=predict(objt8,newdata=nd0,type="response")
		nd1$T8p1=predict(objt8,newdata=nd1,type="response")

		for(i in 2:nrow(nd0)){
			if(nd0$lat[i] > latlim[2] | nd0$lat[i] < latlim[1] | nd0$lon[i] > lonlim[2] | nd0$lon[i] < lonlim[1]){
				nd0$T1p1[i]=0; nd1$T1p1[i]=0;
				nd0$T3p1[i]=0; nd1$T3p1[i]=0;
				nd0$T8p1[i]=0; nd1$T8p1[i]=0;
			}
		}
		nd0$T1p0=1-nd0$T1p1; nd1$T1p0=1-nd1$T1p1;
		nd0$T3p0=1-nd0$T3p1; nd1$T3p0=1-nd1$T3p1;
		nd0$T8p0=1-nd0$T8p1; nd1$T8p0=1-nd1$T8p1;
		
		#Create output data frame
		for(i in 1:nrow(nd0)){
			if(i==1){
				outdf=data.frame(tt=nd0$tt[i],T1at=NA,T1iss=NA,T1cnl=NA,T3at=NA,T3iss=NA,T3cnl=NA,T8at=NA,T8iss=NA,T8cnl=NA); next
			}
			outdf[i,]=NA
			if(i==2){
				outdf$tt[i]=nd0$tt[i]
				if(cursig1==0){outdf$T1at[i]=nd0$T1p1[i];}else{outdf$T1at[i]=nd1$T1p1[i];}
				if(cursig3==0){outdf$T3at[i]=nd0$T3p1[i];}else{outdf$T3at[i]=nd1$T3p1[i];}
				if(cursig8==0){outdf$T8at[i]=nd0$T8p1[i];}else{outdf$T8at[i]=nd1$T8p1[i];}
				outdf$T1iss[i]=nd0$T1p1[i];	outdf$T1cnl[i]=nd1$T1p0[i]
				outdf$T3iss[i]=nd0$T3p1[i];	outdf$T3cnl[i]=nd1$T3p0[i]
				outdf$T8iss[i]=nd0$T8p1[i];	outdf$T8cnl[i]=nd1$T8p0[i]
			} else {
				outdf$tt[i]=nd0$tt[i]
				outdf$T1at[i]=outdf$T1at[i-1]*nd1$T1p1[i] + (1-outdf$T1at[i-1])*nd0$T1p1[i]
				outdf$T1iss[i]=prod(nd0$T1p0[2:(i-1)])*nd0$T1p1[i];	outdf$T1cnl[i]=prod(nd1$T1p1[2:(i-1)])*nd1$T1p0[i]
				outdf$T3at[i]=outdf$T3at[i-1]*nd1$T3p1[i] + (1-outdf$T3at[i-1])*nd0$T3p1[i]
				outdf$T3iss[i]=prod(nd0$T3p0[2:(i-1)])*nd0$T3p1[i];	outdf$T3cnl[i]=prod(nd1$T3p1[2:(i-1)])*nd1$T3p0[i]
				outdf$T8at[i]=outdf$T8at[i-1]*nd1$T8p1[i] + (1-outdf$T8at[i-1])*nd0$T8p1[i]
				outdf$T8iss[i]=prod(nd0$T8p0[2:(i-1)])*nd0$T8p1[i];	outdf$T8cnl[i]=prod(nd1$T8p1[2:(i-1)])*nd1$T8p0[i]
			}
		}
		T1noiss = 1-sum(outdf$T1iss,na.rm=T); T1nocnl = 1-sum(outdf$T1cnl,na.rm=T)
		T3noiss = 1-sum(outdf$T3iss,na.rm=T); T3nocnl = 1-sum(outdf$T3cnl,na.rm=T)
		T8noiss = 1-sum(outdf$T8iss,na.rm=T); T8nocnl = 1-sum(outdf$T8cnl,na.rm=T)
		noiss=data.frame(T1=T1noiss,T3=T3noiss,T8=T8noiss)
		nocnl=data.frame(T1=T1nocnl,T3=T3nocnl,T8=T8nocnl)
		return(list(outdf=outdf,noiss=noiss,nocnl=nocnl))

	})
	output$probtable <- renderTable({
		outtab=datasetInput(); if(!is.null(outtab)){outtab$outdf;}else{data.frame(null=0);}
	},digits=4)
	output$noiss <- renderTable({
		outtab=datasetInput(); if(!is.null(outtab)){outtab$noiss;}else{data.frame(null=0);}
	},digits=4)
	output$nocnl <- renderTable({
		outtab=datasetInput(); if(!is.null(outtab)){outtab$nocnl;}else{data.frame(null=0);}
	},digits=4)
})
