## this makes the chart
spc.chart<-function(data, filename, standardization, chart.type, y.axis.title, hline.location){
  require("zoo")||install.packages("zoo")
  require("lubridate")||install.packages("lubridate")
  require("qcc")||install.packages("qcc")
  require("Hmisc")||install.packages("Hmisc")

  ### Building data frame
  obj3 <- qcc(data$num, data$denom, ylab="", labels = data$moyr, type=chart.type, nsigmas=3, plot=FALSE)
  new.df<-data.frame(date=data[,"moyr"],stat=obj3$statistics*standardization,center=rep(obj3$center*standardization,nrow(data)))
  three.sigma<-data.frame(obj3$limits)*standardization
  new.df$UCL<-three.sigma$UCL
  new.df$LCL<-three.sigma$LCL
  obj2 <- qcc(data$num, data$denom, type=chart.type, nsigmas=2, plot=FALSE)
  two.sigma<-data.frame(obj2$limits)*standardization
  new.df$U2S<-two.sigma$UCL
  new.df$L2S<-two.sigma$LCL
  new.df$L2S<-ifelse(new.df$L2S<0,0,new.df$L2S)
  obj1 <- qcc(data$num, data$denom, type=chart.type, nsigmas=1, plot=FALSE)
  one.sigma<-data.frame(obj1$limits)*standardization
  new.df$U1S<-one.sigma$UCL
  new.df$L1S<-one.sigma$LCL
  new.df$L1S<-ifelse(new.df$L1S<0,0,new.df$L1S)
  
  ### runs of 8 or more
  new.df$diff<-new.df$stat-new.df$center
  new.df$diff[new.df$diff > 0] <- 1
  new.df$diff[new.df$diff < 0] <- -1
  q<-new.df$diff
  attr(q,"names")<-rownames(new.df)
  runs <- rle(q)
  run.df<-data.frame(runs=runs$lengths,date=(as.Date(names(runs$lengths),format="%Y-%m-%d",origin="1970-01-01")-months(1)))
  run.df[nrow(run.df),2]<-data[nrow(data),1]
  rownames(run.df)<-run.df$date
  data3<-merge(new.df,run.df,by="row.names",all.x=T)
  data3<-data3[order(data3$date.x,decreasing=T),]
  data3$runs<-na.locf(data3$runs)
  data3<-data3[order(data3$date.x),-c(1,ncol(data3))]
  
  ### finding above for rule 3 (2 above out of 3 2sig)
  data3$UP2<-data3$stat - data3$U2S
  data3$UP2[data3$UP2 > 0] <- 1
  data3$UP2[data3$UP2 < 0] <- 0
  data3$UP2R<-NA
  for(j in 1:(nrow(data3)-2)){
    data3$UP2R[j]<-data3$UP2[j]+data3$UP2[j+1]+data3$UP2[j+2]
  }
  data3$UP2R2<-0
  for(j in 1:(nrow(data3)-2)){
    if(data3$UP2R[j]==2){
      data3$UP2R2[j]<-1
      data3$UP2R2[j+1]<-1
      data3$UP2R2[j+2]<-1
    }
  }
  
  ### finding above for rule 4 (4 above out of 5 1sig)
  data3$UP1<-data3$stat - data3$U1S
  data3$UP1[data3$UP1 > 0] <- 1
  data3$UP1[data3$UP1 < 0] <- 0
  data3$UP1R<-NA
  for(j in 1:(nrow(data3)-4)){
    data3$UP1R[j]<-data3$UP1[j]+data3$UP1[j+1]+data3$UP1[j+2]+data3$UP1[j+3]+data3$UP1[j+4]
  }
  data3$UP1R2<-0
  for(j in 1:(nrow(data3)-4)){
    if(data3$UP1R[j]==4){
      data3$UP1R2[j]<-1
      data3$UP1R2[j+1]<-1
      data3$UP1R2[j+2]<-1
      data3$UP1R2[j+3]<-1
      data3$UP1R2[j+4]<-1
    }
  }
  
  ### finding below for rule 3 (2 below out of 3 2sig)
  data3$LW2<-data3$L2S - data3$stat
  data3$LW2[data3$LW2 > 0] <- 1
  data3$LW2[data3$LW2 < 0] <- 0
  data3$LW2R<-NA
  for(j in 1:(nrow(data3)-2)){
    data3$LW2R[j]<-data3$LW2[j]+data3$LW2[j+1]+data3$LW2[j+2]
  }
  data3$LW2R2<-0
  for(j in 1:(nrow(data3)-2)){
    if(data3$LW2R[j]==2){
      data3$LW2R2[j]<-1
      data3$LW2R2[j+1]<-1
      data3$LW2R2[j+2]<-1
    }
  }
  
  ### finding below for rule 4 (4 below out of 5 1sig)
  data3$LW1<- data3$L1S - data3$stat
  data3$LW1[data3$LW1 > 0] <- 1
  data3$LW1[data3$LW1 < 0] <- 0
  data3$LW1R<-NA
  for(j in 1:(nrow(data3)-4)){
    data3$LW1R[j]<-data3$LW1[j]+data3$LW1[j+1]+data3$LW1[j+2]+data3$LW1[j+3]+data3$LW1[j+4]
  }
  data3$LW1R2<-0
  for(j in 1:(nrow(data3)-4)){
    if(data3$LW1R[j]==4){
      data3$LW1R2[j]<-1
      data3$LW1R2[j+1]<-1
      data3$LW1R2[j+2]<-1
      data3$LW1R2[j+3]<-1
      data3$LW1R2[j+4]<-1
    }
  }
  
  
  ### Establishing rule 1: 1 above/below +3/-3 sigmas
  data3$rule<-ifelse(data3$stat > data3$UCL
                     | data3$stat < data3$LCL
                     | data3$runs >=8
                     | data3$UP2R2 == 1
                     | data3$UP1R2 == 1
                     | data3$LW2R2 == 1
                     | data3$LW1R2 == 1
                     ,1,0)
  
  range<-c(ifelse(min(data3$stat)<min(data3$LCL),
                  ifelse((min(data3$stat) - 0.05*min(data3$stat))<=0,
                         0,
                         (min(data3$stat) - 0.05*min(data3$stat))),
                  ifelse((min(data3$LCL) - 0.05*min(data3$LCL))<=0,
                         0,
                         (min(data3$LCL) - 0.05*min(data3$LCL)))),
           ifelse(max(data3$stat)>max(data3$UCL),
                  ifelse(max(data3$stat)<5,
                         (max(data3$stat) + 0.05*max(data3$stat)),
                         (max(data3$stat) + (2 - max(data3$stat) %% 2))),
                  ifelse(max(data3$UCL)<5,
                         (max(data3$UCL) + 0.05*max(data3$UCL)),
                         (max(data3$UCL) + (2 - max(data3$UCL) %% 2)))))
  
  pdf(paste0("~/Desktop/", filename, ".pdf"), w=13,h=8)
  par(cex.axis = 1.5, cex.lab=1.5, lab = c(5,10,7), mar=c(7,7,1,3),mgp=c(3,1,0),oma=c(0,0,0,0))
  plot(data3$stat, xlab = "", ylab=y.axis.title, cex.lab=1.35, xaxt="n", las=2,
       ylim=range,type="l",lwd=2,cex=2,col="black")
  lines(data3$LCL, lwd=2, lty=2, col="lightgray")
  lines(data3$UCL, lwd=2, lty=2, col="lightgray")
  lines(data3$L1S, lwd=2, lty=2, col="lightgray")
  lines(data3$L2S, lwd=2, lty=2, col="lightgray")
  lines(data3$U1S, lwd=2, lty=2, col="lightgray")
  lines(data3$U2S, lwd=2, lty=2, col="lightgray")
  if(hline.location > 0 & !is.na(hline.location)){
    abline(a=hline.location,b=0,lwd=2,col="black")
  }
  abline(h=obj3$center*standardization, lwd=3, lty=2, col="darkgray")
  #tck<-axis(side=1, at=1:nrow(data3), labels=F)
  #text(tck, par("usr")[3], labels=as.yearmon(data$moyr,"%Y-%m"), srt=45,
      # xpd=TRUE, adj=c(1.2,.5), cex=0.9)
  axis(1, at=seq(1,nrow(data3), by=2), labels=FALSE)
  ticks <- data$moyr[c(TRUE, FALSE)]
  ## when y axis scale is short, x axis labels are messed up.  Need to set par to fix this issue.
  yo<-ifelse(max(range)<5, par("usr")[3] -0.1, par("usr")[3] - 0.3)
  
  text(seq(1, length(data$moyr), 2), yo, srt = 45, adj = 1, labels = as.yearmon(ticks), xpd = TRUE, cex=0.8)
    lines(data3$stat)
  points(data3$stat,pch=ifelse(data3$rule == 1, 1,19),cex=ifelse(data3$rule == 1, 1.5, 0.5),col="black")
  mtext(side=4, text="+3SD", at=data3$UCL[nrow(data3)], las=2)
  mtext(side=4, text="+2SD", at=data3$U2S[nrow(data3)], las=2)
  mtext(side=4, text="+1SD", at=data3$U1S[nrow(data3)], las=2)
  mtext(side=4, text="Mean", at=obj3$center*standardization, las=2)
  # mtext(side=4, text="-1SD", at=data3$L1S[nrow(data3)], las=2)
  # mtext(side=4, text="-2SD", at=data3$L2S[nrow(data3)], las=2)
  # mtext(side=4, text="-3SD", at=data3$LCL[nrow(data3)], las=2)
  ## try to fix issue of overlapping labels in mtext.
  if (data3$L1S[nrow(data3)]<=0.2) {
    mtext(side=4, text="-1SD\n-2SD\n-3SD", at=data3$LCL[nrow(data3)], las=2)
  } else if (data3$L2S[nrow(data3)]<=0.1 & data3$L1S[nrow(data3)]>0.2) {
    mtext(side=4, text="-2SD\n-3SD", at=data3$LCL[nrow(data3)], las=2)
    mtext(side=4, text="-1SD", at=data3$L1S[nrow(data3)], las=2)
  } else {
    mtext(side=4, text="-1SD", at=data3$L1S[nrow(data3)], las=2)
    mtext(side=4, text="-2SD", at=data3$L2S[nrow(data3)], las=2)
    mtext(side=4, text="-3SD", at=data3$LCL[nrow(data3)], las=2)
  }
  
  dev.off()
}



