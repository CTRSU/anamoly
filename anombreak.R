anombreak<-function(data.anom, data.break, max.anoms=0.05, num.breaks=5, ylabs, plotname){

## libraries
require("devtools") || install.packages("devtools")
require("AnomalyDetection") || devtools::install_github("twitter/AnomalyDetection")
require("BreakoutDetection") || devtools::install_github("twitter/BreakoutDetection")
require("ggplot2") || install.packages("ggplot2")
require("zoo") || install.packages("zoo")

  
  
## run anomaly
res.anomaly = AnomalyDetectionTs(data.anom, max_anoms=max.anoms, direction='both', plot=TRUE)

## create new varaible in dataset for color of anomalies as they match date in dataframe
# create factor of it so we can use in plot
data.anom[,"anoms"]<-"no"
data.anom[,"anoms"][as.POSIXlt(data.anom[,"date"])%in%res.anomaly$anoms$timestamp]<-"yes"
data.anom[,"anoms"]<-factor(data.anom[,"anoms"])

byme<-ifelse(max(data.anom[,"rate"])>5, 1, 0.5)

pdf(paste0("~/Desktop/", plotname, ".pdf"), width=13, height=8)
#### plot anomaly
pchs<-ifelse(data.anom$anoms=="yes", 1,19)
cexme<-ifelse(data.anom$anoms=="yes", 1.5,0.5)
plot(data.anom$rate, type="o", xaxt="n", yaxt="n", ylim=range(c(0, max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2))), ylab=ylabs, xlab="", pch=pchs, lwd=2, cex=cexme)
axis(1, at=seq(1,nrow(data.anom), by=2), labels=FALSE)
ticks <- data.anom$date[c(TRUE, FALSE)]
## when y axis scale is short, x axis labels are messed up.  Need to set par to fix this issue.
yo<-ifelse(max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2)<5, par("usr")[3] -0.1, par("usr")[3] - 0.3)
text(seq(1, length(data.anom$date), 2), yo, srt = 45, adj = 1, labels = as.yearmon(ticks, "%b-%Y"), xpd = TRUE, cex=0.8)
axis(2, at=seq(0, max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2), by=byme), labels=seq(0, max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2), by=byme), cex.axis=0.8, las=2)
abline(h=seq(0,max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2)), lwd=0.7, col="lightgray")
lines(data.anom$rate, type="o", xaxt="n", yaxt="n", ylim=range(c(0, max(data.anom[,"rate"]) + (2 - max(data.anom[,"rate"]) %% 2))), xlab="", pch=pchs, cex=cexme, lwd=2)
points(data.anom$rate,cex=cexme,pch=pchs)



## run breakout algorightm
res.break = breakout(data.break, min.size=num.breaks, method='multi', plot=T)

## create mean segments
segs<-c(0, res.break$loc, nrow(data.break))
means<-vector("list", length(segs))
group<-NULL
for(i in 1:(length(segs)-1)){
  group<-c(group,rep(i, times=diff(segs)[i]))
}
data.break$group<-factor(group)
groupmean<-tapply(data.break$count, data.break$group, function(x) mean(x))


for(i in 1:length(groupmean)){
  segments(x0=segs[i], y0=groupmean[i], x1=segs[i+1]-1 , y1=groupmean[i], col="black", lty=2)
}

## get pvals, format, and plot
pvals<-vector("list", (length(groupmean)-1))

for(i in 1:(length(groupmean)-1)){
  tryCatch({
  pvals[i]<-round(t.test(data.break$count[data.break$group==i], data.break$count[data.break$group==(i+1)])$p.value,2)
}, error=function(e){})
}

pvals<-ifelse(pvals<0.001, paste0(pvals, "***"), 
              ifelse(pvals<0.01, paste0(pvals, "**"), 
                     ifelse(pvals<0.05, paste0(pvals, "*"), 
                            ifelse(pvals==1, ">0.99", 
                                   ifelse(pvals==0, "<0.0001****", pvals)))))
pvals<-paste0("P=", pvals)


if(length(segs)>2) {text(x=res.break$loc, y=0, pvals, cex=0.8, font=1, col="black", pos=1)} else {text(x=res.break$loc, y=0, "")}

dev.off()
}

