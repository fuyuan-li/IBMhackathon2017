library(data.table)
aa <- fread("myMac/Hackthon/d2014.csv")
aa
aa[,cfda_program_num]
as.numeric(aa$cfda_program_num)
aa[,code := as.numeric(aa$cfda_program_num)]
our <- aa[code == 84.369]
dim(our)
View(our)
our[,account_title]
dc = our[principal_place_state=="DISTRICT OF COLUMBIA"]
test1 <- our[,.(total_funding_amount,principal_place_state)]
test1[,lapply(.SD,function(ilist){sum(as.numeric(ilist))}),by = principal_place_state]

grantsAgg_byState <- function(yearDat,grants_code){
  out <- NULL
  # yearDat[,code := as.numeric(yearDat$cfda_program_num)]
  for(cd in grants_code){
    subD <- yearDat[cfda_program_num == cd,.(total_funding_amount,principal_place_state)]
    thisout <- subD[,lapply(.SD,function(ilist){sum(as.numeric(ilist))}),by = principal_place_state]
    thisout[,code:=cd]
    out <- rbind(out,thisout)
  }
  out<- dcast(out,principal_place_state~code,value.var = "total_funding_amount")
  out <- as.data.frame(out)
  out[is.na(out)] <- 0
  out$principal_place_state <- tolower(out$principal_place_state)
  names(out)[1] <- "region"
  return(as.data.table(out))
}

test2 <- grantsAgg(aa,c(84.369,84.365,84.287,84.141))
class(test2)
test2[,region := tolower(test2$principal_place_state)]

library(ggplot2)
library(maps)
all_states <- map_data("state")
test3 <- test2[,.(region,`84.369`)]
Total <- merge(all_states, test3, by="region")

ggplot() + 
  geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$`84.369`),colour="blue") + 
  # scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
  scale_fill_continuous(low = 2, high = 15, guide="colorbar") +
  theme_bw() + 
  labs(fill = "Funding_Names" ,title = "The name of the plot", x="", y="")



aa <- fread("myMac/Hackthon/d2014.csv")
t2014 <- grantsAgg(aa,OECE_OII_CFDA)
ggdat <- merge(all_states, t2014, by="region")
ggplot() + 
  geom_polygon(data=ggdat, aes(x=long, y=lat, group = group, fill=ggdat$`84.369`),colour="blue") + 
  # scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
  scale_fill_continuous(low = 2, high = 15, guide="colorbar") +
  theme_bw() + 
  labs(fill = "Funding_Names" ,title = "The name of the plot", x="", y="")


grantsAgg_byCounty <- function(yearDat,grants_code){
  out <- NULL
  # yearDat[,code := as.numeric(yearDat$cfda_program_num)]
  for(cd in grants_code){
    subD <- yearDat[cfda_program_num == cd,.(total_funding_amount,principal_place_state,recipient_county_name)]
    thisout <- subD[,lapply(.SD,function(ilist){sum(as.numeric(ilist))}),by = .(principal_place_state,recipient_county_name)]
    thisout[,code:=cd]
    out <- rbind(out,thisout)
  }
  out<- dcast(out,principal_place_state+recipient_county_name~code,value.var = "total_funding_amount")
  out <- as.data.frame(out)
  out[is.na(out)] <- 0
  out$principal_place_state <- tolower(out$principal_place_state)
  return(as.data.table(out))
}

test4 <- grantsAgg_byCounty(aa,c(84.287,84.365))
grantsAgg_byCounty(aa,84.287)

t2014_a <- grantsAgg_byCounty(aa,OECE_OII_CFDA)







for(yy in 2007:2014){
  aa <- fread(paste("myMac/Hackthon/d",yy,".csv", sep = ""))
  temp <- grantsAgg_byState(yearDat = aa,OECE_OII_CFDA)
  fwrite(temp,file = paste("myMac/Hackthon/t",yy,".csv", sep = ""),col.names=T)
}

load("Dropbox/hackathon/gr.Rdata")
class(grnew)
gr <- as.data.table(grnew)

gr2014 <- gr[year==2014,.(State,ALL_T,ALL_G/100)]
mid <- gr2014[,lapply(.SD,sum),by=State]
gr2014 <- mid[,.(State,V3/ALL_T)]

getGr <- function(gr,yy){
  mid <- na.omit(gr[year==yy,.(State,ALL_T,ALL_G/100)])
  mid1 <- mid[,lapply(.SD,sum),by=State]
  gr <- mid1[,.(State,V3/ALL_T)]
  gr$State <- tolower(gr$State)
  names(gr) <- c("region","gr")
  return(gr)
}

gr2014 <- getGr(gr,2014)
t2014 <- fread("Dropbox/hackathon/t2011.csv",header = T)
names(t2014)[1] <- "region"
ag2014 <- na.omit(merge(gr2014,t2014,by = "region"))
ag2014[,totalGrants := apply(ag2014[,3:ncol(ag2014)],1,sum)]
m2014 <- ag2014[,.(region,gr,totalGrants)]

gr2013 <- getGr(gr,2013)
t2013 <- fread("Dropbox/hackathon/t2010.csv")
names(t2013)[1] <- "region"
ag2013 <- na.omit(merge(gr2013,t2013,by = "region"))
ag2013[,totalGrants := apply(ag2013[,3:ncol(ag2013)],1,sum)]
m2013 <- ag2013[,.(region,gr,totalGrants)]

gr2012 <- getGr(gr,2012)
t2012 <- fread("Dropbox/hackathon/t2009.csv")
names(t2012)[1] <- "region"
ag2012 <- na.omit(merge(gr2012,t2012,by = "region"))
ag2012[,totalGrants := apply(ag2012[,3:ncol(ag2012)],1,sum)]
m2012 <- ag2012[,.(region,gr,totalGrants)]

gr2011 <- getGr(gr,2011)
t2011 <- fread("Dropbox/hackathon/t2008.csv")
names(t2011)[1] <- "region"
ag2011 <- na.omit(merge(gr2011,t2011,by = "region"))
ag2011[,totalGrants := apply(ag2011[,3:ncol(ag2011)],1,sum)]
m2011 <- ag2011[,.(region,gr,totalGrants)]

gr2010 <- getGr(gr,2010)
t2010 <- fread("Dropbox/hackathon/t2007.csv")
names(t2010)[1] <- "region"
ag2010 <- na.omit(merge(gr2010,t2010,by = "region"))
ag2010[,totalGrants := apply(ag2010[,3:ncol(ag2010)],1,sum)]
m2010 <- ag2010[,.(region,gr,totalGrants)]


cbind(m2010[,1:2],m2011[,2],m2012[,2],m2013[,2],m2014[,2])






m_List <- list(m2010,m2011,m2012,m2013,m2014)
state_gr <- state_fd <- NULL
for(i in 1:5){
  m_List[[i]][,year := 2009+i]
  m_List[[i]]$year <- as.character(m_List[[i]]$year)
}
for(i in 1:5){
  state_gr <- rbind(state_gr,m_List[[i]][,.(region,gr,year)])
  state_fd <- rbind(state_fd,m_List[[i]][,.(region,totalGrants,year)])
}
ts.gr <- dcast(state_gr,region~year, value.var = "gr")
ts.fd <- dcast(state_fd,region~year,value.var = "totalGrants")

ts.gr1 <- na.omit(ts.gr)
ts.fd1 <- na.omit(ts.fd)
val_st <- intersect(ts.gr$region,ts.fd$region)

corTab <- data.table(region=val_st,rho=0)
for(statename in val_st){
  x = as.numeric(ts.gr[region==statename,-1])
  y = as.numeric(ts.fd[region==statename,-1])
  corTab[region==statename,2] = cor(x,y,use = "pairwise.complete.obs")
}
corTab

naSt = corTab[is.na(corTab$rho),1]
x = as.numeric(ts.gr[region=="idaho"])
y = as.numeric(ts.fd[region=="idaho"])


ggdat <- merge(all_states[all_states$region%in%val_st,], corTab, by="region")
ggplot() + 
  geom_polygon(data=ggdat, aes(x=long, y=lat, group = group, fill=ggdat$rho),colour="white") + 
  # scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
  scale_fill_continuous(low = 'orange', high = 'green', guide="colorbar") +
  theme_bw() + 
  labs(fill = "Funding_Names" ,title = "The name of the plot", x="", y="")

# ggdat -> ggdat1
# corTab -> corTab1
# ggdat -> ggdat2
# corTab -> corTab2
# ggdat -> ggdat3
# corTab -> corTab3
# ggdat -> ggdat4
# corTab -> corTab4

#####################################
library(glmmLasso)

t2014 <- fread("Dropbox/hackathon/t2014.csv",header = T)
t2013 <- fread("Dropbox/hackathon/t2013.csv",header = T)
t2012 <- fread("Dropbox/hackathon/t2012.csv",header = T)
t2011 <- fread("Dropbox/hackathon/t2011.csv",header = T)
t2010 <- fread("Dropbox/hackathon/t2010.csv",header = T)

gr2014 <- getGr(gr,2014)
ag2014 <- na.omit(merge(gr2014,t2014,by = "region"))
gr2013 <- getGr(gr,2013)
ag2013 <- na.omit(merge(gr2013,t2013,by = "region"))
gr2012 <- getGr(gr,2012)
ag2012 <- na.omit(merge(gr2012,t2012,by = "region"))
gr2011 <- getGr(gr,2011)
ag2011 <- na.omit(merge(gr2011,t2011,by = "region"))
gr2010 <- getGr(gr,2010)
ag2010 <- na.omit(merge(gr2010,t2010,by = "region"))

avl_fd <- colnames(ag2014)
avl_fd <- intersect(avl_fd,colnames(ag2013))
avl_fd <- intersect(avl_fd,colnames(ag2012))
avl_fd <- intersect(avl_fd,colnames(ag2011))
avl_fd <- intersect(avl_fd,colnames(ag2010))

sub2014 <- ag2014[,avl_fd,with = F][,year:=2014]
sub2013 <- ag2013[,avl_fd,with = F][,year:=2013]
sub2012 <- ag2012[,avl_fd,with = F][,year:=2012]
sub2011 <- ag2011[,avl_fd,with = F][,year:=2011]
sub2010 <- ag2010[,avl_fd,with = F][,year:=2010]

aggrDat <- NULL
aggrDat <- rbind(aggrDat,sub2014)
aggrDat <- rbind(aggrDat,sub2013)
aggrDat <- rbind(aggrDat,sub2012)
aggrDat <- rbind(aggrDat,sub2011)
aggrDat <- rbind(aggrDat,sub2010)

fwrite(aggrDat,"Dropbox/hackathon/aggrDat.csv",col.names = T)

varnames = colnames(aggrDat)[-c(1,2)]
varnames[-length(varnames)]



lm1 <- glmmLasso(fix = gr~`84.004`+`84.010`+`84.011`+`84.013`+`84.041`+`84.060`+`84.141`+`84.144`+`84.149`+
                `84.165`+`84.184`+`84.196`+`84.215`+`84.282`+`84.287`+`84.295`+`84.299`+`84.330`+
                `84.336`+`84.350`+`84.351`+`84.354`+`84.356`+`84.358`+`84.360`+`84.362`+`84.363`+
                `84.365`+`84.366`+`84.367`+`84.368`+`84.369`+`84.371`+`84.374`+`84.377`,rnd = list(region=~1+year), lambda = 0.5, data = aggrDat)

lm(gr~`84.369`,data = aggrDat)

library(glmnet)


lm2 <- glmnet(x = as.matrix(aggrDat[,-c(1,2,38)]),y = as.matrix(aggrDat[,2]),family = 'gaussian',alpha = 1)
plot(lm2)




