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

grantsAgg <- function(yearDat,grants_code){
  out <- NULL
  # yearDat[,code := as.numeric(yearDat$cfda_program_num)]
  for(cd in grants_code){
    subD <- yearDat[cfda_program_num == cd,.(total_funding_amount,principal_place_state)]
    thisout <- subD[,lapply(.SD,function(ilist){sum(as.numeric(ilist))}),by = principal_place_state]
    thisout[,code:=cd]
    out <- rbind(out,thisout)
  }
  out<- dcast(out,principal_place_state~code,value.var = "total_funding_amount")
  return(out)
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




