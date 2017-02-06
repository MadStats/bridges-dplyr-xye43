#install.packages("ggplot2")
#install.packages("plyr")
#install.packages("choroplethr")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("choroplethrMaps")

#required package

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

#data input and clean 

dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest)
tmp2 = read_csv(dest, col_types = "c")
classes = sapply(tmp, class)

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()

dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)

M = x16
M = M[,-14]
is.na(M) %>% rowSums %>% hist
is.na(M) %>% colSums %>% hist(breaks = 100)
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
M = M[,-bad]

#select variables we are interested in

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029" , "YEAR_ADT_030",
         "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", 
         "DATE_OF_INSPECT_090", "FRACTURE_092A", "UNDWATER_LOOK_SEE_092B", "SPEC_INSPECT_092C")
M = as.tbl(M)
x = select(M, one_of(keep))

# build required functions

min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
rateIt = function(cond){
  rate = rep("good", length(cond))
  rate[cond <5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

#choose those has all condition applicable

x = filter(x,x$DECK_COND_058!="N"&x$SUPERSTRUCTURE_COND_059!="N"&x$SUBSTRUCTURE_COND_060!="N"
           &x$CHANNEL_COND_061!="N"&x$CULVERT_COND_062!="N")

#lat and long variable not equals to NA 

x = filter(x,!is.na(x$LAT_016)&!is.na(x$LONG_017)&x$LONG_017>0)
x = mutate(x,cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, 
                              CHANNEL_COND_061,CULVERT_COND_062,na.rm = T))
x = mutate(x,lat = min2dec(LAT_016), lon = min2dec(LONG_017))

#make plots of these bridges
x$rate = rateIt(x$cond)
ggplot(data = x, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()
ggplot(data = x) +geom_point(mapping = aes(y = log(ADT_029), x = STATE_CODE_001))
ggplot(data = x) +geom_point(mapping = aes(y = lat, x = lon, col = log(ADT_029)))
ggplot(data = x) +geom_point(mapping = aes(y = lat, x = lon, col = YEAR_BUILT_027))
ggplot(data = x) +geom_point(mapping = aes(y = log(ADT_029), x = YEAR_BUILT_027, col = cond))

