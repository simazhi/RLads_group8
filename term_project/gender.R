library(tidyverse)
library(plotly)

#Read File
all <- read.csv("D:\\Richard\\碩班課程\\碩二上\\R語言與資料科學\\final\\userdata_all.csv", encoding = "UTF-8")
les <- read.csv("D:\\Richard\\碩班課程\\碩二上\\R語言與資料科學\\final\\userdata_lesbian.csv", encoding = "UTF-8")

#Plot
p.self.all <- ggplot(data=all, aes(x=Guy)) + geom_bar()
p.self.les <- ggplot(data=les, aes(x=Guy)) + geom_bar()

p.intr.all <- ggplot(data=all, aes(x=Girls)) + geom_bar()
p.intr.les <- ggplot(data=les, aes(x=Girls)) + geom_bar()

#######################################################
#ALL
#######################################################

###percent
STM.all <- nrow(filter(all, Guy=="Guy" & Girls=="Girls"))/nrow(all)*100
STF.all <- nrow(filter(all, Guy=="Girl" & Girls=="Guys"))/nrow(all)*100
LES.all <- nrow(filter(all, Guy=="Girl" & Girls=="Girls"))/nrow(all)*100
GAY.all <- nrow(filter(all, Guy=="Guy" & Girls=="Guys"))/nrow(all)*100
OTR.all <- nrow(filter(all, ((Guy=="Couple"|Guy=="F2M"|Guy=="M2F"|Guy=="Other")&(Girls=="Both"|Girls=="Girls"|Girls=="Guys")) | (Guy=="Girl"&Girls=="Both") | (Guy=="Guy"&Girls=="Both")))/nrow(all)*100
NA.all  <- nrow(filter(all, is.na(Guy)|is.na(Girls)|Guy=="NA"|Girls=="NA"))/nrow(all)*100
all.per <- c( STM.all, STF.all, LES.all, GAY.all, OTR.all, NA.all)
all.perr <- round(all.per, digit = 2)
all.name <- c("Straight Male", "Straight Female", "Lesbian", "Gay", "Others", "NA")
all.frame <- data.frame(all.name , all.per)

allper <- ggplot(all.frame, aes(all.name, all.per)) + geom_bar(stat = "identity") + geom_text(aes(label = all.perr),vjust=-0.5)


###Count
STMc.all <- nrow(filter(all, Guy=="Guy" & Girls=="Girls"))
STFc.all <- nrow(filter(all, Guy=="Girl" & Girls=="Guys"))
LESc.all <- nrow(filter(all, Guy=="Girl" & Girls=="Girls"))
GAYc.all <- nrow(filter(all, Guy=="Guy" & Girls=="Guys"))
OTRc.all <- nrow(filter(all, ((Guy=="Couple"|Guy=="F2M"|Guy=="M2F"|Guy=="Other")&(Girls=="Both"|Girls=="Girls"|Girls=="Guys")) | (Guy=="Girl"&Girls=="Both") | (Guy=="Guy"&Girls=="Both")))
NAc.all  <- nrow(filter(all, is.na(Guy)|is.na(Girls)|Guy=="NA"|Girls=="NA"))
all.count <- c( STMc.all, STFc.all, LESc.all, GAYc.all, OTRc.all, NAc.all)
all.cframe <- data.frame(all.name , all.count)

allcon <- ggplot(all.cframe, aes(all.name, all.count)) + geom_bar(stat = "identity") + geom_text(aes(label = all.count),vjust=-0.5)



#######################################################
#LES
#######################################################

###percent
STM.les <- nrow(filter(les, Guy=="Guy" & Girls=="Girls"))/nrow(les)*100
STF.les <- nrow(filter(les, Guy=="Girl" & Girls=="Guys"))/nrow(les)*100
LES.les <- nrow(filter(les, Guy=="Girl" & Girls=="Girls"))/nrow(les)*100
GAY.les <- nrow(filter(les, Guy=="Guy" & Girls=="Guys"))/nrow(les)*100
OTR.les <- nrow(filter(les, ((Guy=="Couple"|Guy=="F2M"|Guy=="M2F"|Guy=="Other")&(Girls=="Both"|Girls=="Girls"|Girls=="Guys")) | (Guy=="Girl"&Girls=="Both") | (Guy=="Guy"&Girls=="Both")))/nrow(les)*100
NA.les  <- nrow(filter(les, is.na(Guy)|is.na(Girls)|Guy=="NA"|Girls=="NA"))/nrow(les)*100
les.per <- c( STM.les, STF.les, LES.les, GAY.les, OTR.les, NA.les)
les.perr <- round(les.per, digit = 2)
les.frame <- data.frame(all.name , les.per)

lesper <- ggplot(les.frame, aes(all.name, les.per)) + geom_bar(stat = "identity") + geom_text(aes(label = les.perr),vjust=-0.5)



###Count
STMc.les <- nrow(filter(les, Guy=="Guy" & Girls=="Girls"))
STFc.les <- nrow(filter(les, Guy=="Girl" & Girls=="Guys"))
LESc.les <- nrow(filter(les, Guy=="Girl" & Girls=="Girls"))
GAYc.les <- nrow(filter(les, Guy=="Guy" & Girls=="Guys"))
OTRc.les <- nrow(filter(les, ((Guy=="Couple"|Guy=="F2M"|Guy=="M2F"|Guy=="Other")&(Girls=="Both"|Girls=="Girls"|Girls=="Guys")) | (Guy=="Girl"&Girls=="Both") | (Guy=="Guy"&Girls=="Both")))
NAc.les  <- nrow(filter(les, is.na(Guy)|is.na(Girls)|Guy=="NA"|Girls=="NA"))
les.count <- c( STMc.les, STFc.les, LESc.les, GAYc.les, OTRc.les, NAc.les)
les.cframe <- data.frame(all.name , les.count)

lescon <- ggplot(les.cframe, aes(all.name, les.count)) + geom_bar(stat = "identity") + geom_text(aes(label = les.count),vjust=-0.5)



###################################################################
allper
allcon
lesper
lescon