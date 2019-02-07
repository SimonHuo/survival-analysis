library(tidyverse)
library(reshape2)
library(flextable)
library(xtable)
library(htmlTable)
library(gcookbook)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyverse)
library(scales)
library(grid)
library(cowplot)
library(survival)
library(dplyr)
library(survminer)
rm(list = ls())
OBLOS <- read_xlsx("yong .xlsx")
age <- OBLOS[,1:4]
age <- age[1:53,]
summary(age[,"follow -up"])
follow <- OBLOS[,1:4]
follow <- follow[1:60,]
follow$"follow -up"=as.numeric(follow$"follow -up",na.rm=T)
summary(follow[,"follow -up"])

#########三线表走起！！！！！
#  xtable 生产Latex和html的格式 但是：：输出word和直接在R中显示支持不够 so: flextable
SXB <- read_xlsx("三线表所用数据1.xlsx")
SXB <- read_xlsx("character.xlsx")
# hemlTable 可转为 heml格式的
htmlTable(SXB)
###xtable and flextable 可将数据转为图片的 完美搞定
M1 <- xtable_to_flextable(xtable(SXB))
M1
require(devEMF)
emf(file = "M1", width = 7, height = 7,
    bg = "transparent", fg = "black", pointsize = 12,
    family = "Helvetica", coordDPI = 300, custom.lty=emfPlus, emfPlusFont = FALSE, emfPlusRaster = FALSE)
######################三线表搞定
######画折线图（年龄）
SXB <- read_xlsx("yong .xlsx")
#qt <- quantile(survival$`follow -up`) ###求四分位数值
head(SXB)
ggplot(SXB, aes(x=age))+geom_freqpoly(color="black")+
     ggthemes::theme_economist()+
  geom_vline(xintercept = 27.25, color="blue", size=1)+
  labs(title="The distribution of age",x="age (year)", y = "number")
###再来个饼图
bone1 <- read_xlsx("bone1.xlsx")
bone1$"shuju"=as.numeric(bone1$"shuju",na.rm=T)
p = ggplot(bone1,aes(x='',y=shuju,fill=Form))+
  geom_bar(stat="identity",width = 1)+theme_minimal()
p
px <- p+coord_polar(theta="y",start=0)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )+ labs(title="The distribution of bone")
px
px+scale_fill_brewer(palette = "Paired")+
  theme(
    axis.text.x=element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank())


bone2 <- read_xlsx("bone2.xlsx")
bone2$"shuju"=as.numeric(bone2$"shuju",na.rm=T)
p = ggplot(bone2,aes(x='',y=shuju,fill=Form))+
  geom_bar(stat="identity",width = 1)+theme_minimal()
p
px <- p+coord_polar(theta="y",start=0)+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )+ labs(title="The distribution of bone")
px
px+scale_fill_brewer(palette = "Paired")+
  theme(
    axis.text.x=element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank())
###########
###NO INTERCEPT
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:59,1:5]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
# 构建生存对象
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2)
#解距建模（entire cohort）
survfit(survival2~1)
survival3 <- survfit(survival2 ~ 1)
summary(survival3,times=seq(0, 120, 6))
plot(survival3)
########
##最合适的切入点。。。。
res.cut <- surv_cutpoint(survival, time = "follow -up", event = "OS_status",
                         variables = c("age"))
summary(res.cut)   
plot(res.cut, "age", palette = "npg",title="Age",legend.title="Group",xlab="Age(year)")
res.cat <- surv_categorize(res.cut)   
head(res.cat)
library("survival")
fit <- survfit(Surv(survival$"follow -up",survival$"OS_status") ~age, data = res.cat)
fit
################
##### age
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival$agecat <- cut(survival$"age", breaks=c(0,13,100),labels=c("young", "old"))
survival = tbl_df(survival)
str(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) #class:class会根据数据对象的存储类型（type）
#与维度属性来自动给出一个类属性
survival2
log<-survdiff(survival2~survival$agecat)
log
survival8 <- survfit(survival2~agecat,data=survival)
survival8
summary(survival8,na.rm=T)
summary(survival8, times=seq(0, 120, 6))
plot(survival8)
# 另一个包（画K-M曲线的）
library(survminer)
ggsurvplot(survival8)
#添加曲线的置信区间，并增加long-rank检验的结果p值以及风险表格
ggsurvplot(survival8, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("young", "old"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen")
  )


##############################
##1.KM
####sex
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
# 构建生存对象
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$sex)
log
survival3 <- survfit(survival2~sex,data=survival)
summary(survival3,na.rm=T)
summary(survival3, times=seq(0, 120, 6))
ggsurvplot(survival3)
ggsurvplot(survival3, conf.int=TRUE, pval=TRUE, risk.table=TRUE,
           legend.labs=c("Female", "Male"),legend.title="Sex",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
   theme_survminer(font.main = c(16,"bold","darkblue"),
                   font.x = c(14,"bold.italic","red"),
                   font.y = c(14,"bold.italic","darkred"),
                   font.tickslab = c(12,"plain","darkgreen")
   )
############
###bone1
OBLOS <- read_xlsx("review12.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
str(survival)
survival$bone1
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
log<- survdiff(survival2~survival$bone1)
log
survival5 <- survfit(survival2~bone1,data=survival)
summary(survival5,na.rm=T)
summary(survival5, times=seq(0, 120, 6))
plot(survival5)
# 另一个包（画K-M曲线的）
library(survminer)
ggsurvplot(survival5)
#添加曲线的置信区间，并增加long-rank检验的结果p值以及风险表格
ggsurvplot(survival5, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("long bone","irregular bone","flat bone", "short bone"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50","#BDB76B","#008080"),xlab="Follow-up(months)",
           risk.table.height=.4)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen")
  )
##############
#bone2
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
log<- survdiff(survival2~survival$bone2)
log 
survival5 <- survfit(survival2~bone2,data=survival)
summary(survival5,na.rm=T)
summary(survival5, times=seq(0, 120, 6))
plot(survival5)
# 另一个包（画K-M曲线的）
library(survminer)
ggsurvplot(survival5)
#添加曲线的置信区间，并增加long-rank检验的结果p值以及风险表格
ggsurvplot(survival5, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("limb bone", "truncal bone","skull bone"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50","#00CED1"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen")
  )
###################
# radiotherapy
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:51,1:20]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$Radiotherapy)
log
survival6 <- survfit(survival2~Radiotherapy,data=survival)
summary(survival6,na.rm=T)
summary(survival6, times=seq(0, 120, 6))
ggsurvplot(survival6, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("Without RT", "RT"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen"),
                  surv.median.line = "hv"
  )
##############
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:60,1:20]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$Chemotherapy)
log
survival6 <- survfit(survival2~Chemotherapy,data=survival)
summary(survival6,na.rm=T)
summary(survival6, times=seq(0, 120, 6))
ggsurvplot(survival6, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("Without CT", "CT"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen"),
                  surv.median.line = "hv"
  )
######################
OBLOS <- read_xlsx("review12.xlsx")
OBLOS <- OBLOS[1:60,1:20]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$bone1)
log
survival6 <- survfit(survival2~bone1,data=survival)
summary(survival6,na.rm=T)
summary(survival6, times=seq(0, 120, 6))
ggsurvplot(survival6, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("long bone", "irregular bone"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen"),
                  surv.median.line = "hv"
  )
##############
OBLOS <- read_xlsx("review22.xlsx")
OBLOS <- OBLOS[1:60,1:20]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$bone2)
log
survival6 <- survfit(survival2~bone2,data=survival)
summary(survival6,na.rm=T)
summary(survival6, times=seq(0, 120, 6))
ggsurvplot(survival6, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("limb bone", "truncal bone"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen"),
                  surv.median.line = "hv"
  )
###########
long and flat
OBLOS <- read_xlsx("review13.xlsx")
OBLOS <- OBLOS[1:53,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
log<-survdiff(survival2~survival$bone1)
log
survival9 <- survfit(survival2~bone1,data=survival)
survival9
summary(survival9,na.rm=T)
summary(survival9, times=seq(0, 120, 6))
plot(survival9)
ggsurvplot(survival9, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("long bone","flat bone"),legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.45)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen")
  )
##########
long and short
OBLOS <- read_xlsx("review14.xlsx")
OBLOS <- OBLOS[1:53,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
log<-survdiff(survival2~survival$bone1)
log
survival9 <- survfit(survival2~bone1,data=survival)
survival9
summary(survival9,na.rm=T)
summary(survival9, times=seq(0, 120, 6))
plot(survival9)
ggsurvplot(survival9, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("long bone","short bone"),legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.45)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen")
  )
###########
OBLOS <- read_xlsx("review23.xlsx")
OBLOS <- OBLOS[1:60,1:20]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
log<-survdiff(survival2~survival$bone2)
log
survival6 <- survfit(survival2~bone2,data=survival)
summary(survival6,na.rm=T)
summary(survival6, times=seq(0, 120, 6))
ggsurvplot(survival6, pval=TRUE, risk.table=TRUE,conf.int=TRUE,
           legend.labs=c("limb bone", "skull bone"), legend.title="Group",
           palette=c("#1E90FF","#FF7F50"),xlab="Follow-up(months)",
           risk.table.height=.3)+
  theme_survminer(font.main = c(16,"bold","darkblue"),
                  font.x = c(14,"bold.italic","red"),
                  font.y = c(14,"bold.italic","darkred"),
                  font.tickslab = c(12,"plain","darkgreen"),
                  surv.median.line = "hv"
  )
###单因素 多因素分析 
rm(list =ls()
library("xlsx")
library(survival)
library(plyr)
##输出表格
##write.xlsx(char,'characteristics.xls',colnames=T,row.names=T,showNA=F)cox风险模型
OBLOS <- read_xlsx("review.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival$agecat <- cut(survival$"age", breaks=c(0,13,100),labels=c("young", "old"))
survival = tbl_df(survival)
survival
str(survival)
survival <- as_tibble(survival) 
survival = tbl_df(survival)
str(survival)
survival[,c(2,3,6,7)]=lapply(survival[,c(2,3,6,7)],as.numeric)
##，levels=c("1",'2'))##查看排序#
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
class(survival2) 
####memory.limit()
unicox = function(x){
  FML<-as.formula(paste0('survival2~',x))
  cox1 <- coxph(FML,data=survival)
  cox1 
  sexC <-summary(cox1)
  CI<-paste0(round(sexC$conf.int[,3:4],2),collapse='-')
  CI
  HR <- paste0(round(sexC$coefficients[,2],2),collapse='-')
  HR
  pvalue<- paste0(round(sexC$coefficients[,5],3),collapse='-')
  pvalue
  unicox<-data.frame('variable'=x,
                     'Hazard Ratio'=HR,
                     'CI95'=CI,
                     "p value"=pvalue)
return(unicox)
}
unicox('sex')
varnames=colnames(survival)
varnames=colnames(survival)[c(3,6,7,8,9,10)]
univar <- lapply(varnames,unicox)
univar <- ldply(univar,data.frame)
univar
#univar$"p.value"=as.numeric(univar$"p.value")
univar$p.value 
#factor(univar$p.value,ordered=T,levels=c("0.037",'0.189','0.614','0.799'))
#levels(univar$p.value)
as.character(univar$p.value)
univar$p.value=as.numeric(as.character(univar$p.value))
univar$variable[univar$p.value < 0.05]
####多因素回归
fml<-as.formula(paste0('survival2~',paste0(univar$variable[univar$p.value < 0.05],collapse = '+')))
  cox1 <- coxph(fml,data=survival)
  sexC <-summary(cox1)
  name<-as.character(univar$variable[univar$p.value < 0.05])
  CI1<-round(sexC$conf.int[,3],2)
  CI2<-round(sexC$conf.int[,4],2)
  CI<-paste0(CI1,"-",CI2)
  HR <- paste0(round(sexC$coefficients[,2],2))
  pvalue<- paste0(round(sexC$coefficients[,5],3))
  Unicox<-data.frame('variable'=name,
                     'Hazard Ratio'=HR,
                     'CI95'=CI,
                     "p value"=pvalue)

Unicox
#整合数据框
Final<- merge.data.frame(univar,Unicox,by='variable',all=T,sort = T)
###write.xlsx(Final,'Final.xls',colnames=T,row.names=T,showNA=F)
OBLOS <- read_xlsx("review23.xlsx")
OBLOS <- OBLOS[1:51,1:11]
survival <- as_tibble(OBLOS) 
survival = tbl_df(survival)
str(survival)
survival$bone2
survival$"follow -up"=as.numeric(survival$"follow -up",na.rm=T)
survival$"OS_status"=as.numeric(survival$"OS_status",na.rm=T)
survival2 <- Surv(time = survival$"follow -up", event = survival$"OS_status")
log<- survdiff(survival2~survival$bone2)
log
survival5 <- survfit(survival2~bone2,data=survival)
summary(survival5,na.rm=T)
summary(survival5, times=seq(0, 120, 6))
cox1 <- coxph(survival2~bone2,data=survival)
cox1 
summary(cox1)
