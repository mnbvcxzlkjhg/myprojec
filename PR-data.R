getwd()
setwd("C:\\RDASH")
s=read.csv("RESANAL.csv")

#FIRST SEM ALL BOYS AND GIRLS PASS PERCENTAGES BARPLOT

s2=subset(s,SEMNO.==1 & GROUP=="M.S.CS.")
for(i in 1:length(s2$RNO))
  ifelse(((s2$HVPE[i]<18) | (s2$ENV[i]<18) | (s2$ENG.I[i]<10) | (s2$ENG.E[i]<30) | (s2$SAN.I[i]<10) | (s2$SAN.E[i]<30) | (s2$C.1.INT[i]<10) | (s2$C.1.EXT[i]<30) | (s2$C.2.INT[i]<10) |(s2$C.2.EXT[i]<30) | (s2$C.2P[i]<18) | (s2$C.3.INT[i]<10) |(s2$C.3.EXT[i]<30) | (s2$C.3P[i]<18)),s2$overallresult[i]<-"FAIL",s2$overallresult[i]<-"PASS")  
s3=s2[-c(8:50)]
s4=subset(s3,GENDER=="M")
s5=subset(s3,GENDER=="F")
s4$overallresult=="FAIL"
bf11=length(s4$overallresult[s4$overallresult=="FAIL"])
s5$overallresult=="FAIL"
gf11=length(s5$overallresult[s5$overallresult=="FAIL"])
BPP11=round(((length(s4$RNO)-bf11)/length(s4$RNO))*100,digits = 0)
GPP11=round(((length(s5$RNO)-gf11)/length(s5$RNO))*100,digits = 0)

s7=subset(s,SEMNO.==1 & GROUP=="M.P.CS.")
for(i in 1:length(s7$RNO))
  ifelse(((s7$HVPE[i]<18) | (s7$ENV[i]<18) | (s7$ENG.I[i]<10) | (s7$ENG.E[i]<30) | (s7$SAN.I[i]<10) | (s7$SAN.E[i]<30) | (s7$C.1.INT[i]<10) | (s7$C.1.EXT[i]<30) | (s7$C.2.INT[i]<10) |(s7$C.2.EXT[i]<30) | (s7$C.2P[i]<18) | (s7$C.3.INT[i]<10) |(s7$C.3.EXT[i]<30) | (s7$C.3P[i]<18)),s7$overallresult[i]<-"FAIL",s7$overallresult[i]<-"PASS")  
s7=s7[-c(8:50)]
s8=subset(s7,GENDER=="M")
s9=subset(s7,GENDER=="F")
bf12=length(s8$overallresult[s8$overallresult=="FAIL"])
gf12=length(s9$overallresult[s9$overallresult=="FAIL"])
BPP12=round(((length(s8$RNO)-bf12)/length(s8$RNO))*100,digits = 0)
GPP12=round(((length(s9$RNO)-gf12)/length(s9$RNO))*100,digits=0)

s11=subset(s,SEMNO.==1 & GROUP=="M.P.C.")
for(i in 1:length(s11$RNO))
  ifelse(((s11$HVPE[i]<18) | (s11$ENV[i]<18) | (s11$ENG.I[i]<10) | (s11$ENG.E[i]<30) | (s11$SAN.I[i]<10) | (s11$SAN.E[i]<30) | (s11$C.1.INT[i]<10) | (s11$C.1.EXT[i]<30) | (s11$C.2.INT[i]<10) |(s11$C.2.EXT[i]<30) | (s11$C.2P[i]<18) | (s11$C.3.INT[i]<10) |(s11$C.3.EXT[i]<30) | (s11$C.3P[i]<18)),s11$overallresult[i]<-"FAIL",s11$overallresult[i]<-"PASS")  
s11=s11[-c(8:50)]
s12=subset(s11,GENDER=="M")
s13=subset(s11,GENDER=="F")
bf13=length(s12$overallresult[s12$overallresult=="FAIL"])
gf13=length(s13$overallresult[s13$overallresult=="FAIL"])
BPP13=round(((length(s12$RNO)-bf13)/length(s12$RNO))*100,digits = 0)
GPP13=round(((length(s13$RNO)-gf13)/length(s13$RNO))*100,digits=0)

s15=subset(s,SEMNO.==1 & GROUP=="B.B.C.")
for(i in 1:length(s15$RNO))
  ifelse(((s15$HVPE[i]<18) | (s15$ENV[i]<18) | (s15$ENG.I[i]<10) | (s15$ENG.E[i]<30) | (s15$SAN.I[i]<10) | (s15$SAN.E[i]<30) | (s15$C.1.INT[i]<10) | (s15$C.1.EXT[i]<30) | (s15$C.2.INT[i]<10) |(s15$C.2.EXT[i]<30) | (s15$C.2P[i]<18) | (s15$C.3.INT[i]<10) |(s15$C.3.EXT[i]<30) | (s15$C.3P[i]<18) | (s15$C.1P[i]<18)), s15$overallresult[i]<-"FAIL",s15$overallresult[i]<-"PASS")  

s15=s15[-c(8:50)]
s16=subset(s15,GENDER=="M")
s17=subset(s15,GENDER=="F")
s16$overallresult=="FAIL"
bf14=length(s16$overallresult[s16$overallresult=="FAIL"])
s17$overallresult=="FAIL"
gf14=length(s17$overallresult[s17$overallresult=="FAIL"])
BPP14=round(((length(s16$RNO)-bf14)/length(s16$RNO))*100,digits = 0)
GPP14=round(((length(s17$RNO)-gf14)/length(s17$RNO))*100,digits=0)

s19=subset(s,SEMNO.==1 & GROUP=="B.C.A.")
for(i in 1:length(s19$RNO))
  ifelse(((s19$HVPE[i]<18) | (s19$ENV[i]<18) | (s19$ENG.I[i]<10) | (s19$ENG.E[i]<30) | (s19$SAN.I[i]<10) | (s19$SAN.E[i]<30) | (s19$C.1.INT[i]<10) | (s19$C.1.EXT[i]<30) | (s19$C.2.INT[i]<10) |(s19$C.2.EXT[i]<30) | (s19$C.2P[i]<18) | (s19$C.3.INT[i]<10) |(s19$C.3.EXT[i]<30) | (s19$C.3P[i]<18)), s19$overallresult[i]<-"FAIL",s19$overallresult[i]<-"PASS")  
s19=s19[-c(8:50)]
s20=subset(s19,GENDER=="M")
s21=subset(s19,GENDER=="F")
bf15=length(s20$overallresult[s20$overallresult=="FAIL"])
gf15=length(s21$overallresult[s21$overallresult=="FAIL"])
BPP15=round(((length(s20$RNO)-bf15)/length(s20$RNO))*100,digits = 0)
GPP15=round(((length(s21$RNO)-gf15)/length(s21$RNO))*100,digits=0)

s22=c(BPP11,GPP11,BPP12,GPP12,BPP13,GPP13,BPP14,GPP14,BPP15,GPP15)
s23=c("BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS")
s24=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
a=c(1,1,2,2,3,3,4,4,5,5)
firstsem=data.frame(a,s22,s23,s24)
colnames(firstsem)=c("SEM NO","PASSPERCENTAGES","GENDER","GROUP")


library(ggplot2)
#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=firstsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FIRST SEM \n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

s26=subset(s,SEMNO.==2 & GROUP=="M.S.CS.")
for(i in 1:length(s26$RNO))
  ifelse(((s26$CSS.1[i]<18) | (s26$ICT.1[i]<18) | (s26$ENG.I[i]<10) | (s26$ENG.E[i]<30) | (s26$SAN.I[i]<10) | (s26$SAN.E[i]<30) | (s26$C.1.INT[i]<10) | (s26$C.1.EXT[i]<30) | (s26$C.2.INT[i]<10) |(s26$C.2.EXT[i]<30) | (s26$C.2P[i]<18) | (s26$C.3.INT[i]<10) |(s26$C.3.EXT[i]<30) | (s26$C.3P[i]<18)),s26$overallresult[i]<-"FAIL",s26$overallresult[i]<-"PASS")  
s26=s26[-c(8:50)]
s27=subset(s26,GENDER=="M")
s28=subset(s26,GENDER=="F")
bf21=length(s27$overallresult[s27$overallresult=="FAIL"])
gf21=length(s28$overallresult[s28$overallresult=="FAIL"])
BPP21=round(((length(s27$RNO)-bf21)/length(s27$RNO))*100,digits=0)
GPP21=round(((length(s28$RNO)-gf21)/length(s28$RNO))*100,digits = 0)

s29=subset(s,SEMNO.==2 & GROUP=="M.P.CS.")
for(i in 1:length(s29$RNO))
  ifelse(((s29$CSS.1[i]<18) | (s29$ICT.1[i]<18) | (s29$ENG.I[i]<10) | (s29$ENG.E[i]<30) | (s29$SAN.I[i]<10) | (s29$SAN.E[i]<30) | (s29$C.1.INT[i]<10) | (s29$C.1.EXT[i]<30) | (s29$C.2.INT[i]<10) |(s29$C.2.EXT[i]<30) | (s29$C.2P[i]<18) | (s29$C.3.INT[i]<10) |(s29$C.3.EXT[i]<30) | (s29$C.3P[i]<18)),s29$overallresult[i]<-"FAIL",s29$overallresult[i]<-"PASS")  
s29=s29[-c(8:50)]
s30=subset(s29,GENDER=="M")
s31=subset(s29,GENDER=="F")
bf22=length(s30$overallresult[s30$overallresult=="FAIL"])
gf22=length(s31$overallresult[s31$overallresult=="FAIL"])
BPP22=round(((length(s30$RNO)-bf22)/length(s30$RNO))*100,digits = 0)
GPP22=round(((length(s31$RNO)-gf22)/length(s31$RNO))*100,digits=0)

s32=subset(s,SEMNO.==2 & GROUP=="M.P.C.")
for(i in 1:length(s32$RNO))
  ifelse(((s32$CSS.1[i]<18) | (s32$ICT.1[i]<18) | (s32$ENG.I[i]<10) | (s32$ENG.E[i]<30) | (s32$SAN.I[i]<10) | (s32$SAN.E[i]<30) | (s32$C.1.INT[i]<10) | (s32$C.1.EXT[i]<30) | (s32$C.2.INT[i]<10) |(s32$C.2.EXT[i]<30) | (s32$C.2P[i]<18) | (s32$C.3.INT[i]<10) |(s32$C.3.EXT[i]<30) | (s32$C.3P[i]<18)),s32$overallresult[i]<-"FAIL",s32$overallresult[i]<-"PASS")  
s32=s32[-c(8:50)]
s33=subset(s32,GENDER=="M")
s34=subset(s32,GENDER=="F")
bf23=length(s33$overallresult[s33$overallresult=="FAIL"])
gf23=length(s34$overallresult[s34$overallresult=="FAIL"])
BPP23=round(((length(s33$RNO)-bf23)/length(s33$RNO))*100,digits = 0)
GPP23=round(((length(s34$RNO)-gf23)/length(s34$RNO))*100,digits=0)

s35=subset(s,SEMNO.==2 & GROUP=="B.B.C.")
for(i in 1:length(s35$RNO))
  ifelse(((s35$CSS.1[i]<18) | (s35$ICT.1[i]<18) | (s35$ENG.I[i]<10) | (s35$ENG.E[i]<30) | (s35$SAN.I[i]<10) | (s35$SAN.E[i]<30) | (s35$C.1.INT[i]<10) | (s35$C.1.EXT[i]<30) | (s35$C.2.INT[i]<10) |(s35$C.2.EXT[i]<30) | (s35$C.2P[i]<18) | (s35$C.3.INT[i]<10) |(s35$C.3.EXT[i]<30) | (s35$C.3P[i]<18) | (s35$C.1P[i]<18)),s35$overallresult[i]<-"FAIL",s35$overallresult[i]<-"PASS")  
s35=s35[-c(8:50)]
s36=subset(s35,GENDER=="M")
s37=subset(s35,GENDER=="F")
bf24=length(s36$overallresult[s36$overallresult=="FAIL"])
gf24=length(s37$overallresult[s37$overallresult=="FAIL"])
BPP24=round(((length(s36$RNO)-bf24)/length(s36$RNO))*100,digits = 0)
GPP24=round(((length(s37$RNO)-gf24)/length(s37$RNO))*100,digits=0)

s38=subset(s,SEMNO.==2 & GROUP=="B.C.A.")
for(i in 1:length(s38$RNO))
  ifelse(((s38$CSS.1[i]<18) | (s38$ICT.1[i]<18) | (s38$ENG.I[i]<10) | (s38$ENG.E[i]<30) | (s38$SAN.I[i]<10) | (s38$SAN.E[i]<30) | (s38$C.1.INT[i]<10) | (s38$C.1.EXT[i]<30) | (s38$C.2.INT[i]<10) |(s38$C.2.EXT[i]<30) | (s38$C.2P[i]<18) | (s38$C.3.INT[i]<10) |(s38$C.3.EXT[i]<30) | (s38$C.3P[i]<18)),s38$overallresult[i]<-"FAIL",s38$overallresult[i]<-"PASS")  
s38=s38[-c(8:50)]
s39=subset(s38,GENDER=="M")
s40=subset(s38,GENDER=="F")
bf25=length(s39$overallresult[s39$overallresult=="FAIL"])
gf25=length(s40$overallresult[s40$overallresult=="FAIL"])
BPP25=round(((length(s39$RNO)-bf25)/length(s39$RNO))*100,digits = 0)
GPP25=round(((length(s40$RNO)-gf25)/length(s40$RNO))*100,digits=0)

s41=c(BPP21,GPP21,BPP22,GPP22,BPP23,GPP23,BPP24,GPP24,BPP25,GPP25)
s42=c("BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS","BOYS","GIRLS")
s43=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
secondsem=data.frame(a,s41,s42,s43)
colnames(secondsem)=c("SEM NO","PASSPERCENTAGES","GENDER","GROUP")
#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=secondsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS SECOND SEM\n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))


s45=subset(s,SEMNO.==3 & GROUP=="M.S.CS.")
for(i in 1:length(s45$RNO))
  ifelse(((s45$CSS.2[i]<18) | (s45$ICT.2[i]<18) | (s45$ENG.I[i]<10) | (s45$ENG.E[i]<30) | (s45$SAN.I[i]<10) | (s45$SAN.E[i]<30) | (s45$C.1.INT[i]<10) | (s45$C.1.EXT[i]<30) | (s45$C.2.INT[i]<10) |(s45$C.2.EXT[i]<30) | (s45$C.2P[i]<18) | (s45$C.3.INT[i]<10) |(s45$C.3.EXT[i]<30) | (s45$C.3P[i]<18)),s45$overallresult[i]<-"FAIL",s45$overallresult[i]<-"PASS")  
s45=s45[-c(8:50)]
s46=subset(s45,GENDER=="M")
s47=subset(s45,GENDER=="F")
bf31=length(s46$overallresult[s46$overallresult=="FAIL"])
gf31=length(s47$overallresult[s47$overallresult=="FAIL"])
BPP31=round(((length(s46$RNO)-bf31)/length(s46$RNO))*100,digits = 0)
GPP31=round(((length(s47$RNO)-gf31)/length(s47$RNO))*100,digits = 0)

s48=subset(s,SEMNO.==3 & GROUP=="M.P.CS.")
for(i in 1:length(s48$RNO))
  ifelse(((s48$CSS.2[i]<18) | (s48$ICT.2[i]<18) | (s48$ENG.I[i]<10) | (s48$ENG.E[i]<30) | (s48$SAN.I[i]<10) | (s48$SAN.E[i]<30) | (s48$C.1.INT[i]<10) | (s48$C.1.EXT[i]<30) | (s48$C.2.INT[i]<10) |(s48$C.2.EXT[i]<30) | (s48$C.2P[i]<18) | (s48$C.3.INT[i]<10) |(s48$C.3.EXT[i]<30) | (s48$C.3P[i]<18)),s48$overallresult[i]<-"FAIL",s48$overallresult[i]<-"PASS")  
s48=s48[-c(8:50)]
s49=subset(s48,GENDER=="M")
s50=subset(s48,GENDER=="F")
bf32=length(s49$overallresult[s49$overallresult=="FAIL"])
gf32=length(s50$overallresult[s50$overallresult=="FAIL"])
BPP32=round(((length(s49$RNO)-bf32)/length(s49$RNO))*100,digits = 0)
GPP32=round(((length(s50$RNO)-gf32)/length(s50$RNO))*100,digits=0)

s51=subset(s,SEMNO.==3 & GROUP=="M.P.C.")
for(i in 1:length(s51$RNO))
  ifelse(((s51$CSS.2[i]<18) | (s51$ICT.2[i]<18) | (s51$ENG.I[i]<10) | (s51$ENG.E[i]<30) | (s51$SAN.I[i]<10) | (s51$SAN.E[i]<30) | (s51$C.1.INT[i]<10) | (s51$C.1.EXT[i]<30) | (s51$C.2.INT[i]<10) |(s51$C.2.EXT[i]<30) | (s51$C.2P[i]<18) | (s51$C.3.INT[i]<10) |(s51$C.3.EXT[i]<30) | (s51$C.3P[i]<18)),s51$overallresult[i]<-"FAIL",s51$overallresult[i]<-"PASS")  
s51=s51[-c(8:50)]
s52=subset(s51,GENDER=="M")
s53=subset(s51,GENDER=="F")
bf33=length(s52$overallresult[s52$overallresult=="FAIL"])
gf33=length(s53$overallresult[s53$overallresult=="FAIL"])
BPP33=round(((length(s52$RNO)-bf33)/length(s52$RNO))*100,digits = 0)
GPP33=round(((length(s53$RNO)-gf33)/length(s53$RNO))*100,digits=0)

s54=subset(s,SEMNO.==3 & GROUP=="B.B.C.")
for(i in 1:length(s54$RNO))
  ifelse(((s54$CSS.2[i]<18) | (s54$ICT.2[i]<18) | (s54$ENG.I[i]<10) | (s54$ENG.E[i]<30) | (s54$SAN.I[i]<10) | (s54$SAN.E[i]<30) | (s54$C.1.INT[i]<10) | (s54$C.1.EXT[i]<30) | (s54$C.2.INT[i]<10) |(s54$C.2.EXT[i]<30) | (s54$C.2P[i]<18) | (s54$C.3.INT[i]<10) |(s54$C.3.EXT[i]<30) | (s54$C.3P[i]<18) | (s54$C.1P[i]<18)),s54$overallresult[i]<-"FAIL",s54$overallresult[i]<-"PASS")  
s54=s54[-c(8:50)]
s55=subset(s54,GENDER=="M")
s56=subset(s54,GENDER=="F")
bf34=length(s55$overallresult[s55$overallresult=="FAIL"])
gf34=length(s56$overallresult[s56$overallresult=="FAIL"])
BPP34=round(((length(s55$RNO)-bf34)/length(s55$RNO))*100,digits = 0)
GPP34=round(((length(s56$RNO)-gf34)/length(s56$RNO))*100,digits=0)

s57=subset(s,SEMNO.==3 & GROUP=="B.C.A.")
for(i in 1:length(s57$RNO))
  ifelse(((s57$CSS.2[i]<18) | (s57$ICT.2[i]<18) | (s57$ENG.I[i]<10) | (s57$ENG.E[i]<30) | (s57$TALLY[i]<18)  | (s57$C.1.INT[i]<10) | (s57$C.1.EXT[i]<30) | (s57$C.2.INT[i]<10) |(s57$C.2.EXT[i]<30) | (s57$C.2P[i]<18) | (s57$C.3.INT[i]<10) |(s57$C.3.EXT[i]<30) | (s57$C.3P[i]<18)),s57$overallresult[i]<-"FAIL",s57$overallresult[i]<-"PASS")  
s57=s57[-c(8:50)]
s58=subset(s57,GENDER=="M")
s59=subset(s57,GENDER=="F")
bf35=length(s58$overallresult[s58$overallresult=="FAIL"])
gf35=length(s59$overallresult[s59$overallresult=="FAIL"])
BPP35=round(((length(s58$RNO)-bf35)/length(s58$RNO))*100,digits = 0)
GPP35=round(((length(s59$RNO)-gf35)/length(s59$RNO))*100,digits=0)

s60=c(BPP31,GPP31,BPP32,GPP32,BPP33,GPP33,BPP34,GPP34,BPP35,GPP35)
s61=c("BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP")
s62=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
thirdsem=data.frame(a,s60,s61,s62)
colnames(thirdsem)=c("SEM NO","PASSPERCENTAGES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=thirdsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS THIRD SEM\n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))


s64=subset(s,SEMNO.==4 & GROUP=="M.S.CS.")
for(i in 1:length(s64$RNO))
  ifelse(((s64$CSS.3[i]<18) | (s64$ANAL[i]<18) | (s64$LEAD[i]<10) | (s64$ENTP[i]<10) | (s64$C.1.INT[i]<10) | (s64$C.1.EXT[i]<30) | (s64$C.2.INT[i]<10) |(s64$C.2.EXT[i]<30) | (s64$C.2P[i]<18) | (s64$C.3.INT[i]<10) |(s64$C.3.EXT[i]<30) | (s64$C.3P[i]<18)),s64$overallresult[i]<-"FAIL",s64$overallresult[i]<-"PASS")  
s64=s64[-c(8:50)]
s65=subset(s64,GENDER=="M")
s66=subset(s64,GENDER=="F")
bf41=length(s65$overallresult[s65$overallresult=="FAIL"])
gf41=length(s66$overallresult[s66$overallresult=="FAIL"])
BPP41=round(((length(s65$RNO)-bf41)/length(s65$RNO))*100,digits = 0)
GPP41=round(((length(s66$RNO)-gf41)/length(s66$RNO))*100,digits = 0)

s67=subset(s,SEMNO.==4 & GROUP=="M.P.CS.")
for(i in 1:length(s67$RNO))
  ifelse(((s67$CSS.3[i]<18) | (s67$ANAL[i]<18) | (s67$LEAD[i]<10) | (s67$ENTP[i]<10) | (s67$C.1.INT[i]<10) | (s67$C.1.EXT[i]<30) | (s67$C.2.INT[i]<10) |(s67$C.2.EXT[i]<30) | (s67$C.2P[i]<18) | (s67$C.3.INT[i]<10) |(s67$C.3.EXT[i]<30) | (s67$C.3P[i]<18)),s67$overallresult[i]<-"FAIL",s67$overallresult[i]<-"PASS")  
s67=s67[-c(8:50)]
s68=subset(s67,GENDER=="M")
s69=subset(s67,GENDER=="F")
bf42=length(s68$overallresult[s68$overallresult=="FAIL"])
gf42=length(s69$overallresult[s69$overallresult=="FAIL"])
BPP42=round(((length(s68$RNO)-bf42)/length(s68$RNO))*100,digits = 0)
GPP42=round(((length(s69$RNO)-gf42)/length(s69$RNO))*100,digits=0)

s70=subset(s,SEMNO.==4 & GROUP=="M.P.C.")
for(i in 1:length(s70$RNO))
  ifelse(((s70$CSS.3[i]<18) | (s70$ANAL[i]<18) | (s70$LEAD[i]<10) | (s70$ENTP[i]<10) | (s70$C.1.INT[i]<10) | (s70$C.1.EXT[i]<30) | (s70$C.2.INT[i]<10) |(s70$C.2.EXT[i]<30) | (s70$C.2P[i]<18) | (s70$C.3.INT[i]<10) |(s70$C.3.EXT[i]<30) | (s70$C.3P[i]<18)),s70$overallresult[i]<-"FAIL",s70$overallresult[i]<-"PASS")  
s70=s70[-c(8:50)]
s71=subset(s70,GENDER=="M")
s72=subset(s70,GENDER=="F")
bf43=length(s71$overallresult[s71$overallresult=="FAIL"])
gf43=length(s72$overallresult[s72$overallresult=="FAIL"])
BPP43=round(((length(s71$RNO)-bf43)/length(s71$RNO))*100,digits = 0)
GPP43=round(((length(s72$RNO)-gf43)/length(s72$RNO))*100,digits=0)

s73=subset(s,SEMNO.==4 & GROUP=="B.B.C.")
for(i in 1:length(s73$RNO))
  ifelse(((s73$CSS.3[i]<18) | (s73$ANAL[i]<18) | (s73$LEAD[i]<10) | (s73$ENTP[i]<10) | (s73$C.1.INT[i]<10) | (s73$C.1.EXT[i]<30) | (s73$C.2.INT[i]<10) |(s73$C.2.EXT[i]<30) | (s73$C.2P[i]<18) | (s73$C.3.INT[i]<10) |(s73$C.3.EXT[i]<30) | (s73$C.3P[i]<18)),s73$overallresult[i]<-"FAIL",s73$overallresult[i]<-"PASS")  
s73=s73[-c(8:50)]
s74=subset(s73,GENDER=="M")
s75=subset(s73,GENDER=="F")
bf44=length(s74$overallresult[s74$overallresult=="FAIL"])
gf44=length(s75$overallresult[s75$overallresult=="FAIL"])
BPP44=round(((length(s74$RNO)-bf44)/length(s74$RNO))*100,digits = 0)
GPP44=round(((length(s75$RNO)-gf44)/length(s75$RNO))*100,digits=0)

s76=subset(s,SEMNO.==4 & GROUP=="B.C.A.")
for(i in 1:length(s76$RNO))
  ifelse(((s76$CSS.3[i]<18) | (s76$ANAL[i]<18) | (s76$DRVW[i]<18) | (s76$CITI[i]<18) | (s76$C.1.INT[i]<10) | (s76$C.1.EXT[i]<30) | (s76$C.2.INT[i]<10) |(s76$C.2.EXT[i]<30) | (s76$C.2P[i]<18) | (s76$C.3.INT[i]<10) |(s76$C.3.EXT[i]<30) | (s76$C.3P[i]<18) | (s76$C.1P[i]<18)),s76$overallresult[i]<-"FAIL",s76$overallresult[i]<-"PASS")  
s76=s76[-c(8:50)]
s77=subset(s76,GENDER=="M")
s78=subset(s76,GENDER=="F")
bf45=length(s77$overallresult[s77$overallresult=="FAIL"])
gf45=length(s78$overallresult[s78$overallresult=="FAIL"])
BPP45=round(((length(s77$RNO)-bf45)/length(s77$RNO))*100,digits = 0)
GPP45=round(((length(s78$RNO)-gf45)/length(s78$RNO))*100,digits=0)

s79=c(BPP41,GPP41,BPP42,GPP42,BPP43,GPP43,BPP44,GPP44,BPP45,GPP45)
s80=c("BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP")
s81=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
fourthsem=data.frame(a,s79,s80,s81)
colnames(fourthsem)=c("SEM NO","PASSPERCENTAGES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=fourthsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FOURTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))


s83=subset(s,SEMNO.==5 & GROUP=="M.S.CS.")
for(i in 1:length(s83$RNO))
  ifelse(((s83$C.4.INT[i]<10) | (s83$C.4.EXT[i]<30) | (s83$C.5.INT[i]<10) | (s83$C.5.EXT[i]<30) | (s83$C.5P[i]<18) | (s83$C.6.INT[i]<10) | (s83$C.6.EXT[i]<30) | (s83$C.6P[i]<18) | (s83$C.1.INT[i]<10) | (s83$C.1.EXT[i]<30) | (s83$C.2.INT[i]<10) |(s83$C.2.EXT[i]<30) | (s83$C.2P[i]<18) | (s83$C.3.INT[i]<10) |(s83$C.3.EXT[i]<30) | (s83$C.3P[i]<18)),s83$overallresult[i]<-"FAIL",s83$overallresult[i]<-"PASS")  
s83=s83[-c(8:50)]
s84=subset(s83,GENDER=="M")
s85=subset(s83,GENDER=="F")
bf51=length(s84$overallresult[s84$overallresult=="FAIL"])
gf51=length(s85$overallresult[s85$overallresult=="FAIL"])
BPP51=round(((length(s84$RNO)-bf51)/length(s84$RNO))*100,digits = 0)
GPP51=round(((length(s85$RNO)-gf51)/length(s85$RNO))*100,digits = 0)

s86=subset(s,SEMNO.==5 & GROUP=="M.P.CS.")
for(i in 1:length(s86$RNO))
  ifelse(((s86$C.4.INT[i]<10) | (s86$C.4.EXT[i]<30) | (s86$C.5.INT[i]<10) | (s86$C.5.EXT[i]<30) | (s86$C.5P[i]<18) | (s86$C.6.INT[i]<10) | (s86$C.6.EXT[i]<30) | (s86$C.6P[i]<18) | (s86$C.1.INT[i]<10) | (s86$C.1.EXT[i]<30) | (s86$C.2.INT[i]<10) |(s86$C.2.EXT[i]<30) | (s86$C.2P[i]<18) | (s86$C.3.INT[i]<10) |(s86$C.3.EXT[i]<30) | (s86$C.3P[i]<18)),s86$overallresult[i]<-"FAIL",s86$overallresult[i]<-"PASS")  
s86=s86[-c(8:50)]
s87=subset(s86,GENDER=="M")
s88=subset(s86,GENDER=="F")
bf52=length(s87$overallresult[s87$overallresult=="FAIL"])
gf52=length(s88$overallresult[s88$overallresult=="FAIL"])
BPP52=round(((length(s87$RNO)-bf52)/length(s87$RNO))*100,digits = 0)
GPP52=round(((length(s88$RNO)-gf52)/length(s88$RNO))*100,digits=0)

s89=subset(s,SEMNO.==5 & GROUP=="M.P.C.")
for(i in 1:length(s89$RNO))
  ifelse(((s89$C.4.INT[i]<10) | (s89$C.4.EXT[i]<30) | (s89$C.5.INT[i]<10) | (s89$C.5.EXT[i]<30) | (s89$C.5P[i]<18) | (s89$C.6.INT[i]<10) | (s89$C.6.EXT[i]<30) | (s89$C.6P[i]<18) | (s89$C.1.INT[i]<10) | (s89$C.1.EXT[i]<30) | (s89$C.2.INT[i]<10) |(s89$C.2.EXT[i]<30) | (s89$C.2P[i]<18) | (s89$C.3.INT[i]<10) |(s89$C.3.EXT[i]<30) | (s89$C.3P[i]<18)),s89$overallresult[i]<-"FAIL",s89$overallresult[i]<-"PASS")  
s89=s89[-c(8:50)]
s90=subset(s89,GENDER=="M")
s91=subset(s89,GENDER=="F")
bf53=length(s90$overallresult[s90$overallresult=="FAIL"])
gf53=length(s91$overallresult[s91$overallresult=="FAIL"])
BPP53=round(((length(s90$RNO)-bf53)/length(s90$RNO))*100,digits = 0)
GPP53=round(((length(s91$RNO)-gf53)/length(s91$RNO))*100,digits=0)

s92=subset(s,SEMNO.==5 & GROUP=="B.B.C.")
for(i in 1:length(s92$RNO))
  ifelse(((s92$C.4.INT[i]<10) | (s92$C.4.EXT[i]<30) | (s92$C.5.INT[i]<10) | (s92$C.5.EXT[i]<30) | (s92$C.5P[i]<18) | (s92$C.6.INT[i]<10) | (s92$C.6.EXT[i]<30) | (s92$C.6P[i]<18) | (s92$C.1.INT[i]<10) | (s92$C.1.EXT[i]<30) | (s92$C.2.INT[i]<10) |(s92$C.2.EXT[i]<30) | (s92$C.2P[i]<18) | (s92$C.3.INT[i]<10) |(s92$C.3.EXT[i]<30) | (s92$C.3P[i]<18) |(s92$C.1P[i]<18) | (s92$C.4P[i]<18)),s92$overallresult[i]<-"FAIL",s92$overallresult[i]<-"PASS")  
s92=s92[-c(8:50)]
s93=subset(s92,GENDER=="M")
s94=subset(s92,GENDER=="F")
bf54=length(s93$overallresult[s93$overallresult=="FAIL"])
gf54=length(s94$overallresult[s94$overallresult=="FAIL"])
BPP54=round(((length(s93$RNO)-bf54)/length(s93$RNO))*100,digits = 0)
GPP54=round(((length(s94$RNO)-gf54)/length(s94$RNO))*100,digits=0)

s95=subset(s,SEMNO.==5 & GROUP=="B.C.A.")
for(i in 1:length(s95$RNO))
  ifelse(((s95$C.4.INT[i]<10) | (s95$C.4.EXT[i]<30) | (s95$C.5.INT[i]<10) | (s95$C.5.EXT[i]<30) | (s95$C.5P[i]<18) | (s95$C.1.INT[i]<10) | (s95$C.1.EXT[i]<30) | (s95$C.2.INT[i]<10) |(s95$C.2.EXT[i]<30) | (s95$C.3.INT[i]<10) |(s95$C.3.EXT[i]<30) | (s95$C.4P[i]<18)),s95$overallresult[i]<-"FAIL",s95$overallresult[i]<-"PASS")  
s95=s95[-c(8:50)]
s96=subset(s95,GENDER=="M")
s97=subset(s95,GENDER=="F")
bf55=length(s96$overallresult[s96$overallresult=="FAIL"])
gf55=length(s97$overallresult[s97$overallresult=="FAIL"])
BPP55=round(((length(s96$RNO)-bf55)/length(s96$RNO))*100,digits = 0)
GPP55=round(((length(s97$RNO)-gf55)/length(s97$RNO))*100,digits=0)

s98=c(BPP51,GPP51,BPP52,GPP52,BPP53,GPP53,BPP54,GPP54,BPP55,GPP55)
s99=c("BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP","BPP","GPP")
s100=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
fifthsem=data.frame(a,s98,s99,s100)
colnames(fifthsem)=c("SEM NO","PASSPERCENTAGES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=fifthsem, aes(x=GROUP, y=PASSPERCENTAGES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=PASSPERCENTAGES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FIFTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

ppresult=data.frame(firstsem,secondsem,thirdsem,fourthsem,fifthsem)
