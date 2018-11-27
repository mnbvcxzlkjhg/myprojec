getwd()
setwd("C:\\RDASH")
s=read.csv("RESANAL.csv")

s2=subset(s,SEMNO.==1 & GROUP=="M.S.CS." )
for(i in 1:length(s2$RNO))
  ifelse(((s2$HVPE[i]==0) | (s2$ENV[i]==0) | (s2$ENG.I[i]==0) | (s2$ENG.E[i]==0) | (s2$SAN.I[i]==0) | (s2$SAN.E[i]==0) | (s2$C.1.INT[i]==0) | (s2$C.1.EXT[i]==0) | (s2$C.2.INT[i]==0) |(s2$C.2.EXT[i]==0) | (s2$C.2P[i]==0) | (s2$C.3.INT[i]==0) |(s2$C.3.EXT[i]==0) | (s2$C.3P[i]==0)),s2$overallresult[i]<-"ABSENT",s2$overallresult[i]<-"PRESENT")  
s3=s2[-c(8:50)]
s4=subset(s3,GENDER=="M")
s5=subset(s3,GENDER=="F")
ba11=length(s4$overallresult[s4$overallresult=="ABSENT"])
ga11=length(s5$overallresult[s5$overallresult=="ABSENT"])

s7=subset(s,SEMNO.==1 & GROUP=="M.P.CS.")
for(i in 1:length(s7$RNO))
  ifelse(((s7$HVPE[i]==0) | (s7$ENV[i]==0) | (s7$ENG.I[i]==0) | (s7$ENG.E[i]==0) | (s7$SAN.I[i]==0) | (s7$SAN.E[i]==0) | (s7$C.1.INT[i]==0) | (s7$C.1.EXT[i]==0) | (s7$C.2.INT[i]==0) |(s7$C.2.EXT[i]==0) | (s7$C.2P[i]==0) | (s7$C.3.INT[i]==0) |(s7$C.3.EXT[i]==0) | (s7$C.3P[i]==0)),s7$overallresult[i]<-"ABSENT",s7$overallresult[i]<-"PRESENT")  
s7=s7[-c(8:50)]
s8=subset(s7,GENDER=="M")
s9=subset(s7,GENDER=="F")
ba12=length(s8$overallresult[s8$overallresult=="ABSENT"])
ga12=length(s9$overallresult[s9$overallresult=="ABSENT"])

s11=subset(s,SEMNO.==1 & GROUP=="M.P.C.")
for(i in 1:length(s11$RNO))
  ifelse(((s11$HVPE[i]==0) | (s11$ENV[i]==0) | (s11$ENG.I[i]==0) | (s11$ENG.E[i]==0) | (s11$SAN.I[i]==0) | (s11$SAN.E[i]==0) | (s11$C.1.INT[i]==0) | (s11$C.1.EXT[i]==0) | (s11$C.2.INT[i]==0) |(s11$C.2.EXT[i]==0) | (s11$C.2P[i]==0) | (s11$C.3.INT[i]==0) |(s11$C.3.EXT[i]==0) | (s11$C.3P[i]==0)),s11$overallresult[i]<-"ABSENT",s11$overallresult[i]<-"PRESENT")  
s11=s11[-c(8:50)]
s12=subset(s11,GENDER=="M")
s13=subset(s11,GENDER=="F")
ba13=length(s12$overallresult[s12$overallresult=="ABSENT"])
ga13=length(s13$overallresult[s13$overallresult=="ABSENT"])

s15=subset(s,SEMNO.==1 & GROUP=="B.B.C.")
for(i in 1:length(s15$RNO))
  ifelse(((s15$HVPE[i]==0) | (s15$ENV[i]==0) | (s15$ENG.I[i]==0) | (s15$ENG.E[i]==0) | (s15$SAN.I[i]==0) | (s15$SAN.E[i]==0) | (s15$C.1.INT[i]==0) | (s15$C.1.EXT[i]==0) | (s15$C.2.INT[i]==0) |(s15$C.2.EXT[i]==0) | (s15$C.2P[i]==0) | (s15$C.3.INT[i]==0) |(s15$C.3.EXT[i]==0) | (s15$C.3P[i]==0) | (s15$C.1P[i]==0)), s15$overallresult[i]<-"ABSENT",s15$overallresult[i]<-"PRESENT")  
s15=s15[-c(8:50)]
s16=subset(s15,GENDER=="M")
s17=subset(s15,GENDER=="F")
ba14=length(s16$overallresult[s16$overallresult=="ABSENT"])
ga14=length(s17$overallresult[s17$overallresult=="ABSENT"])

s19=subset(s,SEMNO.==1 & GROUP=="B.C.A.")
for(i in 1:length(s19$RNO))
  ifelse(((s19$HVPE[i]==0) | (s19$ENV[i]==0) | (s19$ENG.I[i]==0) | (s19$ENG.E[i]==0) | (s19$SAN.I[i]==0) | (s19$SAN.E[i]==0) | (s19$C.1.INT[i]==0) | (s19$C.1.EXT[i]==0) | (s19$C.2.INT[i]==0) |(s19$C.2.EXT[i]==0) | (s19$C.2P[i]==0) | (s19$C.3.INT[i]==0) |(s19$C.3.EXT[i]==0) | (s19$C.3P[i]==0)), s19$overallresult[i]<-"ABSENT",s19$overallresult[i]<-"PRESENT")  
s19=s19[-c(8:50)]
s20=subset(s19,GENDER=="M")
s21=subset(s19,GENDER=="F")
ba15=length(s20$overallresult[s20$overallresult=="ABSENT"])
ga15=length(s21$overallresult[s21$overallresult=="ABSENT"])

s22=c(ba11,ga11,ba12,ga12,ba13,ga13,ba14,ga14,ba15,ga15)
s23=c("BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS")
s24=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
b=c(1,2,1,2,1,2,1,2,1,2)
firstsemA=data.frame(b,s22,s23,s24)
colnames(firstsemA)=c("SEM NO","ABSENTIES","GENDER","GROUP")

library(ggplot2)
#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=firstsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FIRST SEM \n ABSENTIES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

s26=subset(s,SEMNO.==2 & GROUP=="M.S.CS.")
for(i in 1:length(s26$RNO))
  ifelse(((s26$CSS.1[i]==0) | (s26$ICT.1[i]==0) | (s26$ENG.I[i]==0) | (s26$ENG.E[i]==0) | (s26$SAN.I[i]==0) | (s26$SAN.E[i]==0) | (s26$C.1.INT[i]==0) | (s26$C.1.EXT[i]==0) | (s26$C.2.INT[i]==0) |(s26$C.2.EXT[i]==0) | (s26$C.2P[i]==0) | (s26$C.3.INT[i]==0) |(s26$C.3.EXT[i]==0) | (s26$C.3P[i]==0)),s26$overallresult[i]<-"ABSENT",s26$overallresult[i]<-"PRESENT")  
s26=s26[-c(8:50)]
s27=subset(s26,GENDER=="M")
s28=subset(s26,GENDER=="F")
ba21=length(s27$overallresult[s27$overallresult=="ABSENT"])
ga21=length(s28$overallresult[s28$overallresult=="ABSENT"])

s29=subset(s,SEMNO.==2 & GROUP=="M.P.CS.")
for(i in 1:length(s29$RNO))
  ifelse(((s29$CSS.1[i]==0) | (s29$ICT.1[i]==0) | (s29$ENG.I[i]==0) | (s29$ENG.E[i]==0) | (s29$SAN.I[i]==0) | (s29$SAN.E[i]==0) | (s29$C.1.INT[i]==0) | (s29$C.1.EXT[i]==0) | (s29$C.2.INT[i]==0) |(s29$C.2.EXT[i]==0) | (s29$C.2P[i]==0) | (s29$C.3.INT[i]==0) |(s29$C.3.EXT[i]==0) | (s29$C.3P[i]==0)),s29$overallresult[i]<-"ABSENT",s29$overallresult[i]<-"PRESENT")  
s29=s29[-c(8:50)]
s30=subset(s29,GENDER=="M")
s31=subset(s29,GENDER=="F")
ba22=length(s30$overallresult[s30$overallresult=="ABSENT"])
ga22=length(s31$overallresult[s31$overallresult=="ABSENT"])

s32=subset(s,SEMNO.==2 & GROUP=="M.P.C.")
for(i in 1:length(s32$RNO))
  ifelse(((s32$CSS.1[i]==0) | (s32$ICT.1[i]==0) | (s32$ENG.I[i]==0) | (s32$ENG.E[i]==0) | (s32$SAN.I[i]==0) | (s32$SAN.E[i]==0) | (s32$C.1.INT[i]==0) | (s32$C.1.EXT[i]==0) | (s32$C.2.INT[i]==0) |(s32$C.2.EXT[i]==0) | (s32$C.2P[i]==0) | (s32$C.3.INT[i]==0) |(s32$C.3.EXT[i]==0) | (s32$C.3P[i]==0)),s32$overallresult[i]<-"ABSENT",s32$overallresult[i]<-"PRESENT")  
s32=s32[-c(8:50)]
s33=subset(s32,GENDER=="M")
s34=subset(s32,GENDER=="F")
ba23=length(s33$overallresult[s33$overallresult=="ABSENT"])
ga23=length(s34$overallresult[s34$overallresult=="ABSENT"])

s35=subset(s,SEMNO.==2 & GROUP=="B.B.C.")
for(i in 1:length(s35$RNO))
  ifelse(((s35$CSS.1[i]==0) | (s35$ICT.1[i]==0) | (s35$ENG.I[i]==0) | (s35$ENG.E[i]==0) | (s35$SAN.I[i]==0) | (s35$SAN.E[i]==0) | (s35$C.1.INT[i]==0) | (s35$C.1.EXT[i]==0) | (s35$C.2.INT[i]==0) |(s35$C.2.EXT[i]==0) | (s35$C.2P[i]==0) | (s35$C.3.INT[i]==0) |(s35$C.3.EXT[i]==0) | (s35$C.3P[i]==0) | (s35$C.1P[i]==0)),s35$overallresult[i]<-"ABSENT",s35$overallresult[i]<-"PRESENT")  
s35=s35[-c(8:50)]
s36=subset(s35,GENDER=="M")
s37=subset(s35,GENDER=="F")
ba24=length(s36$overallresult[s36$overallresult=="ABSENT"])
ga24=length(s37$overallresult[s37$overallresult=="ABSENT"])

s38=subset(s,SEMNO.==2 & GROUP=="B.C.A.")
for(i in 1:length(s38$RNO))
  ifelse(((s38$CSS.1[i]==0) | (s38$ICT.1[i]==0) | (s38$ENG.I[i]==0) | (s38$ENG.E[i]==0) | (s38$SAN.I[i]==0) | (s38$SAN.E[i]==0) | (s38$C.1.INT[i]==0) | (s38$C.1.EXT[i]==0) | (s38$C.2.INT[i]==0) |(s38$C.2.EXT[i]==0) | (s38$C.2P[i]==0) | (s38$C.3.INT[i]==0) |(s38$C.3.EXT[i]==0) | (s38$C.3P[i]==0)),s38$overallresult[i]<-"ABSENT",s38$overallresult[i]<-"PRESENT")  
s38=s38[-c(8:50)]
s39=subset(s38,GENDER=="M")
s40=subset(s38,GENDER=="F")
ba25=length(s39$overallresult[s39$overallresult=="ABSENT"])
ga25=length(s40$overallresult[s40$overallresult=="ABSENT"])

s41=c(ba21,ga21,ba22,ga22,ba23,ga23,ba24,ga24,ba25,ga25)
s42=c("BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS")
s43=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
secondsemA=data.frame(b,s41,s42,s43)
colnames(secondsemA)=c("SEM NO","ABSENTIES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=secondsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS SECOND SEM\n ABSENTIES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

s45=subset(s,SEMNO.==3 & GROUP=="M.S.CS.")
for(i in 1:length(s45$RNO))
  ifelse(((s45$CSS.2[i]==0) | (s45$ICT.2[i]==0) | (s45$ENG.I[i]==0) | (s45$ENG.E[i]==0) | (s45$SAN.I[i]==0) | (s45$SAN.E[i]==0) | (s45$C.1.INT[i]==0) | (s45$C.1.EXT[i]==0) | (s45$C.2.INT[i]==0) |(s45$C.2.EXT[i]==0) | (s45$C.2P[i]==0) | (s45$C.3.INT[i]==0) |(s45$C.3.EXT[i]==0) | (s45$C.3P[i]==0)),s45$overallresult[i]<-"ABSENT",s45$overallresult[i]<-"PRESENT")  
s45=s45[-c(8:50)]
s46=subset(s45,GENDER=="M")
s47=subset(s45,GENDER=="F")
ba31=length(s46$overallresult[s46$overallresult=="ABSENT"])
ga31=length(s47$overallresult[s47$overallresult=="ABSENT"])

s48=subset(s,SEMNO.==3 & GROUP=="M.P.CS.")
for(i in 1:length(s48$RNO))
  ifelse(((s48$CSS.2[i]==0) | (s48$ICT.2[i]==0) | (s48$ENG.I[i]==0) | (s48$ENG.E[i]==0) | (s48$SAN.I[i]==0) | (s48$SAN.E[i]==0) | (s48$C.1.INT[i]==0) | (s48$C.1.EXT[i]==0) | (s48$C.2.INT[i]==0) |(s48$C.2.EXT[i]==0) | (s48$C.2P[i]==0) | (s48$C.3.INT[i]==0) |(s48$C.3.EXT[i]==0) | (s48$C.3P[i]==0)),s48$overallresult[i]<-"ABSENT",s48$overallresult[i]<-"PRESENT")  
s48=s48[-c(8:50)]
s49=subset(s48,GENDER=="M")
s50=subset(s48,GENDER=="F")
ba32=length(s49$overallresult[s49$overallresult=="ABSENT"])
ga32=length(s50$overallresult[s50$overallresult=="ABSENT"])

s51=subset(s,SEMNO.==3 & GROUP=="M.P.C.")
for(i in 1:length(s51$RNO))
  ifelse(((s51$CSS.2[i]==0) | (s51$ICT.2[i]==0) | (s51$ENG.I[i]==0) | (s51$ENG.E[i]==0) | (s51$SAN.I[i]==0) | (s51$SAN.E[i]==0) | (s51$C.1.INT[i]==0) | (s51$C.1.EXT[i]==0) | (s51$C.2.INT[i]==0) |(s51$C.2.EXT[i]==0) | (s51$C.2P[i]==0) | (s51$C.3.INT[i]==0) |(s51$C.3.EXT[i]==0) | (s51$C.3P[i]==0)),s51$overallresult[i]<-"ABSENT",s51$overallresult[i]<-"PRECENT")  
s51=s51[-c(8:50)]
s52=subset(s51,GENDER=="M")
s53=subset(s51,GENDER=="F")
ba33=length(s52$overallresult[s52$overallresult=="ABSENT"])
ga33=length(s53$overallresult[s53$overallresult=="ABSENT"])

s54=subset(s,SEMNO.==3 & GROUP=="B.B.C.")
for(i in 1:length(s54$RNO))
  ifelse(((s54$CSS.2[i]==0) | (s54$ICT.2[i]==0) | (s54$ENG.I[i]==0) | (s54$ENG.E[i]==0) | (s54$SAN.I[i]==0) | (s54$SAN.E[i]==0) | (s54$C.1.INT[i]==0) | (s54$C.1.EXT[i]==0) | (s54$C.2.INT[i]==0) |(s54$C.2.EXT[i]==0) | (s54$C.2P[i]==0) | (s54$C.3.INT[i]==0) |(s54$C.3.EXT[i]==0) | (s54$C.3P[i]==0) | (s54$C.1P[i]==0)),s54$overallresult[i]<-"ABSENT",s54$overallresult[i]<-"PRESENT")  
s54=s54[-c(8:50)]
s55=subset(s54,GENDER=="M")
s56=subset(s54,GENDER=="F")
ba34=length(s55$overallresult[s55$overallresult=="ABSENT"])
ga34=length(s56$overallresult[s56$overallresult=="ABSENT"])

s57=subset(s,SEMNO.==3 & GROUP=="B.C.A.")
for(i in 1:length(s57$RNO))
  ifelse(((s57$CSS.2[i]==0) | (s57$ICT.2[i]==0) | (s57$ENG.I[i]==0) | (s57$ENG.E[i]==0) | (s57$TALLY[i]==0)  | (s57$C.1.INT[i]==0) | (s57$C.1.EXT[i]==0) | (s57$C.2.INT[i]==0) |(s57$C.2.EXT[i]==0) | (s57$C.2P[i]==0) | (s57$C.3.INT[i]==0) |(s57$C.3.EXT[i]==0) | (s57$C.3P[i]==0)),s57$overallresult[i]<-"ABSENT",s57$overallresult[i]<-"PRESENT")  
s57=s57[-c(8:50)]
s58=subset(s57,GENDER=="M")
s59=subset(s57,GENDER=="F")
ba35=length(s58$overallresult[s58$overallresult=="ABSENT"])
ga35=length(s59$overallresult[s59$overallresult=="ABSENT"])

s60=c(ba31,ga31,ba32,ga32,ba33,ga33,ba34,ga34,ba35,ga35)
s61=c("BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS")
s62=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
thirdsemA=data.frame(b,s60,s61,s62)
colnames(thirdsemA)=c("SEM NO","ABSENTIES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=thirdsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS THIRD SEM\n ABSENTIES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

s64=subset(s,SEMNO.==4 & GROUP=="M.S.CS.")
for(i in 1:length(s64$RNO))
  ifelse(((s64$CSS.3[i]==0) | (s64$ANAL[i]==0) | (s64$LEAD[i]==0) | (s64$ENTP[i]==0) | (s64$C.1.INT[i]==0) | (s64$C.1.EXT[i]==0) | (s64$C.2.INT[i]==0) |(s64$C.2.EXT[i]==0) | (s64$C.2P[i]==0) | (s64$C.3.INT[i]==0) |(s64$C.3.EXT[i]==0) | (s64$C.3P[i]==0)),s64$overallresult[i]<-"ABSENT",s64$overallresult[i]<-"PRESENT")  
s64=s64[-c(8:50)]
s65=subset(s64,GENDER=="M")
s66=subset(s64,GENDER=="F")
ba41=length(s65$overallresult[s65$overallresult=="ABSENT"])
ga41=length(s66$overallresult[s66$overallresult=="ABSENT"])

s67=subset(s,SEMNO.==4 & GROUP=="M.P.CS.")
for(i in 1:length(s67$RNO))
  ifelse(((s67$CSS.3[i]==0) | (s67$ANAL[i]==0) | (s67$LEAD[i]==0) | (s67$ENTP[i]==0) | (s67$C.1.INT[i]==0) | (s67$C.1.EXT[i]==0) | (s67$C.2.INT[i]==0) |(s67$C.2.EXT[i]==0) | (s67$C.2P[i]==0) | (s67$C.3.INT[i]==0) |(s67$C.3.EXT[i]==0) | (s67$C.3P[i]==0)),s67$overallresult[i]<-"ABSENT",s67$overallresult[i]<-"PRESENT")  
s67=s67[-c(8:50)]
s68=subset(s67,GENDER=="M")
s69=subset(s67,GENDER=="F")
ba42=length(s68$overallresult[s68$overallresult=="ABSENT"])
ga42=length(s69$overallresult[s69$overallresult=="ABSENT"])

s70=subset(s,SEMNO.==4 & GROUP=="M.P.C.")
for(i in 1:length(s70$RNO))
  ifelse(((s70$CSS.3[i]==0) | (s70$ANAL[i]==0) | (s70$LEAD[i]==0) | (s70$ENTP[i]==0) | (s70$C.1.INT[i]==0) | (s70$C.1.EXT[i]==0) | (s70$C.2.INT[i]==0) |(s70$C.2.EXT[i]==0) | (s70$C.2P[i]==0) | (s70$C.3.INT[i]==0) |(s70$C.3.EXT[i]==0) | (s70$C.3P[i]==0)),s70$overallresult[i]<-"ABSENT",s70$overallresult[i]<-"PRESENT")  
s70=s70[-c(8:50)]
s71=subset(s70,GENDER=="M")
s72=subset(s70,GENDER=="F")
ba43=length(s71$overallresult[s71$overallresult=="ABSENT"])
ga43=length(s72$overallresult[s72$overallresult=="ABSENT"])

s73=subset(s,SEMNO.==4 & GROUP=="B.B.C.")
for(i in 1:length(s73$RNO))
  ifelse(((s73$CSS.3[i]==0) | (s73$ANAL[i]==0) | (s73$LEAD[i]==0) | (s73$ENTP[i]==0) | (s73$C.1.INT[i]==0) | (s73$C.1.EXT[i]==0) | (s73$C.2.INT[i]==0) |(s73$C.2.EXT[i]==0) | (s73$C.2P[i]==0) | (s73$C.3.INT[i]==0) |(s73$C.3.EXT[i]==0) | (s73$C.3P[i]==0)),s73$overallresult[i]<-"ABSENT",s73$overallresult[i]<-"PRESENT")  
s73=s73[-c(8:50)]
s74=subset(s73,GENDER=="M")
s75=subset(s73,GENDER=="F")
ba44=length(s74$overallresult[s74$overallresult=="ABSENT"])
ga44=length(s75$overallresult[s75$overallresult=="ABSENT"])

s76=subset(s,SEMNO.==4 & GROUP=="B.C.A.")
for(i in 1:length(s76$RNO))
  ifelse(((s76$CSS.3[i]==0) | (s76$ANAL[i]==0) | (s76$DRVW[i]==0) | (s76$CITI[i]==0) | (s76$C.1.INT[i]==0) | (s76$C.1.EXT[i]==0) | (s76$C.2.INT[i]==0) |(s76$C.2.EXT[i]==0) | (s76$C.2P[i]==0) | (s76$C.3.INT[i]==0) |(s76$C.3.EXT[i]==0) | (s76$C.3P[i]==0) | (s76$C.1P[i]==0)),s76$overallresult[i]<-"ABSENT",s76$overallresult[i]<-"PRESENT")  
s76=s76[-c(8:50)]
s77=subset(s76,GENDER=="M")
s78=subset(s76,GENDER=="F")
ba45=length(s77$overallresult[s77$overallresult=="ABSENT"])
ga45=length(s78$overallresult[s78$overallresult=="ABSENT"])

s79=c(ba41,ga41,ba42,ga42,ba43,ga43,ba44,ga44,ba45,ga45)
s80=c("BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS")
s81=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
fourthsemA=data.frame(b,s79,s80,s81)
colnames(fourthsemA)=c("SEM NO","ABSENTIES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=fourthsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FOURTH SEM\n ABSENTIES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))

s83=subset(s,SEMNO.==5 & GROUP=="M.S.CS.")
for(i in 1:length(s83$RNO))
  ifelse(((s83$C.4.INT[i]==0) | (s83$C.4.EXT[i]==0) | (s83$C.5.INT[i]==0) | (s83$C.5.EXT[i]==0) | (s83$C.5P[i]==0) | (s83$C.6.INT[i]==0) | (s83$C.6.EXT[i]==0) | (s83$C.6P[i]==0) | (s83$C.1.INT[i]==0) | (s83$C.1.EXT[i]==0) | (s83$C.2.INT[i]==0) |(s83$C.2.EXT[i]==0) | (s83$C.2P[i]==0) | (s83$C.3.INT[i]==0) |(s83$C.3.EXT[i]==0) | (s83$C.3P[i]==0)),s83$overallresult[i]<-"ABSENT",s83$overallresult[i]<-"PRESENT")  
s83=s83[-c(8:50)]
s84=subset(s83,GENDER=="M")
s85=subset(s83,GENDER=="F")
ba51=length(s84$overallresult[s84$overallresult=="ABSENT"])
ga51=length(s85$overallresult[s85$overallresult=="ABSENT"])

s86=subset(s,SEMNO.==5 & GROUP=="M.P.CS.")
for(i in 1:length(s86$RNO))
  ifelse(((s86$C.4.INT[i]==0) | (s86$C.4.EXT[i]==0) | (s86$C.5.INT[i]==0) | (s86$C.5.EXT[i]==0) | (s86$C.5P[i]==0) | (s86$C.6.INT[i]==0) | (s86$C.6.EXT[i]==0) | (s86$C.6P[i]==0) | (s86$C.1.INT[i]==0) | (s86$C.1.EXT[i]==0) | (s86$C.2.INT[i]==0) |(s86$C.2.EXT[i]==0) | (s86$C.2P[i]==0) | (s86$C.3.INT[i]==0) |(s86$C.3.EXT[i]==0) | (s86$C.3P[i]==0)),s86$overallresult[i]<-"ABSENT",s86$overallresult[i]<-"PRESENT")  
s86=s86[-c(8:50)]
s87=subset(s86,GENDER=="M")
s88=subset(s86,GENDER=="F")
ba52=length(s87$overallresult[s87$overallresult=="ABSENT"])
ga52=length(s88$overallresult[s88$overallresult=="ABSENT"])

s89=subset(s,SEMNO.==5 & GROUP=="M.P.C.")
for(i in 1:length(s89$RNO))
  ifelse(((s89$C.4.INT[i]==0) | (s89$C.4.EXT[i]==0) | (s89$C.5.INT[i]==0) | (s89$C.5.EXT[i]==0) | (s89$C.5P[i]==0) | (s89$C.6.INT[i]==0) | (s89$C.6.EXT[i]==0) | (s89$C.6P[i]==0) | (s89$C.1.INT[i]==0) | (s89$C.1.EXT[i]==0) | (s89$C.2.INT[i]==0) |(s89$C.2.EXT[i]==0) | (s89$C.2P[i]==0) | (s89$C.3.INT[i]==0) |(s89$C.3.EXT[i]==0) | (s89$C.3P[i]==0)),s89$overallresult[i]<-"ABSENT",s89$overallresult[i]<-"PRESENT")  
s89=s89[-c(8:50)]
s90=subset(s89,GENDER=="M")
s91=subset(s89,GENDER=="F")
ba53=length(s90$overallresult[s90$overallresult=="ABSENT"])
ga53=length(s91$overallresult[s91$overallresult=="ABSENT"])

s92=subset(s,SEMNO.==5 & GROUP=="B.B.C.")
for(i in 1:length(s92$RNO))
  ifelse(((s92$C.4.INT[i]==0) | (s92$C.4.EXT[i]==0) | (s92$C.5.INT[i]==0) | (s92$C.5.EXT[i]==0) | (s92$C.5P[i]==0) | (s92$C.6.INT[i]==0) | (s92$C.6.EXT[i]==0) | (s92$C.6P[i]==0) | (s92$C.1.INT[i]==0) | (s92$C.1.EXT[i]==0) | (s92$C.2.INT[i]==0) |(s92$C.2.EXT[i]==0) | (s92$C.2P[i]==0) | (s92$C.3.INT[i]==0) |(s92$C.3.EXT[i]==0) | (s92$C.3P[i]==0) |(s92$C.1P[i]==0) | (s92$C.4P[i]==0)),s92$overallresult[i]<-"ABSENT",s92$overallresult[i]<-"PRESENT")  
s92=s92[-c(8:50)]
s93=subset(s92,GENDER=="M")
s94=subset(s92,GENDER=="F")
ba54=length(s93$overallresult[s93$overallresult=="ABSENT"])
ga54=length(s94$overallresult[s94$overallresult=="ABSENT"])

s95=subset(s,SEMNO.==5 & GROUP=="B.C.A.")
for(i in 1:length(s95$RNO))
  ifelse(((s95$C.4.INT[i]==0) | (s95$C.4.EXT[i]==0) | (s95$C.5.INT[i]==0) | (s95$C.5.EXT[i]==0) | (s95$C.5P[i]==0) | (s95$C.1.INT[i]==0) | (s95$C.1.EXT[i]==0) | (s95$C.2.INT[i]==0) |(s95$C.2.EXT[i]==0) | (s95$C.3.INT[i]==0) |(s95$C.3.EXT[i]==0) | (s95$C.4P[i]==0)),s95$overallresult[i]<-"ABSENT",s95$overallresult[i]<-"PRESENT")  
s95=s95[-c(8:50)]
s96=subset(s95,GENDER=="M")
s97=subset(s95,GENDER=="F")
ba55=length(s96$overallresult[s96$overallresult=="ABSENT"])
ga55=length(s97$overallresult[s97$overallresult=="ABSENT"])

s98=c(ba51,ga51,ba52,ga52,ba53,ga53,ba54,ga54,ba55,ga55)
s99=c("BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS","BOYS ABS","GIRLS ABS")
s100=c("MSCS","MSCS","MPCS","MPCS","MPC","MPC","BBC","BBC","BCA","BCA")
fifthsemA=data.frame(b,s98,s99,s100)
colnames(fifthsemA)=c("SEM NO","ABSENTIES","GENDER","GROUP")

#ggplot(data=MSCS,aes(fill=gender,x=semno,y=PassPercentages))+geom_bar(position="dodge",stat="identity") 

ggplot(data=fifthsemA, aes(x=GROUP, y=ABSENTIES, fill=GENDER)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=ABSENTIES), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()+ ggtitle("BOYS AND GIRLS FIFTH SEM\n PASS PERCENTAGES(5 GROUPS)")+
  theme( plot.title = element_text(color="red", size=14, face="bold.italic"), 
         axis.title.x = element_text(color="blue", size=14, face="bold"),
         axis.title.y = element_text(color="#993333", size=14, face="bold"))


