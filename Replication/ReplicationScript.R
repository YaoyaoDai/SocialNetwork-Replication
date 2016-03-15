### Justin H Kirkland			###
### 9-24-12						###
### Texas Cosponsorship			###
### Is Weird					###
###################################
###################################

###Clear Memory
rm(list=ls())
options(scipen=4)
###Load Libraries
library(statnet)
library(foreign)
library(arm)

###Load the First Set of Data
legs<-read.csv("~/Dropbox/Texas Project/TX/tx_legislators.csv")
billvotes<-read.csv("~/Dropbox/Texas Project/TX/tx_bill_votes.csv")
cosp<-read.csv("~/Dropbox/Texas Project/TX/tx_bill_sponsors.csv")
rollcalls<-read.csv("~/Dropbox/Texas Project/TX/tx_bill_legislator_votes.csv")

###Subset cosp network to lower chamber 81st 
unique.legs<-unique(cosp$leg_id)
cosp2<-subset(cosp, cosp$chamber=="lower" & cosp$session==81)
unique.bills<-unique(cosp2$bill_id)
print(length(unique.bills))

###Generate blank Matrix
matrix3<-matrix(0, length(unique.legs), length(unique.legs))
row.names(matrix3)<-unique.legs
colnames(matrix3)<-unique.legs

###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="author")], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="sponsor")], unique.legs)	
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)
###Subset cosp network to upper chamber 81st 
cosp2<-subset(cosp, cosp$chamber=="upper" & cosp$session==81)
unique.bills<-unique(cosp2$bill_id)
print(length(unique.bills))

###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="author")], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="sponsor")], unique.legs)	
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
#if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)

########Add Party Vector
Party<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Party[i]<-as.character(legs$party[which(legs$leg_id==row.names(matrix3)[i])])
}
}

missing<-which(Party!="Democratic" & Party!="Republican" | is.na(Party)==TRUE)
missing2<-vector(mode="character", length=length(missing))
for(i in 1:length(missing)){
missing2[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[missing[i]])])[1]	
}

missing3<-rep("Republican", length=length(missing))
missing3[2]<-"Democratic"
missing3[3]<-"Democratic"
missing3[4]<-"Democratic"
missing3[6]<-"Democratic"
missing3[7]<-"Democratic"
missing3[8]<-"Democratic"
missing3[9]<-"Democratic"
missing3[10]<-"Democratic"
missing3[12]<-"Democratic"
missing3[14]<-"Democratic"
missing3[15]<-"Democratic"
missing3[20]<-"Democratic"
missing3[21]<-"Democratic"
missing3[24]<-"Democratic"
missing3[27]<-"Democratic"
missing3[28]<-"Democratic"
missing3[29]<-"Democratic"
missing3[30]<-"Democratic"
missing3[31]<-"Democratic"
missing3[32]<-"Democratic"
missing3[37]<-"Democratic"
missing3[38]<-"Democratic"
missing3[39]<-"Democratic"
missing3[35]<-"Democratic"
missing3[34]<-"Democratic"
missing3[40]<-"Democratic"
missing3[41]<-"Democratic"
missing3[42]<-"Democratic"
missing3[43]<-"Democratic"
missing3[44]<-"Democratic"
missing3[45]<-"Democratic"
missing3[46]<-"Democratic"
missing3[47]<-"Democratic"

Party
for(i in 1:length(missing3)){
Party[missing[i]]<-missing3[i]	
}
Party
Party<-ifelse(Party=="Republican", 0, 1)

########Add District Overlap using dyadcov(x, attrname)
#Unsure about implementation
dist.overlap<-read.csv("~/Dropbox/Texas Project/TX/Useful Data/Precincts_Districts.csv")
lowers<-subset(legs, legs$chamber=="lower")
uppers<-subset(legs, legs$chamber=="upper")
dist.mat<-matrix(0, nrow(lowers), nrow(uppers))
row.names(dist.mat)<-lowers$leg_id
colnames(dist.mat)<-uppers$leg_id
for(i in 1:nrow(dist.overlap)){
ll<-which(lowers$district==dist.overlap$STREP[i])	
uu<-which(uppers$district==dist.overlap$STSEN[i])	
dist.mat[ll,uu]<-dist.mat[ll,uu]+1
}

dist.mat2<-matrix(0, nrow(matrix3), nrow(matrix3))
row.names(dist.mat2)<-row.names(matrix3)
colnames(dist.mat2)<-row.names(matrix3)
for(i in 1:nrow(dist.mat)){
for(j in 1:ncol(dist.mat)){
rr<-which(row.names(dist.mat2)==row.names(dist.mat)[i])	
ss<-which(row.names(dist.mat2)==colnames(dist.mat)[j])
dist.mat2[rr,ss]<-dist.mat2[rr,ss]+dist.mat[i,j]
dist.mat2[ss,rr]<-dist.mat2[ss,rr]+dist.mat[i,j]
}
}

###Make a list of Sen Districts
SenDist<-vector("list", length=nrow(dist.mat2))
for(i in 1:length(SenDist)){
if(length(which(row.names(dist.mat)==row.names(matrix3)[i]))>0){
	SenDist[[i]]<-which(as.numeric(dist.mat[which(row.names(dist.mat)==row.names(matrix3)[i]),])==1)
	}

if(length(which(row.names(dist.mat)==row.names(matrix3)[i]))==0){
	SenDist[[i]]<-which(colnames(dist.mat)==row.names(matrix3)[i])
	}
}

########Add Leadership Vector
Leadership<-rep(NA, nrow(matrix3))
for(i in 1:length(Leadership)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Leadership[i]<-as.character(legs$leadership[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Leadership<-ifelse(is.na(Leadership)==FALSE, as.numeric(Leadership), 0)

########Add Appropriations Vector
Appropriations<-rep(0, nrow(matrix3))
for(i in 1:length(Appropriations)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Appropriations[i]<-as.character(legs$Appropriations[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Appropriations<-as.numeric(Appropriations)

########Add Chamber Vector
Chamber<-rep(NA, nrow(matrix3))
for(i in 1:length(Appropriations)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Chamber[i]<-as.character(legs$chamber[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Chamber2<-ifelse(Chamber=="upper", 1, 0)
Chamber2[37]<-0
Chamber2[174]<-0

############Clear Matrix of within-chamber connections
ss<-which(Chamber2==1)
rr<-which(Chamber2==0)

sum(matrix3>0)
matrix3[ss,ss]<-0
matrix3[rr,rr]<-0
sum(matrix3>0)

########Add Rules Vector
Rules<-rep(0, nrow(matrix3))
for(i in 1:length(Rules)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Rules[i]<-as.character(legs$Rules[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Rules<-as.numeric(Rules)

######Rename Legislators from IDs to Names
leg.names<-vector(mode="character", length=length(Party))
for(i in 1:length(leg.names)){
leg.names[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[i])[1]])	
}

####Add Joint Committees
Joint.Comms<-numeric(length=nrow(matrix3))
for(i in 1:length(Joint.Comms)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){	
if(legs$Aging[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-1}
if(legs$Criminal.Commitments[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-2}
if(legs$Dyslexia[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-3}
if(legs$Environmental.Flows[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-4}
if(legs$Bexar[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-5}
if(legs$Criminal.Justice[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-6}
if(legs$Aquifer[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-7}
if(legs$HHS[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-8}
if(legs$Windstorm[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Finance.Weights[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-10}
}		
#print(i)
}


###Set Network
Network<-network(matrix3)

###Add Attributes
set.vertex.attribute(Network, "Party", Party)
set.vertex.attribute(Network, "names", leg.names)
set.vertex.attribute(Network, "leadership", Leadership)
set.vertex.attribute(Network, "Rules", Rules)
set.vertex.attribute(Network, "Appropriations", Appropriations)
set.vertex.attribute(Network, "Chamber", Chamber2)
set.vertex.attribute(Network, "SenDist", SenDist)
set.vertex.attribute(Network, "JointComms", Joint.Comms)

#####Drop Isolates from the data
zz<-which(degree(Network)==0)
matTX<-matrix3[-zz,-zz]
PartyTX<-Party[-zz]
leg.namesTX<-leg.names[-zz]
LeadershipTX<-Leadership[-zz]
RulesTX<-Rules[-zz]
AppropriationsTX<-Appropriations[-zz]
ChamberTX<-Chamber2[-zz]
SenDistTX<-SenDist[-zz]
dist.matTX<-dist.mat2[-zz,-zz]
Joint.CommsTX<-Joint.Comms[-zz]
TXid<-rep(1, nrow(matTX))

###Re-designate network
Network2<-network(matTX)
set.vertex.attribute(Network2, "Party", PartyTX)
set.vertex.attribute(Network2, "names", leg.namesTX)
set.vertex.attribute(Network2, "leadership", LeadershipTX)
set.vertex.attribute(Network2, "Appropriations", AppropriationsTX)
set.vertex.attribute(Network2, "Chamber", ChamberTX)
set.vertex.attribute(Network2, "JointComms", Joint.CommsTX)

#############################
###########Model
set.seed(111)
summary(mod.exTX<-ergm(Network2~edges+nodematch("Chamber")+nodematch("Party", diff=TRUE)+nodematch("leadership", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE)+nodematch("JointComms")+edgecov(dist.mat2)+ostar(2), control=control.ergm(MCMC.samplesize=50000, MCMLE.maxit=50, MCMC.burnin=75000, MCMC.interval=1000)))

dput(mod.exTX, file="TexasResults2.txt")

###############################
##Create Effects Plots#########
sims<-2500
m1<-numeric(sims)
m2<-numeric(sims)
m3<-numeric(sims)
m4<-numeric(sims)
m5<-numeric(sims)
m6<-numeric(sims)

coefs1<-coef(mod.exTX)
coefs2<-coefs1
coefs2[1]<-0
coefs3<-coefs1
coefs3[3:4]<-0
coefs4<-coefs1
coefs4[7:9]<-0


for(i in 1:sims){
sim.ex<-network(simulate(mod.exTX, coef=coefs1))
set.vertex.attribute(sim.ex, "Party", PartyTX)
set.vertex.attribute(sim.ex, "names", leg.namesTX)
set.vertex.attribute(sim.ex, "leadership", LeadershipTX)
#set.vertex.attribute(sim.ex, "Rules", Rules)
set.vertex.attribute(sim.ex, "Appropriations", AppropriationsTX)
set.vertex.attribute(sim.ex, "Chamber", ChamberTX)
#set.vertex.attribute(sim.ex, "SenDist", SenDist2)
set.vertex.attribute(sim.ex, "JointComms", Joint.CommsTX)

sim.ex2<-network(simulate(mod.exTX, coef=coefs2))
set.vertex.attribute(sim.ex2, "Party", PartyTX)
set.vertex.attribute(sim.ex2, "names", leg.namesTX)
set.vertex.attribute(sim.ex2, "leadership", LeadershipTX)
#set.vertex.attribute(sim.ex2, "Rules", Rules)
set.vertex.attribute(sim.ex2, "Appropriations", AppropriationsTX)
set.vertex.attribute(sim.ex2, "Chamber", ChamberTX)
#set.vertex.attribute(sim.ex2, "SenDist", SenDist2)
set.vertex.attribute(sim.ex2, "JointComms", Joint.CommsTX)

sim.ex3<-network(simulate(mod.exTX, coef=coefs3))
set.vertex.attribute(sim.ex3, "Party", PartyTX)
set.vertex.attribute(sim.ex3, "names", leg.namesTX)
set.vertex.attribute(sim.ex3, "leadership", LeadershipTX)
#set.vertex.attribute(sim.ex3, "Rules", Rules)
set.vertex.attribute(sim.ex3, "Appropriations", AppropriationsTX)
set.vertex.attribute(sim.ex3, "Chamber", ChamberTX)
#set.vertex.attribute(sim.ex3, "SenDist", SenDist2)
set.vertex.attribute(sim.ex3, "JointComms", Joint.CommsTX)

sim.ex4<-network(simulate(mod.exTX, coef=coefs4))
set.vertex.attribute(sim.ex4, "Party", PartyTX)
set.vertex.attribute(sim.ex4, "names", leg.namesTX)
set.vertex.attribute(sim.ex4, "leadership", LeadershipTX)
#set.vertex.attribute(sim.ex4, "Rules", Rules)
set.vertex.attribute(sim.ex4, "Appropriations", AppropriationsTX)
set.vertex.attribute(sim.ex4, "Chamber", ChamberTX)
#set.vertex.attribute(sim.ex4, "SenDist", SenDist2)
set.vertex.attribute(sim.ex4, "JointComms", Joint.CommsTX)


a<-summary(sim.ex~edges+nodematch("Party", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE))
b<-summary(sim.ex2~edges)
c<-summary(sim.ex3~nodematch("Party", diff=TRUE))
d<-summary(sim.ex4~mutual+mutual(same="Party", diff=TRUE))

m1[i]<-(a[1]/b[1])
m2[i]<-(a[2]/c[1])
m3[i]<-(a[3]/c[2])
m4[i]<-(a[4]/d[1])
m5[i]<-(a[5]/d[2])
m6[i]<-(a[6]/d[3])

print(i)
}

TXm1<-m1
TXm2<-m2
TXm3<-m3
TXm4<-m4
TXm5<-m5
TXm6<-m6

################################################
################################################

################################################
################################################

################################################
################################################
########Build Oklahoma Network##################

###Load the First Set of Data
legs<-read.csv("~/Documents/Replication/ok_legislators.csv")
billvotes<-read.csv("~/Documents/Replication/ok_bill_votes.csv")
cosp<-read.csv("~/Documents/Replication/ok_bill_sponsors.csv")
rollcalls<-read.csv("~/Documents/Replication/ok_bill_legislator_votes.csv")

###Subset cosp network to lower chamber 81st 
unique.legs<-unique(cosp$leg_id)
cosp2<-subset(cosp, cosp$chamber=="lower")
unique.bills<-unique(cosp2$bill_id)

###Generate blank Matrix
matrix3<-matrix(0, length(unique.legs), length(unique.legs))
row.names(matrix3)<-unique.legs
colnames(matrix3)<-unique.legs

###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")[1]], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="primary")[2]], unique.legs)	
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)
###Subset cosp network to upper chamber 81st 
cosp2<-subset(cosp, cosp$chamber=="upper")
unique.bills<-unique(cosp2$bill_id)


###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")[1]], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="primary")[2]], unique.legs)
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
#if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)
diag(matrix3)<-0

########Add Party Vector
Party<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Party[i]<-as.character(legs$party[which(legs$leg_id==row.names(matrix3)[i])])
}
}

missing<-which(Party!="Democratic" & Party!="Republican" | is.na(Party)==TRUE)
missing2<-vector(mode="character", length=length(missing))
for(i in 1:length(missing)){
missing2[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[missing[i]])])[1]	
}

Party
Party[1]<-"Republican"
Party[20]<-"Republican"
Party[41]<-"Republican"
Party[96]<-"Democratic"
Party[138]<-"Republican"
Party<-ifelse(Party=="Republican", 0, 1)

########Add Chamber Vector
Chamber<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Chamber[i]<-as.character(legs$chamber[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Chamber[1]<-"upper"
Chamber[20]<-"upper"
Chamber[41]<-"upper"
Chamber[96]<-"upper"
Chamber[138]<-"upper"
Chamber2<-ifelse(Chamber=="upper", 1, 0)

############Clear Matrix of within-chamber connections
ss<-which(Chamber2==1)
rr<-which(Chamber2==0)

sum(matrix3>0)
matrix3[ss,ss]<-0
matrix3[rr,rr]<-0
sum(matrix3>0)

########Add District Overlap using dyadcov(x, attrname)
#Unsure about implementation
dist.overlap<-read.csv("~/Dropbox/Texas Project/OK/Precints.csv")
lowers<-subset(legs, legs$chamber=="lower")
uppers<-subset(legs, legs$chamber=="upper")
dist.mat<-matrix(0, nrow(lowers), nrow(uppers))
row.names(dist.mat)<-lowers$leg_id
colnames(dist.mat)<-uppers$leg_id
for(i in 1:nrow(dist.overlap)){
ll<-which(lowers$district==dist.overlap$House[i])	
uu<-which(uppers$district==dist.overlap$Senate[i])	
dist.mat[ll,uu]<-dist.mat[ll,uu]+1
}

dist.mat2<-matrix(0, nrow(matrix3), nrow(matrix3))
row.names(dist.mat2)<-row.names(matrix3)
colnames(dist.mat2)<-row.names(matrix3)
for(i in 1:nrow(dist.mat)){
for(j in 1:ncol(dist.mat)){
rr<-which(row.names(dist.mat2)==row.names(dist.mat)[i])	
ss<-which(row.names(dist.mat2)==colnames(dist.mat)[j])
dist.mat2[rr,ss]<-dist.mat2[rr,ss]+dist.mat[i,j]
dist.mat2[ss,rr]<-dist.mat2[ss,rr]+dist.mat[i,j]
}
}

########Add Leadership Vector
Leadership<-rep(NA, nrow(matrix3))
for(i in 1:length(Leadership)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Leadership[i]<-as.character(legs$leadership[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Leadership<-ifelse(is.na(Leadership)==FALSE, as.numeric(Leadership), 0)

########Add Appropriations Vector
Appropriations<-rep(NA, nrow(matrix3))
for(i in 1:length(Appropriations)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Appropriations[i]<-as.character(legs$Appropriations[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Appropriations<-ifelse(is.na(Appropriations)==FALSE, as.numeric(Appropriations), 0)

########Add Rules Vector	#No Rules committee in Colorado thanks to GAVEL
Rules<-rep(NA, nrow(matrix3))
for(i in 1:length(Rules)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Rules[i]<-as.character(legs$Rules[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Rules<-ifelse(is.na(Rules)==FALSE, as.numeric(Rules), 0)

######Rename Legislators from IDs to Names
leg.names<-vector(mode="character", length=length(Party))
for(i in 1:length(leg.names)){
leg.names[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[i])[1]])	
}

####Replace NA's with 0's
legs$JointAppropriations<-ifelse(is.na(legs$JointAppropriations)==TRUE, 0, legs$JointAppropriations)
legs$JointBreastCancer<-ifelse(is.na(legs$JointBreastCancer)==TRUE, 0, legs$JointBreastCancer)
legs$EnergyCouncil<-ifelse(is.na(legs$EnergyCouncil)==TRUE, 0, legs$EnergyCouncil)
legs$JointFedHealthCare<-ifelse(is.na(legs$JointFedHealthCare)==TRUE, 0, legs$JointFedHealthCare)
legs$JointGrandRiverDam<-ifelse(is.na(legs$JointGrandRiverDam)==TRUE, 0, legs$JointGrandRiverDam)
legs$JointImmigration<-ifelse(is.na(legs$JointImmigration)==TRUE, 0, legs$JointImmigration)
legs$JointInternationalDevelopment<-ifelse(is.na(legs$JointInternationalDevelopment)==TRUE, 0, legs$JointInternationalDevelopment)
legs$JointWater<-ifelse(is.na(legs$JointWater)==TRUE, 0, legs$JointWater)


####Add Joint Committees
Joint.Comms<-numeric(length=nrow(matrix3))
for(i in 1:length(Joint.Comms)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){	
if(legs$JointAppropriations[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-1}
if(legs$JointBreastCancer[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-2}
if(legs$EnergyCouncil[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-3}
if(legs$JointFedHealthCare[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-4}
if(legs$JointGrandRiverDam[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-5}
if(legs$JointImmigration[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-6}
if(legs$JointInternationalDevelopment[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-7}
if(legs$JointWater[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-8}
}		
#print(i)
}


###Set Network
Network<-network(matrix3)

###Add Attributes
set.vertex.attribute(Network, "Party", Party)
set.vertex.attribute(Network, "names", leg.names)
set.vertex.attribute(Network, "leadership", Leadership)
set.vertex.attribute(Network, "Rules", Rules)
set.vertex.attribute(Network, "Appropriations", Appropriations)
set.vertex.attribute(Network, "Chamber", Chamber2)
set.vertex.attribute(Network, "JointComms", Joint.Comms)

####Record Oklahoma Network
matOK<-matrix3
PartyOK<-Party
leg.namesOK<-leg.names
LeadershipOK<-Leadership
RulesOK<-Rules
AppropriationsOK<-Appropriations
ChamberOK<-Chamber2
dist.matOK<-dist.mat2
Joint.CommsOK<-Joint.Comms
OKid<-rep(3, nrow(matOK))


#############################
###########Model
set.seed(111)
summary(mod.exOK<-ergm(Network~edges+nodematch("Chamber")+nodematch("Party", diff=TRUE)+nodematch("leadership", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE)+nodematch("JointComms")+edgecov(dist.mat2)+istar(2), control=control.ergm(MCMC.samplesize=50000, MCMLE.maxit=50, MCMC.burnin=75000, MCMC.interval=1000)))
summary(mod.exOK<-ergm(Network~edges+nodematch("Party", diff=TRUE)+nodematch("leadership", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE)+nodematch("JointComms")+istar(2), control=control.ergm(MCMC.samplesize=50000, MCMLE.maxit=50, MCMC.burnin=75000, MCMC.interval=1000)))

dput(mod.exOK, file="OklahomaResults2.txt")

###############################
##Create Effects Plots#########
sims<-2500
m1<-numeric(sims)
m2<-numeric(sims)
m3<-numeric(sims)
m4<-numeric(sims)
m5<-numeric(sims)
m6<-numeric(sims)

coefs1<-coef(mod.exOK)
coefs2<-coefs1
coefs2[1]<-0
coefs3<-coefs1
coefs3[3:4]<-0
coefs4<-coefs1
coefs4[7:9]<-0

for(i in 1:sims){
sim.ex<-network(simulate(mod.exOK, coef=coefs1))
set.vertex.attribute(sim.ex, "Party", Party)
set.vertex.attribute(sim.ex, "names", leg.names)
set.vertex.attribute(sim.ex, "leadership", Leadership)
#set.vertex.attribute(sim.ex, "Rules", Rules)
set.vertex.attribute(sim.ex, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex, "SenDist", SenDist2)
set.vertex.attribute(sim.ex, "JointComms", Joint.Comms)

sim.ex2<-network(simulate(mod.exOK, coef=coefs2))
set.vertex.attribute(sim.ex2, "Party", Party)
set.vertex.attribute(sim.ex2, "names", leg.names)
set.vertex.attribute(sim.ex2, "leadership", Leadership)
#set.vertex.attribute(sim.ex2, "Rules", Rules)
set.vertex.attribute(sim.ex2, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex2, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex2, "SenDist", SenDist2)
set.vertex.attribute(sim.ex2, "JointComms", Joint.Comms)

sim.ex3<-network(simulate(mod.exOK, coef=coefs3))
set.vertex.attribute(sim.ex3, "Party", Party)
set.vertex.attribute(sim.ex3, "names", leg.names)
set.vertex.attribute(sim.ex3, "leadership", Leadership)
#set.vertex.attribute(sim.ex3, "Rules", Rules)
set.vertex.attribute(sim.ex3, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex3, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex3, "SenDist", SenDist2)
set.vertex.attribute(sim.ex3, "JointComms", Joint.Comms)

sim.ex4<-network(simulate(mod.exOK, coef=coefs4))
set.vertex.attribute(sim.ex4, "Party", Party)
set.vertex.attribute(sim.ex4, "names", leg.names)
set.vertex.attribute(sim.ex4, "leadership", Leadership)
#set.vertex.attribute(sim.ex4, "Rules", Rules)
set.vertex.attribute(sim.ex4, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex4, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex4, "SenDist", SenDist2)
set.vertex.attribute(sim.ex4, "JointComms", Joint.Comms)

a<-summary(sim.ex~edges+nodematch("Party", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE))
b<-summary(sim.ex2~edges)
c<-summary(sim.ex3~nodematch("Party", diff=TRUE))
d<-summary(sim.ex4~mutual+mutual(same="Party", diff=TRUE))

m1[i]<-(a[1]/b[1])
m2[i]<-(a[2]/c[1])
m3[i]<-(a[3]/c[2])
m4[i]<-(a[4]/d[1])
m5[i]<-(a[5]/d[2])
m6[i]<-(a[6]/d[3])

print(i)
}

OKm1<-m1
OKm2<-m2
OKm3<-m3
OKm4<-m4
OKm5<-m5
OKm6<-m6

################################################
################################################

################################################
################################################

################################################
################################################
########Build Colorado Network##################

###Load the First Set of Data
legs<-read.csv("~/Dropbox/Texas Project/CO/co_legislators.csv")
billvotes<-read.csv("~/Dropbox/Texas Project/CO/co_bill_votes.csv")
cosp<-read.csv("~/Dropbox/Texas Project/CO/co_bill_sponsors.csv")
rollcalls<-read.csv("~/Dropbox/Texas Project/CO/co_bill_legislator_votes.csv")

###Subset cosp network to lower chamber 81st 
unique.legs<-unique(cosp$leg_id)
cosp2<-subset(cosp, cosp$chamber=="lower")
unique.bills<-unique(cosp2$bill_id)

###Generate blank Matrix
matrix3<-matrix(0, length(unique.legs), length(unique.legs))
row.names(matrix3)<-unique.legs
colnames(matrix3)<-unique.legs

###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")[1]], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="primary")[2]], unique.legs)	
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)
###Subset cosp network to upper chamber 81st 
cosp2<-subset(cosp, cosp$chamber=="upper")
unique.bills<-unique(cosp2$bill_id)


###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")[1]], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="primary")[2]], unique.legs)
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
#if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)

########Add Party Vector
Party<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Party[i]<-as.character(legs$party[which(legs$leg_id==row.names(matrix3)[i])])
}
}

missing<-which(Party!="Democratic" & Party!="Republican" | is.na(Party)==TRUE)
missing2<-vector(mode="character", length=length(missing))
for(i in 1:length(missing)){
missing2[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[missing[i]])])[1]	
}

Party
Party[2]<-"Republican"
Party<-ifelse(Party=="Republican", 0, 1)

########Add Chamber Vector
Chamber<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Chamber[i]<-as.character(legs$chamber[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Chamber[2]<-"upper"
Chamber2<-ifelse(Chamber=="upper", 1, 0)

############Clear Matrix of within-chamber connections
ss<-which(Chamber2==1)
rr<-which(Chamber2==0)

sum(matrix3>0)
matrix3[ss,ss]<-0
matrix3[rr,rr]<-0
sum(matrix3>0)

########Add Term Limits Vector
TermLimits1<-rep(NA, nrow(matrix3))
for(i in 1:length(TermLimits1)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
TermLimits1[i]<-as.character(legs$Term.Limits[which(legs$leg_id==row.names(matrix3)[i])])
}
}

TermLimits1[2]<-0
TermLimits1<-ifelse(TermLimits1=="1", 1, 0)


TermLimits2<-rep(NA, nrow(matrix3))
for(i in 1:length(TermLimits2)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
TermLimits2[i]<-as.character(legs$Term.Limits2[which(legs$leg_id==row.names(matrix3)[i])])
}
}

TermLimits2[2]<-0
TermLimits2<-ifelse(TermLimits2=="1", 1, 0)

########Add District Overlap using dyadcov(x, attrname)
#Unsure about implementation
dist.overlap<-read.csv("~/Dropbox/Texas Project/CO/Precincts_Districts.csv")
lowers<-subset(legs, legs$chamber=="lower")
uppers<-subset(legs, legs$chamber=="upper")
dist.mat<-matrix(0, nrow(lowers), nrow(uppers))
row.names(dist.mat)<-lowers$leg_id
colnames(dist.mat)<-uppers$leg_id
for(i in 1:nrow(dist.overlap)){
ll<-which(lowers$district==dist.overlap$House.Dist[i])	
uu<-which(uppers$district==dist.overlap$Sen.Dist[i])	
dist.mat[ll,uu]<-dist.mat[ll,uu]+1
}

dist.mat2<-matrix(0, nrow(matrix3), nrow(matrix3))
row.names(dist.mat2)<-row.names(matrix3)
colnames(dist.mat2)<-row.names(matrix3)
for(i in 1:nrow(dist.mat)){
for(j in 1:ncol(dist.mat)){
rr<-which(row.names(dist.mat2)==row.names(dist.mat)[i])	
ss<-which(row.names(dist.mat2)==colnames(dist.mat)[j])
dist.mat2[rr,ss]<-dist.mat2[rr,ss]+dist.mat[i,j]
dist.mat2[ss,rr]<-dist.mat2[ss,rr]+dist.mat[i,j]
}
}

########Add Leadership Vector
Leadership<-rep(NA, nrow(matrix3))
for(i in 1:length(Leadership)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Leadership[i]<-as.character(legs$leadership[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Leadership<-ifelse(is.na(Leadership)==FALSE, as.numeric(Leadership), 0)

########Add Appropriations Vector
Appropriations<-rep(NA, nrow(matrix3))
for(i in 1:length(Appropriations)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Appropriations[i]<-as.character(legs$Appropriations[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Appropriations<-ifelse(is.na(Appropriations)==FALSE, as.numeric(Appropriations), 0)

########Add Rules Vector	#No Rules committee in Colorado thanks to GAVEL
Rules<-rep(NA, nrow(matrix3))
for(i in 1:length(Rules)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Rules[i]<-as.character(legs$Rules[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Rules<-ifelse(is.na(Rules)==FALSE, as.numeric(Rules), 0)

######Rename Legislators from IDs to Names
leg.names<-vector(mode="character", length=length(Party))
for(i in 1:length(leg.names)){
leg.names[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[i])[1]])	
}

####Replace NA's with 0's
legs$JointAg<-ifelse(is.na(legs$JointAg)==TRUE, 0, legs$JointAg)
legs$JointEducation<-ifelse(is.na(legs$JointEducation)==TRUE, 0, legs$JointEducation)
legs$JointBudget<-ifelse(is.na(legs$JointBudget)==TRUE, 0, legs$JointBudget)
legs$JointFinance<-ifelse(is.na(legs$JointFinance)==TRUE, 0, legs$JointFinance)
legs$JointHHS<-ifelse(is.na(legs$JointHHS)==TRUE, 0, legs$JointHHS)
legs$JointJudiciary<-ifelse(is.na(legs$JointJudiciary)==TRUE, 0, legs$JointJudiciary)
legs$JointLG<-ifelse(is.na(legs$JointLG)==TRUE, 0, legs$JointLG)
legs$JointVeterans<-ifelse(is.na(legs$JointVeterans)==TRUE, 0, legs$JointVeterans)
legs$JointTrasportation<-ifelse(is.na(legs$JointTrasportation)==TRUE, 0, legs$JointTrasportation)


####Add Joint Committees
Joint.Comms<-numeric(length=nrow(matrix3))
for(i in 1:length(Joint.Comms)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){	
if(legs$JointAg[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-1}
if(legs$JointEducation[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-2}
if(legs$JointBudget[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-3}
if(legs$JointFinance[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-4}
if(legs$JointHHS[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-5}
if(legs$JointJudiciary[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-6}
if(legs$JointLG[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-7}
if(legs$JointVeterans[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-8}
if(legs$JointTrasportation[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
}		
#print(i)
}


###Set Network
Network<-network(matrix3)

###Add Attributes
set.vertex.attribute(Network, "Party", Party)
set.vertex.attribute(Network, "names", leg.names)
set.vertex.attribute(Network, "leadership", Leadership)
set.vertex.attribute(Network, "Rules", Rules)
set.vertex.attribute(Network, "Appropriations", Appropriations)
set.vertex.attribute(Network, "Chamber", Chamber2)
set.vertex.attribute(Network, "JointComms", Joint.Comms)
set.vertex.attribute(Network, "TermLimits1", TermLimits1)
set.vertex.attribute(Network, "TermLimits2", TermLimits2)


####Record Colorado Network
matCO<-matrix3
PartyCO<-Party
leg.namesCO<-leg.names
LeadershipCO<-Leadership
RulesCO<-Rules
AppropriationsCO<-Appropriations
ChamberCO<-Chamber2
dist.matCO<-dist.mat2
Joint.CommsCO<-Joint.Comms
COid<-rep(4, nrow(matCO))

#############################
###########Model
set.seed(111)
summary(mod.exCO<-ergm(Network~edges+nodematch("Chamber")+nodematch("Party", diff=TRUE)+nodematch("leadership", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE)+nodematch("JointComms")+edgecov(dist.mat2)+ostar(2), control=control.ergm(MCMC.samplesize=50000, MCMLE.maxit=50, MCMC.burnin=75000, MCMC.interval=1000)))

dput(mod.exCO, file="ColoradoResults2.txt")

###############################
##Create Effects Plots#########
sims<-2500
m1<-numeric(sims)
m2<-numeric(sims)
m3<-numeric(sims)
m4<-numeric(sims)
m5<-numeric(sims)
m6<-numeric(sims)

coefs1<-coef(mod.exCO)
coefs2<-coefs1
coefs2[1]<-0
coefs3<-coefs1
coefs3[3:4]<-0
coefs4<-coefs1
coefs4[7:9]<-0

for(i in 1:sims){
sim.ex<-network(simulate(mod.exCO, coef=coefs1))
set.vertex.attribute(sim.ex, "Party", Party)
set.vertex.attribute(sim.ex, "names", leg.names)
set.vertex.attribute(sim.ex, "leadership", Leadership)
#set.vertex.attribute(sim.ex, "Rules", Rules)
set.vertex.attribute(sim.ex, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex, "SenDist", SenDist2)
set.vertex.attribute(sim.ex, "JointComms", Joint.Comms)

sim.ex2<-network(simulate(mod.exCO, coef=coefs2))
set.vertex.attribute(sim.ex2, "Party", Party)
set.vertex.attribute(sim.ex2, "names", leg.names)
set.vertex.attribute(sim.ex2, "leadership", Leadership)
#set.vertex.attribute(sim.ex2, "Rules", Rules)
set.vertex.attribute(sim.ex2, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex2, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex2, "SenDist", SenDist2)
set.vertex.attribute(sim.ex2, "JointComms", Joint.Comms)

sim.ex3<-network(simulate(mod.exCO, coef=coefs3))
set.vertex.attribute(sim.ex3, "Party", Party)
set.vertex.attribute(sim.ex3, "names", leg.names)
set.vertex.attribute(sim.ex3, "leadership", Leadership)
#set.vertex.attribute(sim.ex3, "Rules", Rules)
set.vertex.attribute(sim.ex3, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex3, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex3, "SenDist", SenDist2)
set.vertex.attribute(sim.ex3, "JointComms", Joint.Comms)

sim.ex4<-network(simulate(mod.exCO, coef=coefs4))
set.vertex.attribute(sim.ex4, "Party", Party)
set.vertex.attribute(sim.ex4, "names", leg.names)
set.vertex.attribute(sim.ex4, "leadership", Leadership)
#set.vertex.attribute(sim.ex4, "Rules", Rules)
set.vertex.attribute(sim.ex4, "Appropriations", Appropriations)
set.vertex.attribute(sim.ex4, "Chamber", Chamber2)
#set.vertex.attribute(sim.ex4, "SenDist", SenDist2)
set.vertex.attribute(sim.ex4, "JointComms", Joint.Comms)

a<-summary(sim.ex~edges+nodematch("Party", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE))
b<-summary(sim.ex2~edges)
c<-summary(sim.ex3~nodematch("Party", diff=TRUE))
d<-summary(sim.ex4~mutual+mutual(same="Party", diff=TRUE))

m1[i]<-(a[1]/b[1])
m2[i]<-(a[2]/c[1])
m3[i]<-(a[3]/c[2])
m4[i]<-(a[4]/d[1])
m5[i]<-(a[5]/d[2])
m6[i]<-(a[6]/d[3])

print(i)
}

COm1<-m1
COm2<-m2
COm3<-m3
COm4<-m4
COm5<-m5
COm6<-m6

################################################
################################################

################################################
################################################

################################################
################################################
########Build Maine Network##################


###Load the First Set of Data
legs<-read.csv("~/Dropbox/Texas Project/ME/me_legislators2.csv")
billvotes<-read.csv("~/Dropbox/Texas Project/ME/me_bill_votes.csv")
cosp<-read.csv("~/Dropbox/Texas Project/ME/me_bill_sponsors.csv")
rollcalls<-read.csv("~/Dropbox/Texas Project/ME/me_bill_legislator_votes.csv")

###Subset cosp network to lower chamber 81st 
unique.legs<-unique(cosp$leg_id)
cosp2<-subset(cosp, cosp$chamber=="lower")
unique.bills<-unique(cosp2$bill_id)

###Generate blank Matrix
matrix3<-matrix(0, length(unique.legs), length(unique.legs))
row.names(matrix3)<-unique.legs
colnames(matrix3)<-unique.legs

###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="cosponsor")], unique.legs)	
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)
###Subset cosp network to upper chamber 81st 
cosp2<-subset(cosp, cosp$chamber=="upper")
unique.bills<-unique(cosp2$bill_id)


###Fill in cross-chamber sponsor to author connections
for(i in 1:length(unique.bills)){
cosp3<-subset(cosp2, cosp2$bill_id==unique.bills[i])	
author<-match(cosp3$leg_id[which(cosp3$type=="primary")], unique.legs)	
sponsor<-match(cosp3$leg_id[which(cosp3$type=="cosponsor")], unique.legs)
matrix3[sponsor, author]<-matrix3[sponsor, author]+1
#if(sum(cosp3$type=="sponsor")>1){print(unique.bills[i])}
}
sum(matrix3>0)

########Add Party Vector
Party<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Party[i]<-as.character(legs$party[which(legs$leg_id==row.names(matrix3)[i])])
}
}

missing<-which(Party!="Democratic" & Party!="Republican" & Party!="Independent"| is.na(Party)==TRUE)
missing2<-vector(mode="character", length=length(missing))
for(i in 1:length(missing)){
missing2[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[missing[i]])])[1]	
}

Party
Party[47]<-"Independent"
Party[66]<-"Republican"
Party[79]<-"Democratic"
Party[81]<-"Independent"
Party[154]<-"Republican"
Party<-ifelse(Party=="Republican", 0, Party)
Party<-ifelse(Party=="Democratic", 1, Party)
Party<-ifelse(Party=="Independent", 1, Party)

########Add Chamber Vector
Chamber<-rep(NA, nrow(matrix3))
for(i in 1:length(Party)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Chamber[i]<-as.character(legs$chamber[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Chamber[47]<-"lower"
Chamber[66]<-"upper"
Chamber[79]<-"lower"
Chamber[81]<-"lower"
Chamber[154]<-"lower"

Chamber2<-ifelse(Chamber=="upper", 1, 0)

############Clear Matrix of within-chamber connections
ss<-which(Chamber2==1)
rr<-which(Chamber2==0)

sum(matrix3>0)
matrix3[ss,ss]<-0
matrix3[rr,rr]<-0
sum(matrix3>0)

########Add District Overlap using dyadcov(x, attrname)
dist.house<-read.csv("~/Dropbox/Texas Project/ME/staterepdists.csv")
dist.senate<-read.csv("~/Dropbox/Texas Project/ME/statesenatordists.csv")
lowers<-subset(legs, legs$chamber=="lower")
uppers<-subset(legs, legs$chamber=="upper")
dist.mat<-matrix(0, nrow(lowers), nrow(uppers))
row.names(dist.mat)<-lowers$leg_id
colnames(dist.mat)<-uppers$leg_id
for(i in 1:nrow(dist.house)){
ll<-which(lowers$district==dist.house$HOUSE.DISTRICT[i])	
for(j in 1:length(unique(dist.senate$SENATE.DISTRICT[which(as.character(dist.senate$TOWN)==as.character(dist.house$TOWN[i]))]))){
uu<-which(uppers$district==unique(dist.senate$SENATE.DISTRICT[which(as.character(dist.senate$TOWN)==as.character(dist.house$TOWN[i]))])[j])
dist.mat[ll,uu]<-dist.mat[ll,uu]+1
}
}

dist.mat2<-matrix(0, nrow(matrix3), nrow(matrix3))
row.names(dist.mat2)<-row.names(matrix3)
colnames(dist.mat2)<-row.names(matrix3)
for(i in 1:nrow(dist.mat)){
for(j in 1:ncol(dist.mat)){
rr<-which(row.names(dist.mat2)==row.names(dist.mat)[i])	
ss<-which(row.names(dist.mat2)==colnames(dist.mat)[j])
dist.mat2[rr,ss]<-dist.mat2[rr,ss]+dist.mat[i,j]
dist.mat2[ss,rr]<-dist.mat2[ss,rr]+dist.mat[i,j]
}
}

########Add Leadership Vector
Leadership<-rep(NA, nrow(matrix3))
for(i in 1:length(Leadership)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Leadership[i]<-as.character(legs$leadership[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Leadership<-ifelse(is.na(Leadership)==FALSE, as.numeric(Leadership), 0)

########Add Appropriations Vector
Appropriations<-rep(NA, nrow(matrix3))
for(i in 1:length(Appropriations)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){
Appropriations[i]<-as.character(legs$Appropriations[which(legs$leg_id==row.names(matrix3)[i])])
}
}
Appropriations<-ifelse(is.na(Appropriations)==FALSE, as.numeric(Appropriations), 0)

######Rename Legislators from IDs to Names
leg.names<-vector(mode="character", length=length(Party))
for(i in 1:length(leg.names)){
leg.names[i]<-as.character(cosp$name[which(cosp$leg_id==row.names(matrix3)[i])[1]])	
}

####Replace NA's with 0's
legs$Agriculture<-ifelse(is.na(legs$Agriculture)==TRUE, 0, legs$Agriculture)
legs$Appropriations<-ifelse(is.na(legs$Appropriations)==TRUE, 0, legs$Appropriations)
legs$Criminal.Justice<-ifelse(is.na(legs$Criminal.Justice)==TRUE, 0, legs$Criminal.Justice)
legs$Education<-ifelse(is.na(legs$Education)==TRUE, 0, legs$Education)
legs$Energy<-ifelse(is.na(legs$Energy)==TRUE, 0, legs$Energy)
legs$Environment<-ifelse(is.na(legs$Environment)==TRUE, 0, legs$Environment)
legs$Health<-ifelse(is.na(legs$Health)==TRUE, 0, legs$Health)
legs$Fisheries<-ifelse(is.na(legs$Fisheries)==TRUE, 0, legs$Fisheries)
legs$Judiciary<-ifelse(is.na(legs$Judiciary)==TRUE, 0, legs$Judiciary)
legs$Labor<-ifelse(is.na(legs$Labor)==TRUE, 0, legs$Labor)
legs$Marine<-ifelse(is.na(legs$Marine)==TRUE, 0, legs$Marine)
legs$State.and.Local<-ifelse(is.na(legs$State.and.Local)==TRUE, 0, legs$State.and.Local)
legs$Taxation<-ifelse(is.na(legs$Taxation)==TRUE, 0, legs$Taxation)
legs$Transportation<-ifelse(is.na(legs$Transportation)==TRUE, 0, legs$Transportation)
legs$Veterans<-ifelse(is.na(legs$Veterans)==TRUE, 0, legs$Veterans)


####Add Joint Committees
Joint.Comms<-numeric(length=nrow(matrix3))
for(i in 1:length(Joint.Comms)){
if(length(which(legs$leg_id==row.names(matrix3)[i]))>0){	
if(legs$Agriculture[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-1}
if(legs$Appropriations[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-2}
if(legs$Criminal.Justice[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-3}
if(legs$Education[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-4}
if(legs$Energy[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-5}
if(legs$Environment[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-6}
if(legs$Health[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-7}
if(legs$Fisheries[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-8}
if(legs$Judiciary[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Labor[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Marine[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$State.and.Local[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Taxation[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Transportation[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
if(legs$Veterans[which(legs$leg_id==row.names(matrix3)[i])]==1){Joint.Comms[i]<-9}
}		
#print(i)
}


###Set Network
Network<-network(matrix3)

###Add Attributes
set.vertex.attribute(Network, "Party", Party)
set.vertex.attribute(Network, "names", leg.names)
set.vertex.attribute(Network, "leadership", Leadership)
set.vertex.attribute(Network, "Appropriations", Appropriations)
set.vertex.attribute(Network, "Chamber", Chamber2)
set.vertex.attribute(Network, "JointComms", Joint.Comms)

#####Drop Isolates from the data
zz<-which(degree(Network)==0)
mat4<-matrix3[-zz,-zz]
Party2<-Party[-zz]
leg.names2<-leg.names[-zz]
Leadership2<-Leadership[-zz]
Appropriations2<-Appropriations[-zz]
Chamber3<-Chamber2[-zz]
dist.mat3<-dist.mat2[-zz,-zz]
Joint.Comms2<-Joint.Comms[-zz]

Chamber.Mat<-ifelse(outer(Chamber3, Chamber3, FUN="==")==TRUE, 1, 0)

ss<-which(Chamber3==1)
rr<-which(Chamber3==0)

###Re-designate network
Network2<-network(mat4)
set.vertex.attribute(Network2, "Party", Party2)
set.vertex.attribute(Network2, "names", leg.names2)
set.vertex.attribute(Network2, "leadership", Leadership2)
set.vertex.attribute(Network2, "Appropriations", Appropriations2)
set.vertex.attribute(Network2, "Chamber", Chamber3)
set.vertex.attribute(Network2, "JointComms", Joint.Comms2)



matME<-mat4
PartyME<-Party2
leg.namesME<-leg.names2
LeadershipME<-Leadership2
#RulesME<-Rules
AppropriationsME<-Appropriations2
ChamberME<-Chamber3
dist.matME<-dist.mat3
Joint.CommsME<-Joint.Comms2
MEid<-rep(5, nrow(matME))

#############################
###########Model
set.seed(111)
summary(mod.exME<-ergm(Network2~edges+nodematch("Chamber")+nodematch("Party", diff=TRUE)+nodematch("leadership", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE)+nodematch("JointComms")+edgecov(dist.mat2)+istar(2), control=control.ergm(MCMC.samplesize=50000, MCMLE.maxit=50, MCMC.burnin=75000, MCMC.interval=1000)))
dput(mod.exME, file="MaineResults2.txt")

###############################
##Create Effects Plots#########
sims<-2500
m1<-numeric(sims)
m2<-numeric(sims)
m3<-numeric(sims)
m4<-numeric(sims)
m5<-numeric(sims)
m6<-numeric(sims)

coefs1<-coef(mod.exME)
coefs2<-coefs1
coefs2[1]<-0
coefs3<-coefs1
coefs3[3:4]<-0
coefs4<-coefs1
coefs4[7:9]<-0

for(i in 1:sims){
sim.ex<-network(simulate(mod.exME, coef=coefs1))
set.vertex.attribute(sim.ex, "Party", Party2)
set.vertex.attribute(sim.ex, "names", leg.names2)
set.vertex.attribute(sim.ex, "leadership", Leadership2)
#set.vertex.attribute(sim.ex, "Rules", Rules)
set.vertex.attribute(sim.ex, "Appropriations", Appropriations2)
set.vertex.attribute(sim.ex, "Chamber", Chamber3)
#set.vertex.attribute(sim.ex, "SenDist", SenDist2)
set.vertex.attribute(sim.ex, "JointComms", Joint.Comms2)

sim.ex2<-network(simulate(mod.exME, coef=coefs2))
set.vertex.attribute(sim.ex2, "Party", Party2)
set.vertex.attribute(sim.ex2, "names", leg.names2)
set.vertex.attribute(sim.ex2, "leadership", Leadership2)
#set.vertex.attribute(sim.ex2, "Rules", Rules)
set.vertex.attribute(sim.ex2, "Appropriations", Appropriations2)
set.vertex.attribute(sim.ex2, "Chamber", Chamber3)
#set.vertex.attribute(sim.ex2, "SenDist", SenDist2)
set.vertex.attribute(sim.ex2, "JointComms", Joint.Comms2)

sim.ex3<-network(simulate(mod.exME, coef=coefs3))
set.vertex.attribute(sim.ex3, "Party", Party2)
set.vertex.attribute(sim.ex3, "names", leg.names2)
set.vertex.attribute(sim.ex3, "leadership", Leadership2)
#set.vertex.attribute(sim.ex3, "Rules", Rules)
set.vertex.attribute(sim.ex3, "Appropriations", Appropriations2)
set.vertex.attribute(sim.ex3, "Chamber", Chamber3)
#set.vertex.attribute(sim.ex3, "SenDist", SenDist2)
set.vertex.attribute(sim.ex3, "JointComms", Joint.Comms2)

sim.ex4<-network(simulate(mod.exME, coef=coefs4))
set.vertex.attribute(sim.ex4, "Party", Party2)
set.vertex.attribute(sim.ex4, "names", leg.names2)
set.vertex.attribute(sim.ex4, "leadership", Leadership2)
#set.vertex.attribute(sim.ex4, "Rules", Rules)
set.vertex.attribute(sim.ex4, "Appropriations", Appropriations2)
set.vertex.attribute(sim.ex4, "Chamber", Chamber3)
#set.vertex.attribute(sim.ex4, "SenDist", SenDist2)
set.vertex.attribute(sim.ex4, "JointComms", Joint.Comms2)

a<-summary(sim.ex~edges+nodematch("Party", diff=TRUE)+mutual+mutual(same="Party", diff=TRUE))
b<-summary(sim.ex2~edges)
c<-summary(sim.ex3~nodematch("Party", diff=TRUE))
d<-summary(sim.ex4~mutual+mutual(same="Party", diff=TRUE))


m1[i]<-(a[1]/b[1])
m2[i]<-(a[2]/c[1])
m3[i]<-(a[3]/c[2])
m4[i]<-(a[4]/d[1])
m5[i]<-(a[5]/d[2])
m6[i]<-(a[6]/d[3])

print(i)
}

MEm1<-m1
MEm2<-m2
MEm3<-m3
MEm4<-m4
MEm5<-m5
MEm6<-m6


######################################


#################################
####Remove Infinity##############
MEm1<-ifelse(MEm1==Inf, NA, MEm1)
MEm2<-ifelse(MEm2==Inf, NA, MEm2)
MEm3<-ifelse(MEm3==Inf, NA, MEm3)
MEm4<-ifelse(MEm4==Inf, NA, MEm4)
MEm5<-ifelse(MEm5==Inf, NA, MEm5)
MEm6<-ifelse(MEm6==Inf, NA, MEm6)
XX<-cbind(MEm1, MEm2, MEm3, MEm4, MEm5, MEm6)
colnames(XX)<-c("Edges", "NodeMatchParty0", "NodeMatchParty1", "Mutual", "MutualParty1", "MutualParty2")
#dput(XX, "MaineEffects2.txt")

COm1<-ifelse(COm1==Inf, NA, COm1)
COm2<-ifelse(COm2==Inf, NA, COm2)
COm3<-ifelse(COm3==Inf, NA, COm3)
COm4<-ifelse(COm4==Inf, NA, COm4)
COm5<-ifelse(COm5==Inf, NA, COm5)
COm6<-ifelse(COm6==Inf, NA, COm6)
XX<-cbind(COm1, COm2, COm3, COm4, COm5, COm6)
colnames(XX)<-c("Edges", "NodeMatchParty0", "NodeMatchParty1", "Mutual", "MutualParty1", "MutualParty2")
#dput(XX, "ColoradoEffects2.txt")


OKm1<-ifelse(OKm1==Inf, NA, OKm1)
OKm2<-ifelse(OKm2==Inf, NA, OKm2)
OKm3<-ifelse(OKm3==Inf, NA, OKm3)
OKm4<-ifelse(OKm4==Inf, NA, OKm4)
OKm5<-ifelse(OKm5==Inf, NA, OKm5)
OKm6<-ifelse(OKm6==Inf, NA, OKm6)
XX<-cbind(OKm1, OKm2, OKm3, OKm4, OKm5, OKm6)
colnames(XX)<-c("Edges", "NodeMatchParty0", "NodeMatchParty1", "Mutual", "MutualParty1", "MutualParty2")
#dput(XX, "OklahomaEffects2.txt")

TXm1<-ifelse(TXm1==Inf, NA, TXm1)
TXm2<-ifelse(TXm2==Inf, NA, TXm2)
TXm3<-ifelse(TXm3==Inf, NA, TXm3)
TXm4<-ifelse(TXm4==Inf, NA, TXm4)
TXm5<-ifelse(TXm5==Inf, NA, TXm5)
TXm6<-ifelse(TXm6==Inf, NA, TXm6)
XX<-cbind(TXm1, TXm2, TXm3, TXm4, TXm5, TXm6)
colnames(XX)<-c("Edges", "NodeMatchParty0", "NodeMatchParty1", "Mutual", "MutualParty1", "MutualParty2")
#dput(XX, "TexasEffects2.txt")

###########################################
###Plot Comparative EFfects################
###########################################
#pdf("StateCPties2.pdf")
plot(1, mean(TXm2, na.rm=T), xlim=c(0.75, 4.75), pch="", xaxt="n", xlab="State", ylim=c(0, 5.5), cex=0.75, ylab="Dyad Type Ties (Estimated) /  Dyad Type Ties (Null)")
abline(h=seq(0, 5, by=1), lty=2, lwd=0.8, col="grey")
polygon(x=c(0.75, 0.75, 1, 1), y=c(1, mean(TXm2), mean(TXm2), 1), col="blue")
polygon(x=c(1.75, 1.75, 2, 2), y=c(1, mean(OKm2), mean(OKm2), 1), col="blue")
polygon(x=c(2.75, 2.75, 3, 3), y=c(1, mean(MEm2), mean(MEm2), 1), col="blue")
polygon(x=c(3.75, 3.75, 4, 4), y=c(1, mean(COm2), mean(COm2), 1), col="blue")

polygon(x=c(1, 1, 1.25, 1.25), y=c(1, mean(TXm3), mean(TXm3), 1), col="red")
polygon(x=c(2, 2, 2.25, 2.25), y=c(1, mean(OKm3), mean(OKm3), 1), col="red")
polygon(x=c(3, 3, 3.25, 3.25), y=c(1, mean(MEm3), mean(MEm3), 1), col="red")
polygon(x=c(4, 4, 4.25, 4.25), y=c(1, mean(COm3), mean(COm3), 1), col="red")

polygon(x=c(1.25, 1.25, 1.5, 1.5), y=c(1, mean(TXm1), mean(TXm1), 1), col="grey")
polygon(x=c(2.25, 2.25, 2.5, 2.5), y=c(1, mean(OKm1), mean(OKm1), 1), col="grey")
polygon(x=c(3.25, 3.25, 3.5, 3.5), y=c(1, mean(MEm1), mean(MEm1), 1), col="grey")
polygon(x=c(4.25, 4.25, 4.5, 4.5), y=c(1, mean(COm1), mean(COm1), 1), col="grey")

segments(0.875, quantile(TXm2, 0.025, na.rm=T), 0.875, quantile(TXm2, 0.975, na.rm=T), lwd=3)
segments(1.875, quantile(OKm2, 0.025, na.rm=T), 1.875, quantile(OKm2, 0.975, na.rm=T), lwd=3)
segments(2.875, quantile(MEm2, 0.025, na.rm=T), 2.875, quantile(MEm2, 0.975, na.rm=T), lwd=3)
segments(3.875, quantile(COm2, 0.025, na.rm=T), 3.875, quantile(COm2, 0.975, na.rm=T), lwd=3)

segments(1.125, quantile(TXm3, 0.025, na.rm=T), 1.125, quantile(TXm3, 0.975, na.rm=T), lwd=3)
segments(2.125, quantile(OKm3, 0.025, na.rm=T), 2.125, quantile(OKm3, 0.975, na.rm=T), lwd=3)
segments(3.125, quantile(MEm3, 0.025, na.rm=T), 3.125, quantile(MEm3, 0.975, na.rm=T), lwd=3)
segments(4.125, quantile(COm3, 0.025, na.rm=T), 4.125, quantile(COm3, 0.975, na.rm=T), lwd=3)

segments(1.375, quantile(TXm1, 0.001, na.rm=T), 1.375, quantile(TXm1, 0.999, na.rm=T), lwd=3)
segments(2.375, quantile(OKm1, 0.001, na.rm=T), 2.375, quantile(OKm1, 0.999, na.rm=T), lwd=3)
segments(3.375, quantile(MEm1, 0.001, na.rm=T), 3.375, quantile(MEm1, 0.999, na.rm=T), lwd=3)
segments(4.375, quantile(COm1, 0.001, na.rm=T), 4.375, quantile(COm1, 0.999, na.rm=T), lwd=3)

abline(h=1, lwd=2, lty=1)
axis(1, at=c(1,2,3,4), labels=c("Texas", "Oklahoma", "Maine", "Colorado"), cex.axis=0.8)
title(main="Multiplicative Change In Ties by Dyad Type From \nNull Model to Estimated Model")
legend(3.25,5.25, c("Both Democrats Effect", "Both Republicans Effect", "Cross Party Effect", "95% Confidence Interval"), lty=c(1,1,1,1), col=c("blue", "red", "grey", "black"), lwd=c(3,3,3,2), bg="white", cex=0.8)
#dev.off()

#########################################
####Reciprocity##########################

#pdf("StateCPrecip2.pdf")
plot(1, mean(TXm4, na.rm=T), xlim=c(0.75, 4.75), pch="", xaxt="n", xlab="State", ylim=c(0, 25), cex=0.75, ylab="Dyad Type Ties (Estimated) /  Dyad Type Ties (Null)")
abline(h=seq(0, 25, by=5), lty=2, lwd=0.8, col="grey")
polygon(x=c(0.75, 0.75, 1, 1), y=c(1, mean(TXm4, na.rm=T), mean(TXm4, na.rm=T), 1), col="grey")
polygon(x=c(1.75, 1.75, 2, 2), y=c(1, mean(OKm4, na.rm=T), mean(OKm4, na.rm=T), 1), col="grey")
polygon(x=c(2.75, 2.75, 3, 3), y=c(1, mean(MEm4, na.rm=T), mean(MEm4, na.rm=T), 1), col="grey")
polygon(x=c(3.75, 3.75, 4, 4), y=c(1, mean(COm4, na.rm=T), mean(COm4, na.rm=T), 1), col="grey")

polygon(x=c(1, 1, 1.25, 1.25), y=c(1, mean(TXm5, na.rm=T), mean(TXm5, na.rm=T), 1), col="blue")
polygon(x=c(2, 2, 2.25, 2.25), y=c(1, mean(OKm5, na.rm=T), mean(OKm5, na.rm=T), 1), col="blue")
polygon(x=c(3, 3, 3.25, 3.25), y=c(1, mean(MEm5, na.rm=T), mean(MEm5, na.rm=T), 1), col="blue")
polygon(x=c(4, 4, 4.25, 4.25), y=c(1, mean(COm5, na.rm=T), mean(COm5, na.rm=T), 1), col="blue")

polygon(x=c(1.25, 1.25, 1.5, 1.5), y=c(1, mean(TXm6, na.rm=T), mean(TXm6, na.rm=T), 1), col="red")
polygon(x=c(2.25, 2.25, 2.5, 2.5), y=c(1, mean(OKm6, na.rm=T), mean(OKm6, na.rm=T), 1), col="red")
polygon(x=c(3.25, 3.25, 3.5, 3.5), y=c(1, mean(MEm6, na.rm=T), mean(MEm6, na.rm=T), 1), col="red")
polygon(x=c(4.25, 4.25, 4.5, 4.5), y=c(1, mean(COm6, na.rm=T), mean(COm6, na.rm=T), 1), col="red")

segments(0.875, quantile(TXm4, 0.025, na.rm=T), 0.875, quantile(TXm4, 0.975, na.rm=T), lwd=3)
segments(1.875, quantile(OKm4, 0.025, na.rm=T), 1.875, quantile(OKm4, 0.975, na.rm=T), lwd=3)
segments(2.875, quantile(MEm4, 0.025, na.rm=T), 2.875, quantile(MEm4, 0.975, na.rm=T), lwd=3)
segments(3.875, quantile(COm4, 0.025, na.rm=T), 3.875, quantile(COm4, 0.975, na.rm=T), lwd=3)

segments(1.125, quantile(TXm5, 0.025, na.rm=T), 1.125, quantile(TXm5, 0.975, na.rm=T), lwd=3)
segments(2.125, quantile(OKm5, 0.025, na.rm=T), 2.125, quantile(OKm5, 0.975, na.rm=T), lwd=3)
segments(3.125, quantile(MEm5, 0.025, na.rm=T), 3.125, quantile(MEm5, 0.975, na.rm=T), lwd=3)
segments(4.125, quantile(COm5, 0.025, na.rm=T), 4.125, quantile(COm5, 0.975, na.rm=T), lwd=3)

segments(1.375, quantile(TXm6, 0.025, na.rm=T), 1.375, quantile(TXm6, 0.975, na.rm=T), lwd=3)
segments(2.375, quantile(OKm6, 0.025, na.rm=T), 2.375, quantile(OKm6, 0.975, na.rm=T), lwd=3)
segments(3.375, quantile(MEm6, 0.025, na.rm=T), 3.375, quantile(MEm6, 0.975, na.rm=T), lwd=3)
segments(4.375, quantile(COm6, 0.025, na.rm=T), 4.375, quantile(COm6, 0.975, na.rm=T), lwd=3)


abline(h=1, lwd=2, lty=1)
axis(1, at=c(1,2,3,4), labels=c("Texas", "Oklahoma", "Maine", "Colorado"), cex.axis=0.8)
title(main="Multiplicative Change In Reciprocal Ties by Dyad Type \nFrom Null Model to Estimated Model")
legend(2.5,25, c("General Reciprocity", "Democratic Reciprocity", "Republican Reciprocity", "95% Confidence Interval"), lty=c(1,1,1,1), col=c( "grey", "blue", "red", "black"), lwd=c(3,3,3,2), bg="white", cex=0.8)
#dev.off()

#####################################
##Texas Only Effects#################

#pdf("TexasTies.pdf")
plot(1, mean(TXm1), xlim=c(0.75, 3.25), ylim=c(0.2, 2.5), pch="", cex=1.5, xaxt="n", xlab="Dyad Type", ylab="Ties (Estimated) / Ties (Null)")
polygon(x=c(0.75, 0.75, 1.25, 1.25), y=c(1, mean(TXm1, na.rm=T), mean(TXm1, na.rm=T), 1), col="grey")
polygon(x=c(1.75, 1.75, 2.25, 2.25), y=c(1, mean(TXm2, na.rm=T), mean(TXm2, na.rm=T), 1), col="blue")
polygon(x=c(2.75, 2.75, 3.25, 3.25), y=c(1, mean(TXm3, na.rm=T), mean(TXm3, na.rm=T), 1), col="red")
segments(1, min(TXm1), 1, max(TXm1), lwd=3)
segments(2, quantile(TXm2, 0.005), 2, quantile(TXm2, 0.995), lwd=3)
segments(3, quantile(TXm3, 0.005), 3, quantile(TXm3, 0.995), lwd=3)
axis(1, at=c(1,2,3), labels=c("Cross Party Ties", "Within Democratic Ties", "Within Republican Ties"), cex.axis=0.8)
abline(h=seq(0.5, 2.5, by=0.5), lty=2, lwd=0.75, col="grey")
abline(h=1,lwd=3)
legend(0.75,2.25, c("Both Democrats Effect", "Both Republicans Effect", "Cross Party Effect", "95% Confidence Interval"), lty=c(1,1,1,1), col=c("blue", "red", "grey", "black"), lwd=c(3,3,3,2), bg="white", cex=0.8)
title(main="Multiplicative Change in Frequency of Ties by \nDyad Type from Null Model to Estimated Model", sub="81st Texas State Legislature")
#dev.off()

#pdf("TexasRecip.pdf")
plot(1, mean(TXm4), xlim=c(0.75, 3.25), ylim=c(0, 5), pch="", cex=1.5, xaxt="n", xlab="Dyad Type", ylab="Reciprocity (Estimated) / Reciprocity (Null)")
abline(h=seq(0, 5, by=1), lty=2, lwd=0.75, col="grey")
polygon(x=c(0.75, 0.75, 1.25, 1.25), y=c(1, mean(TXm4, na.rm=T), mean(TXm4, na.rm=T), 1), col="grey")
polygon(x=c(1.75, 1.75, 2.25, 2.25), y=c(1, mean(TXm5, na.rm=T), mean(TXm5, na.rm=T), 1), col="blue")
polygon(x=c(2.75, 2.75, 3.25, 3.25), y=c(1, mean(TXm6, na.rm=T), mean(TXm6, na.rm=T), 1), col="red")
segments(1, quantile(TXm4, 0.025), 1, quantile(TXm4, 0.975), lwd=3)
segments(2, quantile(TXm5, 0.025), 2, quantile(TXm5, 0.975), lwd=3)
segments(3, quantile(TXm6, 0.025), 3, quantile(TXm6, 0.975), lwd=3)
axis(1, at=c(1,2,3), labels=c("Cross Party \nReciprocal Dyads", "Within Democratic \nReciprocal Dyads", "Within Republican \nReciprocal Dyads"), cex.axis=0.8)
abline(h=1,lwd=3)
legend(2.25,5, c("General Reciprocity", "Democratic Reciprocity", "Republican Reciprocity", "95% Confidence Interval"), lty=c(1,1,1,1), col=c( "grey", "blue", "red", "black"), lwd=c(3,3,3,2), bg="white", cex=0.8)
title(main="Multiplicative Change in Reciprocal Ties by \nDyad Type from Null Model to Estimated Model", sub="81st Texas State Legislature")
#dev.off()

