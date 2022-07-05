library(runjags)
library(rjags)
library(coda)

# load LEPC count data
lepc.ini<-read.csv("LEPC_count_data.csv")

# input first and second visits
visit.idx=rep(NA,17) # create vector of NAs
visit.1=9999 # create 9999 placeholder

for(j in 15:52){ 
  sub.idx=subset(lepc.ini,lepc.ini$Year==j+1962) # subset years to 1977-2014
  sub2.idx=subset(sub.idx,sub.idx$Visit==1) # subset visit 1
  sub.order=sub2.idx[order(sub2.idx$Route),] # order by route
  visit.idx[sub.order$Route]=sub.order$Count # insert count
  visit.1=c(visit.1,visit.idx) # add year of count data as a column to the matrix
  visit.idx=rep(NA,17) # reset to empty vector
}
visit.1=visit.1[-1] # drop initial 9999 placeholder

visit.2=9999 # create new 9999 placeholder
for(k in 15:52){
  sub.idx=subset(lepc.ini,lepc.ini$Year==k+1962) # subset years to 1977-2014
  sub2.idx=subset(sub.idx,sub.idx$Visit==2) # subset visit 2
  sub.order=sub2.idx[order(sub2.idx$Route),] # order by route
  visit.idx[sub.order$Route]=sub.order$Count # insert count
  visit.2=c(visit.2,visit.idx) # add year's count data as a column to the matrix
  visit.idx=rep(NA,17) # reset to empty vector
}
visit.2=visit.2[-1] # drop initial 9999 placeholder

lepc.data=data.frame(route=rep(seq(1,17),38),visit1=visit.1,
                    visit2=visit.2,Year=rep(1977:2014,each=17)) # combine two visits into single data frame

# rearrange into an array
y <- array(NA,dim=c(17,2,38)) # create empty array of size routes x visits x years
for(k in 1:(length(y[1,1,]))){ # fill array with values from data frame
  sel.rows <- lepc.data$Year == k+1976
  y[,,k] <- as.matrix(lepc.data)[sel.rows,2:3]
}

y<-y[,,c(2:29,35:38)] # subset data to years with land cover data

# PDSI data
env.dat<-read.csv("LEPC_environmental_data.csv") # load full environmental data
pdsi.ini<- env.dat[,c(1,2,9)]# subset to PDSI data

pdsi.idx=rep(NA,17) # create empty vector
pdsi.1=9999 # create 9999 placeholder

for(j in 1:38){
  sub.idx=subset(pdsi.ini,pdsi.ini$Year==j+1976) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  pdsi.idx[sub.order$Route]=sub.order$PDSI # insert PDSI values
  pdsi.1=c(pdsi.1,pdsi.idx) # add year's PDSI values as a column to the matrix
  pdsi.idx=rep(NA,17) # reset to empty vector
}
pdsi.1=pdsi.1[-1] # drop initial 9999 placeholder

pdsi.data=data.frame(route=rep(seq(1,17),38),PDSI=pdsi.1,Year=rep(1977:2014,each=17)) # convert to data frame
pdsi.data$PDSI<-scale(pdsi.data$PDSI, center = TRUE, scale = TRUE) # scale and center data

pdsi <- matrix(NA,nrow=17,ncol=38) # create empty matrix of size routes x years
for(k in 1:(length(pdsi[1,]))){ # fill matrix with values from data frame
  sel.rows <- pdsi.data$Year == k+1976
  pdsi[,k] <- as.matrix(pdsi.data)[sel.rows,2]
}

pdsi0<-pdsi[,1] # PDSI value for the summer preceding the first year of survey
pdsi<-pdsi[,c(2:29,35:38)] # subset data to years with land cover data

# PCP data
pcp.ini<-env.dat[,c(1,2,11)] # subset PCP data

pcp.idx=rep(NA,17) # create empty vector
pcp.1=9999 # create 9999 placeholder

for(j in 1:38){
  sub.idx=subset(pcp.ini,pcp.ini$Year==j+1976) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  pcp.idx[sub.order$Route]=sub.order$PCP # insert PCP values
  pcp.1=c(pcp.1,pcp.idx) # add year's PCP values as a column to the matrix
  pcp.idx=rep(NA,17) # reset to empty vector
}
pcp.1=pcp.1[-1] # drop initial 9999 placeholder

pcp.data=data.frame(route=rep(seq(1,17),38),PCP=pcp.1,Year=rep(1977:2014,each=17)) # convert to data frame
pcp.data$PCP<-scale(pcp.data$PCP, center = TRUE, scale = TRUE) # scale and center data

pcp <- matrix(NA,nrow=17,ncol=38) # create empty matrix of size routes x years
for(k in 1:(length(pcp[1,]))){ # fill matrix with values from data frame
  sel.rows <- pcp.data$Year == k+1976
  pcp[,k] <- as.matrix(pcp.data)[sel.rows,2]
}

pcp<-pcp[,c(2:29,35:38)] # subset data to years with land cover data

# TMAX data
tmax.ini<-env.dat[,c(1,2,10)] # subset TMAX data

tmax.idx=rep(NA,17) # create empty vector
tmax.1=9999 # create 9999 placeholder

for(j in 1:38){
  sub.idx=subset(tmax.ini,tmax.ini$Year==j+1976) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  tmax.idx[sub.order$Route]=sub.order$TMAX # insert TMAX values
  tmax.1=c(tmax.1,tmax.idx) # add year's TMAX values as a column to the matrix
  tmax.idx=rep(NA,17) # reset to empty vector
}
tmax.1=tmax.1[-1] # drop initial 9999 placeholder

tmax.data=data.frame(route=rep(seq(1,17),38),TMAX=tmax.1,Year=rep(1977:2014,each=17)) # convert to data frame
tmax.data$TMAX<-scale(tmax.data$TMAX, center = TRUE, scale = TRUE) # scale and center data

tmax <- matrix(NA,nrow=17,ncol=38) # create empty matrix of size routes x years
for(k in 1:(length(tmax[1,]))){ # fill matrix with values from data frame
  sel.rows <- tmax.data$Year == k+1976
  tmax[,k] <- as.matrix(tmax.data)[sel.rows,2]
}

tmax0<-tmax[,1] # TMAX value for the summer preceding the first year of survey data
tmax<-tmax[,c(2:29,35:38)] # subset data to years with land cover data

# TMIN data
tmin.ini<-env.dat[,c(1,2,12)] # subset TMIN data

tmin.idx=rep(NA,17) # create empty vector
tmin.1=9999 # create 9999 placeholder

for(j in 1:38){
  sub.idx=subset(tmin.ini,tmin.ini$Year==j+1976) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  tmin.idx[sub.order$Route]=sub.order$TMIN # insert TMIN values
  tmin.1=c(tmin.1,tmin.idx) # add year's TMIN values as a column to the matrix
  tmin.idx=rep(NA,17) # reset to empty vector
}
tmin.1=tmin.1[-1] # drop initial 9999 placeholder

tmin.data=data.frame(route=rep(seq(1,17),38),TMIN=tmin.1,Year=rep(1977:2014,each=17)) # convert to data frame
tmin.data$TMIN<-scale(tmin.data$TMIN, center = TRUE, scale = TRUE) # scale and center data

tmin <- matrix(NA,nrow=17,ncol=38) # create empty matrix of size route x years
for(k in 1:(length(tmin[1,]))){ # fill matrix with values from data frame
  sel.rows <- tmin.data$Year == k+1976
  tmin[,k] <- as.matrix(tmin.data)[sel.rows,2]
}

tmin<-tmin[,c(2:29,35:38)] # subset data to years with land cover data

# GRASS data
grass.ini<-env.dat[,c(1,2,3)] # subset % grass data 
# (use column 3 for 3km buffer, 4 for 5km buffer, or 5 for 10km buffer)

grass.idx=rep(NA,17) # create empty vector
grass.1=9999 # create 9999 placeholder

for(j in 1:38){
  sub.idx=subset(grass.ini,grass.ini$Year==j+1976) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  grass.idx[sub.order$Route]=sub.order$GRASS_3km # insert % grass values
  grass.1=c(grass.1,grass.idx) # add year's % grass values as a column to the matrix
  grass.idx=rep(NA,17) # reset to empty vector
}
grass.1=grass.1[-1] # drop initial 9999 placeholder

grass.data=data.frame(route=rep(seq(1,17),38),GRASS=grass.1,Year=rep(1977:2014,each=17)) # convert to data frame
grass.data$GRASS<-scale(grass.data$GRASS, center = TRUE, scale = TRUE) # scale and center data

grass <- matrix(NA,nrow=17,ncol=38) # create empty matrix of size routes x years
for(k in 1:(length(grass[1,]))){ # fill matrix with values from data frame
  sel.rows <- grass.data$Year == k+1976
  grass[,k] <- as.matrix(grass.data)[sel.rows,2]
}

grass<-grass[,c(2:29,35:38)] # subset data to years with land cover data

# ED data
ed.ini<-env.dat[,c(1,2,6)] # subset % grass data 
# (use column 6 for 3km buffer, 7 for 5km buffer, or 8 for 10km buffer)

ed.idx=rep(NA,17) # create empty vector
ed.1=9999 # create 9999 placeholder

for(j in 1:37){
  sub.idx=subset(ed.ini,ed.ini$Year==j+1977) # subset years to 1977-2014
  sub.order=sub.idx[order(sub.idx$Route),] # order by route
  ed.idx[sub.order$Route]=sub.order$ED # insert % grass values
  ed.1=c(ed.1,ed.idx) # add year's % grass values as a column to the matrix
  ed.idx=rep(NA,17) # reset to empty vector
}
ed.1=ed.1[-1] # drop initial 9999 placeholder

ed.data=data.frame(route=rep(seq(1,17),37),ED=ed.1,Year=rep(1978:2014,each=17)) # convert to data frame
ed.data$ED<-scale(ed.data$ED, center = TRUE, scale = TRUE) # scale and center data

ed <- matrix(NA,nrow=17,ncol=37) # create empty matrix of size routes x years
for(k in 1:(length(ed[1,]))){ # fill matrix with values from data frame
  sel.rows <- ed.data$Year == k+1977
  ed[,k] <- as.matrix(ed.data)[sel.rows,2]
}

ed<-ed[,c(1:28,34:37)] # subset data to years with land cover data

# set up indexing 
nroute=nrow(y) # number of rows in count data array to use in model text file
nvisit=ncol(y) # number of visits in count data array to use in model text file
nyear=length(y[1,1,]) # number of years in count data to use in model text file

# initial values for N
y2 <- y
y2[is.na(y2)]=0
Nst <- apply(y2,c(1,3),max) + 1

# lower/upper bounds for threshold point priors
l.grass=min(grass)
u.grass=max(grass)
l.ed=min(ed)
u.ed=max(ed)

# bundle data
jags.data.grass <- list(y=y,pdsi=pdsi,pdsi0=pdsi0,pcp=pcp,tmax=tmax,tmax0=tmax0,tmin=tmin,grass=grass,nroute=nroute,nvisit=nvisit,nyear=nyear,l=l.grass,u=u.grass)
jags.data.ed <- list(y=y,pdsi=pdsi,pdsi0=pdsi0,pcp=pcp,tmax=tmax,tmax0=tmax0,tmin=tmin,ed=ed,nroute=nroute,nvisit=nvisit,nyear=nyear,l=l.ed,u=u.ed)

# initial values function
inits <- function(){list(N=Nst,beta0=runif(1,-1,1),
                         #phi=runif(1,0,3.5),delta=runif(1,-1,1),
                         sd.p=runif(1,0,5),
                         beta.p=runif(nyear,-1,1),
                        .RNG.name="base::Super-Duper")}

# list of parameters to monitor
params <- c("beta0","beta1","beta2","beta3","beta4","beta5",
            "delta","phi","sd.p","sd.mu","beta.p","over","totalN","fit","fit.new")

# run % grass model
grass.out<-run.jags(data=jags.data.grass,inits=inits,monitor=params,
              model="lepc_grpc_grass_threshold_model.txt",
              n.chains=3,adapt=1000,sample=10000,burnin=200000,
              thin=5)

save(grass.out, file="lepc_grass_3k.RData")

# run ED model
ed.out<-run.jags(data=jags.data.ed,inits=inits,monitor=params,
           model="lepc_grpc_ed_threshold_model.txt",
           n.chains=3,adapt=1000,sample=10000,burnin=200000,
           thin=5)

save(ed.out, file="lepc_ed_3k.RData")
