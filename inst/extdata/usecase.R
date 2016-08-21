fp<-makeFlightPlan(rootDir="~/proj",
                   workingDir="/drone/uniwald",
                   missionName = "ascent",
                   surveyArea="test4.json", 
                   followSurface = TRUE, 
                   flightAltitude = 50,
                   uavViewDir=30,
                   overlap = 0.6,
                   demFn = "mrbiko.tif",
                   altFilter = 3.5,
                   maxSpeed = 35,
                   followSurfaceRes = 10,
                   windCondition = 3)

mapview(fp[[5]],color="darkblue", alpha.regions = 0.1,lwd=0.5)+
  mapview(fp[[1]],zcol = "altitude",lwd=1,cex=4)+
  mapview(fp[[3]],color="red",cex=5)+
  mapview(fp[[4]],color="darkblue", alpha.regions = 0.1,lwd=0.5)