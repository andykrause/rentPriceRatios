##########################################################################################
#                                                                                        #
#  Script for analyzing PRR from APM data                                                #
#                                                                                        #
##########################################################################################

### Preliminary commands -----------------------------------------------------------------

 ## Set parameters and paths

  # Parameters
  reBuildData <- FALSE
  reAnalyze <- FALSE
  verbose <- TRUE

  # Paths and file names
  dataPath <- file.path(getwd(), 'data')
  exportPath <- dataPath
  saleFile <- 'newSales.csv'
  rentFile <- 'newRentals.csv'
  geoFiles <- list(suburb = 'shapefiles/Vic_Suburbs.shp',
                   lga = 'shapefiles/Vic_LGAs.shp',
                   sla1 = 'shapefiles/Vic_SLA1.shp',
                   postcode = 'shapefiles/Vic_PostCodes.shp',
                   ssFile = 'spatialData/allSS.csv')

  # Source functions
  invisible(sapply(file.path(getwd(), 'functions', 
                             list.files(file.path(getwd(), 'functions'))),
            source, echo=FALSE))
  
  
  # Set the global options
  apmSetOptions()
 
### Load Data ----------------------------------------------------------------------------

  load(file.path(dataPath, 'cleanTrans.RData'))
       
### Analyze Raw Data ---------------------------------------------------------------------

  ## Do data analysis

   if(reAnalyze){

    # Calculate full results
    apmFullDataAnalysis(trans.data=cleanTrans,
                        data.path=dataPath,
                        writeout=TRUE,
                        return.values=FALSE)

   }
 
  ## Load analysis results      
   
   load(paste0(dataPath, 'yieldResults.RData'))
   load(paste0(dataPath, 'studyShps.RData'))
#   
#   
#   sa1s.shp <- readShapePoly('c:/dropbox/research/prRatio/analyses/apmAnalysis/sa1s.shp',
#                             proj4string=CRS("+init=epsg:4283"),
#                             delete_null_obj=TRUE)
# 
# 
#  ## Calculate only with matched sample
#   
#   match.sample <- trans.data[trans.data$UID %in% match.data$saleID |
#                                trans.data$UID %in% match.data$rentID, ]
#   
#   ms <- apmFullDataAnalysis(trans.data=match.sample,
#                       data.path=dataPath,
#                       writeout=FALSE,
#                       return.values=TRUE)
#   
#   ms$match.data <- match.sample
#   ms$results$match <- match.results
#   
#  ## Add full sample back into a list
#   
#   full <- list(yield.data=yield.data,
#                impute.data=trans.data,
#                match.data=match.data,
#                index.data=index.data,
#                results=list(index=index.results,
#                             hedimp=hedimp.results,
#                             match=match.results))
#   
#     
# ### Prep Data Visualization --------------------------------------------------------------  
#   
#  ## Set up graphics parameters 
#   
#   apmPlotOptions()
#   
#  ## Isolate and Rename the necessary data
# 
#   # Full sample
#   full$yield.data$method <- as.character(full$yield.data$method)
#   full$yield.data$method[full$yield.data$method=='hedimp'] <- 'Impute'
#   full$yield.data$method[full$yield.data$method=='srm'] <- 'Match'
#   full$yield.data$method <- factor(full$yield.data$method, 
#                                    levels=c('Index', 'Impute', 'Match'))
#   full.geo.data <- split(full$yield.data, full$yield.data$geo.level)
#   
#   # Matched sample
#   ms$yield.data$method <- as.character(ms$yield.data$method)
#   ms$yield.data$method[ms$yield.data$method=='hedimp'] <- 'Impute'
#   ms$yield.data$method[ms$yield.data$method=='srm'] <- 'Match'
#   ms$yield.data$method <- factor(ms$yield.data$method, 
#                                    levels=c('Index', 'Impute', 'Match'))
#   ms.geo.data <- split(ms$yield.data, ms$yield.data$geo.level)
#   
# ### Create appreciation rates ------------------------------------------------------------ 
#  
#  ## Full Sample  
#   
#  ## Extract the index values
#   
#   full.glob.index <- full$index.data$Global$Global
#   
#  ## Build appr rate datas
#   
#   # House
#   full.har <- c(0, (full.glob.index$house.sale[2:20] - 
#                       full.glob.index$house.sale[1:19]))/100
#   full.har <- data.frame(time=1:20, app.rate=full.har)
#   
#   # Unit
#   full.uar <- c(0, (full.glob.index$unit.sale[2:20] - 
#                       full.glob.index$unit.sale[1:19]))/100
#   full.uar <- data.frame(time=1:20, app.rate=full.uar)
#   
#  ## Add appreciation rates to all geo.levels  
#   
#   full.dif.data <- calcDifGeoWrap(full.geo.data, full.har, full.uar, 
#                                   apmOptions$geo.levels)
# 
#   
#  ## Matched Sample  
#   
#   ## Extract the index values
#   
#   ms.glob.index <- ms$index.data$Global$Global
#   
#   ## Build appr rate datas
#   
#   # House
#   ms.har <- c(0, (ms.glob.index$house.sale[2:20] - 
#                       ms.glob.index$house.sale[1:19]))/100
#   ms.har <- data.frame(time=1:20, app.rate=ms.har)
#   
#   # Unit
#   ms.uar <- c(0, (ms.glob.index$unit.sale[2:20] - 
#                       ms.glob.index$unit.sale[1:19]))/100
#   ms.uar <- data.frame(time=1:20, app.rate=ms.uar)
#   
#   ## Add appreciation rates to all geo.levels  
#   
#   ms.dif.data <- calcDifGeoWrap(ms.geo.data, ms.har, ms.uar, 
#                                   apmOptions$geo.levels)
#   
# ### Save Workspace -----------------------------------------------------------------------
#   
#   save(full, full.dif.data, full.geo.data,
#        ms, ms.dif.data, ms.geo.data, studyShapes,
#        file=paste0(dataPath, 'rprWrkspce.RData'))
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   ##########################################################################################
#   ##  BELOW HERE:  Sandbox
#   ##########################################################################################
#   
#   ##
#   #  The join count statistic relates the number of
#   #  observed connections between the zones of property "presence" and those
#   #  of property "absence", with the theoretical number of connections of a 
#   #  random distribution. The definition of the theoretical number of 
#   #  connections of a random distribution is related to two factors:
#   
#   if(F){
#     
# ### Calculate Components of Composition Bias ---------------------------------------------  
#   
#  ## Composition Bias
#   
#   # Metro Level Str. Bias
#   metro.sb <- apmStrBias(trans.data, 'transQtr', 
#                          c('Bedrooms', 'Baths', 'AreaSize'),
#                          c('Bedrooms', 'Baths', 'Parking'))
#   metro.sb$geo.level <- 'Metro'
#   
#   # Lga Level
#   lgas <- names(table(trans.data$lga))
#   lga.list <- list()
#   for(i in 1:length(lgas)){
#     temp.sb <- apmStrBias(trans.data[trans.data$lga == lgas[i], ], 'transQtr',
#                           c('Bedrooms', 'Baths', 'AreaSize'),
#                           c('Bedrooms', 'Baths', 'Parking'))
#     temp.sb$lga <- lgas[i]
#     lga.list[[i]] <- temp.sb
#   }
#   lga.class <- lapply(lga.list, class)
#   lga.list <- lga.list[lga.class=='data.frame']
#   lga.sb <- plyr::rbind.fill(lga.list)
#   lga.sb$geo.level <- 'LGA'
#   
#   # Suburb Level
#   subs <- names(table(trans.data$suburb))
#   sub.list <- list()
#   for(i in 1:length(subs)){
#     temp.sb <- apmStrBias(trans.data[trans.data$suburb == subs[i], ], 'transQtr',
#                           c('Bedrooms', 'Baths', 'AreaSize'),
#                           c('Bedrooms', 'Baths', 'Parking'))
#     temp.sb$suburb <- subs[i]
#     sub.list[[i]] <- temp.sb
#   }
#   sub.class <- lapply(sub.list, class)
#   sub.list <- sub.list[sub.class=='data.frame']
#   sub.sb <- plyr::rbind.fill(sub.list)
#   sub.sb$geo.level <- 'Suburb'
#   
#   # Combine all
#   all.sb <- rbind(metro.sb, lga.sb[,-5], sub.sb[,-5])
#   comb.sb <- all.sb[all.sb$field == 'Combined',]
#   
#   ggplot(comb.sb, aes(x=time, y=ratio, group=geo.level, color=geo.level)) +
#     geom_point(size=.1, alpha=.1) +
#     stat_smooth(fullrange=TRUE) +
#     facet_wrap(~type) +
#     scale_y_continuous(limits=c(.9,1.2))
# 
#  ## Location Bias
#   
#   # Set up data
#    trans.sp <- SpatialPointsDataFrame(cbind(trans.data$Property_Longitude,
#                                             trans.data$Property_Latitude),
#                                       trans.data,
#                                       proj4string=CRS("+init=epsg:4283"))
#    # Add SA1s to data
#    spJoin <- over(trans.sp, sa1s.shp)
#    trans.sp@data$sa1 <- spJoin$SA1_MAIN11
#  
#  # Metro Level  
#    
#   lb.list <- list() 
#   data.list <- list()
#   for(j in 1:20){
#     res <- apmLocBias(trans.sp[trans.sp@data$transQtr == j,], geo.field='suburb')
#     temp <- data.frame(time=j,
#                        type=c('house', 'unit'),
#                        dif=res$dif,
#                        loc='sa1')
#     lb.list[[j]] <- temp
#     data.list[[j]] <- res$data
#   }  
#   lb.metro <- plyr::rbind.fill(lb.list)
#   
#      ggplot(lb.metro, aes(x=time, y=dif, group=type, color=type)) +
#        geom_point()+
#        stat_smooth() 
#        
#   
# #  # LGA Level
# #   lb.lga <- list()
# #   lgas <- names(table(trans.sp@data$lga))
# #   
# #   for(i in 1:length(lgas)){
# #     lb.list <- list() 
# #     data.list <- list()
# #     for(j in 1:20){
# #       res <- try(apmLocBias(trans.sp[trans.sp@data$transQtr == j &
# #                                    trans.sp@data$lga == lgas[i],], geo.field='sa1'),
# #                  silent=TRUE)
# #       if(class(res) == 'try-error' || sum(is.na(res$dif)) > 0){
# #         temp <- NULL
# #       } else {
# #         temp <- data.frame(time=j,
# #                            type=c('house', 'unit'),
# #                            dif=res$dif,
# #                            loc='mb')
# #       }
# #       lb.list[[j]] <- temp
# #       #data.list[[j]] <- res$data
# #     }
# #     x.temp <- plyr::rbind.fill(lb.list)
# #     x.temp$lga <- lgas[i]
# #     lb.lga[[i]] <- x.temp
# #   }
# #   
# #   good <- which(lapply(lb.lga, class) == 'data.frame')
# #   lb.lga <- plyr::rbind.fill(lb.lga[good])
# #   ggplot(lb.lga, aes(x=time, y=dif, group=type, color=type)) +
# #     geom_point()+
# #     stat_smooth() +
# #   
# #   # LGA Level
# #   lb.sub <- list()
# #   subs <- names(table(trans.sp@data$suburb))
# #   
# #   for(i in 1:length(subs)){
# #     lb.list <- list() 
# #     data.list <- list()
# #     for(j in 1:20){
# #       res <- try(apmLocBias(trans.sp[trans.sp@data$transQtr == j &
# #                                        trans.sp@data$suburb == subs[i],], geo.field='sa1'),
# #                  silent=TRUE)
# #       if(class(res) == 'try-error' || sum(is.na(res$dif)) > 0){
# #         temp <- NULL
# #       } else {
# #         temp <- data.frame(time=j,
# #                            type=c('house', 'unit'),
# #                            dif=res$dif,
# #                            loc='mb')
# #       }
# #       lb.list[[j]] <- temp
# #       #data.list[[j]] <- res$data
# #     }
# #     x.temp <- plyr::rbind.fill(lb.list)
# #     x.temp$suburb <- subs[i]
# #     lb.sub[[i]] <- x.temp
# #   }
# #   
# #   good <- which(lapply(lb.sub, class) == 'data.frame')
# #   lb.sub <- plyr::rbind.fill(lb.sub[good])
# #   ggplot(lb.sub, aes(x=time, y=dif, group=type, color=type)) +
# #     geom_point()+
# #     stat_smooth() 
# #     
#     
#       
#  
#  ## Omit bias controlling for space
#  
#   # Make loc dif data
#   dif.data <- plyr::rbind.fill(data.list)
#  
#   # Calc omit variable bias
#   om.list <- list() 
#   for(j in 1:20){
#     res <- apmOmitBias(dif.data[dif.data$transQtr == j,], 
#                     match.data=match.data)
#     temp <- data.frame(time=j,
#                        type=c('house', 'unit'),
#                        dif=res$dif,
#                        loc='sa1')
#     om.list[[j]] <- temp
#   }  
#   om.dif <- plyr::rbind.fill(om.list)
#  
#   
#   ggplot(om.dif, aes(x=time, y=dif, group=type, color=type)) +
#    geom_point()+
#    stat_smooth(alpha=.25)
#  
# 
# 
#  
#  mesh.shp <- readShapePoly('C:/Data/AUS/VIC/Geographic/vic_meshblocks.shp',
#                            proj4string=CRS("+init=epsg:4283"),
#                            delete_null_obj=TRUE)
#  
#  
#  apmLocBiasCluster <- function(sp.trans,
#                                clus.field='transType',
#                                k=5){
#    
#    house.sp <- sp.trans[sp.trans@data$PropertyType == 'House', ]
#    unit.sp <- sp.trans[sp.trans@data$PropertyType == 'Unit', ]
#    
#    house.knn <- knn2nb(knearneigh(house.sp, k))
#    house.nb <- nb2listw(house.knn, style="B",zero.policy=T)
#    
#    unit.knn <- knn2nb(knearneigh(unit.sp, k))
#    unit.nb <- nb2listw(unit.knn, style="B",zero.policy=T)
#    
#    house.jc <- joincount.test(as.factor(house.sp@data[,clus.field]), house.nb)
#    unit.jc <- joincount.test(as.factor(unit.sp@data[,clus.field]), unit.nb)
#    
#    return(c(house.jc[[1]]$statistic))
#    
#  } 
#  
#  
#  library(spatstat) 
#  t11 <- t1@data
#  
#  
#  t1pp <- ppp(t11$Property_Longitude-t1@bbox[1,1], 
#              t11$Property_Latitude-t1@bbox[2,1],
#              c(0,1.5), 
#              c(0,1))
#  marks(t1pp) <- as.factor(t11$transType)
#  Kcross(t1pp)
#  
#  
#  
#  
#  
#  
#  
#  miHouse <- joincount.test(t1@data$Sale[1:8000], swmH10, zero.policy=TRUE)
#  
#  data(oldcol)
#  HICRIME <- cut(COL.OLD$CRIME, breaks=c(0,35,80), labels=c("low","high"))
#  names(HICRIME) <- rownames(COL.OLD)
#  joincount.test(HICRIME, nb2listw(COL.nb, style="B"))
#  
#  t5000 <- t1[c(1:1000, 34000:35000),]
#  
#  nbList <- knn2nb(knearneigh(t5000, 9))
#  aa<-nb2listw(nbList, style="B",zero.policy=T)
#  
#  qq<-joincount.test(as.factor(t5000@data$transType), aa)
#  
#  y.list <- list() 
#  for(y in 1:20){
#    y.list[[y]] <- locSA1Bias(trans.sp@data[trans.sp@data$transQtr==y, ])
#  }
#  
#  
#  ## Look at str bias vs appr
#  metro.dif.h <- dif.data$houses[dif.data$houses$geo.level=='Global' & 
#                                   dif.data$houses$method == 'Index', ]
#  metro.dif.u <- dif.data$units[dif.data$units$geo.level=='Global' & 
#                                  dif.data$units$method == 'Index', ]
#  lga.dif.h <- dif.data$houses[dif.data$houses$geo.level=='lga' & 
#                                 dif.data$houses$method == 'Index', ]
#  lga.dif.u <- dif.data$units[dif.data$units$geo.level=='lga' & 
#                                dif.data$units$method == 'Index', ]
#  sub.dif.h <- dif.data$houses[dif.data$houses$geo.level=='suburb' & 
#                                 dif.data$houses$method == 'Index', ]
#  sub.dif.u <- dif.data$units[dif.data$units$geo.level=='suburb' & 
#                                dif.data$units$method == 'Index', ]
#  
#  metro.sb.h <- metro.sb[metro.sb$type == 'House',]
#  metro.sb.u <- metro.sb[metro.sb$type == 'Unit',]
#  lga.sb.h <- lga.sb[lga.sb$type == 'House',]
#  lga.sb.u <- lga.sb[lga.sb$type == 'Unit',]
#  sub.sb.h <- sub.sb[sub.sb$type == 'House',]
#  sub.sb.u <- sub.sb[sub.sb$type == 'Unit',]
#  
#  metro.sb.h$ID <- paste0('Global..', metro.sb.h$time)
#  metro.sb.u$ID <- paste0('Global..', metro.sb.u$time)
#  lga.sb.h$ID <- paste0(lga.sb.h$lga,'..', lga.sb.h$time)
#  lga.sb.u$ID <- paste0(lga.sb.u$lga,'..', lga.sb.u$time)
#  sub.sb.h$ID <- paste0(sub.sb.h$suburb,'..', sub.sb.h$time)
#  sub.sb.u$ID <- paste0(sub.sb.u$suburb,'..', sub.sb.u$time)
#  
#  metro.sb.h <- merge(metro.sb.h, metro.dif.h[,c('UID', 'appr.rate')], 
#                      by.x='ID', by.y='UID')
#  metro.sb.u <- merge(metro.sb.u, metro.dif.u[,c('UID', 'appr.rate')], 
#                      by.x='ID', by.y='UID')
#  lga.sb.h <- merge(lga.sb.h, lga.dif.h[,c('UID', 'appr.rate')], 
#                    by.x='ID', by.y='UID')
#  lga.sb.u <- merge(lga.sb.u, lga.dif.u[,c('UID', 'appr.rate')], 
#                    by.x='ID', by.y='UID')
#  sub.sb.h <- merge(sub.sb.h, sub.dif.h[,c('UID', 'appr.rate')], 
#                    by.x='ID', by.y='UID')
#  sub.sb.u <- merge(sub.sb.u, sub.dif.u[,c('UID', 'appr.rate')], 
#                    by.x='ID', by.y='UID')
#  
#  metro.sb <- rbind(metro.sb.h, metro.sb.u)
#  lga.sb <- rbind(lga.sb.h, lga.sb.u)
#  sub.sb <- rbind(sub.sb.h, sub.sb.u)
#  
#  all.sb <- rbind(metro.sb, lga.sb[,-6], sub.sb[,-6])
#  
#  ggplot(all.sb[all.sb$field=='Combined',],
#         aes(x=appr.rate, y=ratio, color=geo.level, 
#             group=geo.level)) +
#    geom_point()+
#    stat_smooth(fullrange=TRUE)+
#    facet_wrap(~type)+
#    scale_y_continuous(limits=c(.9,1.2))
#  
#  spJoin <- over(trans.sp, mesh.shp)
#  trans.sp@data$mb <- spJoin$mb_code11
#  
#  ## Test for difference in spatial aggregation sales prices 
#  
#  # LGA Level
#  lga.spag <- list()
#  for(i in 1:20){
#    temp <- locBiasSpag(trans.sp[trans.sp$transQtr==i,], 'lga')
#    lga.spag[[i]] <- data.frame(time=i,house=temp[1], unit=temp[2], geo.level='lga')
#  }
#  lga.spag <- plyr::rbind.fill(lga.spag)
#  
#  # Suburb Level 
#  sub.spag <- list()
#  for(i in 1:20){
#    temp <- locBiasSpag(trans.sp[trans.sp$transQtr==i,], 'suburb')
#    sub.spag[[i]] <- data.frame(time=i,house=temp[1], unit=temp[2], geo.level='suburb')
#  }
#  sub.spag <- plyr::rbind.fill(sub.spag)
#  
#  # SA1 Level
#  sa1.spag <- list()
#  for(i in 1:20){
#    temp <- locBiasSpag(trans.sp[trans.sp$transQtr==i,], 'sa1')
#    sa1.spag[[i]] <- data.frame(time=i,house=temp[1], unit=temp[2], geo.level='sa1')
#  }
#  sa1.spag <- plyr::rbind.fill(sa1.spag)
#  
#  # Mesh Block Level 
#  mb.spag <- list()
#  for(i in 1:20){
#    temp <- locBiasSpag(trans.sp[trans.sp$transQtr==i,], 'mb')
#    mb.spag[[i]] <- data.frame(time=i,house=temp[1], unit=temp[2], geo.level='mb')
#  }
#  mb.spag <- plyr::rbind.fill(mb.spag)
#  
#  all.spag <- rbind(lga.spag, sub.spag, sa1.spag, mb.spag)
#  ggplot(all.spag, aes(x=time, y=house, group=geo.level, color=geo.level)) +
#    geom_point(alpha=.3) +
#    stat_smooth()
#  
#  ggplot(all.spag, aes(x=time, y=unit, group=geo.level, color=geo.level)) +
#    geom_point(alpha=.3) +
#    stat_smooth()
#  
#  
#  
#  locBiasSpag <- function(sp.data,
#                          geo.field){ 
#    
#    if(class(sp.data) == 'SpatialPointsDataFrame'){
#      sp.data <- sp.data@data
#    }
#    
#    sp.sales <- sp.data[sp.data$transType == 'sale',]
#    sp.rents <- sp.data[sp.data$transType == 'rent',]
#    sp.house <- sp.data[sp.data$PropertyType == 'House', ]
#    sp.unit <- sp.data[sp.data$PropertyType == 'Unit', ]
#    
#    h.sales <- sp.sales[sp.sales$PropertyType == 'House',]
#    u.sales <- sp.sales[sp.sales$PropertyType == 'Unit',]
#    
#    h.rents <- sp.rents[sp.rents$PropertyType == 'House',]
#    u.rents <- sp.rents[sp.rents$PropertyType == 'Unit',]
#    
#    h.me <- tapply2DF(h.sales$transValue, h.sales[,geo.field], median)
#    u.me <- tapply2DF(u.sales$transValue, u.sales[,geo.field], median)
#    
#    h.mr <- tapply2DF(h.rents$transValue, h.rents[,geo.field], median)
#    u.mr <- tapply2DF(u.rents$transValue, u.rents[,geo.field], median)
#    
#    sp.house$loc.s <- h.me$Var[match(sp.house[,geo.field], h.me$ID)]
#    sp.unit$loc.s <- u.me$Var[match(sp.unit[,geo.field], u.me$ID)]
#    
#    sp.house$loc.r <- h.mr$Var[match(sp.house[,geo.field], h.mr$ID)]
#    sp.unit$loc.r <- u.mr$Var[match(sp.unit[,geo.field], u.mr$ID)]
#    
#    s.house.ratio <- tapply(sp.house$loc.s, sp.house$transType, mean, na.rm=TRUE)
#    s.unit.ratio <- tapply(sp.unit$loc.s, sp.unit$transType, mean, na.rm=TRUE)
#    
#    r.house.ratio <- tapply(sp.house$loc.r, sp.house$transType, mean, na.rm=TRUE)
#    r.unit.ratio <- tapply(sp.unit$loc.r, sp.unit$transType, mean, na.rm=TRUE)
#    
#    s.house.ratio <- s.house.ratio[2]/s.house.ratio[1]
#    s.unit.ratio <- s.unit.ratio[2]/s.unit.ratio[1]
#    
#    r.house.ratio <- r.house.ratio[2]/r.house.ratio[1]
#    r.unit.ratio <- r.unit.ratio[2]/r.unit.ratio[1]
#    
#    return(c(s.house.ratio, s.unit.ratio, r.house.ratio, r.unit.ratio))
#  } 
#  
# }
#  