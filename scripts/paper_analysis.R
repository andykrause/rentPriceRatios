#*****************************************************************************************
#
#      Script to prepare figures for the PRMC paper (JHR edition)
#
#*****************************************************************************************

### Preliminary Commands -----------------------------------------------------------------

 ## Set paths

  data_path <- file.path(getwd(), 'data')
  text_path <- file.path(getwd(), 'papers', 'prmc')
  fig_path <- file.path(text_path, 'figures')
 
 ## Load libraries
  
  library(knitr)
  library(grid)
  library(ggplot2)
  library(maptools)
  library(rgeos)
  library(ggmap)
  library(RColorBrewer)
  library(plyr)
  library(dplyr)
  library(hexbin)

 ## Source functions
  
  invisible(sapply(file.path(getwd(), 'functions', 
                             list.files(file.path(getwd(), 'functions'))),
                   source, echo=FALSE))

 ## Load workspace
  
  load(file.path(data_path, 'rprWrkspce.RData'))

### Create Map ---------------------------------------------------------------------------
  
 ## Extract Poly boundaries
  
  # LGAs
  studyLGAs <- studyShapes$lga
  studyLGAs@data$ID <- rownames(studyLGAs@data)
  lgasGG <-  fortify(studyLGAs, region="ID")
  
  # Suburbs
  studySubs <- studyShapes$suburb
  studySubs@data$ID <- rownames(studySubs@data)
  subsGG <-  fortify(studySubs, region="ID")
  
 ## Extract Point (observation) data  
  
  map.data <- full$impute.data[,c('Property_Longitude', 'Property_Latitude',
                                  'PropertyType', 'transType')]
  map.data$transType <- as.character(map.data$transType)
  map.data$PropertyType <- as.character(map.data$PropertyType)
  
  map.data$transType[map.data$transType == 'rent'] <- "Rentals"
  map.data$transType[map.data$transType == 'sale'] <- "Sales"
  
  map.data$PropertyType[map.data$PropertyType == 'Unit'] <- "Apartments"
  map.data$PropertyType[map.data$PropertyType == 'House'] <- "Houses"
  
 ## Build Plot Object  
  
  loc.plot <- ggplot(data=map.data,
                     aes(x=Property_Longitude, y=Property_Latitude)) +
    geom_path(data=subsGG, aes(x=long, y=lat, group=id), color='gray80') +
    geom_path(data=lgasGG, aes(x=long, y=lat, group=id), color='gray50') +
    stat_binhex(bins=100, aes(alpha=(..count..) ^ (1 / 3), fill=PropertyType)) +
    scale_fill_manual(values=c('black', 'black'), guide=FALSE) +
    coord_cartesian(xlim=c(144.5, 145.6),
                    ylim=c(-38.2, -37.4)) +
    facet_grid(transType~PropertyType) +
    theme_bw() +
    theme(legend.position='none',
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 30),
          strip.text.y = element_text(size = 30))
  
 ## Save to JPG  
  
  jpeg(file.path(fig_path, 'melbMap.jpg'), 
       width = 1100, height = 900, quality=100)
      loc.plot
  dev.off()
 
### Index Plot (Figure 2) ----------------------------------------------------------------  
   
  # Extract the index values
  glob.index <- full$index.data$Global$Global
  
  # Convert to a data.frame
  index.df <- data.frame(type=c(rep('Houses', 40), rep('Apartments',40)),
                         tenure=rep(c(rep('Sales   ', 20), rep('Rentals        ', 20))),
                         index=c(glob.index$house.sale, glob.index$house.rent,
                                 glob.index$unit.sale, glob.index$unit.rent),
                         time=rep(1:20,4))
  
  # Build Plot
  index.plot <- ggplot(index.df,
                       aes(x=time, y=index, color=type, linetype=tenure)) +
    geom_line(size=1.8) +
    scale_colour_manual(values=c('darkorange', 'blue'), guide=FALSE)+
    scale_linetype_manual(values=c(3, 1), name='Tenure Type') +
    scale_x_continuous(labels=c(2011:2016),
                       breaks=c(seq(0, 20, 4))) +
    xlab("") +
    ylab("Index Value\n") +
    facet_wrap(~type) +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position='bottom',
          legend.key.width=unit(2, "cm"),
          legend.key=element_rect(fill='white', color='white'),
          strip.text.x = element_text(size = 18),
          axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=16))
  
  # Export to JPR
  jpeg(file.path(fig_path, 'index1.jpg'), width = 1000, height = 400, quality=100)
    index.plot
  dev.off()
  
  
### Global Comparison Plot (Figure 3) ----------------------------------------------------  
  
  # Set colors for plots
  unitCols <- colorRampPalette(brewer.pal(9, 'Oranges'))(100)[c(95, 50, 25)]
  houseCols <- c('navy', 'royalblue2', 'skyblue') #, 'gray50')
  
  # Set sizes and line types
  methSizes <- c(1, 1.25, 1.5)
  meth.type <- c('dotted', 'longdash', 'solid')
  
  # Data Prep
  glob.house <- full.geo.data$Global[full.geo.data$Global$type == 'house', ]
  glob.unit <- full.geo.data$Global[full.geo.data$Global$type == 'unit', ]
  glob.all <- rbind(glob.house, glob.unit)
  glob.all$type <- as.character(glob.all$type)
  glob.all$type[glob.all$type == 'house'] <- 'Houses'
  glob.all$type[glob.all$type == 'unit'] <- 'Apartments'
  glob.all$mt <- paste0(glob.all$method, glob.all$type)
  glob.all$sample <- "Full Sample"
  
  # Build Plot Object
  all.glob <- ggplot(glob.all,
                     aes(x=time, y=yield, group=method, color=type))+
    geom_line(size=1.1, aes(linetype=method)) +
    facet_wrap(~type) +
    scale_size_manual(values=methSizes) +
    scale_colour_manual(values=c('darkorange', 'blue'), guide=FALSE) +
    scale_linetype_manual(values=meth.type) +
    xlab("") +
    ylab("Rent-Price Ratio\n") +
    scale_x_continuous(breaks=seq(0, 20, 4),
                       labels=2011:2016) +
    scale_y_continuous(breaks=seq(.031, .049, .002),
                       labels=seq(.031, .049, .002)) +
    coord_cartesian(ylim=c(.03, .049))+
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position='bottom',
          legend.key.width=unit(2, "cm"),
          legend.key=element_rect(fill='white', color='white'),
          strip.text.x = element_text(size = 15),
          axis.title=element_text(size=16, face="bold"),
          axis.text=element_text(size=12),
          legend.text=element_text(size=16))
  
  # Export to JPG
  jpeg(file.path(fig_path, 'allglob.jpg'), width = 1000, height = 550, quality=100)
    all.glob
  dev.off()
  
### Global Matched Sample Plot (Figure 4) ------------------------------------------------  
  
  # Data Prep
  ms.glob.house <- ms.geo.data$Global[ms.geo.data$Global$type == 'house',]
  ms.glob.unit <- ms.geo.data$Global[ms.geo.data$Global$type == 'unit',]
  ms.glob.all <- rbind(ms.glob.house, ms.glob.unit)
  ms.glob.all$type <- as.character(ms.glob.all$type)
  ms.glob.all$type[ms.glob.all$type=='house'] <- 'Houses'
  ms.glob.all$type[ms.glob.all$type=='unit'] <- 'Apartments'
  ms.glob.all$mt <- paste0(ms.glob.all$method, ms.glob.all$type)
  ms.glob.all$sample <- 'Matched Sample'
  
  # Combine
  comb.glob <- rbind(glob.all, ms.glob.all)
  comp.df <- comb.glob
  comp.df[c(161:180),] <- comp.df[41:60, ]
  comp.df[c(221:240),] <- comp.df[101:120, ]
  comp.df$meth.plot <- as.character(comp.df$method)
  comp.df$meth.plot[41:60] <- comp.df$meth.plot[101:120] <- 'Index'
  comp.df$meth.plot[161:180] <- comp.df$meth.plot[221:240] <- 'Index'
  comp.df$plot <- as.character(comp.df$sample)
  comp.df$plot[41:60] <- comp.df$plot[101:120] <- 'Reference (Match)'
  comp.df$plot[161:180] <- comp.df$plot[221:240] <- 'Reference (Match)'
  match.copy <- c(41:60, 101:120, 161:180, 221:240)
  comp.df <- rbind(comp.df, comp.df[match.copy,])
  comp.df$meth.plot[match.copy] <- 'Impute'

  # Label
  comp.df$plot <- factor(comp.df$plot, levels=c('Full Sample', 'Matched Sample',
                                                'Reference (Match)'))
  comp.df$col <- ifelse(comp.df$type == 'Houses', 2, 1)
  comp.df$col[comp.df$plot=='Reference (Match)'] <- 3
  comp.df$meth.plot <- factor(comp.df$meth.plot, levels=c('Impute', 'Index'))
  
  # Build Plot Object
  comp.plot <- ggplot(comp.df,
                      aes(x=time, y=yield, color=as.factor(col), 
                          linetype=plot, size=plot)) +
    geom_line() +
    facet_grid(meth.plot~type) +
    scale_color_manual(values=c('darkorange', 'blue', 'gray80'), guide=FALSE) +
    scale_size_manual(values=c(1.7, 1.7, .9), guide=FALSE) +
    scale_linetype_manual(values=c(1, 2, 1),
                          labels=c('Regular Sample', 'Matched Sample',
                                   'Reference (Match)')) +
    xlab("") +
    ylab("Rent-Price Ratio\n") +
    scale_x_continuous(breaks=seq(0, 20, 4),
                       labels=2011:2016) +
    scale_y_continuous(breaks=seq(.031, .049, .003),
                       labels=seq(.031, .049, .003)) +
    coord_cartesian(ylim=c(.031, .049)) +
    guides(linetype=guide_legend(
      override.aes = list(size=c(1.5, 1.5, .5),color=c('black', 'black', 'gray80')))) +
    theme_bw() +
    theme(legend.title=element_blank(),
          legend.position='bottom',
          legend.key.width=unit(2.5, "cm"),
          legend.key=element_rect(fill='white', color='white'),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15),
          axis.title=element_text(size=16, face="bold"),
          axis.text=element_text(size=12),
          legend.text=element_text(size=16))
  
  # Export to JPG
  jpeg(file.path(fig_path, 'compplot.jpg'), width = 1000, height = 550, quality=100)
    comp.plot
  dev.off()
  
### House and Unit Differences (Figs 5 and 6) --------------------------------------------    
    
 ## Data Prep
    
    hd.data <- full.dif.data$houses
    ud.data <- full.dif.data$units
    
    hd.data <- hd.data[!is.na(hd.data$meth.dif), ]
    ud.data <- ud.data[!is.na(ud.data$meth.dif), ]
    
    hd.data$geo.level[hd.data$geo.level == 'lga'] <- 'LGA'
    hd.data$geo.level[hd.data$geo.level == 'suburb'] <- 'Suburb'
    ud.data$geo.level[ud.data$geo.level == 'lga'] <- 'LGA'
    ud.data$geo.level[ud.data$geo.level == 'suburb'] <- 'Suburb'
    ud.data$geo.level[ud.data$geo.level == 'Global'] <- 'Metro'
    hd.data$geo.level[hd.data$geo.level == 'Global'] <- 'Metro'
    
    hd.data$geo.level <- factor(hd.data$geo.level,
                                levels=c('Metro', 'LGA', 'Suburb'))
    ud.data$geo.level <- factor(ud.data$geo.level,
                                levels=c('Metro', 'LGA', 'Suburb'))
    
    hd.data$comp.method <- factor(hd.data$comp.method,
                                  levels=c('Index - Impute',
                                           'Index - Match',
                                           'Impute - Match'))
    ud.data$comp.method <- factor(ud.data$comp.method,
                                  levels=c('Index - Impute',
                                           'Index - Match',
                                           'Impute - Match'))
    hd.data$sample <- 'Full Sample'
    ud.data$sample <- 'Full Sample'
    
    hd.ms.data <- ms.dif.data$houses
    ud.ms.data <- ms.dif.data$units
    
    hd.ms.data <- hd.ms.data[!is.na(hd.ms.data$meth.dif), ]
    ud.ms.data <- ud.ms.data[!is.na(ud.ms.data$meth.dif), ]
    
    hd.ms.data$geo.level[hd.ms.data$geo.level == 'lga'] <- 'LGA'
    hd.ms.data$geo.level[hd.ms.data$geo.level == 'suburb'] <- 'Suburb'
    ud.ms.data$geo.level[ud.ms.data$geo.level == 'lga'] <- 'LGA'
    ud.ms.data$geo.level[ud.ms.data$geo.level == 'suburb'] <- 'Suburb'
    ud.ms.data$geo.level[ud.ms.data$geo.level == 'Global'] <- 'Metro'
    hd.ms.data$geo.level[hd.ms.data$geo.level == 'Global'] <- 'Metro'
    
    hd.ms.data$geo.level <- factor(hd.ms.data$geo.level,
                                   levels=c('Metro', 'LGA', 'Suburb'))
    ud.ms.data$geo.level <- factor(ud.ms.data$geo.level,
                                   levels=c('Metro', 'LGA', 'Suburb'))
    
    hd.ms.data$comp.method <- factor(hd.ms.data$comp.method,
                                     levels=c('Index - Impute',
                                              'Index - Match',
                                              'Impute - Match'))
    ud.ms.data$comp.method <- factor(ud.ms.data$comp.method,
                                     levels=c('Index - Impute',
                                              'Index - Match',
                                              'Impute - Match'))
    hd.ms.data$sample <- 'Matched Sample'
    ud.ms.data$sample <- 'Matched Sample'
    
  ## Combine data  
    
    house.dd <- rbind(hd.data, hd.ms.data)
    unit.dd <- rbind(ud.data, ud.ms.data)
    house.dd$CID <- paste0(house.dd$sample, substr(house.dd$geo.level,1,3),
                           house.dd$comp.method, '.', house.dd$time)
    house.calc <- tapply(house.dd$meth.dif, house.dd$CID, median)
    house.ddd <- data.frame(sample=c(rep('Full Sample', 180), rep('Matched Sample', 180)),
                            time=rep(c(1,10:19,2,20,3:9), 18),
                            meth.dif=house.calc,
                            geo.level=rep(c(rep('LGA', 60), rep('Metro', 60),
                                            rep('Suburb', 60)), 2),
                            comp.method=rep(c(rep("Impute - Match", 20),
                                              rep('Index - Impute', 20),
                                              rep('Index - Match', 20)), 3))
    house.ddd$geo.level <- factor(house.ddd$geo.level, levels=c('Metro', 'LGA', 'Suburb'))
    house.ddd$comp.method <- factor(house.ddd$comp.method,
                                    levels=c('Index - Impute', 'Index - Match',
                                             'Impute - Match'))
    
    unit.dd$CID <- paste0(unit.dd$sample, substr(unit.dd$geo.level,1,3),
                          unit.dd$comp.method, '.', unit.dd$time)
    unit.calc <- tapply(unit.dd$meth.dif, unit.dd$CID, median)
    unit.ddd <- data.frame(sample=c(rep('Full Sample', 180), rep('Matched Sample', 180)),
                           time=rep(c(1,10:19,2,20,3:9), 18),
                           meth.dif=unit.calc,
                           geo.level=rep(c(rep('LGA', 60), rep('Metro', 60),
                                           rep('Suburb', 60)), 2),
                           comp.method=rep(c(rep("Impute - Match", 20),
                                             rep('Index - Impute', 20),
                                             rep('Index - Match', 20)), 3))
    unit.ddd$geo.level <- factor(unit.ddd$geo.level, levels=c('Metro', 'LGA', 'Suburb'))
    unit.ddd$comp.method <- factor(unit.ddd$comp.method,
                                   levels=c('Index - Impute', 'Index - Match',
                                            'Impute - Match'))
  
  ## Build plot object  
    
   hd.plot <- ggplot(house.ddd,
                     aes(x=time, y=meth.dif, group=geo.level,
                         colour=geo.level, size=geo.level)) +
      #geom_line() +
      stat_smooth(se=FALSE)+
      xlab("") +
      ylab("Difference in RPR Estimate\n") +
      scale_colour_manual(values=c('navy', 'blue', 'lightblue'))+
      scale_size_manual(values=c(.7, 1.1, 1.7))+
      scale_y_continuous(breaks=seq(-.016, 0, .002)) +
      scale_x_continuous(breaks=seq(0, 20, 4),
                         labels=2011:2016) +
      facet_grid(sample~comp.method) +
      coord_cartesian(ylim=c(-.008, 0.002)) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(legend.title=element_blank(),
            legend.position='bottom',
            legend.key.width=unit(2.5, "cm"),
            legend.key=element_rect(fill='white', color='white'),
            strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 16),
            axis.title=element_text(size=16, face="bold"),
            axis.text=element_text(size=12),
            legend.text=element_text(size=16))
    
  # Export to JPG
   jpeg(file.path(fig_path, 'hdplot.jpg'), width = 1000, height = 500, quality=100)
     hd.plot
   dev.off()
   
  ## Create Plot 
  ud.plot <- ggplot(unit.ddd,
                        aes(x=time, y=meth.dif, group=geo.level,
                            colour=geo.level, size=geo.level)) +
      #geom_line() +
      stat_smooth(se=FALSE)+
      xlab("") +
      ylab("Difference in RPR Estimate\n") +
      scale_colour_manual(values=c('darkorange4', 'orange', 'gold'))+
      scale_size_manual(values=c(.7, 1.1, 1.7))+
      scale_y_continuous(breaks=seq(-.016, 0, .002)) +
      scale_x_continuous(breaks=seq(0, 20, 4),
                         labels=2011:2016) +
      facet_grid(sample~comp.method) +
      coord_cartesian(ylim=c(-.008, 0.001)) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(legend.title=element_blank(),
            legend.position='bottom',
            legend.key.width=unit(2.5, "cm"),
            legend.key=element_rect(fill='white', color='white'),
            strip.text.x = element_text(size = 16),
            strip.text.y = element_text(size = 16),
            axis.title=element_text(size=16, face="bold"),
            axis.text=element_text(size=12),
            legend.text=element_text(size=16))
    
      
 # Export to JPG
      
  jpeg(file.path(fig_path, 'udplot.jpg'), width = 1000, height = 500, quality=100)
       ud.plot
  dev.off()
      
#*****************************************************************************************
#*****************************************************************************************
  
  