# requests
library(httr)

# x-axis for plotting
months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct", "Nov","Dec")

# api call
request = function (geometry, crop_type=-1) { 
    # api server address
    server = 'http://127.0.0.1:5000/'
    
    # api endpoint
    endpoint = 'forecast'
    
    # create query and make request
    response = GET(paste0(server,endpoint), 
                   query = list(api_key='ythsn82poy',
                                interval = 'daily',
                                geometry = geometry,
                                crop_type = crop_type,
                                interpolate = TRUE,
                                aggregation = 'mean',
                                moving_average = 0,
                                output_format = 'csv')
    )
    
    # obtain content of api return 
    content = as.data.frame(content(response, as="parsed", type="text/csv"))
    
    return (content)
}

# parse api return
parse = function (content) {
    # empty container 
    id = c()      # satellite identifier
    time = c()    # observation date
    ndvi = c()    # normalized difference vegetation index
    fc = c()      # fractional cover
    eto = c()     # reference evapotranspiration
    kcb = c()     # basil crop coefficient
    etc= c()      # sims evapotranspiration
    cdl = c()     # cropland data layers code
    
    # parse return and create variable storage containers
    for (row in content) {
        # split row into vector 
        obs = strsplit(row, "\\s+")
        
        # iterate row and parse
        for (i in 1:nrow(content)) {
            # check if observation is interpolated
            if (grepl('2021-', obs[[i]][2], fixed=TRUE)) {
                id = c(id, 'INTERPOLATED')
                time = c(time, toString(obs[[i]][3-1]))
                ndvi = c(ndvi, as.double(obs[[i]][4-1]))
                fc = c(fc, as.double(obs[[i]][5-1]))
                eto = c(eto, as.double(obs[[i]][6-1]))
                kcb = c(kcb, as.double(obs[[i]][7-1]))
                etc = c(etc, as.double(obs[[i]][8-1]))
                cdl = c(cdl, as.integer(obs[[i]][9-1]))
                # cast values correctly
            } else {
                id = c(id, toString(obs[[i]][2]))
                time = c(time, toString(obs[[i]][3]))
                ndvi = c(ndvi, as.double(obs[[i]][4]))
                fc = c(fc, as.double(obs[[i]][5]))
                eto = c(eto, as.double(obs[[i]][6]))
                kcb = c(kcb, as.double(obs[[i]][7]))
                etc = c(etc, as.double(obs[[i]][8]))
                cdl = c(cdl, as.integer(obs[[i]][9]))  
            }
        }
    }
    
    # add to usable container
    df = data.frame(id, time, ndvi, fc, eto, kcb, etc, cdl)
    
    return (df)
}


# post-process
process = function (df) {
    # remove any nan values
    df = na.omit(df)
    
    # remove any negative values
    df$ndvi = ifelse(df$ndvi <= 0, NA, df$ndvi)
    
    # time handling
    df$time = round(abs(as.numeric(difftime(paste0("2021-01-01"), df$time, units="days"))),1)
    
    return (df)
}


# beautiful ndvi plotting 
visualize_ndvi = function (df, crop, dark=TRUE) {
    
    # empty plot for ndvi
    x = c(0,1)
    y = c(0,364)
    
    if (dark) {
        # background
        primary = '#1D2329'
        secondary = '#768390'
        
        # text
        main = '#CDD9E5'
        lesser = '#768390'
        
        # set background color and margins
        par(bg='#1D2329', mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,1), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col.axis=lesser)
        # overlay plot with solid background color
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=primary)
        # add grid
        grid(col=secondary)
        
        # add graph labels 
        title(main=paste0("NDVI Forecasting ",crop," (2021)"), col.main=main, cex.main=2.1, line=2)
        title(xlab="Month", col.lab=main, cex.lab=2, line=4)
        title(ylab="NDVI", col.lab=main, cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, col=lesser, xpd=TRUE)
        
        # add data for ground truth 
        lines(ndvi~time, data=df[df$id != "PREDICTED", ], col="#42E24E", type='l', lwd=3.5)
        # highlight forecast 
        lines(ndvi~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(ndvi~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.02), legend=c("Observed", "Predicted", "Trend"), 
               col=c("#42E24E", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8, text.col=lesser)
        
    } else {
        
        # set background color and margins
        par(mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,1), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8)
        # add grid
        grid()
        
        # add graph labels 
        title(main=paste0("NDVI Forecasting ",crop," (2021)"), cex.main=2.1, line=2)
        title(xlab="Month", cex.lab=2, line=4)
        title(ylab="NDVI", cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, xpd=TRUE)
        
        # add data for ground truth 
        lines(ndvi~time, data=df[df$id != "PREDICTED", ], col="#42E24E", type='l', lwd=3.5)
        # highlight forecast 
        lines(ndvi~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(ndvi~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.02), legend=c("Observed", "Predicted", "Trend"), 
               col=c("#42E24E", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8)
    }
}


# beautiful eto plotting 
visualize_eto = function (df, crop, dark=TRUE) {
    
    # empty plot for eto
    x = c(0,10)
    y = c(0,364)
    
    if (dark) {
        # background
        primary = '#1D2329'
        secondary = '#768390'
        
        # text
        main = '#CDD9E5'
        lesser = '#768390'
        
        # set background color and margins
        par(bg='#1D2329', mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,10), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col.axis=lesser)
        # overlay plot with solid background color
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=primary)
        # add grid
        grid(col=secondary)
        
        # add graph labels 
        title(main=paste0("Eto Forecasting ",crop," (2021)"), col.main=main, cex.main=2.1, line=2)
        title(xlab="Month", col.lab=main, cex.lab=2, line=4)
        title(ylab="Eto (mm)", col.lab=main, cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, col=lesser, xpd=TRUE)
        
        # add data for ground truth 
        lines(eto~time, data=df[df$id != "PREDICTED", ], col="steelblue1", type='l', lwd=3.5)
        # highlight forecast 
        lines(eto~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(eto~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.02), legend=c("Observed", "Predicted", "Trend"), 
               col=c("steelblue1", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8, text.col=lesser)
        
    } else {
        
        # set background color and margins
        par(mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,10), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8)
        # add grid
        grid()
        
        # add graph labels 
        title(main=paste0("Eto Forecasting ",crop," (2021)"), cex.main=2.1, line=2)
        title(xlab="Month", cex.lab=2, line=4)
        title(ylab="Eto (mm)", cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, xpd=TRUE)
        
        # add data for ground truth 
        lines(eto~time, data=df[df$id != "PREDICTED", ], col="steelblue1", type='l', lwd=3.5)
        # highlight forecast 
        lines(eto~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(eto~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.02), legend=c("Observed", "Predicted", "Trend"), 
               col=c("steelblue1", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8)
    }
}

# beautiful kcb plotting 
visualize_kcb = function (df, crop, dark=TRUE) {
    
    # empty plot for kcb
    x = c(0,1.2)
    y = c(0,364)
    
    if (dark) {
        # background
        primary = '#1D2329'
        secondary = '#768390'
        
        # text
        main = '#CDD9E5'
        lesser = '#768390'
        
        # set background color and margins
        par(bg='#1D2329', mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,1.2), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col.axis=lesser)
        # overlay plot with solid background color
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=primary)
        # add grid
        grid(col=secondary)
        
        # add graph labels 
        title(main=paste0("Kcb Forecasting ",crop," (2021)"), col.main=main, cex.main=2.1, line=2)
        title(xlab="Month", col.lab=main, cex.lab=2, line=4)
        title(ylab="Kcb (mm)", col.lab=main, cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, col=lesser, xpd=TRUE)
        
        # add data for ground truth 
        lines(kcb~time, data=df[df$id != "PREDICTED", ], col="#A17FFD", type='l', lwd=3.5)
        # highlight forecast 
        lines(kcb~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(kcb~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.01), legend=c("Observed", "Predicted", "Trend"), 
               col=c("#A17FFD", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8, text.col=lesser)
        
    } else {
        
        # set background color and margins
        par(mar=c(8,3,3.5,1), oma=c(0,4,0,0))
        # make empty plot with labels
        plot(x,y, xlim=c(0,364), ylim=c(0,1.2), xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8)
        # add grid
        grid()
        
        # add graph labels 
        title(main=paste0("Kcb Forecasting ",crop," (2021)"), cex.main=2.1, line=2)
        title(xlab="Month", cex.lab=2, line=4)
        title(ylab="Kcb (mm)", cex.lab=2, line=2, outer=TRUE, adj=.56)
        
        # reformat x-axis
        xtick<-seq(0, 364, by=31)
        axis(side=1, at=xtick, labels=FALSE)
        text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, xpd=TRUE)
        
        # add data for ground truth 
        lines(kcb~time, data=df[df$id != "PREDICTED", ], col="#A17FFD", type='l', lwd=3.5)
        # highlight forecast 
        lines(kcb~time, data=df[c(df$id == "PREDICTED", TRUE)[-1], ], col="tomato1", type='l', lwd=3.5)
        
        # create regression model 
        fit <- lm(kcb~poly(1:length(df$time), 8, raw=TRUE), df) 
        
        # add regression line  
        lines(fit$fitted.values~df$time, col="#FFC101", lty=2, lwd=3.5)
        
        # add legend
        legend("topright", inset=c(.008,.01), legend=c("Observed", "Predicted", "Trend"), 
               col=c("#A17FFD", "tomato1", "#FFC101"), pch=16, xpd=TRUE, bty="n", cex=1.8)
    }
}