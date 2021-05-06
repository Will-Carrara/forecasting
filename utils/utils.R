# requests
library(httr)

# opacity
library(ggplot2)

# api call
request = function (geometry, endpoint, year, crop_type=-1) {
    
    # dates for comparison
    start_date = paste0(year,'-01-01')
    end_date = paste0(year,'-12-31')
    
    # api server address
    server = 'http://127.0.0.1:5000/'
    
    # create query and make request
    response = GET(paste0(server,endpoint), 
                   query = list(api_key='ythsn82poy',
                                interval = 'daily',
                                geometry = geometry,
                                crop_type = crop_type,
                                interpolate = TRUE,
                                aggregation = 'mean',
                                moving_average = 0,
                                output_format = 'csv',
                                start_date = start_date,
                                end_date = end_date,
                                reference_year = '2018')
    )
    
    # obtain content of api return 
    content = as.data.frame(content(response, as="parsed", type="text/csv"))
    
    return (content)
}

# parse api return
parse = function (content, year) {
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

            if (grepl(paste0(year,'-'), obs[[i]][2], fixed=TRUE)) {
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
    
    # obtain request year
    year = substr(df$time[1], 1, 4)
 
    # time handling
    df$time = round(abs(as.numeric(difftime(paste0(year,"-01-01"), df$time, units="days"))),1)
    
    return (df)
}

# beautiful plotting 
visualize = function (forecast, truth, var, crop, year, dark=TRUE) {
    
    # set margins
    par(mar=c(8,3,3.5,1), oma=c(0,4,0,0))
    
    # set variable for empty plot 
    x = c(0,364)
    
    # x-axis for plotting
    months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct", "Nov","Dec")
    
    # forecast color 
    t_col = 'tomato1'
    # cutoff line
    c_col = '#FFA9FC'
    # linear fir color 
    l_col = '#FFC101'
    
    # set parameters for variables
    # normalized difference vegetation index
    if (var == 'NDVI') {
        # y bounds
        y = c(0,1)    
        # line color 
        color = '#42E24E'
    # reference evapotranspiration
    } else if (var == 'Eto') {
        # y bounds
        y = c(0,10)
        # line color 
        color = 'steelblue1'
    # basil crop coefficient
    } else if (var == 'Kcb') {
        # y bounds
        y = c(0,1.2)
        # line color 
        color = '#32FFFF'
    }
    
    # set parameters for dark plots
    if (dark) {
        # background
        primary = '#1D2329'
        secondary = '#768390'
        
        # text
        main = '#CDD9E5'
        lesser = '#768390'
        
        # set background color
        par(bg=primary)
        
        # make empty plot
        plot(x~y, xlim=x, ylim=y, xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col.axis=lesser)
        
        # overlay plot with solid background color
        rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=primary)
        
        # add grid after background
        grid(col=secondary)
        
    } else {
        # background
        primary = 'white'
        secondary = 'black'
        
        # text
        main = 'black'
        lesser = 'black'
        
        # make empty plot
        plot(x~y, xlim=x, ylim=y, xlab="", ylab="", xaxt="n", las=1, cex.axis=1.8, col=primary)
        grid()
    }
    # separate forecasting data
    #forecast = df[c(df$id == "FORECASTED", TRUE)[-1], ]
    #truth = df[df$id != "FORECASTED", ]
    
    # add graph labels 
    title = gsub("_", " ", crop, ignore.case=TRUE)
    title(main=paste0(var, " Forecasting ", title," (", year, ")"), col.main=main, cex.main=2.1, line=2)
    title(xlab="Month", col.lab=main, cex.lab=2, line=4)
    title(ylab=var, col.lab=main, cex.lab=2, line=2, outer=TRUE, adj=.56)
    
    # reformat x-axis
    xtick<-seq(0, 364, by=30.5)
    axis(side=1, at=xtick, labels=FALSE)
    text(x=xtick, par("usr")[3], labels=months, pos=1, cex=1.8, offset=1.1, col=lesser, xpd=TRUE)
    
    # add line for cutoff
    segments(x0=forecast[c(forecast$id == "FORECASTED", TRUE), 'time'][1],y0=-2,
             x1=forecast[c(forecast$id == "FORECASTED", TRUE), 'time'][1], y1=15, col=c_col, lty=2, lwd=3.5)
    
    # add data for ground truth 
    lines(truth[,tolower(var)]~time, data=truth, col=alpha(t_col, .6), type='l', lwd=3.5)
    
    # highlight forecast 
    lines(forecast[,tolower(var)]~time, data=forecast, col=color, type='l', lwd=3.5)
    
    # create regression model 
    fit <- lm(truth[,tolower(var)]~poly(1:length(truth$time), 8, raw=TRUE), truth) 
    
    # add regression line  
    lines(fit$fitted.values~truth$time, col=l_col, lty=2, lwd=3.5)
    
    # add legend
    legend("topright", inset=c(.008,.02), legend=c("Observed", "Predicted", "Trend"), 
           col=c(t_col, color, l_col), pch=16, xpd=TRUE, bty="n", cex=1.8, text.col=lesser)
}