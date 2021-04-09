# requests
library(httr)

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
