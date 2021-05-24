library(rmarkdown)

# script code
source('../utils/utils.R')

# read data
data = read.csv("../data/crops.csv", header=TRUE)

# generate light home page
rmarkdown::render("index.Rmd",output_file='index.html') 

# generate dark home page
rmarkdown::render("index_dark.Rmd",output_file='index_dark.html') 

# generate website 
for (i in 1:nrow(data)) {
    # get row information
    row <- data[i,]
    
    # get crop name
    crop = row['name']
    
    #year = row['historical']
    year = 2019
    
    # title to pass to html
    title = gsub("_", " ", crop, ignore.case=TRUE)
    
    # get spatial coordinates
    lon = row['longitude']
    lat = row['latitude']
    
    # crop type 
    cdl = row['cdl']
    
    # format for api call
    geometry = paste0(lon, ',', lat)
    
    # make api request for forecast
    df = process(parse(request(geometry, 'forecast', year, cdl, shift=0), year))
    
    # add shift
    df_1 = process(parse(request(geometry, 'forecast', year, cdl, shift=1), year))
    df_2 = process(parse(request(geometry, 'forecast', year, cdl, shift=2), year))
    df_3 = process(parse(request(geometry, 'forecast', year, cdl, shift=3), year))
    
    # make api request for ground truth
    gt = process(parse(request(geometry, 'time_series', year, cdl, shift=0), year))
    
    # make api request for previous year
    pr = process(parse(request(geometry, 'time_series', year-1, cdl, shift=0), year-1))

    # generate dark report
    rmarkdown::render("dark_template.Rmd",output_file=paste0(tolower(crop), '_dark.html')) 
    
    # generate light report
    rmarkdown::render("light_template.Rmd",output_file=paste0(tolower(crop), '_light.html')) 
}