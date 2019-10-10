
library(shiny)
library(shinythemes)
library(DT)
library(data.table)
library(magrittr)
library(tidyverse)
library(plotly)
library(leaflet)
library(Rcpp)
library(varhandle)
library(gpclib)
library(shinyWidgets)
library(formattable)
library(RColorBrewer)
library(tigris)
library(rgdal)
library(maptools)
library(rgeos)
library(bit64)

gpclibPermit()

#data_raw_1 <- fread('../data/Raw Data 1.csv')
#data_raw_2 <- fread('../data/Raw Data 2.csv')
data_raw_1 <- fread('Raw Data 1.csv')
data_raw_2 <- fread('Raw Data 2.csv')
data_raw <- rbind(data_raw_1,data_raw_2)

df<- data_raw
df <- df[,c('CAMIS','DBA','BORO','BUILDING','STREET','ZIPCODE','PHONE','CUISINE DESCRIPTION','GRADE','INSPECTION DATE','Longitude','Latitude')]

#nbhd <- fread('../data/nbhd.csv')
nbhd <- fread('nbhd.csv')
allNbhd <- nbhd$NEIGHBORHOOD
##############Cleaning the raw data######################
#getting rid of data where BORO = 0
data_raw %<>% filter(`BORO` != '0')

df %<>% filter(`BORO` != '0')
df %<>% filter(`ZIPCODE` != 'N/A')

#########################################################

vio_map <- unique(data_raw[,c('VIOLATION DESCRIPTION','VIOLATION CODE','CRITICAL FLAG')])

##load zipcode and map data
# From https://data.cityofnewyork.us/Business/Zip-Code-Boundaries/i8iw-xf4u/data
NYCzipcodes <- readOGR("ZIP_CODE_040114.shp",
                       #layer = "ZIP_CODE", 
                       verbose = FALSE)
# selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% nbhd$ZIPCODE)
# selZip <- unionSpatialPolygons(selZip,nbhd$NEIGHBORHOOD[match(selZip$ZIPCODE,nbhd$ZIPCODE)])




########filter function for convenience##################
filterByCuisineBorough <- function(dataRaw, cuisine,borough,critFlag){
  if ('All' %in% cuisine){
    cuisineFilter <- unique(dataRaw$`CUISINE DESCRIPTION`)
  }else{
    cuisineFilter <- cuisine
  }
  if ('All' %in% borough){
    boroFilter <- unique(dataRaw$BORO)
  }else{
    boroFilter <- borough
  }
  
  dataRaw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% critFlag,
                                  `VIOLATION DESCRIPTION` != "")
}

filterByCuisineBorough2 <- function(dataRaw, cuisine,borough){
  if ('All' %in% cuisine){
    cuisineFilter <- unique(dataRaw$`CUISINE DESCRIPTION`)
  }else{
    cuisineFilter <- cuisine
  }
  if ('All' %in% borough){
    boroFilter <- unique(dataRaw$BORO)
  }else{
    boroFilter <- borough
  }
  
  dataRaw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter)
}
########f################################################
##### Making a Zipcode List for map Boundaries ##########
## data from https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm# 


bronx_zip.df=c(10453, 10457, 10460,
               10458, 10467, 10468,
               10451, 10452, 10456,
               10454, 10455, 10459, 10474,
               10463, 10471,
               10466, 10469, 10470, 10475,
               10461, 10462,10464, 10465, 10472, 10473)



brook_zip.df =c(11212, 11213, 11216, 11233, 11238,
                11209, 11214, 11228,
                11204, 11218, 11219, 11230,
                11234, 11236, 11239,
                11223, 11224, 11229, 11235,
                11201, 11205, 11215, 11217, 11231,
                11203, 11210, 11225, 11226,
                11207, 11208,
                11211, 11222,
                11220, 11232,
                11206, 11221, 11237)


man_zip.df =c(10026, 10027, 10030, 10037, 10039,
              10001, 10011, 10018, 10019, 10020,
              10036, 10029, 10035,
              10010, 10016, 10017, 10022,
              10012, 10013, 10014,
              10004, 10005, 10006, 10007, 10038, 10280,
              10002, 10003, 10009,
              10021, 10028, 10044, 10065, 10075, 10128,
              10023, 10031, 10032, 10033, 10034, 10040)


queens_zip.df =c(11361, 11362, 11363, 11364,
                 11354, 11355, 11356, 11357, 11358, 11359, 11360,
                 11365, 11366, 11367,
                 11412, 11423, 11432, 11433, 11434, 11435, 11436,
                 11101, 11102, 11103, 11104, 11105, 11106,
                 11374, 11375, 11379, 11385,
                 11691, 11692, 11693, 11694, 11695, 11697,
                 11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429,
                 11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421,
                 11368, 11369, 11370, 11372, 11373, 11377, 11378)




staten_zip.df  =c(10302, 10303, 10310,
                  10306, 10307, 10308, 10309, 10312,
                  10301, 10304, 10305,
                  10314)



#Lists of all the zipcode that belong to each borough
bronx_zip.df<-as.data.frame(bronx_zip.df)
brook_zip.df<-as.data.frame(brook_zip.df)
man_zip.df<-as.data.frame(man_zip.df)
queens_zip.df<-as.data.frame(queens_zip.df)
staten_zip.df<-as.data.frame(staten_zip.df)

#give the column the name region(ZIPCODE) to make it easier to merge later
names(bronx_zip.df)<- "region"
names(brook_zip.df)<- "region"
names(man_zip.df)<- "region"
names(queens_zip.df)<- "region"
names(staten_zip.df)<- "region"

########################################################

# input <- list('cuisine1' = 'French','boro1' = 'Manhattan','cuisine2' = 'Chinese','boro2' = 'Manhattan','critFlag' = 'Y','variable' = 'A', 'speech1' = 'Chinese','speech2' = 'Manhattan','cuisinemap' = 'All','boromap' = 'All','nbhd'='All')

shinyServer(function(input, output,session) {
  
  ##top violation barchart1
  # output$top_vio_bar1 <- renderPlotly({
  # 
  #   if ('All' %in% input$cuisine1){
  #     cuisineFilter <- unique(data_raw$`CUISINE DESCRIPTION`)
  #   }else{
  #     cuisineFilter <- input$cuisine1
  #   }
  #   if ('All' %in% input$boro1){
  #     boroFilter <- unique(data_raw$BORO)
  #   }else{
  #     boroFilter <- input$boro1
  #   }
  # 
  # 
  #   data_sub <- data_raw %>% filter(`CUISINE DESCRIPTION` %in% cuisineFilter,BORO %in% boroFilter,`CRITICAL FLAG` %in% input$critFlag,
  #                                   `VIOLATION DESCRIPTION` != "")
  # 
  #   vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
  #     arrange(desc(`# of Cases`))
  #   vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
  #                         ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
  #     select(Code, Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)
  # 
  #   x_names <- vio_count$Code[1:input$slider1]
  #   x_names <- factor(x_names,levels = rev(x_names))
  #   plot_ly(
  #     x = vio_count$`# of Cases`[1:input$slider1],
  #     y = x_names,
  #     text =  substr(paste0(vio_count$`# of Cases`[1:input$slider1], ", ",vio_count$`Description`[1:input$slider1]),1,150),
  #     hoverinfo = 'text',
  #     name = "Bigram",
  #     type = "bar",
  #     orientation = 'h',
  #     marker = list(color = 'rgb(158,202,225)',
  #                   line = list(color = 'rgb(8,48,107)',
  #                               width = 1.5))) %>% 
  #   layout(paper_bgcolor = 'azure1',
  #          plot_bgcolor = 'rgba(245, 246, 249, 1)',
  #          showlegend = FALSE)
  #   
  # })
  
  ##top violation datatable1
  output$top_vio_table1 <-  renderDataTable({
  
    data_sub <- filterByCuisineBorough(data_raw,input$cuisine1,input$boro1,input$critFlag)
    
    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)

    
    formattable::as.datatable(formattable(vio_count[1:input$slider1,], align = c("l",rep("c", ncol(vio_count) - 1)),
                                          list(`Description` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                                               `# of Cases` = color_bar("#FA614B",),
                                               `Critical` = formatter("span", style = ~ style(color = "black", font.weight = "bold")))),
                              options = list(columnDefs = list(list(className = 'dt-center', targets = 2:3))),
                              caption = htmltools::tags$caption(htmltools::tags$span("Top Violations for "),
                                                                htmltools::tags$span(paste(paste(input$cuisine1,collapse=', '), 'Cuisines'), style="color:red;"),
                                                                htmltools::tags$span("In Boroughs: "),
                                                                htmltools::tags$span(paste(paste(input$boro1,collapse = ', ')), style="color:green;"))) %>%
      formatStyle(1:ncol(vio_count),color = 'black') %>% 
      formatStyle(2:ncol(vio_count),border = '1px solid #ddd')
  })
  
  ##top violation datatable2
  output$top_vio_table2 <-  renderDataTable({
    data_sub <- filterByCuisineBorough(data_raw,input$cuisine2,input$boro2,input$critFlag)
    
    vio_count <- data_sub %>% group_by(`VIOLATION DESCRIPTION`) %>% summarise(`# of Cases` = n()) %>%
      arrange(desc(`# of Cases`))
    vio_count %<>% mutate(Code = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'VIOLATION CODE']
                          ,Critical = vio_map[match(vio_count$`VIOLATION DESCRIPTION`, vio_map$`VIOLATION DESCRIPTION`),'CRITICAL FLAG']) %>%
      select(Description = `VIOLATION DESCRIPTION`, `# of Cases`,`Critical`)
    
    
    formattable::as.datatable(formattable(vio_count[1:input$slider1,], align = c("l",rep("c", ncol(vio_count) - 1)),
                                          list(`Description` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                                               `# of Cases` = color_bar("#FA614B",),
                                               `Critical` = formatter("span", style = ~ style(color = "black", font.weight = "bold")))),
                              options = list(columnDefs = list(list(className = 'dt-center', targets = 2:3))),
                              caption = htmltools::tags$caption(htmltools::tags$span("Top Violations for "),
                                      htmltools::tags$span(paste(paste(input$cuisine2,collapse=', '), 'Cuisines'), style="color:red;"),
                                      htmltools::tags$span("In Boroughs: "),
                                      htmltools::tags$span(paste(paste(input$boro2,collapse = ', ')), style="color:green;"))) %>%
      formatStyle(1:ncol(vio_count),color = 'black') %>% 
      formatStyle(2:ncol(vio_count),border = '1px solid #ddd')
    

  })
  
  
  ###histogram for inspection scores
  output$score_hist <- renderPlot({
    
    data_sub <- data_raw %>% mutate(SCORE = replace(SCORE,ACTION == "No violations were recorded at the time of this inspection.",0)) %>% filter(!is.na(SCORE)) 
    data_sub_1 <- filterByCuisineBorough(data_sub,input$cuisine1,input$boro1,input$critFlag)
    data_sub_2 <- filterByCuisineBorough(data_sub,input$cuisine2,input$boro2,input$critFlag)
    
    

    
    score_1 <- data_sub_1 %>% group_by(`CAMIS`,`INSPECTION DATE`) %>% summarise(Score = mean(SCORE)) %>% group_by(`CAMIS`) %>% summarise(Score = mean(Score)) %>% transmute(Category = paste0(paste(input$cuisine1,collapse = ', '),' cuisines in ',paste(input$boro1,collapse = ', ')),Score = Score) 
    score_2 <- data_sub_2 %>% group_by(`CAMIS`,`INSPECTION DATE`) %>% summarise(Score = mean(SCORE)) %>% group_by(`CAMIS`) %>% summarise(Score = mean(Score)) %>% transmute(Category = paste0(paste(input$cuisine2,collapse = ', '),' cuisines in ',paste(input$boro2,collapse = ', ')),Score = Score) 
    
    hist_data <- rbind(score_1,score_2)
    hist_data$Category <- factor(hist_data$Category,levels =  unique(hist_data$Category))
    mean_data <- hist_data %>% group_by(Category) %>% summarise(Mean = mean(Score))
    
    # hist_plot <- ggplot(hist_data, aes(x=Score, fill=Category, color=Category)) +
    #   geom_histogram(position="identity",binwidth = 1,alpha = 0.5) +
    #   geom_vline(data=mean_data, aes(xintercept=Mean, color=Category),
    #              linetype="dashed",size = 1.1,show.legend = FALSE) +
    #   theme(legend.title=element_blank(),legend.position = 'top',axis.title.x = element_text(face = 'bold'),axis.title.y = element_text(face = 'bold'))
    # 
    # hist_plot + geom_text(aes(x = mean_data$Mean[1]+1.5,y = diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9,label = round(mean_data$Mean[1],1)),show.legend = FALSE,colour = 'black') +
    #   geom_text(aes(x = mean_data$Mean[2]+1.5,y = diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9,label = round(mean_data$Mean[2],1)),show.legend = FALSE,colour = 'black')
    # 
    hist_plot<-ggplot(hist_data, aes(x=Score))+geom_histogram(aes(x = Score,fill = Category), binwidth = 1)+facet_grid(Category ~ .,scales = 'free_y') +
      geom_vline(data=mean_data, aes(xintercept=Mean,color = Category),linetype="dashed",size = 1,show.legend = FALSE) + scale_fill_brewer(palette = "Pastel1", name = "Category") +
      theme(legend.title=element_blank(),legend.position = 'top',axis.title.x = element_text(face = 'bold'),axis.title.y = element_text(face = 'bold'))
    
    
    if(input$cuisine1==input$cuisine2 & input$boro1==input$boro2){
      hist_yrange <- diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range)*0.9
    }else{
      hist_yrange <- c(diff(ggplot_build(hist_plot)$layout$panel_params[[1]]$y.range),diff(ggplot_build(hist_plot)$layout$panel_params[[2]]$y.range))*0.9
    }
    
    hist_plot + geom_text(data = mean_data,aes(x = Mean+2,y =hist_yrange ,label = round(mean_data$Mean,1),color = Category),show.legend = FALSE)
      
  })
  
  
  ##pie chart for grades
  output$grade_pie <- renderPlotly({
    
    data_sub <- data_raw %>% filter(!GRADE %in% c("","G")) ##filtering out empty grades and G (we do not know what G means, and it has less than 5 observations anyway)
    data_sub_1 <- filterByCuisineBorough(data_sub,input$cuisine1,input$boro1,input$critFlag)
    data_sub_2 <- filterByCuisineBorough(data_sub,input$cuisine2,input$boro2,input$critFlag)
    
    
    ##get the latest(most recent) restaurant grade for each filter
    grade_1 <- data_sub_1 %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>% slice(n()) %>% transmute(Category = paste0(paste(input$cuisine1,collapse = ', '),' cuisines in ',paste(input$boro1,collapse = ', ')),Grade = GRADE) 
    grade_2 <- data_sub_2 %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>% slice(n()) %>% transmute(Category = paste0(paste(input$cuisine2,collapse = ', '),' cuisines in ',paste(input$boro2,collapse = ', ')),Grade = GRADE) 

    
    pie_data <- rbind(grade_1,grade_2)
    pie_data$Category <- factor(pie_data$Category,levels =  unique(pie_data$Category))
    
    
    plot_ly(textposition = 'inside',textinfo = 'label+percent',marker = list(colors = brewer.pal(8,"Set2"),line = list(color = '#FFFFFF', width = 1)),type = 'pie') %>%
      add_pie(data = count(grade_1[,-1], Grade), labels = ~Grade, values = ~n,
              name = grade_1$Category[1], domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
      add_pie(data = count(grade_2[,-1], Grade,sort = TRUE), labels = ~Grade, values = ~n,
              name = grade_2$Category[1], domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
      layout(paper_bgcolor='#E2E2E2',legend = list(
        font = list(
          family = "sans-serif",
          size = 12,
          color = "#000"),
        bgcolor = "#gray",
        bordercolor = "black",
        borderwidth = 2),annotations = list(text = sprintf("<b>%s</b>", paste(grade_1$Category[1],paste(rep(" ",90),collapse = ''),grade_2$Category[1])),  x = 0.5, y = 1,showarrow=FALSE))
    
  })
  
  output$map_data_table <- renderDataTable({
    validate(
      need(input$cuisinemap,"Please Select Cuisine"),
      need(input$boromap, "Please Select Borough")
    )
       
    #get unique restaurants and get the most current date's row for each restraunt
    data_sub <- filterByCuisineBorough2(data_raw,input$cuisinemap,input$boromap)
    
    unique_Restaurant <- data_sub %>% filter(!is.na(SCORE),SCORE != "",ZIPCODE %in% as.character(nbhd$ZIPCODE)) %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>%
      slice(n())
    
    unique_Restaurant$NEIGHBORHOOD <- nbhd$NEIGHBORHOOD[match(unique_Restaurant$ZIPCODE,as.character(nbhd$ZIPCODE))]
    
    count.df<- unique_Restaurant%>%
      group_by(NEIGHBORHOOD)%>%
      summarise(
        value=signif(mean(SCORE), digits = 4),
        count = n()
      )
    colnames(count.df) <- c('Neighborhood','Average Score','# of Restaurants')
    datatable(count.df) %>% formatStyle(1:ncol(count.df),color = 'black')
  })
  
  
  output$nycmap <- renderLeaflet({
    validate(
      need(input$cuisinemap,"Please Select Cuisine"),
      need(input$boromap, "Please Select Borough")
    )
    
    #get unique restaurants and get the most current date's row for each restraunt
    data_sub <- filterByCuisineBorough2(data_raw,input$cuisinemap,input$boromap)
    
    unique_Restaurant <- data_sub %>% filter(!is.na(SCORE),SCORE != "",ZIPCODE %in% as.character(nbhd$ZIPCODE)) %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>%
      slice(n())
    
    unique_Restaurant$NEIGHBORHOOD <- nbhd$NEIGHBORHOOD[match(unique_Restaurant$ZIPCODE,as.character(nbhd$ZIPCODE))]
    
    count.df<- unique_Restaurant%>%
      group_by(NEIGHBORHOOD)%>%
      summarise(
        value=signif(mean(SCORE), digits = 4),
        count = n()
      )
   

    #prepare spatial database
    selZip <- subset(NYCzipcodes, NYCzipcodes$ZIPCODE %in% as.numeric(unique(unique_Restaurant$ZIPCODE)))
    selZip <- unionSpatialPolygons(selZip,nbhd$NEIGHBORHOOD[match(selZip$ZIPCODE,nbhd$ZIPCODE)])
    
    # ----- Transform to EPSG 4326 - WGS84 (required)
    subdat<-spTransform(selZip, CRS("+init=epsg:4326"))
    
    
    subdat_data <- data.frame(NEIGHBORHOOD = names(subdat),value = count.df$value[match(names(subdat),count.df$NEIGHBORHOOD)],
                              count = count.df$count[match(names(subdat),count.df$NEIGHBORHOOD)])
    rownames(subdat_data) <- names(subdat)
    
    # ----- to write to geojson we need a SpatialPolygonsDataFrame
    subdat <- SpatialPolygonsDataFrame(subdat,data = subdat_data)
    popup1 <- paste0('<strong>Neighborhood: </strong><br>', subdat@data$NEIGHBORHOOD, 
                    '<br><strong>Score: </strong><br>', subdat@data$value,
                    '<br><strong># of Restaurants: </strong><br>', subdat@data$count)
    
    
    # ----- set uo color pallette https://rstudio.github.io/leaflet/colors.html
    # Create a continuous palette function
    pal <- colorNumeric(
      palette = "Reds",
      domain = subdat$value
    )
    
    leaflet(subdat) %>%
      addProviderTiles("CartoDB.Positron")%>%
      addPolygons(
        stroke = T, weight=1,
        fillOpacity = 0.5,
        color = ~pal(value),
        popup = popup1
      ) %>%
      addLegend("bottomright", pal = pal, values = ~value,
                title = "Average Score By Neighborhood",
                opacity = 1
      )
  })
  
  output$NYC_Restaurants <- renderDataTable({

    #get unique restaurants and get the most current date's row for each restraunt
    
    data_sub <- filterByCuisineBorough2(df,input$speech1,input$speech2)
    data_sub$Neighborhood <- nbhd$NEIGHBORHOOD[match(as.numeric(data_sub$ZIPCODE),nbhd$ZIPCODE)]
    
    if('All' %in% input$nbhd){
      nbFilter <- nbhd$NEIGHBORHOOD
    }else{
      nbFilter <- input$nbhd
    }
    
    df <- data_sub %>% filter(ZIPCODE %in% as.character(nbhd$ZIPCODE),GRADE %in% input$variable,Neighborhood %in% nbFilter) %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>%
      slice(n())
    df$Address <- paste(df$BUILDING,df$STREET)
    df <- df[,c('DBA','BORO','Address','PHONE','CUISINE DESCRIPTION','GRADE','Neighborhood')]
    colnames(df) <- c('Name','Borough', 'Address','Phone','Cuisine Type','Grade','Neighborhood')    
    
    datatable(df) %>% formatStyle(1:ncol(df),color='white',target='row',backgroundColor='black')},
    options = list(pageLength=5, scrollX = TRUE, scrollY = TRUE
    )
  )
  
  
  observeEvent(input$speech2,{
    updatePickerInput(session,"nbhd","Neighborhood:" ,choices = unique(nbhd$NEIGHBORHOOD[nbhd$BORO %in% input$speech2]))
  })
  
  observeEvent(input$NYC_Restaurants_rows_selected,{
    output$mymap2 <- renderLeaflet({


      data_sub <- filterByCuisineBorough2(df,input$speech1,input$speech2)
      data_sub$Neighborhood <- nbhd$NEIGHBORHOOD[match(as.numeric(data_sub$ZIPCODE),nbhd$ZIPCODE)]

      if('All' %in% input$nbhd){
        nbFilter <- nbhd$NEIGHBORHOOD
      }else{
        nbFilter <- input$nbhd
      }

      df <- data_sub %>% filter(ZIPCODE %in% as.character(nbhd$ZIPCODE),GRADE %in% input$variable,Neighborhood %in% nbFilter) %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>%
        slice(n())
      df <- df[input$NYC_Restaurants_rows_selected,]


      m <- leaflet(data=df) %>%
        addTiles() %>%
        setView(lng=-73.98928, lat=40.75042 , zoom=11)%>%
        addProviderTiles("Stamen.Toner")%>%
        addMarkers(lng = ~Longitude,
                   lat = ~Latitude,

        )

      m
    })

    output$Restaurant_Detail <- renderDataTable({
      
      #get unique restaurants and get the most current date's row for each restraunt
      
      data_sub <- filterByCuisineBorough2(data_raw,input$speech1,input$speech2)
      data_sub$Neighborhood <- nbhd$NEIGHBORHOOD[match(as.numeric(data_sub$ZIPCODE),nbhd$ZIPCODE)]
      
      if('All' %in% input$nbhd){
        nbFilter <- nbhd$NEIGHBORHOOD
      }else{
        nbFilter <- input$nbhd
      }
      
      df <- data_sub %>% filter(ZIPCODE %in% as.character(nbhd$ZIPCODE),GRADE %in% input$variable,Neighborhood %in% nbFilter) %>% arrange(`CAMIS`,as.Date(`INSPECTION DATE`,'%m/%d/%Y'))%>% group_by(`CAMIS`) %>%
        slice(n())
      
      
      restSel <- unique(df[input$NYC_Restaurants_rows_selected,'CAMIS',drop = T])
      
      dfSel <- data_raw %>% filter(CAMIS %in% restSel) %>% arrange(CAMIS,desc(as.Date(`INSPECTION DATE`,'%m/%d/%Y'))) 
      dfSel$Address <- paste(dfSel$BUILDING,dfSel$STREET)
      dfSel$Neighborhood <- nbhd$NEIGHBORHOOD[match(as.numeric(dfSel$ZIPCODE),nbhd$ZIPCODE)]
      dfSel <- dfSel[,c('DBA','BORO','Neighborhood','Address','PHONE','CUISINE DESCRIPTION','VIOLATION DESCRIPTION','SCORE', 'GRADE','INSPECTION DATE')]
      colnames(dfSel) <- c('Name','Borough','Neighborhood', 'Address','Phone','Cuisine Type','Violation','Score','Grade','Inspection Date')    
      
      datatable(dfSel) %>% formatStyle(1:ncol(dfSel),color='white',target='row',backgroundColor='black')
      
      
      formattable::as.datatable(formattable(dfSel, 
                                            list(`Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold"))
                                            )
      ),
      options = list(pageLength=5, scrollX = TRUE, scrollY = TRUE
      )
      ) %>%
        formatStyle(1:ncol(dfSel),color = 'black') %>% 
        formatStyle(2:ncol(dfSel),border = '1px solid #ddd')
    }
    )
    
  
  })
  
})


