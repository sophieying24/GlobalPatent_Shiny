
function(input, output){

   
############################## World Overview ##############################
    
output$map = renderPlotly({
    
        test = df %>% filter(!(location %in% c("WLD", "EU28")) )
        test =
            test %>%
            filter(year == input$world_year,
                   ipc == "TOTAL") %>%
            group_by(location, country) %>%
            summarise(sum = sum(value)) %>% arrange(desc(sum))
        test$sum = round(test$sum)
        
        fig = plot_geo(test)
        fig %>% add_trace(z = test$sum, color = test$sum, colors = "Blues",
                          text = test$country, locations = test$location,
                          marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>% 
            colorbar(title = '', ticksuffix = '') %>% 
            layout(geo = list(
                showframe = FALSE,
                projection = list(type = 'Mercator')
            ))
})


output$max_country = renderInfoBox({
    
    max = df %>% 
        filter(year == input$world_year, technology_domain=="Total Patents", country != "World") %>% 
        group_by(country) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    
    infoBox(title = paste("Top Country by Number of Patents in", input$world_year),
            value = max$country, icon = icon("flag"))
    
    
})

output$max_domain = renderInfoBox({
    
    max = df %>% 
        filter(year == input$world_year, technology_domain!="Total Patents", country == "World") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    
    infoBox(title = paste("Top Domain by Number of Patents in", input$world_year),
            value = max$technology_domain, icon = icon("robot"))
    
    
})


output$count_world = renderInfoBox({
    a = df %>% filter(year == input$world_year, technology_domain=="Total Patents", country == "World") %>% summarise(sum = sum(value))
    infoBox(title = paste("Total Number of Patents in", input$world_year),
            value = a$sum)
})



output$country_rank = renderPlotly({
    
    
    test = df %>% filter(!(location %in% c("WLD", "EU28")) )
    test =
        test %>%
        filter(year == input$world_year,
               ipc == "TOTAL") %>%
        group_by(location, country) %>%
        summarise(sum = sum(value)) %>% arrange(desc(sum))
    
    ggplot(head(test, 10), aes(x = reorder(country, sum), y = sum)) + geom_bar(stat = "identity", fill = "#222d32") + 
        coord_flip() +
        labs(x = "", y = "Number of Patents") + theme_light()
    
})    


output$domain_rank = renderPlotly({
    df %>% 
        filter(year == input$world_year, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = reorder(technology_domain, sum), y = sum)) + geom_bar(stat = "identity", fill = "#222d32") + coord_flip() + 
        labs(x = "", y = "Number of Patents") + theme_light()
    
})

    

############################## By Country ##############################



output$box1 = renderInfoBox({
    
    max = df %>% 
        filter(country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    infoBox(title = paste("Top Technology Domain in ", input$country),
            value = max$technology_domain, icon = icon("robot"))
    
    
})

output$box2 = renderInfoBox({
    a = df %>% filter(country == input$country, ipc =="TOTAL")
    b = df %>% filter(country == "World", ipc =="TOTAL")
    infoBox(title = paste0("World Patent Contribution by ", input$country),
            value = paste0(signif(sum(a$value)/sum(b$value)*100,2) , "%"),
            icon = icon("percentage"))
})


output$country_count = renderInfoBox({
    a = df %>% filter(year == input$country_year, country ==input$country,ipc =="TOTAL") %>% summarise(sum = sum(value)) 
    infoBox(title = paste("Total Number of Patents in", input$country, input$country_year),
            value = trunc(a$sum))
})



output$country_domain = renderPlotly({
    df %>% 
        filter(country == input$country,
               technology_domain!="Total Patents") %>% 
        group_by(year, technology_domain) %>% summarise(num_patent = sum(value)) %>% 
        ggplot(aes(x = year, y = num_patent)) + geom_line(aes(color = technology_domain)) + theme_bw() +
        labs(x = "Year", y = "Number of Patents", title = paste("Trend of Patent Count by Technology Domain in",input$country)) +
        theme(legend.title = element_blank())
    
})
    

output$graph1 = renderPlotly({
    df %>% 
        filter(year == input$country_year, country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = reorder(technology_domain, sum), y = sum)) + geom_bar(stat = "identity", fill = "#222d32") + coord_flip() + theme_bw() +
        labs(x = "", y = "Year", title = paste("Rank of Patent Count by Technology Domain in", input$country, input$country_year))
    
})



output$graph2 = renderPlotly({
    df %>% 
        filter(country == input$country, ipc == "TOTAL") %>% 
        group_by(year) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = year, y = sum)) + geom_line() + xlim(2004, 2016) + 
        labs(x = "Year", y = "Number of Patents", title = "Total Patent Count") + theme_bw()
    
})



output$graph3 = renderPlotly({
    info %>% 
        filter(country == input$country) %>% 
        select(year, GDP_HAB_PPP) %>% 
        ggplot(aes(x = year, y = GDP_HAB_PPP)) + geom_line() + xlim(2004,2016) +
        labs(x = "Year", y = "GDP per Capita (USD PPPs)", title = "GDP Per Capita") + theme_bw()
    
})


output$graph4 = renderPlotly({
   ggplotly(
     
       info %>% 
        filter(country == input$country) %>% 
        group_by(year) %>% 
        summarise(total = sum(total_pat),
                  collab = sum(co_wrd),
                  collab_jpn = sum(co_jpn, na.rm = TRUE),
                  collab_usa = sum(co_usa, na.rm = TRUE),
                  collab_world = collab/total*100,
                  collab_ratio_jpn = collab_jpn/total*100,
                  collab_ratio_usa = collab_usa/total*100) %>% 
        gather(key = "ratio", value = "value", collab_world:collab_ratio_usa) %>% 
        ggplot(aes(x = year, y = value)) + geom_line(aes(color = ratio)) + xlim(2004,2016) + 
        theme(legend.position="top") + theme_bw()+
        labs(x = "Year", y = "%", title = "Percentage of Patent with Foreign Collaboration")
        
     
     ) %>% layout(legend = list(orientation = "h"))
})

output$graph5 = renderPlotly({
    ggplotly(
    info %>% 
        filter(country == input$country) %>% 
        select(year, G_XGDP, HE_XGDP) %>% 
        gather(key = "measure", value = "value", G_XGDP:HE_XGDP, na.rm = TRUE) %>% 
        ggplot(aes(x = year, y = value, col = measure)) + geom_line() +xlim(2004,2016) +
        theme(legend.position="top") + theme_bw()+
        labs(x = "Year", y = "% of GDP", title = "Gross Domestic and Higher Education Expenditure on R&D")
    ) %>% layout(legend = list(orientation = "h"))
})



############################## By Domain ##############################


output$box3 = renderInfoBox({
    
    max = df %>% 
        filter(technology_domain == input$domain, country!="World") %>% 
        group_by(country) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    infoBox(title = paste("Top Country in ", input$domain),
            value = max$country, icon = icon("flag"))
    
    
})

output$box4 = renderInfoBox({
    a = df %>% filter(technology_domain == input$domain, country =="World")
    b = df %>% filter(technology_domain == "Total Patents", country =="World")
    infoBox(title = paste(input$domain,"as % of Total Patents"),
            value = paste0(signif(sum(a$value)/sum(b$value)*100,2) , "%"),
            icon = icon("percentage"))
})




output$graph6 = renderPlotly({

    df %>% 
        filter(country!= "World", technology_domain==input$domain) %>% 
        group_by(country) %>% summarise(sum = sum(value)) %>% top_n(10, sum) %>% 
        inner_join(df, by = "country") %>% 
        filter(technology_domain == input$domain) %>% 
        group_by(country, year) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = year, y = sum, col = country)) + geom_line() + theme_bw() + 
        labs(x = "Year", y = "Number of Patents", title = paste("Trend of Patent Count by Top 10 Countries in", input$domain)) + 
        theme(legend.title = element_blank())
})


output$graph7 = renderPlotly({
    df %>% 
        filter(year == input$domain_year, technology_domain == input$domain, country !="World") %>%
        group_by(country) %>% summarise(sum = sum(value)) %>% arrange(desc(sum)) %>% top_n(10) %>% 
        ggplot(aes(x = reorder(country, sum), y = sum)) + geom_bar(stat = "identity", fill = "#222d32") + coord_flip() + theme_bw()+
        labs(x = "Year", y = "Number of Patents", title = paste("Patent Count of top 10 Countries in", input$domain, input$domain_year))
    
})


output$domain_count = renderInfoBox({
    a = df %>% filter(year == input$domain_year, technology_domain==input$domain, country=="World") %>% summarise(sum = sum(value)) 
    infoBox(title = paste("Total Patents in", input$domain, input$country_year),
            value = trunc(a$sum))
})


############################## Data Tables ##############################

output$patent = renderDataTable({
    datatable(df, rownames = FALSE)
})

output$info = renderDataTable({
    datatable(info, rownames = FALSE)
})


    
}




