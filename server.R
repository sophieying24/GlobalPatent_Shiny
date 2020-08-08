
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
        
        fig = plot_geo(test)
        fig %>% add_trace(z = test$sum, color = test$sum, colors = "Blues",
                          text = test$country, locations = test$location,
                          marker = list(line = list(color = toRGB("grey"), width = 0.5)))
})



output$country_rank = renderPlotly({
    test = df %>% filter(!(location %in% c("WLD", "EU28")) )
    test =
        test %>%
        filter(year == input$world_year,
               ipc == "TOTAL") %>%
        group_by(location, country) %>%
        summarise(sum = sum(value)) %>% arrange(desc(sum))
    
    ggplot(head(test, 20), aes(x = reorder(country, sum), y = sum)) + geom_bar(stat = "identity") + 
        coord_flip() 
})    


output$domain_rank = renderPlotly({
    df %>% filter(ipc !="TOTAL") %>% group_by(year, technology_domain) %>% 
        filter(year <= input$world_year) %>% 
        summarise(num_patent = sum(value)) %>% 
        ggplot(aes(x = year, y = num_patent)) + geom_line(aes(color = technology_domain))
    
    
})

    
output$collab = renderPlotly({
    ggplotly(
        info %>% 
            group_by(year) %>% 
             filter(year <= input$world_year) %>% 
            summarise(total = sum(total_pat),
                      collab = sum(co_wrd),
                      collab_jpn = sum(co_jpn, na.rm = TRUE),
                      collab_usa = sum(co_usa, na.rm = TRUE),
                      collab_world = collab/total,
                      collab_ratio_jpn = collab_jpn/total,
                      collab_ratio_usa = collab_usa/total) %>% 
            gather(key = "ratio", value = "value", collab_world:collab_ratio_usa) %>% 
            ggplot(aes(x = year, y = value)) + geom_line(aes(color = ratio))
    )
})

############################## By Country ##############################

output$box1 = renderInfoBox({
    
    max = df %>% 
        filter(country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    infoBox(title = paste(input$country, "Top Technology Domain"),
            value = max$technology_domain)
    
    
})

output$box2 = renderInfoBox({
    a = df %>% filter(country == input$country, ipc =="TOTAL")
    b = df %>% filter(country == "World", ipc =="TOTAL")
    infoBox(title = paste0(input$country,"'s % World Patent Contribution"),
            value = paste0(signif(sum(a$value)/sum(b$value)*100,2) , "%"))
})



output$country_domain = renderPlotly({
    df %>% 
        filter(country == input$country,
               technology_domain!="Total Patents") %>% 
        group_by(year, technology_domain) %>% summarise(num_patent = sum(value)) %>% 
        # top_n(8, sum_patent) %>% 
        ggplot(aes(x = year, y = num_patent)) + geom_line(aes(color = technology_domain))
    
})
    

output$graph1 = renderPlotly({
    df %>% 
        filter(year == input$country_year, country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = reorder(technology_domain, sum), y = sum)) + geom_bar(stat = "identity") + coord_flip()
    
})



output$graph2 = renderPlotly({
    df %>% 
        filter(country == input$country, ipc == "TOTAL") %>% 
        group_by(year) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = year, y = sum)) + geom_line() + ggtitle("Total Patents through Years") + xlim(2004, 2016)
    
})



output$graph3 = renderPlotly({
    info %>% 
        filter(country == input$country) %>% 
        select(year, GDP_HAB_PPP) %>% 
        ggplot(aes(x = year, y = GDP_HAB_PPP)) + geom_line() + xlim(2004,2016) +
        ggtitle("GDP Per Capita Trend")
    
    
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
                  collab_world = collab/total,
                  collab_ratio_jpn = collab_jpn/total,
                  collab_ratio_usa = collab_usa/total) %>% 
        gather(key = "ratio", value = "value", collab_world:collab_ratio_usa) %>% 
        ggplot(aes(x = year, y = value)) + geom_line(aes(color = ratio)) + xlim(2004,2016) + 
        ggtitle("Patent Collaboration Trend") + theme(legend.position="top")
        
     
     ) %>% layout(legend = list(orientation = "h"))
})

output$graph5 = renderPlotly({
    ggplotly(
    info %>% 
        filter(country == input$country) %>% 
        select(year, G_XGDP, HE_XGDP) %>% 
        gather(key = "measure", value = "value", G_XGDP:HE_XGDP, na.rm = TRUE) %>% 
        ggplot(aes(x = year, y = value, col = measure)) + geom_line() +xlim(2005,2015) +
        ggtitle("% of GERD & HERD GDP Trends") + theme(legend.position="top")
    ) %>% layout(legend = list(orientation = "h"))
})

    
}




