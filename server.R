
library(shiny)

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
    
    # output$maxBox1 <- renderInfoBox({
    #     max <- data %>% 
    #         filter(Year == input$Year1, `Rate Type` == 'Opioid Overdose') %>% top_n(1)
    #     
    #     infoBox("Max Opioid Death Rate", 
    #             max$Rate, max$Location,
    #             color = "purple", fill = TRUE)
    # })
    # 
    max = df %>% 
        filter(country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% top_n(1,sum)
    infoBox(title = "Top Technology Domain",
            value = max$technology_domain)
    
    
})

output$box2 = renderInfoBox({
    a = df %>% filter(country == input$country, ipc =="TOTAL")
    b = df %>% filter(country == "World", ipc =="TOTAL")
    infoBox(title = "Precent World Patent Contribution",
            value = paste0(signif(sum(a$value)/sum(b$value)*100,2) , "%"))
})



output$country_domain = renderPlotly({
    df %>% 
        filter(country == input$country,
               technology_domain!="Total Patents") %>% 
        group_by(year, technology_domain) %>% summarise(num_patent = sum(value)) %>% 
        ggplot(aes(x = year, y = num_patent)) + geom_line(aes(color = technology_domain))
    
})
    

output$graph1 = renderPlotly({
    df %>% 
        filter(year == input$country_year, country == input$country, technology_domain!="Total Patents") %>% 
        group_by(technology_domain) %>% summarise(sum = sum(value)) %>% 
        ggplot(aes(x = reorder(technology_domain, sum), y = sum)) + geom_bar(stat = "identity") + coord_flip()
    
})


    
}




