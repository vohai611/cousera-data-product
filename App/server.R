#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# setup -----------
df = read_rds(here("data/billboard.rds"))

performer_style = read_rds(here("data/performer_style.rds"))


more_than_20 = df %>% 
  count(performer) %>% 
  filter(n >= 20)

n_songs = df %>% 
group_by(performer) %>% 
  summarise(n_songs = n_distinct(song))

n_songs10 = df %>% 
  filter(week_position < 10) %>% 
  group_by(performer) %>% 
  summarise(n_songs = n_distinct(song), n_weeks = n_distinct(week_id)) 

famous_rank = df %>% 
  group_by(performer) %>% 
  summarise(rank = sum(1/week_position)) %>% 
  arrange(- rank) %>% 
  mutate(rank = row_number())


performer_names = more_than_20$performer
# function---
mean_rank_plot= function(input){
  p1 =df %>% 
    select(week_id, week_position, song, performer) %>% 
    filter(performer == input) %>% 
    mutate(week_id = lubridate::mdy(week_id),
           year= lubridate::year(week_id),
           week_position = 100 - week_position) %>%
    complete(year= 1990:2020) %>% 
    group_by(year) %>% 
    summarise(position = mean(week_position)) %>% 
    ggplot(aes(year, position))+
    geom_col()+
    theme_light()+
    labs(x= "", y = "rank")+
    scale_y_continuous(labels = rev(seq(0, 100, by = 20)), breaks = seq(0,100, by = 20))
  
    p1
}


# Server side ----------
shinyServer(function(input, output, session) {
  updateSelectizeInput(session, "performer",selected = "Mariah Carey",
                         choices = performer_names,server = TRUE)

  ## Info box -----
  output$famous_rank = renderInfoBox(
    infoBox(title = "Famous rank",
            value =  famous_rank[famous_rank$performer == input$performer, ]$rank
            )
  )
  output$n_song_top = renderInfoBox(
    infoBox(title = "Number of song on BILLBOARD",
            value =  n_songs[n_songs$performer == input$performer, ]$n_songs
            )
  )
  
  song_of_performer = reactive({df %>% 
    filter(performer == input$performer) %>% 
      group_by(song) %>% 
      summarise(top_rank = min(week_position))
    })
  
  ## On-click box -----
  onclick("box1", 
          showModal(modalDialog(
            title = "Songs",
            renderDataTable({
              song_of_performer()
            }))))
  
  onclick("box0", 
          showModal(modalDialog(
            title = "Songs",
            renderDataTable({
              famous_rank
            }))))
  
  
  output$n_song_top10 = renderInfoBox(
    infoBox(title = "Number of song on BILLBOARD top 10",
            value =  n_songs10[n_songs10$performer == input$performer, ]$n_songs
            )
  )

  output$n_week_top10 = renderInfoBox(
    infoBox(title = "Number of weeks on top 10",
            value =  n_songs10[n_songs10$performer == input$performer, ]$n_weeks
            )
  )
  
  ## Main body ---------
  
  # summarize performer style

  performer_style_react = reactive({
    performer_style %>% 
      filter(performer == input$performer) %>% 
      select(-performer)
  })

  
  
  output$performer_radar = renderPlotly({
    
    plot_ly(type = "scatterpolar",
            r = performer_style_react()[1,] %>% as.numeric(),
            theta = names(performer_style_react()),
            fill= 'toself') %>% 
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,1)
          )
        ),
        showlegend = F
      ) %>% 
      config(displayModeBar= FALSE)#
  })
  
  output$mean_rank_plot = renderPlot({
    mean_rank_plot(input$performer)
  })
  
})
