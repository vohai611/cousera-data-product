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
song_select = df %>% distinct(song, performer) 
songs = read_rds(here("data/songs.rds"))

# 
# more_than_20 = df %>% 
#   count(performer) %>% 
#   filter(n >= 20)

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


performer_names = unique(df$performer)

# function---
song_rank = function(performer, song){
  song_rank = df %>% 
    filter(performer == {{ performer }}, song == {{ song}}) %>% 
    mutate(week_id = lubridate::mdy(week_id)) %>% 
    transmute(week = lubridate::week(week_id), year= lubridate::year(week_id), 
              week_position) %>% 
    arrange(week, year) %>% 
    mutate(wyear= factor(paste0(week,"-", year))) %>% 
    ggplot(aes(wyear, week_position))+
    geom_point()+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90))+
    labs(y = "Rank", x= "Week-year")
  
  ggplotly(song_rank) %>% 
    config(displayModeBar = FALSE)
}


# Server side ----------
shinyServer(function(input, output, session) {
  ## Update UI ---
  updateSelectizeInput(session, "performer",selected = "Mariah Carey",
                         choices = performer_names,server = TRUE)
  observeEvent(input$performer, updateSelectInput(session,
                                                  "song_selected",
                                                  choices = song_select[song_select$performer == input$performer, ]$song ))

  ## Info box -----
  output$famous_rank = renderInfoBox(
    infoBox(title = "Famous rank",
            value =  paste0("#", famous_rank[famous_rank$performer == input$performer, ]$rank)
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

  song_selected = reactive({
    songs %>% 
      filter(performer == input$performer,
             song == input$song_selected) %>% 
      select(-performer, -song)
  })

  
  
  output$performer_radar = renderPlotly({
    
    plot_ly(type = "scatterpolar",
            r = song_selected()[1,] %>% as.numeric(),
            theta = names(song_selected()),
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
  
  output$song_rank = renderPlotly({
    song_rank(input$performer, input$song_selected)
  })
  
})
