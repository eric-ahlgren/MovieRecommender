## server.R

# load functions
library(recommenderlab)
library(tidyverse)

# define functions
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

server_recommend = function(inGenre, n=5) {
  tmp = dplyr::filter(all_genre_top10, str_detect(Genres, inGenre)) %>%
    arrange(desc(ave_ratings))
  return(tmp[1:n,])
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

all_genre_top10 = as_tibble(read.csv("data/all_genre_top10.csv"))

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
Rmat = new('realRatingMatrix', data = Rmat)
train = Rmat[1:4500, ]

recommender.UBCF <- Recommender(train, method = "UBCF",
                                parameter = list(normalize = 'center',
                                                 method = 'Cosine',
                                                 nn = 20))

# recommender.IBCF <- Recommender(train, method = "IBCF",
#                                 parameter = list(normalize = 'center', 
#                                                  method = 'Cosine', 
#                                                  k = 30))

n.item = ncol(getModel(recommender.UBCF)$data)
movieIDs = colnames(getModel(recommender.UBCF)$data)

shinyServer(function(input, output, session) {
  
  # get genre selection
  recommendTable = reactive({
    server_recommend(input$genre)
  })
  
  output$genreTable = renderTable(recommendTable())
  
  output$genreTableImgs = renderUI({
    num_rows = 1
    num_movies = 5
    recom_genreTable = recommendTable()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[movies$MovieID == recom_genreTable$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID == recom_genreTable$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the submit button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      idxs = which(movies$MovieID %in% user_ratings$MovieID)
      new.ratings = rep(NA, n.item)
      new.ratings[idxs] = user_ratings$Rating
      new.user = matrix(new.ratings, 
                        nrow=1, ncol=n.item,
                        dimnames = list(
                          user=paste('input'),
                          item=movieIDs
                        ))
      new.Rmat = as(new.user, 'realRatingMatrix')
      
      recom1 = predict(recommender.UBCF, new.Rmat, type = 'topN')
      # recom1 = predict(recommender.IBCF, new.Rmat, type = 'topN')

      user_predicted_ids = recom1@itemLabels[recom1@items$`0`]
      pred_ratings = recom1@ratings$`0`
      
      user_predicted_ids = as.numeric(sub('.','',user_predicted_ids))
      if(length(user_predicted_ids) == 0) {
        top_without_userRated = all_genre_top10[! all_genre_top10$MovieID %in% user_ratings$MovieID,]
        user_predicted_ids = sample(top_without_userRated$MovieID, 10)
      }
      recom = movies %>%
        filter(MovieID %in% user_predicted_ids)
      recom = recom[match(user_predicted_ids, recom$MovieID),]
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = recom$MovieID, 
                                  Title = recom$Title, 
                                  Predicted_rating =  pred_ratings)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function