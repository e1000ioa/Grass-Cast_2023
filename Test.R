Taylor_Maker <- function(df, zone, n, x, title) {
  
  #Where:
  #n = date start
  #x = date end
  #Season = Spring or Summer
  
  model_select <- function(df,scenario) {
    # df: Data frame to select models from
    
    # Extract the model names
    model_names <- names(df)
    
    # Initialize an empty list to hold the models
    models <- list()
    
    # Add the models to the list
    for (i in 1:length(df)) {
      model_name <- model_names[i]
      model <- df[[i]][[scenario]]
      models[[model_name]] <- model
    }
    
    return(models)
  }
  
  year <- year(as.Date(n))
  
  modelsA <- model_select(ANPP_FORECAST_ALL,4) #Bellow
  modelsB <- model_select(df,5) #Average
  modelsC <- model_select(df,6) #Above
  
  ColorA <- "#CB3446"
  ColorB <- "#3446CB"
  ColorC <- "#46CB34"
  
  #Creates new dataframe  filtering the dates
  dfz <- orderModels[orderModels$name >= as.Date('2020-05-15') & orderModels$name <= as.Date('2020-06-02'),]
  
  #Creates the point of re fences, the last observation all the values are on Models
  ref <- as.numeric(modelsA[[tail(dfz$order, n=1)]])
  
  
