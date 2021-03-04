# Sheets = "SUM total mort"
# Sheets <- NULL
# Data <- NULL
# Chinook_Data <- NULL

get_chinook_data <- function(Sheets){
  
  Chinook_Raw_Data <- read_excel("/Volumes/Enterprise/Data/NRCAN_chapter/Species_Data/2018 Chinook Total Mortality Distribution.xlsx",
                             sheet = Sheets, col_names = FALSE)
  
  colnames(Chinook_Raw_Data) <- as.character(seq(1:33))
  
  Data <- Chinook_Raw_Data %>%
    filter(`1` == "09–17") %>% 
    gather("Original_Fishery","Percentage",4:33) %>% 
    mutate(
      Catch = (as.numeric(Percentage)/100)*as.numeric(`2`), # Converts percentage to weight
      Fishery = Fishery_Set
    ) %>% 
    # Aggregates weights by fisheries
    group_by(Fishery) %>% 
    summarize(Fishery_Catch = sum(Catch)) %>% 
    left_join(Fishery_to_Country) %>%
    filter(!Fishery %in% c("Stray","Esc")) %>% 
    # Aggregates fisherie's catch by Country
    group_by(Fishing_Country) %>%
    summarize(Catch_Country = sum(Fishery_Catch)) %>% 
    mutate(StockName = Chinook_Raw_Data$`1`[1]) %>% 
    left_join(Acronyms, by ="StockName")
  
  return(Data)
  
}
