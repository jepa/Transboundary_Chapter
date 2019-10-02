library(dplyr)
library(tidyr)
library(ggplot2)

min(chinook_data$X.1, na.rm=T)
unique(chinook_data$X.1)
names(chinook_data)[1:5]
View(head(chinook_data))
unique(chinook_data$`AABM Fishery..8`)


AABM_SEAK_N <- chinook_data %>% 
  gather("Erraseme","AABM_SEAK_N",`AABM Fishery..5`:`AABM Fishery..8`) %>% 
  filter(!is.na(AABM_SEA_N)) %>% 
  mutate(Region = "SEAK",
         Gear = "N") %>% 
  select(1:2,Region,Gear,AABM_SEAK_N)

AABM_SEAK_S <- chinook_data %>% 
  gather("Erraseme","AABM_SEAK_S",`AABM Fishery..9`:`AABM Fishery..13`) %>% 
  filter(!is.na(AABM_SEAK_S)) %>% 
  select(1:2,AABM_SEAK_S)

ISBM_NBC_T <- chinook_data %>% 
gather("Erraseme","ISBM_NBC_T",ISBM_Fishery..14:ISBM_Fishery..17) %>% 
  filter(!is.na(ISBM_NBC_T)) %>% 
  select(1:2,ISBM_NBC_T)

ISBM_NBC_S <- chinook_data %>% 
  gather("Erraseme","ISBM_NBC_S",ISBM_Fishery..18:ISBM_Fishery..21) %>% 
  filter(!is.na(ISBM_NBC_S)) %>% 
  select(1:2,ISBM_NBC_S)

ISBM_WCVI_T <- chinook_data %>% 
  gather("Erraseme","ISBM_WCVI_T",ISBM_Fishery..22:ISBM_Fishery..25) %>% 
  filter(!is.na(ISBM_WCVI_T)) %>% 
  select(1:2,ISBM_WCVI_T)

  


Col_Names <- c(
  "Year",
  "Est",
  "Agesa",
  "Agesb",
  paste("SEAK-",letters,sep="")[1:8],
  paste("NBC-",letters,sep="")[1:7],
  paste("WCVI-",letters,sep="")[1:5],
  paste("NBC_CBC-",letters,sep="")[1:10],
  paste("SBC-",letters,sep="")[1:10],
  paste("Falcon-",letters,sep="")[1:11],
  paste("WAC-",letters,sep="")[1:3],
  paste("Puget_Sd-",letters,sep="")[1:7],
  paste("Terminal_Seak-",letters,sep="")[1:7],
  paste("Terminal_Canada-",letters,sep="")[1:6],
  paste("Terminal_SUS-",letters,sep="")[1:9],
  paste("Stray-",letters,sep="")[1:4],
  paste("Esc-",letters,sep="")[1:8]
)
  
Places <- c(
  "Alaska Spring (Alaska South SE)", 
  "Atnarko River (North/Central BC) total fishing mortalities",
  "Atnarko River (North/Central BC) 5-age ERA output total",
  "Atnarko Yearling (North/Central BC) total fishing mortalities",
  "Atnarko Yearling (North/Central BC) 5-age ERA output total",
  "Big Qualicum River Fall (Lower Strait of Georgia Hatchery and Natural)",
  "Chilliwack River Fall (Fraser Late)",
  "Chilkat River",
  "Cowichan River Fall (Lower Strait of Georgia Natural)",
  "Cowlitz Fall Tule (Fall Cowlitz Hatchery)",
  "Dome Creek Spring (Fraser Early)",
  "Elk River (Oregon Coast)",
  "Elwha River",
  "George Adams Fall Fingerling",
  "Hanford Wild Brights",
  "Harrison River (Fraser Late)",
  "Hoko Fall Fingerling",
  "Kitsumkalum River Summer (North/Central BC)",
  "Kitsumkalum Yearling (North/Central BC)",
  "Lower River Hatchery Tule (Lower Bonneville Hatchery)",
  "Lewis River Wild (Lewis River Wild)",
  "Lyons Ferry (Lyons Ferry Hatchery)",
  "Lyons Ferry Yearling",
  "Middle Shuswap River Summer (Fraser Early)",
  "Nanaimo River Fall (Lower Strait of Georgia Natural)",
  "Nicola River Spring (Fraser Early)",
  "Nisqually Fall Fingerling",
  "Nooksack Spring Yearling (Nooksack Spring Yearling)",
  "Nooksack Spring Fingerling (Nooksack Spring Yearling)",
  "Phillips River Fall (Upper Strait of Georgia)",
  "Puntledge River Summer (Lower Strait of Georgia Hatchery)",
  "Queets Fall Fingerling (Washington Coastal Wild)",
  "Quinsam River Fall (Upper Strait of Georgia)",
  "River Fall (Upper Strait of Georgia) 5-age ERA",
  "Robertson Creek Fall (WCVI Hatchery and Natural)",
  "Samish Fall Fingerling (Nooksack Fall Fingerling)",
  "Lower Shuswap River Summer (Fraser Early)",
  "Skagit Spring Fingerling",
  "Skagit Spring Yearling",
  "Skykomish Fall Fingerling (Snohomish Wild)",
  "Sooes (now Tsoo-Yess) Fall Fingerling (Washington Coastal Wild)",
  "Spring Creek Tule (Spring Creek Hatchery)",
  "South Puget Sound Fall Fingerling (Puget Sound Hatchery Fingerling)",
  "South Puget Sound Fall Yearling (Puget Sound Hatchery Yearling)",
  "Squaxin Pens Fall Yearling (Puget Sound Hatchery Yearling)",
  "Salmon River (Oregon Coast)",
  "Skagit Summer Fingerling (Skagit Wild)",
  "Stikine River",
  "Stillaguamish Fall Fingerling (Stillaguamish Wild)",
  "Columbia River Summers (Columbia River Summer)",
  "Taku River",
  "Unuk River",
  "Columbia River Upriver Bright (Columbia River Upriver Brights)",
  "University Of Washington Accelerated",
  "White River Spring Yearling",
  "Willamette Spring (Willamette River Hatchery)"
)

Rivers <- c(
  "Alaska Spring", 
  "Atnarko",
  "Atnarko",
  "Atnarko",
  "Atnarko",
  "Big Qualicum",
  "Chilliwack",
  "Chilkat",
  "Cowichan",
  "Cowlitz",
  "Dome Creek Spring",
  "Elk",
  "Elwha",
  "George Adams",
  "Hanford",
  "Harrison",
  "Hoko",
  "Kitsumkalum",
  "Kitsumkalum",
  "Lower",
  "Lewis",
  "Lyons Ferry",
  "Lyons Ferry",
  "Middle Shuswap",
  "Nanaimo",
  "Nicola",
  "Nisqually",
  "Nooksack",
  "Nooksack",
  "Strait of Georgia",
  "Strait of Georgia",
  "Queets",
  "Strait of Georgia",
  "Strait of Georgia",
  "Robertson Creek",
  "Samish",
  "Shuswap",
  "Skagit",
  "Skagit",
  "Skykomish",
  "Sooes (now Tsoo-Yess)",
  "Spring Creek",
  "Puget Sound",
  "Puget Sound",
  "Puget Sound",
  "Salmon",
  "Skagit",
  "Stikine",
  "Stillaguamish",
  "Columbia",
  "Taku",
  "Unuk",
  "Columbia",
  "University of Washington Accelerated",
  "White",
  "Willamette"
)



colnames(chinook_data) <- Col_Names




Chiiii <- chinook_data %>% 
  filter(Year == 2016) %>% 
  mutate(Source = Places,
         River = Rivers) %>% 
  gather("Regions","Value",5:88) %>% 
  select(Year,River,Regions,Value) %>% 
  mutate(Regions = gsub("\\-.*","",Regions),
         Value = as.numeric(Value)
  ) %>% 
  group_by(River,Regions,Year) %>% 
  summarise(Per_Region = sum(Value,na.rm=T)) %>% 
  filter(Per_Region > 0) %>% 
  mutate(State = ifelse(Regions %in% BC_Regions, "BC",
                        ifelse(Regions %in% Al_Regions,"Alaska","US_Mainlyand")))


ggplot(Chiiii) +
  geom_bar(aes(
    x = Regions,
    y = Per_Region,
    fill = Regions
  ),
  stat = "identity"
  ) +
  facet_wrap(~ River) +
  coord_polar("y", start=0)


# Please make no-place over 100%
# Non is over 100%

Places_per <- Chiiii %>% 
  group_by(River) %>% 
  summarise(sum(Per_Region,na.rm=T))





