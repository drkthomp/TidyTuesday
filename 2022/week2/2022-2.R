library(tidyverse)
library(lubridate) # for date processing 
library(geofacet)
library(colorspace)

setwd("2022/week2")
colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')


data <- left_join(colony, stressor %>% pivot_wider(id_cols=c("year", "months", "state"),names_from="stressor",values_from="stress_pct"))
# combine data
#data <- left_join(colony, stressor)
# add date time
data  <- data %>% janitor::clean_names() %>% mutate(start_date = ym(paste(year, "-", match(str_split_fixed(months, "-", 2)[,1],month.name)))) %>% 
  mutate(start_month = month(start_date))  %>%
  # and state abb
  mutate(state_abb = state.abb[match(state,state.name)])


my_us_grid <- us_state_grid2[us_state_grid2$name %in% data$state, ]
unique(stressor$stressor) %>% janitor::make_clean_names()

# Varroa mites 
varroa_state_correl <- data %>% group_by(state) %>% summarize(state_correl = cor(varroa_mites, colony_lost_pct,use="complete.obs"))

varroa_mites_plot <- ggplot(left_join(data, varroa_state_correl), 
                            aes(varroa_mites, colony_lost_pct,
                                color=state_correl,fill=state_correl)) +
  geom_point(alpha=0.3,size=0.2,color="black") +
  geom_smooth(method = "lm",alpha=0.3) +
  facet_geo(~state,grid = my_us_grid) + 
  labs(title="Do Varroa mites impact colony loss?",
       subtitle=paste0(
         "It varies by state, but overall correlation is weak (",
         round(cor(data$varroa_mites, data$colony_lost_pct,use = "complete.obs"),2),
         ")"),
       y="% Loss", x="% of Colonies Stressed by Varroa mites") + 
  theme_bw() + 
  theme(panel.spacing=unit(0, "lines"),
        plot.title=element_text(size=25,vjust = -9),
        plot.subtitle=element_text(size=15,vjust=-14),
        strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm"),
                                    size=6),
        strip.background = element_rect(fill=FALSE,color=FALSE)) + 
  scale_color_continuous_diverging(name="Pearson\nCorrelation",breaks=seq(from=-1,to=1,by=0.25),palette="Purple-Green,", mid=0) + 
  scale_fill_continuous_diverging(name="Pearson\nCorrelation",breaks=seq(from=-1,to=1,by=0.25),palette="Purple-Green,", mid=0) + 
  theme(axis.text.x = element_text(size=6))
varroa_mites_plot
ggsave("varroa_mites_plot.png", plot=varroa_mites_plot,dpi=600,scale = 1.5)


# pesticides 
pesticides_state_correl <- data %>% group_by(state) %>% summarize(state_correl = cor(pesticides, colony_lost_pct,use="complete.obs"))

pesticides_plot <- ggplot(left_join(data, pesticides_state_correl), aes(pesticides, colony_lost_pct,color=state_correl,fill=state_correl)) +
  geom_point(alpha=0.3,size=0.2,color="black") +
  geom_smooth(alpha=0.8,method = "lm") +
  facet_geo(~state,grid = my_us_grid) + 
  labs(title="Do Pesticides impact colony loss?",
       subtitle=paste0(
         "It varies by state, but overall correlation is minimal (",
         round(cor(data$pesticides, data$colony_lost_pct,use = "complete.obs"),2),
         ")"),
       y="% Loss", x="% of Colonies Stressed by Pesticides") + 
  theme_bw() + 
  theme(panel.spacing=unit(0, "lines"),
        plot.title=element_text(size=25,vjust = -9),
        plot.subtitle=element_text(size=15,vjust=-14),
        strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm"),
                                    size=6),
        strip.background = element_rect(fill=FALSE,color=FALSE)) + 
  scale_color_continuous_diverging(name="Pearson\nCorrelation",breaks=seq(from=-1,to=1,by=0.25),palette="Purple-Green,", mid=0) + 
  scale_fill_continuous_diverging(name="Pearson\nCorrelation",breaks=seq(from=-1,to=1,by=0.25),palette="Purple-Green,", mid=0) + 
  theme(axis.text.x = element_text(size=6))
pesticides_plot
ggsave("pesticides_plot.png", plot=pesticides_plot,dpi=600,scale = 1.5)
