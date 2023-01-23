# Irti Haq
# Packages ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, here)

# Load Data & Clean ----
data <- read_csv(here("data", 'test_data.csv')) 
data[2:8 ][ data[2:8 ] > 5 | data[2:8 ] < 0 ] <- NA
data <- data %>% drop_na()

# Re-codded ----

data_recode <- data %>% mutate_at(vars(ends_with('_r')), 
                                  ~ recode(., "1" = 5, "2" = 4, "3" = 3, 
                                           "4" = 2, "5" = 1))

# Analysis ----

## Data with Composite Score ---

t_cohesion <- data %>% select(contains('cohesion')) %>% names()
t_interdep <- data %>% select(contains('interdepdence')) %>% names()

data_comp <- data_recode %>% transmute( id = id , 
                                 team_cohesion_score = rowMeans(subset(data_recode, 
                                                                       select = t_cohesion)), 
                                 team_performance = team_performance , 
                                 task_interdepdence_score = rowMeans(subset(data_recode, 
                                                                            select = t_interdep)))

## Stat Table ----



Sum_table <- data_comp %>% select(-id) %>% 
                           gather(key = Variable, value = Composite_Score) %>% 
                           group_by(Variable) %>% 
                           summarize(Mean = mean(Composite_Score), 
                                     sd = sd(Composite_Score))



Corr_table <- data_comp %>% select(-id) %>% cor(method = 'spearman') %>% 
              as.data.frame() %>% 
              rename(team_cohesion_corr = team_cohesion_score,
                     team_performance_corr = team_performance,
                     task_interdepdence_corr = task_interdepdence_score) %>% 
              rownames_to_column()

# Final Table
Summary_table <- Sum_table %>% left_join(Corr_table, 
                                         by = c('Variable' = 'rowname'))


## Linear Regression  ----
model <- lm(team_performance ~ team_cohesion_score * task_interdepdence_score, 
            data = data_comp)
summary(model)

model_coef <- coef(model)

## Plot  ----

plotproccessed <- data_comp %>% 
  mutate(task_interdepdence_grouped = cut(task_interdepdence_score, 
                                          breaks = c(0,3,5), 
                                          labels = c("Low","High")))

plot <- ggplot(plotproccessed, aes(x=team_cohesion_score, y=team_performance, 
                                   color= task_interdepdence_grouped)) +
  geom_point() +
  geom_smooth(method="lm", se = F) +
  ggtitle("Task Interdependence & Team Cohesion vs Team Performance")+
  ylab("Team Performance Score") +
  theme_light()

plot
ggsave('plot.png', width = 1920, height = 1080, units = "px")


