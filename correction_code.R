#libraries ----
library(dplyr)
library(skimr)

#dummy data creation ----
field_data <- tibble(
  particle_id = 1:1000,
  sample_id = sample(1:10, size = 1000, replace = T),
  morphology = sample(c("fiber", "film", "sphere", "fragment"), size = 1000, replace = T),
  color = sample(c("green", "red", "blue", "black"), size = 1000, replace = T),
  size = sample(c("<10", "10-100", "100-1000"), size = 1000, replace = T)
)

blank_control_data <- tibble(
  blank_particle_id = 1:100,
  blank_sample_id = sample(1:3, size = 100, replace = T),
  morphology = sample(c("fiber", "film", "sphere", "fragment"), size = 100, replace = T),
  color = sample(c("green", "red", "blue", "black"), size = 100, replace = T),
  size = sample(c("<10", "10-100", "100-1000"), size = 100, replace = T)
)

#add spike?

#calculate the blank correction measures ----

#simple count based removal.
blank_correction_pure_count <- blank_control_data %>%
  group_by(blank_sample_id) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pull(count) %>%
  mean() %>%
  round()

field_data_corrected <- field_data %>%
  group_by(sample_id) %>%
  slice_sample(n = blank_correction_pure_count) %>%
  ungroup() %>%
  select(particle_id) %>%
  anti_join(x = field_data, y = .)
  
#morphology based removal, could easily replace morphology with color or size class here. 
blank_correction_morphology <- blank_control_data %>%
  group_by(blank_sample_id, morphology) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(morphology) %>%
  summarise(mean = round(mean(count)))

remove_these <- field_data[0,"particle_id"]
for(row in 1:nrow(blank_correction_morphology)){
  remove_these <- field_data %>%
    filter(morphology == blank_correction_morphology[[row,"morphology"]]) %>%
    group_by(sample_id) %>%
    slice_sample(n = blank_correction_morphology[[row,"mean"]]) %>%
    ungroup() %>%
    select(particle_id) %>%
    bind_rows(remove_these)
}

field_data_corrected_morphology <- field_data %>%
  anti_join(remove_these)
