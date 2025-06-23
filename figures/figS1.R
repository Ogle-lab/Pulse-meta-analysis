### Stacked histogram of category + scale

library(tidyverse)
library(ggforce) # for facet grids
library(cowplot) # for ggsave2

path_out = "./figures/" # set save path

# Load output data
load("data_output/df_all.Rdata") # df_all

df_all <- df_all %>%
  drop_na(sID)

df_all2 <- df_all %>%
  mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  mutate(varGroup = as.factor(varGroup), 
         Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/footprint")), 
         Pulse.type = as.factor(Pulse.type), 
         response_cat = as.factor(response_cat),
         varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                               varGroup == "water" ~ "Water-related"),
         varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                              "ecosystemR", "belowgroundR",
                                              "ET", "T", "Gs", "PWP")))
# varType2 = case_when(varType == "Anet" ~ "A[net]",
#                      varType == "belowgroundR" ~ "R[below]",
#                      varType == "ecosystemR" ~ "R[eco]",
#                      varType == "Gs" ~ "g[s]",
#                      varType == "PWP" ~ "Psi[plant]",
#                      .default = as.character(varType)))
#df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))
tot_df <- df_all2 %>% 
  group_by(varType, varGroup2) %>%
  count()

labs <- c("classic", "intermediate", "linear" , "no pulse")
# labs1 <- lapply(c("C-related", "H[2]O-related"), function(i) bquote(.(i)))


# figS1.1 <- ggplot(df_all2) +
#   geom_bar(position = "stack", 
#            aes(x = response_cat,
#                fill = Sample.unit)) +
#   facet_wrap(~varGroup2,
#              labeller = label_parsed) +
#   scale_x_discrete(labels = labs) +
#   scale_fill_brewer(palette = "Paired",
#                     direction = -1) +
#   theme_bw(base_size = 14) +
#   theme(panel.grid = element_blank(),
#         strip.background = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_text(angle = 45,
#                                    hjust = 1,
#                                    vjust = 1),
#         legend.title = element_blank(),
#         legend.position = c(0.6, 0.8),
#         legend.background = element_rect(fill = NA))


figS1.1 <- df_all2 %>%
  filter(varGroup2=="Carbon-related") %>%
  ggplot() +
  geom_bar(position = "stack", 
           aes(x = response_cat,
               fill = Sample.unit)) +
  facet_wrap(~varGroup2,
             labeller = label_parsed) +
  scale_x_discrete(labels = labs) +
  scale_fill_brewer(palette = "Paired",
                    direction = -1) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1),
        legend.title = element_blank(),
        legend.position = c(0.6, 0.8),
        legend.background = element_rect(fill = NA))

figS1.2 <-  df_all2 %>%
  filter(varGroup2=="Water-related") %>%
  ggplot() +
  geom_bar(position = "stack", 
           aes(x = response_cat,
               fill = Sample.unit)) +
  facet_wrap(~varGroup2,
             labeller = label_parsed) +
  scale_x_discrete(labels = labs) +
  scale_fill_brewer(palette = "Paired",
                    direction = -1) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1),
        legend.title = element_blank(),
        legend.position = "none")

figS1 <- plot_grid(figS1.1,figS1.2,
                   nrow = 1,
                   align = "h",
                   labels = "AUTO")


ggsave2("figS1.png", plot = figS1, 
        path = path_out, width = 8, height = 4)
