
library(ggpubr)
library(tidyverse)
library(cowplot)
library(ggh4x)
library(ggforce)

# p value heatmap
df_p <- read.csv("data_output/p_mat.csv")

rowp <- df_p$X
df_p <- df_p %>%
  dplyr::select(-X)
rownames(df_p) <- rowp
p <-as.matrix(df_p)
# par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
# #p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
# plot(as.pvalue(p), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))

# coeff heatmap

df_q <- read.csv("data_output/coeff_mat.csv")

rowp <- df_q$X
df_q <- df_q %>%
  dplyr::select(-X)
rownames(df_q) <- rowp
q <-as.matrix(df_q)


# make non-significant coefficients 0
for(i in 1:nrow(p)){
  for(j in 1:ncol(p)){
    if(!is.na(p[i,j])){
      if(p[i,j]>0.05){ # if the p-value is not significant
        q[i,j]<-0 # then make the coefficient 0
      }
    }
  }
}


# Reorganize as a data frame and rename columns
df <- as.data.frame(q)
df <- df %>%
  rownames_to_column() %>%
  pivot_longer(c(2:5)) %>%
  rename(question = name, coefficient = rowname)

# Create labels
df <- df %>%
  mutate(label = ifelse(value>0, "positive", "negative")) %>%
  mutate(label = ifelse(value==0, "nonsignificant", label)) %>%
  mutate(value = ifelse(value==0, NA, value))

# Manually add in reference labels for reading ease (! remember to update in-between runs !)
df_temp <- data.frame(coefficient = c("Carbon-related","Carbon-related","Carbon-related","Carbon-related",
                                      "Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf"),
                      question = c("GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response",
                                   "GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response"),
                      value = c(NA,NA,NA,NA,
                                NA,NA,NA,NA),
                      label = c("nonsignificant", "nonsignificant", "positive","nonsignificant",
                                "nonsignificant", "positive", "positive","nonsignificant"))

df <- rbind(df,df_temp)

# Reorder coefficients and questions, rename to things that make sense

df$coefficient <- factor(df$coefficient, levels =  c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                                     "Carbon-related",
                                                     "(1) Carbon-related (2) water-related",
                                                     "Pulse amount",
                                                     "MAT",
                                                     "MAP",
                                                     "Spatial scale: leaf",
                                                     "Sample.unitindividual",
                                                     "Sample.unitplot/footprint"),
                         labels = c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                    "Carbon-related",
                                    "Water-related",
                                    "Pulse amount",
                                    "MAT",
                                    "MAP",
                                    "Spatial scale: leaf",
                                    "Spatial scale: individual",
                                    "Spatial scale: plot/footprint"))

df <- df %>%
  mutate(coefficient.label = case_when(coefficient == "Spatial scale: leaf" ~ "Spatial Scale Covariates",
                                       coefficient == "Spatial scale: individual" ~ "Spatial Scale Covariates",
                                       coefficient == "Spatial scale: plot/footprint" ~ "Spatial Scale Covariates",
                                       coefficient == "MAP x MAT" ~ "Interactions",
                                       coefficient == "MAP x carbon- or water-related" ~ "Interactions",
                                       coefficient == "Pulse amount x carbon- or water-related" ~ "Interactions",
                                       coefficient == "Pulse amount" ~ "Pulse-related Covariates",
                                       coefficient == "Water-related" ~ "Pulse-related Covariates",
                                       coefficient == "Carbon-related" ~ "Pulse-related Covariates",
                                       coefficient == "MAP" ~ "Site Covariates",
                                       coefficient == "MAT" ~ "Site Covariates"),
         question.label = case_when(question == "GLM.for.response.or.no.response" ~ "Question 1",
                                    question == "GLM.for.time.of.peak" ~ "Question 2",
                                    question == "GLM.for.magnitude.of.peak" ~ "Question 2",
                                    question == "GLM.for.speed.of.linear.response" ~ "Question 2"))

df$coefficient.label <- factor(df$coefficient.label, levels =  c("Interactions","Pulse-related Covariates",
                                                           "Site Covariates","Spatial Scale Covariates"))

df$question <- factor(df$question, levels =  c("GLM.for.response.or.no.response","GLM.for.time.of.peak","GLM.for.magnitude.of.peak","GLM.for.speed.of.linear.response"),
                         labels = c("GLM for response or no response","GLM for time of peak","GLM for magnitude of peak","GLM for speed of linear response"))

#df$coefficient.label <- ifelse(is.na(df$coefficient.label), "Other", df$coefficient.label)

# Plot

# p <- df %>%
#   ggplot(aes(question, interaction(coefficient,coefficient.label, sep = "!"), fill = label)) +
#   geom_tile(aes(width=.5, height=.7), size=2) +
#   #geom_tile() +
#   #geom_text(aes(label=round(value, digits=4))) +
#   scale_fill_manual(values = c("blue", "darkgray","red")) +
#   facet_grid(coefficient.label~question.label, scales = "free", space = "free") +
#   xlab("Response Variables") +
#   #scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
#   scale_y_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
#   theme_bw() +
#   theme(#panel.spacing = unit(0, "lines"),
#     #plot.margin=margin(grid::unit(0, "cm")),
#     #panel.border = element_blank(),
#     panel.grid = element_blank(),
#     #panel.spacing = element_blank(),
#     legend.position = "top",
#         strip.background = element_blank(),
#         strip.placement = "outside",
#         strip.text.y = element_blank(),
#         text = element_text(size=14),
#         legend.title = element_blank(),
#         axis.text.x = element_text(size=14, angle = 90, vjust = 0.8, hjust = 1))
# p

p <- df %>%
  ggplot(aes(question, interaction(coefficient,coefficient.label, sep = "!"), fill = label)) +
  geom_tile(aes(width=.5, height=.7), size=1) +
  #geom_tile() +
  #geom_text(aes(label=round(value, digits=4))) +
  scale_fill_manual(values = c("blue", "darkgray","red")) +
  xlab("Response Variables") +
  #scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  scale_y_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  theme_bw() +
  theme(#panel.spacing = unit(0, "lines"),
    #plot.margin=margin(grid::unit(0, "cm")),
    #panel.border = element_blank(),
    panel.grid = element_blank(),
    #panel.spacing = element_blank(),
    legend.position = "top",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_blank(),
    text = element_text(size=14),
    legend.title = element_blank(),
    axis.text.x = element_text(size=14, angle = 90, vjust = 0.8, hjust = 1))
p


ggsave2("fig5v2.png", plot = p, path = "./figures/", width = 8, height = 8.7)


