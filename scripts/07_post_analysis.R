### Post-analysis analysis
# glm code
library(tidyverse)
library(tibble)
library(cowplot)

# Load output data
load("data_output/df_all.Rdata") # df_all

## Question 1: Is there a pulse?
## Response variable of interest: response_cat (1-4)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
# scale(as.numeric(dataIN$VPD),center=TRUE,scale=TRUE)
# df_all <- df_all %>%
#   mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
#          Pulse.type = as.factor(Pulse.type),
#          response_cat = as.numeric(response_cat),
#          MAT.C.wc = scale(as.numeric(MAT.C.wc),center=TRUE,scale=TRUE),
#          MAP.mm.wc = scale(as.numeric(MAP.mm.wc),center=TRUE,scale=TRUE),
#          Pulse.amount.mm = scale(as.numeric(Pulse.amount.mm),center=TRUE,scale=TRUE),
#          preVar = scale(as.numeric(preVar),center=TRUE,scale=TRUE),
#          obs = scale(as.numeric(obs),center=TRUE,scale=TRUE))

df_all <- df_all %>%
  mutate(MAP.mm = ifelse(is.na(MAP.mm), MAP.mm.wc, MAP.mm)) %>%
  mutate(MAT.C = ifelse(is.na(MAT.C), MAT.C.wc, MAT.C)) %>%
  mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  mutate(varGroup = case_when(varGroup=="carbon"~"Carbon-related",
                              varGroup=="water"~"Water-related")) %>%
  mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
         Pulse.type = as.factor(Pulse.type),
         response_cat = as.numeric(response_cat))

df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/footprint"))

summary(lm(MAP.mm~MAT.C, data = df_all)) # MAP and MAT are very not correlated, Multiple R-squared:  1.261e-05,	Adjusted R-squared:  -0.001697 
summary(lm(Pulse.amount.mm~MAP.mm, data = df_all)) # Multiple R-squared:  0.03441,	Adjusted R-squared:  0.03276

## Some summary stats
df_all %>%
  count(response_cat)
# 1            1   308
# 2            2    14
# 3            3   120
# 4            4   145
df_all %>%
  count(mean_w==1)
#TRUE            241
df_all %>%
  count(mean_w==0)
#TRUE            265
df_all %>%
  count(mean_w!=0 & mean_w!=1)
#TRUE            81

df_all %>%
  group_by(varGroup) %>%
  count(response_cat)
# Groups:   varGroup [2]
# varGroup response_cat     n
# <fct>           <dbl> <int>
+   labs(x = "Pulse amount (mm)", y = "Timing of peak (days)", title = "(n)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
Warning message:
  Pulse.amount.mm and MAT.C are not included in an interaction with one another
in the model. 
> p3
> p3 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Timing of peak (days)", title = "(n)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p1.2 <- grid.arrange(patchworkGrob(p))
> library(tidyverse)
> library(ggforce) # for facet grids
> library(cowplot) # for ggsave2
> path_out = "./figures/" # set save path
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> df_all2 <- df_all %>%
  +   mutate(varGroup = as.factor(varGroup), 
             +          Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint")), 
             +          Pulse.type = as.factor(Pulse.type), 
             +          response_cat = as.factor(response_cat),
             +          varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                                              +                                varGroup == "water" ~ "Water-related"),
             +          varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                                             +                                                "ecosystemR", "belowgroundR",
                                                             +                                                "ET", "T", "Gs", "PWP")))
>          # varType2 = case_when(varType == "Anet" ~ "A[net]",
  >          #                      varType == "belowgroundR" ~ "R[below]",
  >          #                      varType == "ecosystemR" ~ "R[eco]",
  >          #                      varType == "Gs" ~ "g[s]",
  >          #                      varType == "PWP" ~ "Psi[plant]",
  >          #                      .default = as.character(varType)))
  > #df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))
  > tot_df <- df_all2 %>%
  +   group_by(varType, varGroup2) %>% 
  +   count()
> tot_df2 <- df_all2 %>%
  +   group_by(varGroup2, response_cat) %>%
  +   count()
> labs <- c("NPP", "GPP", "A[net]", "R[eco]", "R[below]",
            +           "ET", "T", "g[s]", "Psi[plant]")
> fig4 <- ggplot(df_all2, aes(x = varType)) +
  +   geom_bar(position = "stack", 
               +            aes(fill = response_cat)) +
  +   geom_text(data = tot_df,
                +             aes(x = varType,  y = n, label = n),
                +             vjust = -0.2,
                +             size = 4) +
  +   scale_y_continuous("Number of pulse events") +
  +   scale_x_discrete(labels = parse(text = labs), breaks = levels(df_all2$varType)) +
  +   scale_fill_brewer(palette = "PRGn",
                        +                     labels = c("classic", "intermediate", "linear", "no pulse")) +
  +   facet_row("varGroup2", scales = "free_x", space = "free",
                +             labeller = label_parsed) +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         axis.title.x = element_blank(),
            +         legend.title = element_blank(),
            +         legend.position = c(0.1, 0.8),
            +         legend.background = element_rect(fill = NA))
> fig4
> ggsave2("fig4.png", plot = fig4, path = path_out, width = 8, height = 4)
> ### Post-analysis analysis
  > # glm code
  > library(tidyverse)
> library(tibble)
> library(cowplot)
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> df_all <- df_all %>%
  +   mutate(MAP.mm = ifelse(is.na(MAP.mm), MAP.mm.wc, MAP.mm)) %>%
  +   mutate(MAT.C = ifelse(is.na(MAT.C), MAT.C.wc, MAT.C)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  +   mutate(varGroup = case_when(varGroup=="carbon"~"Carbon-related",
                                  +                               varGroup=="water"~"Water-related")) %>%
  +   mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
             +          Pulse.type = as.factor(Pulse.type),
             +          response_cat = as.numeric(response_cat))
> df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/footprint"))
> summary(lm(MAP.mm~MAT.C, data = df_all)) # MAP and MAT are very not correlated, Multiple R-squared:  1.261e-05,	Adjusted R-squared:  -0.001697 

Call:
  lm(formula = MAP.mm ~ MAT.C, data = df_all)

Residuals:
  Min      1Q  Median      3Q     Max 
-322.55 -181.94  -42.59  103.69  883.96 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 431.1124    29.2140  14.757   <2e-16 ***
  MAT.C        -0.4073     2.0307  -0.201    0.841    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 254.8 on 464 degrees of freedom
(121 observations deleted due to missingness)
Multiple R-squared:  8.668e-05,	Adjusted R-squared:  -0.002068 
F-statistic: 0.04022 on 1 and 464 DF,  p-value: 0.8411

> summary(lm(Pulse.amount.mm~MAP.mm, data = df_all)) # Multiple R-squared:  0.03441,	Adjusted R-squared:  0.03276

Call:
  lm(formula = Pulse.amount.mm ~ MAP.mm, data = df_all)

Residuals:
  Min      1Q  Median      3Q     Max 
-32.770 -14.038  -7.237  15.670 267.667 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 12.431458   2.368626   5.248 2.34e-07 ***
  MAP.mm       0.021632   0.004776   4.529 7.54e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.22 on 464 degrees of freedom
(121 observations deleted due to missingness)
Multiple R-squared:  0.04234,	Adjusted R-squared:  0.04027 
F-statistic: 20.51 on 1 and 464 DF,  p-value: 7.545e-06

> ## Some summary stats
  > df_all %>%
  +   count(response_cat)
# A tibble: 4 × 2
response_cat     n
<dbl> <int>
  1            1   308
2            2    14
3            3   120
4            4   145
> # 1            1   226
  > # 2            2    53
  > # 3            3   148
  > # 4            4   160
  > df_all %>%
  +   count(mean_w==1)
# A tibble: 2 × 2
`mean_w == 1`     n
<lgl>         <int>
  1 FALSE           346
2 TRUE            241
> #TRUE            198
  > df_all %>%
  +   count(mean_w==0)
# A tibble: 2 × 2
`mean_w == 0`     n
<lgl>         <int>
  1 FALSE           322
2 TRUE            265
> #TRUE            308
  > df_all %>%
  +   count(mean_w!=0 & mean_w!=1)
# A tibble: 2 × 2
`mean_w != 0 & mean_w != 1`     n
<lgl>                       <int>
  1 FALSE                         506
2 TRUE                           81
> df_all %>%
  +   group_by(varGroup) %>%
  +   count(response_cat)
# A tibble: 8 × 3
# Groups:   varGroup [2]
varGroup       response_cat     n
<fct>                 <dbl> <int>
  1 Carbon-related            1   245
2 Carbon-related            2     3
3 Carbon-related            3    58
4 Carbon-related            4    92
5 Water-related             1    63
6 Water-related             2    11
7 Water-related             3    62
8 Water-related             4    53
> library(ggpubr)
> library(tidyverse)
> library(cowplot)
> library(ggh4x)
> library(ggforce)
> # p value heatmap
  > df_p <- read.csv("data_output/p_mat.csv")
> rowp <- df_p$X
> df_p <- df_p %>%
  +   dplyr::select(-X)
> rownames(df_p) <- rowp
> p <-as.matrix(df_p)
> df_q <- read.csv("data_output/coeff_mat.csv")
> rowp <- df_q$X
> df_q <- df_q %>%
  +   dplyr::select(-X)
> rownames(df_q) <- rowp
> q <-as.matrix(df_q)
> # make non-significant coefficients 0
  > for(i in 1:nrow(p)){
    +   for(j in 1:ncol(p)){
      +     if(!is.na(p[i,j])){
        +       if(p[i,j]>0.05){ # if the p-value is not significant
          +         q[i,j]<-0 # then make the coefficient 0
          +       }
        +     }
      +   }
    + }
> # Reorganize as a data frame and rename columns
  > df <- as.data.frame(q)
> df <- df %>%
  +   rownames_to_column() %>%
  +   pivot_longer(c(2:5)) %>%
  +   rename(question = name, coefficient = rowname)
> # Create labels
  > df <- df %>%
  +   mutate(label = ifelse(value>0, "positive", "negative")) %>%
  +   mutate(label = ifelse(value==0, "nonsignificant", label)) %>%
  +   mutate(value = ifelse(value==0, NA, value))
> # Manually add in reference labels for reading ease
  > df_temp <- data.frame(coefficient = c("Carbon-related","Carbon-related","Carbon-related","Carbon-related",
                                          +                                       "Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf"),
                          +                       question = c("GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response",
                                                               +                                    "GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response"),
                          +                       value = c(NA,NA,NA,NA,
                                                            +                                 NA,NA,NA,NA),
                          +                       label = c("nonsignificant", "positive", "negative","nonsignificant",
                                                            +                                 "negative", "positive", "positive","nonsignificant"))
> df <- rbind(df,df_temp)
> df$coefficient <- factor(df$coefficient, levels =  c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                                       +                                                      "Carbon-related",
                                                       +                                                      "(1) Carbon-related (2) water-related",
                                                       +                                                      "Pulse amount",
                                                       +                                                      "MAT",
                                                       +                                                      "MAP",
                                                       +                                                      "Spatial scale: leaf",
                                                       +                                                      "Sample.unitindividual",
                                                       +                                                      "Sample.unitplot/footprint"),
                           +                          labels = c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                                                 +                                     "Carbon-related",
                                                                 +                                     "Water-related",
                                                                 +                                     "Pulse amount",
                                                                 +                                     "MAT",
                                                                 +                                     "MAP",
                                                                 +                                     "Spatial scale: leaf",
                                                                 +                                     "Spatial scale: individual",
                                                                 +                                     "Spatial scale: plot/footprint"))
> df <- df %>%
  +   mutate(coefficient.label = case_when(coefficient == "Spatial scale: leaf" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "Spatial scale: individual" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "Spatial scale: plot/footprint" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "MAP x MAT" ~ "Interactions",
                                           +                                        coefficient == "MAP x carbon- or water-related" ~ "Interactions",
                                           +                                        coefficient == "Pulse amount x carbon- or water-related" ~ "Interactions",
                                           +                                        coefficient == "Pulse amount" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "Water-related" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "Carbon-related" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "MAP" ~ "Site Covariates",
                                           +                                        coefficient == "MAT" ~ "Site Covariates"),
             +          question.label = case_when(question == "GLM.for.response.or.no.response" ~ "Question 1",
                                                   +                                     question == "GLM.for.time.of.peak" ~ "Question 2",
                                                   +                                     question == "GLM.for.magnitude.of.peak" ~ "Question 2",
                                                   +                                     question == "GLM.for.speed.of.linear.response" ~ "Question 2"))
> df$coefficient.label <- factor(df$coefficient.label, levels =  c("Interactions","Pulse-related Covariates",
                                                                   +                                                            "Site Covariates","Spatial Scale Covariates"))
> df$question <- factor(df$question, levels =  c("GLM.for.response.or.no.response","GLM.for.time.of.peak","GLM.for.magnitude.of.peak","GLM.for.speed.of.linear.response"),
                        +                          labels = c("GLM for response or no response","GLM for time of peak","GLM for magnitude of peak","GLM for speed of linear response"))
> p <- df %>%
  +   ggplot(aes(question, interaction(coefficient,coefficient.label, sep = "!"), fill = label)) +
  +   geom_tile(aes(width=.5, height=.7), size=1) +
  +   #geom_tile() +
  +   #geom_text(aes(label=round(value, digits=4))) +
  +   scale_fill_manual(values = c("blue", "darkgray","red")) +
  +   xlab("Response Variables") +
  +   #scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  +   scale_y_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  +   theme_bw() +
  +   theme(#panel.spacing = unit(0, "lines"),
    +     #plot.margin=margin(grid::unit(0, "cm")),
      +     #panel.border = element_blank(),
      +     panel.grid = element_blank(),
    +     #panel.spacing = element_blank(),
      +     legend.position = "top",
    +     strip.background = element_blank(),
    +     strip.placement = "outside",
    +     strip.text.y = element_blank(),
    +     text = element_text(size=14),
    +     legend.title = element_blank(),
    +     axis.text.x = element_text(size=14, angle = 90, vjust = 0.8, hjust = 1))
> p
> View(df_temp)
> library(ggpubr)
> library(tidyverse)
> library(cowplot)
> library(ggh4x)
> library(ggforce)
> 
  > # p value heatmap
  > df_p <- read.csv("data_output/p_mat.csv")
> 
  > rowp <- df_p$X
> df_p <- df_p %>%
  +   dplyr::select(-X)
> rownames(df_p) <- rowp
> p <-as.matrix(df_p)
> # par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
  > # #p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
  > # plot(as.pvalue(p), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
  > 
  > # coeff heatmap
  > 
  > df_q <- read.csv("data_output/coeff_mat.csv")
> 
  > rowp <- df_q$X
> df_q <- df_q %>%
  +   dplyr::select(-X)
> rownames(df_q) <- rowp
> q <-as.matrix(df_q)
> 
  > 
  > # make non-significant coefficients 0
  > for(i in 1:nrow(p)){
    +   for(j in 1:ncol(p)){
      +     if(!is.na(p[i,j])){
        +       if(p[i,j]>0.05){ # if the p-value is not significant
          +         q[i,j]<-0 # then make the coefficient 0
          +       }
        +     }
      +   }
    + }
> 
  > 
  > # Reorganize as a data frame and rename columns
  > df <- as.data.frame(q)
> df <- df %>%
  +   rownames_to_column() %>%
  +   pivot_longer(c(2:5)) %>%
  +   rename(question = name, coefficient = rowname)
> 
  > # Create labels
  > df <- df %>%
  +   mutate(label = ifelse(value>0, "positive", "negative")) %>%
  +   mutate(label = ifelse(value==0, "nonsignificant", label)) %>%
  +   mutate(value = ifelse(value==0, NA, value))
> View(df)
> View(df_p)
> View(df_q)
> View(df_q1)
> View(df_q2.t)
> View(df_q2.y)
> library(ggpubr)
> library(tidyverse)
> library(cowplot)
> library(ggh4x)
> library(ggforce)
> 
  > # p value heatmap
  > df_p <- read.csv("data_output/p_mat.csv")
> 
  > rowp <- df_p$X
> df_p <- df_p %>%
  +   dplyr::select(-X)
> rownames(df_p) <- rowp
> p <-as.matrix(df_p)
> # par(mar=c(4.1, 9, 4.1, 4.1)) # adapt margins
  > # #p <- plot(as.pvalue(pval), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
  > # plot(as.pvalue(p), las = 1, fmt.cell='%.2f', col=c("#fcffa4","#f98e09","#bc3754","#57106e","#000004"))
  > 
  > # coeff heatmap
  > 
  > df_q <- read.csv("data_output/coeff_mat.csv")
> 
  > rowp <- df_q$X
> df_q <- df_q %>%
  +   dplyr::select(-X)
> rownames(df_q) <- rowp
> q <-as.matrix(df_q)
> 
  > 
  > # make non-significant coefficients 0
  > for(i in 1:nrow(p)){
    +   for(j in 1:ncol(p)){
      +     if(!is.na(p[i,j])){
        +       if(p[i,j]>0.05){ # if the p-value is not significant
          +         q[i,j]<-0 # then make the coefficient 0
          +       }
        +     }
      +   }
    + }
> 
  > 
  > # Reorganize as a data frame and rename columns
  > df <- as.data.frame(q)
> df <- df %>%
  +   rownames_to_column() %>%
  +   pivot_longer(c(2:5)) %>%
  +   rename(question = name, coefficient = rowname)
> 
  > # Create labels
  > df <- df %>%
  +   mutate(label = ifelse(value>0, "positive", "negative")) %>%
  +   mutate(label = ifelse(value==0, "nonsignificant", label)) %>%
  +   mutate(value = ifelse(value==0, NA, value))
> View(df)
> # Manually add in reference labels for reading ease (! remember to update in-between runs !)
  > df_temp <- data.frame(coefficient = c("Carbon-related","Carbon-related","Carbon-related","Carbon-related",
                                          +                                       "Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf","Spatial scale: leaf"),
                          +                       question = c("GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response",
                                                               +                                    "GLM.for.response.or.no.response","GLM.for.time.of.peak", "GLM.for.magnitude.of.peak", "GLM.for.speed.of.linear.response"),
                          +                       value = c(NA,NA,NA,NA,
                                                            +                                 NA,NA,NA,NA),
                          +                       label = c("nonsignificant", "nonsignificant", "positive","nonsignificant",
                                                            +                                 "nonsignificant", "positive", "positive","nonsignificant"))
> df <- rbind(df,df_temp)
> df$coefficient <- factor(df$coefficient, levels =  c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                                       +                                                      "Carbon-related",
                                                       +                                                      "(1) Carbon-related (2) water-related",
                                                       +                                                      "Pulse amount",
                                                       +                                                      "MAT",
                                                       +                                                      "MAP",
                                                       +                                                      "Spatial scale: leaf",
                                                       +                                                      "Sample.unitindividual",
                                                       +                                                      "Sample.unitplot/footprint"),
                           +                          labels = c("MAP x MAT","MAP x carbon- or water-related","Pulse amount x carbon- or water-related",
                                                                 +                                     "Carbon-related",
                                                                 +                                     "Water-related",
                                                                 +                                     "Pulse amount",
                                                                 +                                     "MAT",
                                                                 +                                     "MAP",
                                                                 +                                     "Spatial scale: leaf",
                                                                 +                                     "Spatial scale: individual",
                                                                 +                                     "Spatial scale: plot/footprint"))
> df <- df %>%
  +   mutate(coefficient.label = case_when(coefficient == "Spatial scale: leaf" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "Spatial scale: individual" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "Spatial scale: plot/footprint" ~ "Spatial Scale Covariates",
                                           +                                        coefficient == "MAP x MAT" ~ "Interactions",
                                           +                                        coefficient == "MAP x carbon- or water-related" ~ "Interactions",
                                           +                                        coefficient == "Pulse amount x carbon- or water-related" ~ "Interactions",
                                           +                                        coefficient == "Pulse amount" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "Water-related" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "Carbon-related" ~ "Pulse-related Covariates",
                                           +                                        coefficient == "MAP" ~ "Site Covariates",
                                           +                                        coefficient == "MAT" ~ "Site Covariates"),
             +          question.label = case_when(question == "GLM.for.response.or.no.response" ~ "Question 1",
                                                   +                                     question == "GLM.for.time.of.peak" ~ "Question 2",
                                                   +                                     question == "GLM.for.magnitude.of.peak" ~ "Question 2",
                                                   +                                     question == "GLM.for.speed.of.linear.response" ~ "Question 2"))
> df$coefficient.label <- factor(df$coefficient.label, levels =  c("Interactions","Pulse-related Covariates",
                                                                   +                                                            "Site Covariates","Spatial Scale Covariates"))
> df$question <- factor(df$question, levels =  c("GLM.for.response.or.no.response","GLM.for.time.of.peak","GLM.for.magnitude.of.peak","GLM.for.speed.of.linear.response"),
                        +                          labels = c("GLM for response or no response","GLM for time of peak","GLM for magnitude of peak","GLM for speed of linear response"))
> p <- df %>%
  +   ggplot(aes(question, interaction(coefficient,coefficient.label, sep = "!"), fill = label)) +
  +   geom_tile(aes(width=.5, height=.7), size=1) +
  +   #geom_tile() +
  +   #geom_text(aes(label=round(value, digits=4))) +
  +   scale_fill_manual(values = c("blue", "darkgray","red")) +
  +   xlab("Response Variables") +
  +   #scale_x_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  +   scale_y_discrete(guide = guide_axis_nested(delim = "!"), name = NULL) +
  +   theme_bw() +
  +   theme(#panel.spacing = unit(0, "lines"),
    +     #plot.margin=margin(grid::unit(0, "cm")),
      +     #panel.border = element_blank(),
      +     panel.grid = element_blank(),
    +     #panel.spacing = element_blank(),
      +     legend.position = "top",
    +     strip.background = element_blank(),
    +     strip.placement = "outside",
    +     strip.text.y = element_blank(),
    +     text = element_text(size=14),
    +     legend.title = element_blank(),
    +     axis.text.x = element_text(size=14, angle = 90, vjust = 0.8, hjust = 1))
> p
> ggsave2("fig5v2.png", plot = p, path = "./figures/", width = 8, height = 8.7)
> library(tidyverse)
> library(ggforce) # for facet grids
> library(cowplot) # for ggsave2
> path_out = "./figures/" # set save path
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> df_all2 <- df_all %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  +   mutate(varGroup = as.factor(varGroup), 
             +          Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/footprint")), 
             +          Pulse.type = as.factor(Pulse.type), 
             +          response_cat = as.factor(response_cat),
             +          varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                                              +                                varGroup == "water" ~ "Water-related"),
             +          varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                                             +                                               "ecosystemR", "belowgroundR",
                                                             +                                               "ET", "T", "Gs", "PWP")))
> # varType2 = case_when(varType == "Anet" ~ "A[net]",
  > #                      varType == "belowgroundR" ~ "R[below]",
  > #                      varType == "ecosystemR" ~ "R[eco]",
  > #                      varType == "Gs" ~ "g[s]",
  > #                      varType == "PWP" ~ "Psi[plant]",
  > #                      .default = as.character(varType)))
  > #df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))
  > tot_df <- df_all2 %>% 
  +   group_by(varType, varGroup2) %>%
  +   count()
> labs <- c("classic", "intermediate", "linear" , "no pulse")
> figS1.1 <- df_all2 %>%
  +   filter(varGroup2=="Carbon-related") %>%
  +   ggplot() +
  +   geom_bar(position = "stack", 
               +            aes(x = response_cat,
                                +                fill = Sample.unit)) +
  +   facet_wrap(~varGroup2,
                 +              labeller = label_parsed) +
  +   scale_x_discrete(labels = labs) +
  +   scale_fill_brewer(palette = "Paired",
                        +                     direction = -1) +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         axis.title.x = element_blank(),
            +         axis.text.x = element_text(angle = 45,
                                                 +                                    hjust = 1,
                                                 +                                    vjust = 1),
            +         legend.title = element_blank(),
            +         legend.position = c(0.6, 0.8),
            +         legend.background = element_rect(fill = NA))
> figS1.2 <-  df_all2 %>%
  +   filter(varGroup2=="Water-related") %>%
  +   ggplot() +
  +   geom_bar(position = "stack", 
               +            aes(x = response_cat,
                                +                fill = Sample.unit)) +
  +   facet_wrap(~varGroup2,
                 +              labeller = label_parsed) +
  +   scale_x_discrete(labels = labs) +
  +   scale_fill_brewer(palette = "Paired",
                        +                     direction = -1) +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         axis.title.x = element_blank(),
            +         axis.text.x = element_text(angle = 45,
                                                 +                                    hjust = 1,
                                                 +                                    vjust = 1),
            +         legend.title = element_blank(),
            +         legend.position = "none")
> figS1 <- plot_grid(figS1.1,figS1.2,
                     +                    nrow = 1,
                     +                    align = "h",
                     +                    labels = "AUTO")
> figS1
> View(df_all)
> # Figure 4. Histogram? of t.peak and y.peaks for each variable
  > 
  > library(tidyverse)
> library(ggforce) # for facet grids
> library(cowplot) # for ggsave2
> library(ggh4x)
> 
  > path_out = "./figures/" # set save path
> 
  > # Load output data
  > load("data_output/df_all.Rdata") # df_all
> 
  > df_all2 <- df_all %>%
  +   mutate(varGroup = as.factor(varGroup), 
             +          Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint")), 
             +          Pulse.type = as.factor(Pulse.type), 
             +          response_cat = as.factor(response_cat),
             +          varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                                              +                                varGroup == "water" ~ "Water-related"),
             +          varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                                             +                                               "ecosystemR", "belowgroundR",
                                                             +                                               "ET", "T", "Gs", "PWP"))) %>% 
  +   pivot_longer(cols = c(mean_y.peak, mean_t.peak, mean_mm),
                   +                names_to = "param") %>%
  +   mutate(param = case_when(param == "mean_t.peak" ~ "t[peak]~(days)", 
                               +                            param == "mean_y.peak" ~ "y[peak]",
                               +                            param == "mean_mm" ~ "slope"),
             +          response_cat_name = case_when(response_cat == 1 ~ "classic", 
                                                      +                                        response_cat == 2 ~ "intermediate",
                                                      +                                        response_cat == 3 ~ "linear",
                                                      +                                        response_cat == 4 ~ "no pulse"))
> 
  > 
  > df_sum <- df_all2 %>%
  +   group_by(varGroup2, varType, param) %>% 
  +   summarize(param_m = mean(value, na.rm = TRUE),
                +             param_sd = sd(value, na.rm = TRUE))
`summarise()` has grouped output by 'varGroup2', 'varType'. You can override
using the `.groups` argument.
> 
  > labs <- c("NPP", "GPP", "A[net]", "R[eco]", "R[below]",
              +           "ET", "T", "g[s]", "Psi[plant]")
> 
  > p <- df_all2 %>%
  +   ggplot() +
  +   geom_jitter(data = df_all2, 
                  +               aes(x = varType, y = value,
                                      +                   color = response_cat_name),
                  +               width = 0.1, alpha = 0.4) +
  +   geom_errorbar(data = df_sum,
                    +                 aes(x = varType, ymin = param_m - param_sd,
                                          +                     ymax = param_m + param_sd),
                    +                 width = 0.1) +
  +   geom_point(data = df_sum,
                 +              aes(x = varType,y = param_m)) +
  +   # facet_wrap2(cols = vars(varType),
  +   #            rows = vars(param), scales = "free", axes = "all",
  +   #            labeller = label_parsed,
  +   #            switch = "y") +
  +   facet_grid2(vars(param),vars(varType), scales = "free",labeller = label_parsed, independent = "y", switch="y") +
  +   scale_x_discrete(labels = parse(text = labs), breaks = levels(df_all2$varType)) +
  +   scale_color_brewer(palette = "Dark2") +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         strip.placement = "outside",
            +         strip.text.x = element_blank(),
            +         axis.title = element_blank(),
            +         legend.title = element_blank(),
            +         legend.position = "top",
            +         legend.background = element_rect(fill = NA))
> p
Warning message:
  Removed 771 rows containing missing values or values outside the scale range
(`geom_point()`). 
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> View(df_all)
> df_all <- df_all %>%
  +   drop_na(sID)
> df_all2 <- df_all %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  +   mutate(varGroup = as.factor(varGroup), 
             +          Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/footprint")), 
             +          Pulse.type = as.factor(Pulse.type), 
             +          response_cat = as.factor(response_cat),
             +          varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                                              +                                varGroup == "water" ~ "Water-related"),
             +          varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                                             +                                               "ecosystemR", "belowgroundR",
                                                             +                                               "ET", "T", "Gs", "PWP")))
> # varType2 = case_when(varType == "Anet" ~ "A[net]",
  > #                      varType == "belowgroundR" ~ "R[below]",
  > #                      varType == "ecosystemR" ~ "R[eco]",
  > #                      varType == "Gs" ~ "g[s]",
  > #                      varType == "PWP" ~ "Psi[plant]",
  > #                      .default = as.character(varType)))
  > #df_all$varType <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))
  > tot_df <- df_all2 %>% 
  +   group_by(varType, varGroup2) %>%
  +   count()
> labs <- c("classic", "intermediate", "linear" , "no pulse")
> figS1.1 <- df_all2 %>%
  +   filter(varGroup2=="Carbon-related") %>%
  +   ggplot() +
  +   geom_bar(position = "stack", 
               +            aes(x = response_cat,
                                +                fill = Sample.unit)) +
  +   facet_wrap(~varGroup2,
                 +              labeller = label_parsed) +
  +   scale_x_discrete(labels = labs) +
  +   scale_fill_brewer(palette = "Paired",
                        +                     direction = -1) +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         axis.title.x = element_blank(),
            +         axis.text.x = element_text(angle = 45,
                                                 +                                    hjust = 1,
                                                 +                                    vjust = 1),
            +         legend.title = element_blank(),
            +         legend.position = c(0.6, 0.8),
            +         legend.background = element_rect(fill = NA))
> figS1.2 <-  df_all2 %>%
  +   filter(varGroup2=="Water-related") %>%
  +   ggplot() +
  +   geom_bar(position = "stack", 
               +            aes(x = response_cat,
                                +                fill = Sample.unit)) +
  +   facet_wrap(~varGroup2,
                 +              labeller = label_parsed) +
  +   scale_x_discrete(labels = labs) +
  +   scale_fill_brewer(palette = "Paired",
                        +                     direction = -1) +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         axis.title.x = element_blank(),
            +         axis.text.x = element_text(angle = 45,
                                                 +                                    hjust = 1,
                                                 +                                    vjust = 1),
            +         legend.title = element_blank(),
            +         legend.position = "none")
> figS1 <- plot_grid(figS1.1,figS1.2,
                     +                    nrow = 1,
                     +                    align = "h",
                     +                    labels = "AUTO")
> ggsave2("figS1.png", plot = figS1, 
          +         path = path_out, width = 8, height = 4)
> figS1
> library(tidyverse)
> library(ggforce) # for facet grids
> library(cowplot) # for ggsave2
> library(ggh4x)
> path_out = "./figures/" # set save path
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> df_all2 <- df_all %>%
  +   mutate(varGroup = as.factor(varGroup), 
             +          Sample.unit = factor(Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint")), 
             +          Pulse.type = as.factor(Pulse.type), 
             +          response_cat = as.factor(response_cat),
             +          varGroup2 = case_when(varGroup == "carbon" ~ "Carbon-related",
                                              +                                varGroup == "water" ~ "Water-related"),
             +          varType = factor(varType, levels = c("NPP", "GPP", "Anet",
                                                             +                                               "ecosystemR", "belowgroundR",
                                                             +                                               "ET", "T", "Gs", "PWP"))) %>% 
  +   pivot_longer(cols = c(mean_y.peak, mean_t.peak, mean_mm),
                   +                names_to = "param") %>%
  +   mutate(param = case_when(param == "mean_t.peak" ~ "t[peak]~(days)", 
                               +                            param == "mean_y.peak" ~ "y[peak]",
                               +                            param == "mean_mm" ~ "slope"),
             +          response_cat_name = case_when(response_cat == 1 ~ "classic", 
                                                      +                                        response_cat == 2 ~ "intermediate",
                                                      +                                        response_cat == 3 ~ "linear",
                                                      +                                        response_cat == 4 ~ "no pulse"))
> df_sum <- df_all2 %>%
  +   group_by(varGroup2, varType, param) %>% 
  +   summarize(param_m = mean(value, na.rm = TRUE),
                +             param_sd = sd(value, na.rm = TRUE))
`summarise()` has grouped output by 'varGroup2', 'varType'. You can override using
the `.groups` argument.
> labs <- c("NPP", "GPP", "A[net]", "R[eco]", "R[below]",
            +           "ET", "T", "g[s]", "Psi[plant]")
> p <- df_all2 %>%
  +   ggplot() +
  +   geom_jitter(data = df_all2, 
                  +               aes(x = varType, y = value,
                                      +                   color = response_cat_name),
                  +               width = 0.1, alpha = 0.4) +
  +   geom_errorbar(data = df_sum,
                    +                 aes(x = varType, ymin = param_m - param_sd,
                                          +                     ymax = param_m + param_sd),
                    +                 width = 0.1) +
  +   geom_point(data = df_sum,
                 +              aes(x = varType,y = param_m)) +
  +   # facet_wrap2(cols = vars(varType),
  +   #            rows = vars(param), scales = "free", axes = "all",
  +   #            labeller = label_parsed,
  +   #            switch = "y") +
  +   facet_grid2(vars(param),vars(varType), scales = "free",labeller = label_parsed, independent = "y", switch="y") +
  +   scale_x_discrete(labels = parse(text = labs), breaks = levels(df_all2$varType)) +
  +   scale_color_brewer(palette = "Dark2") +
  +   theme_bw(base_size = 14) +
  +   theme(panel.grid = element_blank(),
            +         strip.background = element_blank(),
            +         strip.placement = "outside",
            +         strip.text.x = element_blank(),
            +         axis.title = element_blank(),
            +         legend.title = element_blank(),
            +         legend.position = "top",
            +         legend.background = element_rect(fill = NA))
> p
Warning message:
  Removed 771 rows containing missing values or values outside the scale range
(`geom_point()`). 
> ggsave2("figS2.png", plot = p, path = path_out, width = 8, height = 4)
Warning message:
  Removed 771 rows containing missing values or values outside the scale range
(`geom_point()`). 
> # Basic Interaction Plot 
  > library(interactions)
> p1 <- interact_plot(q2.t_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "MAP (mm)", y = "Timing of peak (days)", title = "(A)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14),
            +         panel.grid.major = element_blank(),
            +         panel.grid.minor = element_blank())
> p1
> p1 <- interact_plot(q2.y_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "MAP (mm)", y = "Timing of peak (days)", title = "(A)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14),
            +         panel.grid.major = element_blank(),
            +         panel.grid.minor = element_blank())
> p1
> p3 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Timing of peak (days)", title = "(n)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p3 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Magnitude of peak", title = "(n)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p1 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Magnitude of peak", title = "(A)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14),
            +         panel.grid.major = element_blank(),
            +         panel.grid.minor = element_blank())
> p1
> p1 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Magnitude of peak", title = "(A)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14),
            +         panel.grid.major = element_blank(),
            +         panel.grid.minor = element_blank())
> p1
> p2 <- interact_plot(q2.y_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "MAP (mm)", y = "Magnitude of peak", title = "(B)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p2
> p3 <- interact_plot(q2.t_glm, pred = MAT.C, modx = MAP.mm, interval = T, legend.main = "MAP", colors = c("#bdd7e7","#6baed6","#2171b5")) +
  +   labs(x = "MAT (C)", y = "Timing of peak (days)", title = "(C)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p3 <- interact_plot(q2.t_glm, pred = MAP.mm, modx = MAT.C, interval = T, legend.main = "MAP", colors = c("#bdd7e7","#6baed6","#2171b5")) +
  +   labs(x = "MAT (C)", y = "Timing of peak (days)", title = "(C)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p1 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "Pulse amount (mm)", y = "Magnitude of peak", title = "(A)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14),
            +         panel.grid.major = element_blank(),
            +         panel.grid.minor = element_blank())
> p1
> p2 <- interact_plot(q2.y_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  +   labs(x = "MAP (mm)", y = "Magnitude of peak", title = "(B)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p2
> p3 <- interact_plot(q2.t_glm, pred = MAT.C, modx = MAP.mm, interval = T, legend.main = "MAP", colors = c("#bdd7e7","#6baed6","#2171b5")) +
  +   labs(x = "MAT (C)", y = "Timing of peak (days)", title = "(C)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> p3 <- interact_plot(q2.t_glm, pred = MAP.mm, modx = MAT.C, interval = T, legend.main = "MAT", colors = c("#bdd7e7","#6baed6","#2171b5")) +
  +   labs(x = "MAP (mm)", y = "Timing of peak (days)", title = "(C)") +
  +   #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  +   theme_bw() +
  +   theme(text = element_text(size=14))
> p3
> library(gridExtra)
> library(patchwork)
> library(grid)
> layout <- '
+ ABC
+ '
> p <- wrap_plots(A = p1, B = p2, C = p3, design = layout)
> p <- p + plot_layout(guides = "collect") & theme(legend.position = "bottom")
> p1.2 <- grid.arrange(patchworkGrob(p))
> path_out = "./figures/" # set save path
> ggsave2("fig6abc.png", plot = p1.2, path = path_out, width = 8, height = 3)
> ### Post-analysis analysis
  > # glm code
  > library(tidyverse)
> library(tibble)
> library(cowplot)
> # Load output data
  > load("data_output/df_all.Rdata") # df_all
> df_all <- df_all %>%
  +   mutate(MAP.mm = ifelse(is.na(MAP.mm), MAP.mm.wc, MAP.mm)) %>%
  +   mutate(MAT.C = ifelse(is.na(MAT.C), MAT.C.wc, MAT.C)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="plot/collar", "plot/footprint", Sample.unit)) %>%
  +   mutate(Sample.unit = ifelse(Sample.unit=="footprint", "plot/footprint", Sample.unit)) %>%
  +   mutate(varGroup = case_when(varGroup=="carbon"~"Carbon-related",
                                  +                               varGroup=="water"~"Water-related")) %>%
  +   mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit),
             +          Pulse.type = as.factor(Pulse.type),
             +          response_cat = as.numeric(response_cat))
> df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/footprint"))
> summary(lm(MAP.mm~MAT.C, data = df_all)) # MAP and MAT are very not correlated, Multiple R-squared:  1.261e-05,	Adjusted R-squared:  -0.001697 

Call:
  lm(formula = MAP.mm ~ MAT.C, data = df_all)

Residuals:
  Min      1Q  Median      3Q     Max 
-322.55 -181.94  -42.59  103.69  883.96 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 431.1124    29.2140  14.757   <2e-16 ***
  MAT.C        -0.4073     2.0307  -0.201    0.841    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 254.8 on 464 degrees of freedom
(121 observations deleted due to missingness)
Multiple R-squared:  8.668e-05,	Adjusted R-squared:  -0.002068 
F-statistic: 0.04022 on 1 and 464 DF,  p-value: 0.8411

> summary(lm(Pulse.amount.mm~MAP.mm, data = df_all)) # Multiple R-squared:  0.03441,	Adjusted R-squared:  0.03276

Call:
  lm(formula = Pulse.amount.mm ~ MAP.mm, data = df_all)

Residuals:
  Min      1Q  Median      3Q     Max 
-32.770 -14.038  -7.237  15.670 267.667 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 12.431458   2.368626   5.248 2.34e-07 ***
  MAP.mm       0.021632   0.004776   4.529 7.54e-06 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.22 on 464 degrees of freedom
(121 observations deleted due to missingness)
Multiple R-squared:  0.04234,	Adjusted R-squared:  0.04027 
F-statistic: 20.51 on 1 and 464 DF,  p-value: 7.545e-06

> ## Some summary stats
  > df_all %>%
  +   count(response_cat)
# A tibble: 4 × 2
response_cat     n
<dbl> <int>
  1            1   308
2            2    14
3            3   120
4            4   145
> 226+53+148+160
[1] 587
> 308+14+120+145
[1] 587
> # 1            1   308
  > # 2            2    14
  > # 3            3   120
  > # 4            4   145
  > df_all %>%
  +   count(mean_w==1)
# A tibble: 2 × 2
`mean_w == 1`     n
<lgl>         <int>
  1 FALSE           346
2 TRUE            241
> #TRUE            241
  > df_all %>%
  +   count(mean_w==0)
# A tibble: 2 × 2
`mean_w == 0`     n
<lgl>         <int>
  1 FALSE           322
2 TRUE            265
> #TRUE            265
  > df_all %>%
  +   count(mean_w!=0 & mean_w!=1)
# A tibble: 2 × 2
`mean_w != 0 & mean_w != 1`     n
<lgl>                       <int>
  1 FALSE                         506
2 TRUE                           81
> df_all %>%
  +   group_by(varGroup) %>%
  +   count(response_cat)
# A tibble: 8 × 3
# Groups:   varGroup [2]
# varGroup       response_cat     n
# <fct>                 <dbl> <int>
#   1 Carbon-related           1   245
# 2 Carbon-related            2     3
# 3 Carbon-related            3    58
# 4 Carbon-related            4    92
# 5 Water-related             1    63
# 6 Water-related             2    11
# 7 Water-related             3    62
# 8 Water-related             4    53
# Carbon 245/398 62%
# Water 59/189 31%

df_all %>%
  count(is.na(preSWC))
402/587 # 68% of SWC data is missing


df_all %>%
  group_by(varType) %>%
  summarise(scales = unique(Sample.unit))

# check corr !!!!!!
test <- lm(mean_t.peak~mean_y.peak, data = df_all)
summary(test) # 0.007002

df_all_test <- df_all %>%
  mutate(study_pulse = paste0(Study.ID, "_", Pulse.ID)) 
sp_list <- unique(df_all_test$study_pulse)
df_all_test <- df_all_test %>%
  filter(study_pulse %in% sp_list)
  
test <- lm(mean_t.peak~mean_y.peak, data = df_all_test)
summary(test)

# df_all_test <- df_all %>%
#   mutate(study_pulse = paste0(Study.ID, "_", Pulse.ID))
# sp_list <- unique(df_all_test$study_pulse)
# 
# list_out <- list()
# list_sp <- list()
# for(s in c(1:length(sp_list))){
#     df_temp <- df_all_test %>%
#       filter(study_pulse==sp_list[s]) %>%
#       drop_na(mean_y.peak) %>%
#       drop_na(mean_t.peak)
#       
#     
#     vars <- unique(df_temp$varType)
#     number_same <- length(vars)
#     
#     
#     if(number_same > 1){
#       test <- lm(mean_t.peak~mean_y.peak, data = df_temp)
#       print(summary(test)$r.squared)
#       
#       list_out[[s]] <- as.data.frame(summary(test)$r.squared)
#       list_sp[[s]] <- as.data.frame(sp_list[s])
# 
#     }
# }
# df_list <- bind_rows(list_out)
# df_sp <- bind_rows(list_sp)
# df <- cbind(df_list, df_sp)


# load("models/model_input.Rdata") # out_list
# out_all <- bind_rows(out_list)
# out_all <- out_all %>%
#   mutate(study_pulse = paste0(Study.ID, "_", Pulse.ID))
# sp_list <- unique(out_all$study_pulse)
# 
# list_out <- list()
# var1_out <- list()
# var2_out <- list()
# number_same <- list()
# for(s in c(1:length(sp_list))){
#   df_temp <- out_all %>%
#     filter(study_pulse==sp_list[s])
#   
#  vars <- unique(df_temp$varType)
#  number_same[s] <- length(vars) #2,3,4,6,7
#  
#  if(length(vars)==2){
#    var1 <- vars[1]
#    var2 <- vars[2]
#    
#    df_var1 <- df_temp %>%
#      filter(varType==var1)
#    
#    df_var2 <- df_temp %>%
#      filter(varType==var2)
#    
#    col1 = df_var1$LRR
#    col2 = df_var2$LRR
#    
#    if(length(col1) > length(col2)){
#      col1 = col1[1:length(col2)]
#    }
#    if(length(col2) > length(col1)){
#      col2 = col2[1:length(col1)]
#    }
#    
#    test <- lm(col1~col2)
#    #print(summary(test)$r.squared)
#    
#    list_out[[s]] <- as.data.frame(summary(test)$r.squared)
#    var1_out[[s]] <- as.data.frame(var1)
#    var2_out[[s]] <- as.data.frame(var2)
#    
#  }else{
#    
#  }
#  
# }
# 
# df_list <- bind_rows(list_out)
# df_var1<- bind_rows(var1_out)
# df_var2<- bind_rows(var2_out)
# df <- cbind(df_list, df_var1)
# df <- cbind(df, df_var2)
# mean(df[,1]) # 0.52
# sum(df[,1]>0.5) # 59/108
#write.csv(df, "./data_output/corr_between_LRR.csv")


## Question 1: Is there a pulse? (Q1 but cat responses are collapsed)
## Response variable of interest: response_cat (1-2)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
df_all1 <- df_all
df_all1$response_cat <- ifelse(df_all1$response_cat< 4, 1, 2)


q1_glm <- glm(as.factor(response_cat) ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all1, family = binomial) #Pulse.amount.mm*varGroup
sink("data_output/q1_glm.txt") # sink will save the summary output to a text file
summary(q1_glm)
sink() # end what we save to the text file

## Question 2: If there is a pulse, do t.peak (time to peak response) - NOTE: use 'lm" instead
## and y.peak (magnitude of peak response) depend on the variables of interest?
## Response of interest: y.peak and t.peak mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Filter for all data that is at least somewhat Ricker-like
df_all2 <- df_all %>%
  filter(mean_w != 0)

q2.t_glm <- glm(mean_t.peak ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all2) #+Pulse.amount.mm*Sample.unit
sink("data_output/q2.t_glm.txt")
summary(q2.t_glm)
sink()

q2.y_glm <- glm(mean_y.peak ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all2) #+Pulse.amount.mm*Sample.unit
sink("data_output/q2.y_glm.txt")
summary(q2.y_glm)
sink()

## Question 3: If there is a linear pulse response, how steep is mm (slope, speed of response) depending on the variables of interest?
## Response of interest:  mm mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR
## intercept should be 0, maybe look into

# Filter for all pulses with at least some linear characteristics
df_all3 <- df_all %>%
  filter(mean_w != 1) %>%
  mutate(mean_mm = abs(mean_mm)) # take absolute value, because we only care about steepness

q3_glm <- glm(mean_mm ~ varGroup+Sample.unit+MAT.C+MAP.mm+Pulse.amount.mm+MAP.mm*MAT.C+MAP.mm*varGroup+Pulse.amount.mm*varGroup, data = df_all3)
sink("data_output/q3_glm.txt")
summary(q3_glm)
sink()


q3_glm_coeff <- summary(q3_glm)$coef[,1]


#### Save tables in convenient format for plotting

# Data for figure of heatmap for p-values

df_q1 <- as.data.frame(summary(q1_glm)$coefficients[,4]) %>%
  rownames_to_column()
df_q1 <- df_q1[-1,]
colnames(df_q1) <- c("pred", "q1")

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,4]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred", "q2.t")

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred", "q2.y")

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred", "q3")


df_q <- full_join(df_q1, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "GLM for response or no response", "GLM for time of peak", "GLM for magnitude of peak", "GLM for speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  dplyr::select(-pred)
rownames(df_q) <- rowp
#[1] "varGroupWater-related"        "Sample.unitindividual"        "Sample.unitplot/collar"      
#[4] "Sample.unitfootprint"         "MAT.C"                        "MAP.mm"                      
#[7] "Pulse.typeNatural"            "Pulse.amount.mm"                      
#[10] "MAT.C:MAP.mm"                 "varGroupWater-related:MAP.mm" 
# "varGroupWater-related:Pulse.amount.mm"    
# [11] "Sample.unitindividual:Pulse.amount.mm"     "Sample.unitplot/footprint:Pulse.amount.mm"
rownames(df_q) <- c("(1) Carbon-related (2) water-related",
                    "Sample.unitindividual", 
                    "Sample.unitplot/footprint",
                    "MAT", 
                    "MAP",
                    "Pulse amount",
                    "MAP x MAT",
                    "MAP x carbon- or water-related",
                    "Pulse amount x carbon- or water-related")

write.csv(df_q, file = "data_output/p_mat.csv")


# Data for figure of heatmap for coefficients

df_q1 <- as.data.frame(summary(q1_glm)$coefficients[,c(1,4)])%>%
  rownames_to_column()
df_q1 <- df_q1[-1,]
colnames(df_q1) <- c("pred","est","q1")
df_q1$est <- ifelse(df_q1$q1>0.05, 0, df_q1$est)
df_q1 <- df_q1 %>%
  dplyr::select(c(1,2)) %>%
  rename(q1 = est)

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred","est", "q2.t")
df_q2.t$est <- ifelse(df_q2.t$q2.t>0.05, 0, df_q2.t$est)
df_q2.t <- df_q2.t %>%
  dplyr::select(c(1,2)) %>%
  rename(q2.t = est)

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred","est", "q2.y")
df_q2.y$est <- ifelse(df_q2.y$q2.y>0.05, 0, df_q2.y$est)
df_q2.y <- df_q2.y %>%
  dplyr::select(c(1,2)) %>%
  rename(q2.y = est)

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred","est", "q3")
df_q3$est <- ifelse(df_q3$q3>0.05, 0, df_q3$est)
df_q3 <- df_q3 %>%
  dplyr::select(c(1,2)) %>%
  rename(q3 = est)


df_q <- full_join(df_q1, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "GLM for response or no response", "GLM for time of peak", "GLM for magnitude of peak", "GLM for speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  dplyr::select(-pred)
rownames(df_q) <- rowp
rownames(df_q) <-  c("(1) Carbon-related (2) water-related",
                     "Sample.unitindividual", 
                     "Sample.unitplot/footprint",
                     "MAT", 
                     "MAP",
                     "Pulse amount",
                     "MAP x MAT",
                     "MAP x carbon- or water-related",
                     "Pulse amount x carbon- or water-related")

write.csv(df_q, file = "data_output/coeff_mat.csv")


# Basic Interaction Plot 
library(interactions)

p1 <- interact_plot(q2.y_glm, pred = Pulse.amount.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "Pulse amount (mm)", y = "Magnitude of peak", title = "(A)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
p1

p2 <- interact_plot(q2.y_glm, pred = MAP.mm, modx = varGroup, interval = T, legend.main = element_blank(), colors = "Dark2") +
  labs(x = "MAP (mm)", y = "Magnitude of peak", title = "(B)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14))
p2
p3 <- interact_plot(q2.t_glm, pred = MAP.mm, modx = MAT.C, interval = T, legend.main = "MAT", colors = c("#bdd7e7","#6baed6","#2171b5")) +
  labs(x = "MAP (mm)", y = "Timing of peak (days)", title = "(C)") +
  #coord_cartesian(ylim = c(0, 14), xlim = c(-.3,1.3)) +
  theme_bw() +
  theme(text = element_text(size=14))
p3


library(gridExtra)
library(patchwork)
library(grid)
layout <- '
ABC
'
p <- wrap_plots(A = p1, B = p2, C = p3, design = layout)
p <- p + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p1.2 <- grid.arrange(patchworkGrob(p))
path_out = "./figures/" # set save path
ggsave2("fig6abc.png", plot = p1.2, path = path_out, width = 8, height = 3)


