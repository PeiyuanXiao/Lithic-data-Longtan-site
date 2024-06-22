library("readxl")
library("dunn.test")
library("MetBrewer")

# import the data 
qn_scrapers <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1)
scraper <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Scrapers")
notch <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Notches")
denticulate <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Denticulates")
flakes <- read_excel("Longtan site lithic data-Flakes.xlsx", skip = 0)
qn_scrapers_reduction <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1, sheet = "Reduction")
qn_scrapers_Edge <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 2, sheet = "Edge")
resharpening <- read_excel("Longtan lithic data-Reaffutage.xlsx", skip = 1)

# Flakes detached from these cores show 
# significantly smaller interior platform angle than 
# other types of flakes

kruskal.test(flakes$IPA, factor(flakes$...1))
dunn.test(flakes$IPA, as.integer(factor(flakes$...1)), method = "bh")

# Quina flake dims ----------------------------------------------------------- 

# set the plot text size 
base_size_value <- 10

library(tidyverse)
library(apastats)
library("ggpmisc")
library(ggbeeswarm)

flakes <- read_excel("Longtan site lithic data-Flakes.xlsx", skip = 0)
resharpening <- read_excel("Longtan lithic data-Reaffutage.xlsx", skip = 1)

resharpening_clean <- 
  resharpening %>%
  select(`Length (mm)` = ...2,
         `Width (mm)` = `...3`,
         `Thickness (mm)` = `...4`,
         `Weight (mm)` = `...5`,
         `Interior platform angle` = `IPA`,
         `Platform length (mm)` = `Width`,
         `Platform width (mm)` = `Depth`) %>%
  mutate(typology = "Resharpening flakes") %>%
  drop_na()

flakes_clean <- 
  flakes %>%
  select(typology = ...1,
         `Length (mm)` = `长（mm）`,
         `Width (mm)` = `宽（mm）`,
         `Thickness (mm)` = `厚（mm）`,
         `Weight (mm)` = `重量（g）`,
         `Interior platform angle` = `IPA`,
         `Platform length (mm)` = `台面长（mm)`,
         `Platform width (mm)` = `台面厚（mm）`) %>%
  mutate(typology = case_when(
    typology ==  "Kombewa" ~ "Kombewa flakes", 
    typology ==  "Surface" ~ "Surface flakes",   
    typology ==  "Discoid" ~ "Discoidal flakes",
    typology ==  "Quina flake" ~ "Quina flakes",
  )) %>%
  drop_na() 

all_flakes_clean <- 
  bind_rows(resharpening_clean,
            flakes_clean) 

# Unretouched Quina flakes (n=15) have also been identified and 
# show significantly larger dimensions on both the overall
# size and the striking platform compared to other types of flakes

res.man <- 
  manova(cbind(`Length (mm)`,
               `Width (mm)`,
               `Thickness (mm)`,
               `Weight (mm)`,
               `Interior platform angle`,
               `Platform length (mm)`,
               `Platform width (mm)`) ~ typology, 
         data = all_flakes_clean)
summary(res.man)
summary.aov(res.man)

# thicknes -----------------------------------------------------

# Quina scrapers are the most diagnostic type and often show 
# significantly larger thickness values 

tools_df <-
  bind_rows(
    .id = "id",
    list(
      `Quina scraper` =
        qn_scrapers %>%
        select(Length, Breadth, Thickness, Weight),
      Scraper =
        scraper %>%
        select(Length, Breadth, Thickness, Weight),
      Notch =
        notch %>%
        select(Length, Breadth, Thickness, Weight),
      Denticulate  =
        denticulate %>%
        select(Length, Breadth, Thickness, Weight)
    )
  ) %>%
  drop_na()

kruskal.test(Thickness ~ factor(id), data = tools_df) 

# Resharpening-------------------------------------------------------

Resharpening_df <- 
  bind_rows(.id = "id",
            list( `Quina scrapers` = 
                    qn_scrapers_Edge %>%
                    select(a = ...20, b =  ...40, c = ...60) %>%
                    mutate(a = parse_number(a),
                           b = parse_number(b),
                           c = parse_number(c)) %>%
                    rowwise() %>%
                    mutate(angle = mean(c_across(c("a", "b", "c")), na.rm = TRUE)),
                  `Reshapening flakes` = 
                    resharpening %>%
                    select(angle = EPA) %>% drop_na()
            ))

# The median exterior platform angle for resharpening 
# flakes is 71°, with no significant difference observed 
# from the edge angle of Quina scrapers  

plot_resharpening <- 
  ggplot(Resharpening_df) +
  aes(x = id, 
      y = angle,
      color = id) +
  geom_boxplot(outliers = FALSE) +
  geom_quasirandom(alpha = 0.2, 
                   size =1.5) +
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2.5, 
             color = "black", 
             show.legend = FALSE) +
  scale_color_manual(values = c(met.brewer("Nattier", 4)[1:2])) +
  xlab("") +
  ylab("Edge angle & EPA") +
  theme_minimal(base_size = base_size_value) +
  theme(legend.position = "none")

t.test(angle ~ factor(id), data = Resharpening_df )

# Quina scraper edge angle ----------------------------------------------------------

# Quina scrapers generally have larger edge angles 
# (mean=70.4°; sd=7.6°), which are significantly higher
# than that of ordinary scrapers

Edge_angle_df <- 
  bind_rows(.id = "id",
            list( `Quina scrapers` = 
                    qn_scrapers_Edge %>%
                    select(a = ...20, b =  ...40, c = ...60) %>%
                    mutate(a = parse_number(a),
                           b = parse_number(b),
                           c = parse_number(c)) %>%
                    rowwise() %>%
                    mutate(ave = mean(c_across(c("a", "b", "c")), na.rm = TRUE)),
                  Scraper = 
                    scraper %>%
                    mutate(ave = parse_number(...20)) %>%
                    select(ave)
            ))

plot_edge_angle <- 
  ggplot(Edge_angle_df, 
         aes(x = id, y = ave, color = id)) +
  geom_boxplot(outliers = FALSE) +
  geom_quasirandom(alpha = 0.2, 
                   size =1.5) +
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2.5, 
             color = "black", 
             show.legend = FALSE) +
  scale_color_manual(values = c(met.brewer("Nattier", 4)[1:2])) +
  xlab("") +
  ylab("Edge angle") +
  theme_minimal(base_size = base_size_value) +
  theme(legend.position = "none")

ggsave(filename = "RR.png", width = 6, height = 8, dpi = 800, bg = "white")

t.test(ave ~ factor(id), data = Edge_angle_df) 

# Reduction intensity------------------------------------------------

# the reduction intensity of Quina scrapers, the results show a 
# median GIUR value of 0.81, significantly higher than that of 
# ordinary scrapers

giur_df <- 
  bind_rows(.id = "id",
            list( `Quina scraper` = 
                    qn_scrapers_reduction %>%
                    select(...5),
                  Scraper = 
                    scraper %>%
                    select(...33)
            )) %>%
  mutate(giur = parse_number(...1))


plot_giur <- 
  ggplot(giur_df) +
  aes(id, giur, color = id) +
  geom_boxplot(outliers = FALSE) +
  geom_quasirandom(alpha = 0.2, 
                   size =1.5) +
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2.5, 
             color = "black", 
             show.legend = FALSE) +
  scale_color_manual(values = c(met.brewer("Nattier", 4)[1:2])) +
  xlab("") +
  ylab("Mean GIUR") +
  theme_minimal(base_size = base_size_value) +
  theme(legend.position = "none")

ggsave(filename = "GIUR.png", width = 6, height = 8, dpi = 800, bg = "white")

t.test(giur_df$giur, as.integer(factor(giur_df$id)))


# Thickness-------------------------------------------------

tools_df$id <- factor(tools_df$id, levels = c("Quina scraper", 
                                              "Scraper", 
                                              "Denticulate", 
                                              "Notch"))

plot_thick <- 
ggplot(tools_df) +
  aes(reorder(id, -Thickness), 
      Thickness, 
      color = id) +
  geom_boxplot(outliers = FALSE) +
  geom_quasirandom(alpha = 0.2, size =1.5) +
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 2.5, 
             color = "black", 
             show.legend = FALSE) +
  scale_color_manual(values = c(met.brewer("Nattier", 4))) +
  xlab("") +
  ylab("Thickness (mm)") +
  theme_minimal(base_size = base_size_value) +
  theme(legend.position = "none")
  

 

library(ggbeeswarm)   

tools_df %>%
  pivot_longer(-id) %>%
  ggplot() +
  aes(id, value) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  facet_wrap( ~ name, scales = "free_y")



#combine plots into one panel
library(cowplot)

plot_grid(plot_thick,
          plot_edge_angle,
          plot_giur,
          plot_resharpening,
           nrow = 1
          )

ggsave(filename = "panel_thick_edgeangle_retouch.png", 
       width = 17, height = 3, dpi = 800, bg = "white")

