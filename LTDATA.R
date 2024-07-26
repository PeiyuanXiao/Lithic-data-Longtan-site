library("readxl")
library("dunn.test")
library("ggdist")
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

# Thickness -----------------------------------------------------

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

remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
}

Quina_scrapers_df <- qn_scrapers_Edge %>%
  select(a = ...20, b = ...40, c = ...60) %>%
  mutate(a = parse_number(a),
         b = parse_number(b),
         c = parse_number(c)) %>%
  rowwise() %>%
  mutate(angle = mean(c_across(c("a", "b", "c")), na.rm = TRUE)) %>%
  ungroup() %>%
  remove_outliers("angle") %>%
  mutate(id = "Quina scrapers")

Reshapening_flakes_df <- resharpening %>%
  select(angle = EPA) %>%
  drop_na() %>%
  remove_outliers("angle") %>%
  mutate(id = "Resharpening flakes")
Resharpening_df <- bind_rows(Quina_scrapers_df, Reshapening_flakes_df)
Resharpening_df$id <- factor(Resharpening_df$id, levels = c("Quina scrapers", "Resharpening flakes"))


# The median exterior platform angle for resharpening 
# flakes is 71°, with no significant difference observed 
# from the edge angle of Quina scrapers  

colors <- c("#68A7BE", "#EE7E77")

plot_resharpening <- 
  ggplot(Resharpening_df, fill = id, aes(x = id, y = angle)) +
  geom_errorbar(mapping = aes(color = id), stat = "boxplot", width = 0.1, linewidth = 1, position = position_dodge(width = 0.1)) +
  geom_boxplot(mapping = aes(color = id), width = 0.5, size = 1, outlier.shape = NA, alpha = 1) +  
  geom_quasirandom(mapping = aes(color = id), cex = 3, size = 2, alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 3, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_discrete(limits = c("Quina scrapers", "Resharpening flakes"), 
                   expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Edge angle/EPA (°)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")

t.test(angle ~ factor(id), data = Resharpening_df )

# Quina scraper edge angle ----------------------------------------------------------

# Quina scrapers generally have larger edge angles 
# (mean=70.4°; sd=7.6°), which are significantly higher
# than that of ordinary scrapers

remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df %>% filter(df[[col]] >= lower_bound & df[[col]] <= upper_bound)
}

Quina_scrapers_df <- qn_scrapers_Edge %>%
  select(a = ...20, b = ...40, c = ...60) %>%
  mutate(a = parse_number(a),
         b = parse_number(b),
         c = parse_number(c)) %>%
  rowwise() %>%
  mutate(ave = mean(c_across(c("a", "b", "c")), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(id = "Quina scrapers") %>%
  remove_outliers("ave")

Ordinary_scrapers_df <- scraper %>%
  mutate(ave = parse_number(...20)) %>%
  select(ave) %>%
  mutate(id = "Ordinary scrapers") %>%
  remove_outliers("ave")
Edge_angle_df <- bind_rows(Quina_scrapers_df, Ordinary_scrapers_df)
Edge_angle_df$id <- factor(Edge_angle_df$id, levels = c("Quina scrapers", "Ordinary scrapers"))

colors <- c("#68A7BE", "#EE7E77")
plot_edge_angle <- 
  ggplot(Edge_angle_df, fill = id, aes(x = id, y = ave)) +
  geom_errorbar(mapping = aes(color = id), stat = "boxplot", width = 0.1, linewidth = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = id), width = 0.5, size = 1, outlier.shape = NA, alpha = 1) +  
  geom_quasirandom(mapping = aes(color = id), cex = 3, size = 2, alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 3, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Edge angle (°)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")

ggsave(filename = "RR.png", width = 6, height = 8, dpi = 800, bg = "white")

t.test(ave ~ factor(id), data = Edge_angle_df) 

# Reduction intensity------------------------------------------------

# the reduction intensity of Quina scrapers, the results show a 
# median GIUR value of 0.81, significantly higher than that of 
# ordinary scrapers

giur_df <- 
  bind_rows(.id = "id",
            list( `Quina scrapers` = 
                    qn_scrapers_reduction %>%
                    select(...5),
                  `Ordinary scrapers` = 
                    scraper %>%
                    select(...34)
            )) %>%
  mutate(giur = parse_number(...1))

giur_df$id <- factor(giur_df$id, levels = c("Quina scrapers", 
                                            "Ordinary scrapers"))

colors <- c("#68A7BE", "#EE7E77")
plot_giur <- 
  ggplot(giur_df, fill = id, aes(x = id, y = giur)) +
  geom_errorbar(mapping = aes(color = id), stat = "boxplot", width = 0.1, linewidth = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = id), width = 0.5, size = 1, outlier.shape = NA, alpha = 1) +  
  geom_quasirandom(mapping = aes(color = id), cex = 3, size = 2, alpha = 0.5) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 3, color = "black", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Reduction intensity") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
  theme(legend.position = "none")

ggsave(filename = "GIUR.png", width = 6, height = 8, dpi = 800, bg = "white")

t.test(giur_df$giur, as.integer(factor(giur_df$id)))


# Thickness box-plot-------------------------------------------------

Thickness_plot <-
  bind_rows(
    list(
      `Quina scrapers` = qn_scrapers %>%
        select(Length, Breadth, Thickness, Weight),
      `Ordinary scrapers` = scraper %>%
        select(Length, Breadth, Thickness, Weight)
    ),
    .id = "id"
  ) %>%
  drop_na()

Thickness_plot$id <- factor(Thickness_plot$id, levels = c("Quina scrapers", 
                                              "Ordinary scrapers"))

colors <- c("#68A7BE", "#EE7E77")
plot_thick <- 
ggplot(Thickness_plot, fill = id) +
  aes(x = reorder(id, -Thickness), y = Thickness) +
  geom_errorbar(mapping = aes(color = id), stat = "boxplot", width = 0.1, linewidth = 1, position = position_dodge(width = 0.75)) +
  geom_boxplot(mapping = aes(color = id), width = 0.5, size = 1, outlier.shape = NA, alpha = 1) +  
  geom_quasirandom(mapping = aes(color = id), cex = 1.5, width = 0.4, alpha = 0.5, method = "quasirandom", varwidth = TRUE) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 5, color = "#3B3B3B", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  xlab("") +
  ylab("Thickness (mm)") +
  theme_bw(base_size = base_size_value) +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 18, color = "black"),
        axis.ticks.x = element_blank(), 
        panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white")) +
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
       width = 18, height = 5, dpi = 800, bg = "white")







