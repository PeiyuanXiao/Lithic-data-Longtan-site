library("readxl")

# import the data 
qn_scrapers <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1)
scraper <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Scrapers")
notch <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Notches")
denticulate <- read_excel("Longtan site lithic data-Tools.xlsx", skip = 1, sheet = "Denticulates")
flakes <- read_excel("Longtan site lithic data-Flakes.xlsx", skip = 0)
qn_scrapers_reduction <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 1, sheet = "Reduction")
qn_scrapers_Edge <-  read_excel("Longtan site lithic data-Quina scrapers.xlsx", skip = 2, sheet = "Edge")
resharpening <- read_excel("Longtan lithic data-Reaffutage.xlsx", skip = 1)

kruskal.test(flakes$IPA, factor(flakes$...1))
dunn.test(flakes$IPA, as.integer(factor(flakes$...1)), method = "bh")

# set the plot text size 
base_size_value <- 10

library(tidyverse)
library(apastats)
library("ggpmisc")

#Integrate data
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


plot_resharpening <- 
  ggplot(Resharpening_df) +
  aes(x = id, y = angle, color = id) +
  geom_boxplot(outliers = FALSE) +
  geom_point(stat = "summary", 
             fun = "mean",
             shape = 19, 
             size = 4, 
             color = "black",
             show.legend = FALSE) +
  geom_quasirandom(alpha = 0.2, size =3 ) +
  scale_color_manual(values = c( "Quina scrapers" = "#EEF8B4", "Resharpening flakes" = "#CDEBB3")) +
  scale_fill_manual(values = c( "Quina scrapers" = "#EEF8B4", "Resharpening flakes" = "#CDEBB3")) +
  xlab("") +
  ylab("Edge angle") +
  theme_minimal(base_size = base_size_value)

t.test(angle ~ factor(id), data = Resharpening_df )

plot_thick <- 
ggplot(tools_df) +
  aes(reorder(id, -Thickness), Thickness) +
  geom_boxplot(outliers = FALSE) +
  geom_quasirandom(alpha = 0.2, size =3) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 4, color = "black", show.legend = FALSE) +
  xlab("") +
  ylab("Thickness (mm)") +
  theme_minimal(base_size = base_size_value)

  kruskal.test(Thickness ~ factor(id), data = tools_df)  

library(ggbeeswarm)   

tools_df %>%
  pivot_longer(-id) %>%
  ggplot() +
  aes(id, value) +
  geom_boxplot() +
  geom_quasirandom(alpha = 0.2) +
  facet_wrap( ~ name, scales = "free_y")



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
  aes(id, giur) +
  geom_boxplot(outliers = FALSE) + 
  geom_quasirandom(alpha = 0.2, size =3) +
  geom_point(stat = "summary", 
             fun = "mean", 
             shape = 19, 
             size = 4, 
             color = "black") +
  xlab("") +
  ylab("Mean GIUR") +
  theme_minimal(base_size = base_size_value)

ggsave(filename = "GIUR.png", plot = p, width = 6, height = 8, dpi = 800, bg = "white")

wilcox.test(giur_df$giur, as.integer(factor(giur_df$id)))


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
ggplot(Edge_angle_df) +
  aes(id, ave, color = ) +
  geom_boxplot(outliers = FALSE) +
  geom_point(stat = "summary", fun = "mean", shape = 19, size = 4, color = "black", show.legend = FALSE) +
  geom_quasirandom(alpha = 0.2, size =3) +
  xlab("") +
  ylab("Edge angle") +
  theme_minimal(base_size = base_size_value)


ggsave(filename = "RR.png", width = 6, height = 8, dpi = 800, bg = "white")

wilcox.test(Edge_angle_df$ave, as.integer(factor(Edge_angle_df$id)) )

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

