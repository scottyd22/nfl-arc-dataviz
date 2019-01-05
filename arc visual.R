# Inspiration: https://www.economist.com/graphic-detail/2018/12/08/why-ticket-prices-on-long-haul-flights-have-plummeted

# Setup ----
library(tidyverse)
library(ggforce)
library(extrafont)
library(readxl)
nfl_stats <- read_excel("./nfl_stats.xlsx")

# Transform data ----
df <- nfl_stats %>%
  select(Year,
         Team = TeamAbbr,
         PPG = `Pts/G`,
         YPG = `Yds/G`) %>%
  mutate(YPG = round(YPG, 0))

plot.df <- df %>%
  select(-YPG) %>%
  spread(key = Year, value = PPG) %>%
  mutate(PPGchange = (`2018` - `2017`)/`2017`) %>%
  select(Team, PPGchange) %>%
  left_join(
    df %>% filter(Year == 2018) %>% select(Team, YPG)
  ) %>%
  rename(YPG2018 = YPG)

max.delta <- 1  # corresponds to PPGchange value (100%) at pi/2 on the unit circle

# Translate ----
# Convert % change into radians and categorize as AFC/NFC
plot.df <- plot.df %>%
  mutate(Radians = PPGchange/max.delta * (pi/2),
         x = cos(Radians) * YPG2018,
         y = sin(Radians) * YPG2018) %>%
  mutate(Conference = ifelse(Team %in% c('ARI', 'ATL', 'CAR', 'CHI', 'DAL', 'DET', 'GB', 'LAR', 
                                         'MIN', 'NO', 'NYG', 'PHI', 'SEA', 'SF', 'TB', 'WAS'), 'NFC', 'AFC'))
# Arcs ----
max.arc <- round(max(plot.df$YPG2018), -1)
levels <- 9 # number of arcs to make

# Determine largest postive and largest negative percentages (in radians) in the data set to create the outer bounds of the graphic
max.positive.pct <- (round(max(plot.df$PPGchange), 1) + 0.05)/max.delta * pi/2
max.negative.pct <- (round(min(plot.df$PPGchange), 1) - 0.05)/max.delta * pi/2

# Function and lapply to generate each arc of YPG (and labels)
a <- function(i) {
  data.frame(start = seq(pi/2 - max.positive.pct, pi/2 - max.negative.pct, length.out = 11)[-11], 
             end = seq(pi/2 - max.positive.pct, pi/2 - max.negative.pct, length.out = 11)[-1], 
             r = i)
}

arcs <- lapply(seq(0, max.arc, round(max.arc/levels, -1)), a) %>% 
  bind_rows()

arc.labels <- data.frame(x = 0.95*(cos(arcs$start) * arcs$r),
                         y = 0.95*(sin(arcs$end) * arcs$r),
                         label = arcs$r,
                         stringsAsFactors = F) %>% 
  group_by(label) %>%
  filter(row_number() == floor(n()/2)) %>%
  ungroup()

# Percent Segments ----
# Function and lapply to generate each percentage gridline from upper to lower bound
p <- function(i) {
  data.frame(x = cos(i/max.delta * pi/2) * max.arc, 
             y = sin(i/max.delta * pi/2) * max.arc,
             label = paste0(round(100 * i, 0), '%'),
             stringsAsFactors = F)
}

pct.segments <- lapply(seq(round(min(plot.df$PPGchange), 1) - 0.05, round(max(plot.df$PPGchange), 1) + 0.05, 0.05), p) %>% 
  bind_rows()


# Plot ----
ggplot() +
  # arcs
  ggforce::geom_arc(data = arcs, aes(x0 = 0, y0 = 0, r = r, start = start, end = end), color = 'grey85', size = 0.5 , linetype = 1) +
  geom_text(data = arc.labels, aes(x = x, y = y, label = label), size = 3, color = 'grey60') +
  # pct segments
  geom_segment(data = pct.segments, aes(x = 0, xend = x, y = 0, yend = y), color = 'grey85', size = 0.5 , linetype = 2) +
  geom_text(data = pct.segments, aes(x = x * 1.05, y = y * 1.05, label = label), size = 3, color = 'grey60') +
  # 0% line
  geom_segment(data = plot.df, aes(x = 0, xend = max.arc, y = 0, yend = 0), size = 0.5, color = 'grey60') +
  # points, segments, & labels
  geom_segment(data = plot.df, aes(x = 0, xend = x, y = 0, yend = y, color = Conference), size = 1.25, alpha = 0.22) +
  geom_point(data = plot.df, aes(x = x, y = y, color = Conference), size = 2.5, shape = 21, fill = 'white') +
  ggrepel::geom_text_repel(data = plot.df, aes(x = x, y = y, label = Team, color = Conference), size = 3.5, fontface = 'bold') +
  # annotations, themes, etc.
  annotate('text', x = 75, y = 260, label = 'Yards per game (2018)', angle = 64, size = 4.25, color = 'grey30') +
  coord_fixed(ratio = 1/1.15) +
  ggthemes::theme_fivethirtyeight() +
  scale_color_manual(values = c('NFC' = '#013369', 'AFC' = '#d50a0a')) +
  labs(x = '',
       y = '',
       title = "Majority of NFL Teams Scored More in 2018",
       subtitle = 'Percent change in points scored between the 2017 and 2018 NFL regular seasons\nSource | www.nfl.com') +
  theme(text = element_text(family = 'Segoe UI'), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = 'bold', size = 18, family = 'Segoe UI Semibold'),
        plot.subtitle = element_text(color = 'grey30', size = 10),
        plot.margin = margin(1, .75, 1, .75, 'cm'),
        legend.position = 'right',
        legend.direction = 'vertical')
  
