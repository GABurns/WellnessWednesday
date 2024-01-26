# Title: Improving a Plot
# Author: Gareth Burns
# Creation Date: 26/01/2024
# Description: I wanted to improve a plot in a local healthcare related publication
#   that was aimed at the general public. I quickly Googled and found the annual
#   Belfast Health and Social Care Trust Annual report and found many examples of
#   poor quality graphs that seem to just be included as a picture to fill a section
#   that achieve a specific goal - i.e. we have data lets create a bar chart and put
#   it in and include a table.
#   https://hscqi.hscni.net/wp-content/uploads/2022/12/Annual-Quality-Report-2021-22-2.pdf
#   A lot of effort goes into the styling of the document and he graphs seem are
#   extremely inconsisent. I picked the "No. of Falls" graph on page 39 as I wanted
#   to show an alternative to dual-axis graphs.
#   From their Data Strategy they aim to "Our data visualisation and presentation
#   capabilities enable us to illustrate insights that are actionable and facilitate
#   evidence-based decision making" - most future references for Data visualisation
#   refer to data visualisation dashboards.


# Load Libraries ----------------------------------------------------------

library(ggplot2)
library(patchwork)
library(tidyr)


font_add_google("Noto sans", "Noto")
showtext_auto()


# Local Variables ---------------------------------------------------------


# Create Data ---------------------------------------------------------------

# create a sequence of dates every 3 months


bhsciFallsData <-
  data.frame(
    date = seq(
      from = as.Date("2020/4/1"),
      to = as.Date("2022/3/1"),
      by = "months"
    ),
    mod_falls = c(3, 4, 4, 0, 2, 6, 6, 8, 8, 3, 10, 3, 5, 5, 3, 6, 4, 10, 3, 5, 6, 10, 2, 9),
    total_falls = c(
      120,
      140,
      165,
      105,
      115,
      195,
      170,
      175,
      172,
      205,
      200,
      170,
      151,
      142,
      125,
      170,
      180,
      201,
      180,
      210,
      215,
      201,
      195,
      165
    )
  )

# Data for Arrows
ArrowData <-
  data.frame(
    x = rep(as.Date(max(
      bhsciFallsData$date
    )) + 31, 2),
    y = c(10, -10),
    xend = rep(as.Date(max(
      bhsciFallsData$date
    )) + 31, 2),
    yend = c(90, -90)
  )

# Data Wrangling ----------------------------------------------------------
# TODO seasonal mean
# Shift dates for a a grouped barchart effect in lollipop chart

bhsciFallsData <- bhsciFallsData |>
  pivot_longer(!date)


meanModFalls <- mean(bhsciFallsData$mod_falls)
bhsciFallsData$mod_change_per <-
  (bhsciFallsData$mod_falls - meanModFalls) / meanModFalls * 100

meanTotalFalls <- mean(bhsciFallsData$total_falls)
bhsciFallsData$total_change_per <-
  (bhsciFallsData$total_falls - meanTotalFalls) / meanTotalFalls * 100

# Plot
# TODO
# Add above average and below average labels
ggplot(bhsciFallsData[bhsciFallsData$name %in% c("total_change_per", "mod_change_per"), ], aes(
  x = date,
  y = value,
  fill = name,
  colour = name
)) +
  geom_segment(aes(xend = date, y = 1, yend = value)) +
  geom_segment(
    data = ArrowData,
    aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    arrow = arrow(length = unit(2, "mm")),
    size = c(1.5, 1),
    colour = c("red", "lightblue"),
    inherit.aes = FALSE
  ) +
  geom_point(size = 4) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(
      color = "lightblue",
      face = "bold",
      size = 18,
      family = "Noto",
      margin = margin(t = 10)
    ),
    axis.title.y = element_blank(),
    axis.text.x = element_text(
      color = "lightblue",
      size = 16,
      family = "Noto"
    ),
    axis.text.y = element_text(
      color = "lightblue",
      face = "bold",
      size = 16,
      family = "Noto"
    ),
    axis.line.x = element_line(color = "lightblue"),
    axis.ticks.x = element_line(color = "lightblue"),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.caption = element_text(
      family = "Noto",
      colour = "lightblue",
      size = 14
    ),
    plot.margin = margin(rep(20, 4))
  ) +
  ggtitle(label = "Incidences of Falls") +
  xlab("Date") +
  ylab("") +
  labs(caption = "*More falls account")
