---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# ggGantt

<!-- badges: start -->
<!-- badges: end -->

The goal of ggGantt is to make a gantt plot.

## Installation

You can install the development version of ggGantt like so:

``` r
# install.packages("devtools")
devtools::install_github("BioSenior/ggGantt")
```

## Example
### Basic example

- This is a basic example which shows you how to plot a gantt graph using `ggGantt`:


```r
library(ggGantt)

## basic example code
p <- ggplot(ToothGrowth, aes(supp, len))

# gantt plot
p + geom_gantt(aes(fill = supp), width = 0.1)
```

<img src="man/figures/README-example-1.png" title="plot of chunk example" alt="plot of chunk example" width="100%" />
- Add stroke and median points
  - `stroke`: used to adjust the stroke of the median points


```r
# Add stroke and median points
p + geom_gantt(aes(fill = as.factor(dose)),
               color = "black", stroke = 0.5, width = 0.1,
               position = position_dodge(0.2))
```

<img src="man/figures/README-unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" width="100%" />
- Change the theme and colors

```r
# Change the theme and colors
p + geom_gantt(aes(fill = as.factor(dose)),
               color = "black", stroke = 0.5, width = 0.1,
               position = position_dodge(0.2)) +
  ggsci::scale_fill_jco()+
  theme_classic()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="100%" />

### More interesting beautification effects
- `add_line`: add a dashed line
- `linecolor`: change the color of the line

```r
# Change the theme and colors
p + geom_gantt(aes(fill = as.factor(dose)),
               color = "black", 
               # parameter stroke: the stroke of the middle point and the size of the dashed line
               stroke = 1, width = 0.1,
               position = position_dodge(0.2),
               # add a dashed line, and change the color of the line with     linecolor parameter
               add_line = TRUE, linecolor = "grey"
               ) +
  ggsci::scale_fill_jco()+
  theme_classic()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="100%" />
- `coord_flip`: filp the x and y axis
- `flip`: if you want to flip the x and y axis, please set the `flip = TRUE`

```r
# Change the theme and colors
p + geom_gantt(aes(fill = as.factor(dose)),
               color = "black", 
               # parameter stroke: the stroke of the middle point and the size of the dashed line
               stroke = 1, width = 0.1,
               position = position_dodge(0.2),
               # add a dashed line, and change the color of the line with     linecolor parameter
               add_line = TRUE, linecolor = "grey",
               # flip or not
               flip = TRUE
               ) +
  ggsci::scale_fill_jco()+
  theme_classic()+
  coord_flip()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" width="100%" />

### `geom_gantt2`
- `geom_gantt2` requires a processed wider data as input. See Examples for details. This function is an extension of `geom_gantt`.


```r
data("gantt_data_wider")
# You can use head(gantt_data_wider) to view the data type of the input data.
head(gantt_data_wider)
#>   CancerType  low high middle  group number
#> 1   Prostate 0.80 0.95  0.875 group1    217
#> 2   Prostate 0.60 0.80  0.700 group2     77
#> 3   Prostate 0.45 0.70  0.575 group3     19
#> 4   Prostate 0.25 0.80  0.525 group4      7
#> 5   Prostate 0.27 0.50  0.385 group5     38
#> 6       Lung 0.55 0.72  0.635 group1    952

ggplot(gantt_data_wider)+
  geom_gantt2(aes(x = CancerType,
                  minimum = low, maximum = high,
                  middle = middle, fill = group),
              position = position_dodge(0.3),
              # stroke and color of meddle points
              stroke = 0.5, point_size = 2, color = "black",
              # widths of the rectangles
              width = 0.2,
              # add dashed line
              add_line = T
  )
```

<img src="man/figures/README-unnamed-chunk-6-1.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" width="100%" />

### Use `geom_gantt` and `geom_gantt2` to reproduce the figures in NC
![gantt plot](https://tva1.sinaimg.cn/large/e6c9d24egy1h1roqmufj6j21bu0km43a.jpg)

#### `geom_gantt`
- `geom_gantt` requires unprocessed long data as input. See Examples for details.

```r
data("gantt_data_long")
# You can use head(gantt_data_long) to view the data type of the input data.
head(gantt_data_long)
#>   CancerType  Group    Values
#> 1   Prostate group1 0.8500467
#> 2   Prostate group1 0.8468580
#> 3   Prostate group1 0.8151187
#> 4   Prostate group1 0.8813291
#> 5   Prostate group1 0.8270323
#> 6   Prostate group1 0.8982327

p <- ggplot(gantt_data_long, aes(CancerType, Values))

# More advanced examples: the figure repetition
p + 
  # builds the empty coordinate system:
  geom_point(alpha = 0)+
  # background shadow 1:
  geom_rect(aes(xmin=1.5, xmax=2.5, ymin=0, ymax=Inf), fill = "#e6e6e6")+
  # background shadow 2:
  geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0, ymax=Inf), fill = "#e6e6e6")+
  geom_gantt(aes(fill = Group), 
               position = position_dodge(0.3), 
               # stroke and color of meddle points
               stroke = 0.5, point_size = 2, color = "black",
               # widths of the rectangles 
               width = 0.2,
               # add dashed line
               add_line = T,
               # flip or not
               flip = TRUE
            )+
  scale_fill_manual(values = c("#c97d80", "#947559", "#8678b0", "#9f436b", "#9abc6d"))+
  # Remove the gap between the graph and the coordinate axes and set the y-axis scale:
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 1.25, 0.25))+
  # change the x lable and y lable
  ylab("Area Under Kaplan-Meier Plot of Overall Survival\n(40 months follow-up)")+
  xlab("")+
  # theme
  theme_classic() +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust = 0.5, vjust = 2,
                                   size = 10, color = "black"))+
  coord_flip()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" width="100%" />

#### `geom_gantt2`

```r
data("gantt_data_wider")
# You can use head(gantt_data_long) to view the data type of the input data.
head(gantt_data_wider)
#>   CancerType  low high middle  group number
#> 1   Prostate 0.80 0.95  0.875 group1    217
#> 2   Prostate 0.60 0.80  0.700 group2     77
#> 3   Prostate 0.45 0.70  0.575 group3     19
#> 4   Prostate 0.25 0.80  0.525 group4      7
#> 5   Prostate 0.27 0.50  0.385 group5     38
#> 6       Lung 0.55 0.72  0.635 group1    952

# More advanced examples: the figure repetition
ggplot(gantt_data_wider)+
  # builds the empty coordinate system:
  geom_point(aes(CancerType, middle), alpha = 0)+
  # background shadow 1:
  geom_rect(aes(xmin=1.5, xmax=2.5, ymin=0, ymax=Inf), fill = "#e6e6e6")+
  # background shadow 2:
  geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0, ymax=Inf), fill = "#e6e6e6")+
  geom_gantt2(aes(x = CancerType, 
                  minimum = low, maximum = high, 
                  middle = middle, fill = group),
              position = position_dodge(0.3), 
              # stroke and color of meddle points
              stroke = 0.5, point_size = 2, color = "black",
              # widths of the rectangles 
              width = 0.2,
              # add dashed line
              add_line = T,
              # flip or not
              flip = TRUE
  )+
  scale_fill_manual(values = c("#c97d80", "#947559", "#8678b0", "#9f436b", "#9abc6d"))+
  # Remove the gap between the graph and the coordinate axes and set the y-axis scale:
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 1.25, 0.25))+
  # change the x lable and y lable
  ylab("Area Under Kaplan-Meier Plot of Overall Survival\n(40 months follow-up)")+
  xlab("")+
  # theme
  theme_classic() +
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(angle=90, hjust = 0.5, vjust = 2,
                                   size = 10, color = "black"))+
  coord_flip()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" width="100%" />

> Welcome to pay attention to the BioSenior to get more practical tutorials on scientific mapping!
