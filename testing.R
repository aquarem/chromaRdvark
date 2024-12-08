library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

library(here)

chromatograms <- here("chromatogram") |> list.files(full.names =TRUE)

# sheesh
icv_chrom <- chromatograms[2] |> readxl::read_excel(skip = 42)

icv_chrom |> 
  ggplot(aes(x = `Time (min)`, y = `Value (µS)`)) +
  geom_point()


# maybe we'll start with actually integrating first
# how do you plot the integration??? drop lines?

# if we didn't have step built-in we could just calculate it
# using map or rowwise
fluoride_peak <- icv_chrom |> 
  filter(`Time (min)` > 2.8, `Time (min)` < 3.2) |> 
  mutate(
    rise = last(`Value (µS)`) - first(`Value (µS)`),
    run = last(`Time (min)`) - first(`Time (min)`),
    slope = rise / run,
    
    baseline = slope * (`Time (min)` - min(`Time (min)`)) + first(`Value (µS)`),
    height = `Value (µS)` - baseline,
    area = height * ( as.numeric(`Step (s)`) / 60 )
  )

# now can we plot this without trimming the chromatogram?
fluoride_peak |>   ggplot() +
  geom_point(aes(x = `Time (min)`, y = `Value (µS)`)) +
  geom_point(aes(x = `Time (min)`, y = baseline)) +
  geom_ribbon(aes(x = `Time (min)`,  ymin = baseline, ymax = `Value (µS)`), alpha = 0.2)

# so here we can plot it on top of the original
icv_chrom |> 
  ggplot() +
  geom_line(aes(x = `Time (min)`, y = `Value (µS)`)) +
  geom_ribbon(data = fluoride_peak, aes(x = `Time (min)`, ymin = baseline, ymax = `Value (µS)`), alpha = 0.2)


# functionizing ---------------------------------------------------------------------------------

integrate_peak <- function(dat, peakStart, peakStop) {
  dat |> 
    filter(`Time (min)` > peakStart, `Time (min)` < peakStop) |> 
    mutate(
      slope = ( last(`Value (µS)`) - first(`Value (µS)`) ) / ( last(`Time (min)`) - first(`Time (min)`) ),
      baseline = slope * (`Time (min)` - min(`Time (min)`)) + first(`Value (µS)`),
      height = `Value (µS)` - baseline,
      area = height * ( as.numeric(`Step (s)`) / 60 )
    )
}

# do we want to pass in just a list of start/stop times? 
# or do we want to pass in a list of integrated peaks?
# I guess this can just return the plot? 
plot_peak <- function(chrom, peak) {
  chrom + 
    geom_ribbon(
      data = peak, 
      aes(x = `Time (min)`, ymin = baseline, ymax = `Value (µS)`), alpha = 0.2, fill = "blue"
    )
}

plot_chrom <- function(dat) {
  dat |> 
    ggplot() +
    geom_line(aes(x = `Time (min)`, y = `Value (µS)`)) +
    theme_bw()
}

# I guess we could do a tail call thing but it's not really worth it
# must have a named list as input along with raw chrom data
# returns chrom plot
plot_peaks <- function(dat, peaks) {
  peak_dat <- map( peaks, \(pair) integrate_peak(dat, pair[1], pair[2]) )
  reduce(peak_dat, \(init, nex) plot_peak(init, nex), .init = plot_chrom(dat))
}

# must have named list as input along with raw chrom data

get_areas <- function(dat, peaks) {
  peaks |> 
    map(\(pair) {
      dat |> 
        integrate_peak(pair[1], pair[2]) |> 
        summarize(
          height = max(height),
          area = sum(area),
          rt = mean(`Time (min)`), # could be mean or tmax
          width = max(`Time (min)`) - min(`Time (min)`),
          maxval = max(`Value (µS)`)
        )
    }) |> 
    bind_rows() |> 
    mutate(constituent = map_chr(names(peaks), \(x) if (is.null(x)) "unk" else x) ) |> 
    select(constituent, rt, area, height, width, maxval)
}


# this is going to have to change at some point
# since there will be unnamed peaks
peakList <- list(
  fluoride = c(2.8, 3.2),
  chloride = c(3.5, 4.5),
  nitrite = c(4.7, 5.5),
  bromide = c(6, 7),
  nitrate = c(7, 8),
  phosphate = c(8.7, 9.5),
  sulfate = c(10, 12)
)

integrated_plot <- plot_peaks(icv_chrom, peakList)
sample_summary <- get_areas(icv_chrom, peakList)

label_plot <- function(int_plot, samp_sum) {
  int_plot +
  geom_text(
    data = samp_sum, 
    aes(rt, maxval, label = paste0(
      constituent, "\n", 
      "area: ", round(area, 2), "\n",
      "rt: ", round(rt, 2)
      )
    ),
    # hjust = 0,
    nudge_y = 0.2, size = 3
  )
}

label_plot(integrated_plot, sample_summary)

# maybe we can put the areas on the plot as well? and the names?
# I guess using the rt and height, but height incorporates baseline so 
# maybe we should return the raw max value as well to keep things lined up





# integration bits ----------------------------------------------------------------------------
# so now we did the fun part of figuring out how to make the plots appear
# but now we need to figure out how to identify the peaks with some set limits

# so how do we find local peak maxes?
# we could do by point I guess? 


# here's how you can do it
# start at whatever arbitrary set point (integration on)
# or we can do rt reference windows
# and then step forwards
# wait until the next one is an increase and then mark it as a potential peak start
# then if it keeps increasing, go until you hit the top and start decreasing, mark as peak center

#   check if the peak height exceeds the chosen criteria
#   check if peak width exceeds chosen criteria (if the 1/2 width is very small, then it's probs noise)

# then go until we hit the bottom of the peak (stops decreasing), mark as peak end
# then draw straight line across start and end, and sum height the whole way


# alternative fancier way would be to chop it up into overlapping pieces
# and then find the local maxima in each piece
# then extend out from each until we hit the minimum in between each piece?



# maybe we can incremenetally take the maximum
# and filter out the bits we've already processed?


# either start with maxima or start with minima
# I guess better to start with maxima to do height filtering
# I suppose it will be linear time
# need to track start time as well to make sure the peaks aren't one step wide
# height_filter <- 0.3
# maxima <- list()
# curr_max <- NULL
# curr_time <- NULL
# for (i in seq_along(icv_chrom$`Value (µS)`)) {
#   
#   val <- icv_chrom$`Value (µS)`[i]
#   time <- icv_chrom$`Time (min)`[i]
#   
#   if (is.null(curr_max) || val > curr_max) {
#     curr_max <- val
#     curr_time <- time
#   } else if (val < curr_max & curr_max > height_filter) {
#     maxima <- c(
#       maxima, 
#       setNames(
#         list(list(rt = curr_time, val = curr_max)),
#         paste0("peak", length(maxima) + 1)
#       )
#     )
#     # this is causing it to reset and then hit all of the points on the way down
#     # new point -> NULL -> new max -> new point -> NULL etc.
#     curr_max <- NULL
#     curr_time <- NULL
#   }
# }
# 
# maxima |> bind_rows() |> unique()

# so I guess the problem is that we need to know if we're hitting the top of a peak
# and then if we are then we need to get out of find max mode
# the alternative would be to either do smoothing or do some kind of sliding window thing
# like if the minimum peak width was 0.2 min then we could at least bin then by that much
# which would cut things down by a lot

# can we do smoothing? doesn't really help anything
# it feels like we need use integrals / derivatives
# like with peak detection we'd want to find all points where the slope is zero
# so how do we do derivative? 

# with(icv_chrom, 
#     ksmooth(`Time (min)`, `Value (µS)`, kernel = "box", bandwidth = 0.2)) |> 
#   as_tibble() |> 
#   ggplot(aes(x, y)) + geom_line()


# testing just getting and plotting the derivative
# we could just map across them, but technically it'd be a reframe because we'd lose a row
# right?
# only things that will change will be time and value
# I guess we can use the step to get the run part right?
# and I guess it would be an accumulate step right?
# too bad there's no like window map where we could do sliding pairs and just use diff
# there is a slider package!!!!!

# maybe we should write a function that takes the first and second derivative
# and then we can just plot them all together
# to ID
# but just looking atit obviously where slope is zero is the maximum
# but we also need to be able to ID the start and end, since slope is zero before as well
# that's where the second derivative comes in

# ugh this is sick
icv_chrom |> 
  reframe(
    rise = slider::slide_dbl(`Value (µS)`, diff, .after = 1, .complete = TRUE),
    run = slider::slide_dbl(`Time (min)`, diff, .after = 1, .complete = TRUE),
    # using same names to use the plot chrom function
    `Value (µS)` = rise / run,
    `Time (min)` = slider::slide_dbl(`Time (min)`, mean, .after = 1, .complete = TRUE),
  ) |> 
  plot_chrom()

# so how do we do this like sequentially? like if we want to do multiple
# we'd want to add a type column I guess
# would be programmatic but since we know we only need the first and second
# I think we can just write it up and bind rows
get_derivatives <- function(dat) {
  # using reframe since we lose one row per derivative
  make_derivative <- function(dat, dtype) {
    dat |> 
      reframe(
        rise = slider::slide_dbl(`Value (µS)`, diff, .after = 1, .complete = TRUE),
        run = slider::slide_dbl(`Time (min)`, diff, .after = 1, .complete = TRUE),
        `Value (µS)` = rise / run,
        `Time (min)` = slider::slide_dbl(`Time (min)`, mean, .after = 1, .complete = TRUE),
        type = dtype
      )
  }
  first <- make_derivative(dat, "first")
  second <- make_derivative(first, "second")
  
  dat |> 
    mutate(type = "base") |> 
    bind_rows(first) |> 
    bind_rows(second)
}

get_derivatives(icv_chrom) |> 
  ggplot(aes(`Time (min)`, `Value (µS)`, color = type)) +
  geom_line() +
  facet_grid(rows = vars(type), scales = "free") +
  theme_bw()

# So i guess the minima in 2nd deriv are the peak maxima
# then the maxima in the 2nd deriv are the begin / end? more noise in 2nd though



















