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


startTime = 2.6
minHeight = 0.2
minWidth = 0.1

peakStart = 2.7
peakStop = 3.2

icv_chrom |> 
  ggplot(aes(x = `Time (min)`, y = `Value (µS)`)) +
  geom_point() +
  geom_vline(xintercept = c(startTime, peakStart, peakStop))


# maybe we can incremenetally take the maximum
# and filter out the bits we've already processed?
# maybe we'll start with actually integrating first
# how do you plot the integration??? drop lines?
icv_chrom |> 
  write_csv(here("temp.csv"))

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
    geom_ribbon(data = peak, aes(x = `Time (min)`, ymin = baseline, ymax = `Value (µS)`), alpha = 0.2, fill = "blue")
}

plot_chrom <- function(dat) {
  dat |> 
    ggplot() +
    geom_line(aes(x = `Time (min)`, y = `Value (µS)`)) +
    theme_bw()
}

# I guess we could do a tail call thing but it's not really worth it
add_peaks <- function(dat, peaks) {
  peak_dat <- map( peaks, \(pair) integrate_peak(dat, pair[1], pair[2]) )
  reduce(peak_dat, \(init, nex) plot_peak(init, nex), .init = plot_chrom(dat))
}


peakList <- list(
  fluoride = c(2.8, 3.2),
  chloride = c(3.5, 4.5),
  nitrite = c(4.8, 5.3),
  bromide = c(6, 7),
  nitrate = c(7, 8),
  phosphate = c(8.7, 9.5),
  sulfate = c(10, 12)
)
add_peaks(icv_chrom, peakList)


# integration bits ----------------------------------------------------------------------------
# so now we did the fun part of figuring out how to make the plots appear
# but now we need to figure out how to 








