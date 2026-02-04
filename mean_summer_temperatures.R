library(tidyverse)

# define the link to the data - you can try this in your browser too.  Note that the URL ends in .txt.
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"

#the next line tells the NASA site to create the temporary file
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

# the next lines download the data
temp=read.csv(dataurl,
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col.names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary becuase they used dashes ("-")
# in the column names and R doesn't like that.

#to open the table in a browsable ‘excel-like’ window. 
View(temp)

#to get summaries of each column.
summary(temp)

glimpse(temp)

#Clean: make sure YEAR is numeric (and drop missing JJA)
temp_clean <- temp %>%
  mutate(YEAR = as.integer(YEAR)) %>%
  filter(!is.na(JJA))

#Plot: JJA annual mean temperature time series + smooth trend
p <- ggplot(temp_clean, aes(x = YEAR, y = JJA)) +
  geom_line(linewidth = 0.7) +
  geom_smooth(color = "red", se = TRUE) +
  xlab("Year") +
  ylab("Mean summer temperature (JJA) (°C)") +
  ggtitle("Buffalo, NY Mean Summer Temperatures Over Time") +
  labs(subtitle = "Source: NASA GISS station record (Buffalo Niagara Airport, USW00014733)")

print(p)

#let's save 
ggsave(
  filename = "buffalo_JJA_temperature_decadal.png",
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)

# (Optional) Decadal averages plot idea for extra time
decadal <- temp_clean %>%
  mutate(decade = floor(YEAR / 10) * 10) %>%
  group_by(decade) %>%
  summarize(JJA_mean = mean(JJA, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(decadal, aes(x = decade, y = JJA_mean)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  xlab("Decade") +
  ylab("Decadal mean summer temperature (JJA) (°C)") +
  ggtitle("Decadal Mean Summer Temperatures in Buffalo, NY") +
  labs(subtitle = "Source: NASA GISS station record (Buffalo Niagara Airport, USW00014733)")

print(p2)

ggsave(
  filename = "buffalo_JJA_temperature_decadal.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)
