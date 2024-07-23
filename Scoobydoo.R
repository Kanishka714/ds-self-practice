 #scooby doo
scooby <- read.csv("/Users/kanishkarathnayake/Downloads/archive/Scooby-Doo Completed.csv", header = TRUE, sep = ",")
scooby
glimpse(scooby)

scooby_sm <- scooby |>
  select(series.name:format) |>
  mutate(imdb = as.double(imdb))

#A prelimenary vizualization

ggplot(scooby_sm, aes(x = date.aired,
                      y = imdb,
                      color = format)) +
  geom_point()+
    scale_color_brewer(palette = "Dark2")


#explore the format
table(scooby_sm$format)

crossover <- filter(scooby_sm, format == "Crossover")
View(crossover)

#filter non tv series 

scooby_sm <- scooby_sm |> 
  filter(format != "Crossover",
         format != "Movie",
         format != "Movie (Theatrical)")
table(scooby_sm$format)
view(scooby_sm)

#lets combine the segmented episodes for the purposes
#of considering imdb rating

# Process segmented data
segmented <- scooby_sm |>
  filter(format == "TV Series (segmented)") |>
  group_by(date.aired) |>
  summarize(imdb = mean(imdb),
            network = unique(network),
            series.name = unique(series.name),
            total_runtime = sum(run.time)) |>
  ungroup() |>
  select(date.aired, imdb, network, series.name, total_runtime)

# Process non-segmented data
non_seg <- scooby_sm |>
  filter(format != "TV Series (segmented)") |>
  select(date.aired,
         imdb,
         network,
         series.name,
         total_runtime = run.time)

# Ensure column names match
colnames(segmented)
colnames(non_seg)

# Combine the data frames
scooby_tidies <- rbind(non_seg, segmented)

# View the combined data frame
View(scooby_tidies)

#A new ggplot
ggplot(scooby_tidies, aes(x= date.aired,
                          y = imdb,
                          col= network)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2")

cw <- scooby_tidies |>
  filter(network == "The CW")
View(cw)


#Is IMDB really bad?
model <- aov(imdb ~ network,
             data = scooby_tidies)
summary(model)

TukeyHSD(model)
#yes it is.

scooby_tidies |>
  group_by(network) |>
  summarize(mean_imdb = mean(imdb, na.rm = TRUE)) |>
  ggplot(aes(x = fct_reorder(network, -mean_imdb),
             y = mean_imdb,
             fill = network)) +
  geom_col() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Network",
       y = "Mean IMDb Ratings",
       title = "Scooby Dooby Doo!") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 15, hjust = 1))















