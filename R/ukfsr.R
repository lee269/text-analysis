library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(tidytext)
library(stringr)

read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

words <- read_folder(here("data")) |>
  mutate(text = str_extract(text, "[a-zA-Z' ]+")) |> 
  unnest_ptb(word, text) |> 
  anti_join(stop_words) |>
  filter(!is.na(word)) |> 
  filter(!word %in% c("figure", "source", "download", "image", "td", "tr", "data", "chart"))

freqs <- words |> count(word, sort = TRUE)

pdf(width = 10,height = 10)
wordcloud::wordcloud(freqs$word, 
                     freqs$n,
                     colors = afcolours::af_colours(n=6),
                     min.freq = 5,
                     scale = c(10, 1),
                     random.order = FALSE,
                     random.color = TRUE, rot.per = .35)

dev.off()

wordcloud2::wordcloud2(data = freqs,
                       color = afcolours::af_colours(n = 4),size = 5, gridSize = 10)


wordcloud2::letterCloud(data = freqs, word = "FOOD")


