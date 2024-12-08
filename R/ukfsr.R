library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(here)
library(tidytext)
library(stringr)
library(ggplot2)
library(forcats)
library(ukfsr)
library(tm)
library(topicmodels)

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

# by theme ---------------------------------------------------------------------
read_folder(here("data")) |>
  mutate(text = str_extract(text, "[a-zA-Z' ]+")) |> 
  unnest_tokens(word, text) |> 
  filter(!is.na(word)) |> 
  filter(!word %in% c("td", "tr", "style", "class")) |> 
  mutate(theme = str_extract(id, "T[1-5]")) |> 
  select(theme, word) |> 
  count(theme, word) |> 
  bind_tf_idf(word, theme,n) |> 
  group_by(theme) |> 
  slice_max(tf_idf, n = 10) |> 
  ungroup() |> 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = theme)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~theme, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) + 
  scale_fill_manual(values = afcolours::af_colours(n = 5)) +
  theme_ukfsr(base_size = 10)


# dtm --------------------------------------------------------------------------


x <- read_folder(here("data")) |>
  mutate(text = str_extract(text, "[a-zA-Z' ]+")) |> 
  unnest_tokens(word, text) |> 
  filter(!is.na(word)) |> 
  filter(!word %in% c("td", "tr", "style", "class", "figure", "source", "download", "image", "food", "data", "chart")) |> 
  mutate(theme = str_extract(id, "T[1-5]")) |> 
  select(theme, word) |> 
  count(theme, word) |> anti_join(stop_words)

dtm <- cast_dtm(x,term =  word,document =  theme,value =  n)

theme_lda <- LDA(dtm, k = 5)

topics <- tidytext::tidy(theme_lda)

z <- topics |>
  group_by(topic) |> 
  slice_max(beta, n = 10)
