# SJH
# 데이터 불러오기
library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")
# 전처리
library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  filter(president %in% c("이명박", "노무현")) %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches

# 명사 추출
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

# 연설문별 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequency

# long form을 wide form으로 변환
library(tidyr)
frequency_wide <- frequency %>%
  pivot_wider(names_from = president, # 변수명으로 만들 값
              values_from = n, # 변수에 채워 넣을 값
              values_fill = list(n = 0)) # 결측치 0으로 변환
frequency_wide

# 로그 오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((이명박 + 1) / (sum(이명박 + 1))) /
                                ((노무현 + 1) / (sum(노무현 + 1)))))
frequency_wide

# 상대적으로 중요한 단어 추출
top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "lee", "roh")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

library(ggplot2)
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip () +
  labs(x = NULL)

# 데이터 불러오기
library(readr)
raw_speeches <- read_csv("inaugural_address.csv")
# 전처리
library(dplyr)
library(stringr)
speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches

# 명사 기준 토큰화
library(tidytext)
library(KoNLP)
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

# 단어 빈도 구하기
frequecy <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequecy

# TF-IDF 구하기
frequecy <- frequecy %>%
  bind_tf_idf(term = word, # 단어
              document = president, # 텍스트 구분 변수
              n = n) %>% # 단어 빈도
  arrange(-tf_idf)
frequecy

# 상대적으로 중요한 단어 추출
top10 <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)
head(top10)

library(ggplot2)
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = F) +
  coord_flip () +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)
