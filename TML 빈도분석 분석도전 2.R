# SJH
# 분석 도전
# 박근혜 전 대통령의 대선 출마 선언문이 들어있는 speech_park.txt를 이용해 문제를 해결해 보세요.
# Q1. speech_park.txt를 불러와 분석에 적합하게 전처리한 다음 연설문에서 명사를 추출하세요.
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
# 전처리
library(dplyr)
library(stringr)
park <- raw_park %>%
  str_replace_all("[^가-힣]", " ") %>% # 한글만 남기기
  str_squish() %>% # 연속된 공백 제거
  as_tibble() # tibble로 변환
park
# 명사 기준 토큰화
library(tidytext)
library(KoNLP)
word_noun <- park %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
word_noun
# Q2. 가장 자주 사용된 단어 20개를 추출하세요.
top20 <- word_noun %>%
  count(word, sort = T) %>%
  filter(str_count(word) > 1) %>%
  head(20)
top20
# Q3. 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프를 만드세요.
library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip () +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL)
# Q4. 전처리하지 않은 연설문에서 연속된 공백을 제거하고 tibble 구조로 변환한 다음 
sentences_park <- raw_park %>%
  str_squish() %>% # 연속된 공백 제거
  as_tibble() %>% # tibble로 변환
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")
sentences_park
# 문장 기준으로 토큰화하세요.
# Q5. 연설문에서 "경제"가 사용된 문장을 출력하세요.
sentences_park %>%
  filter(str_detect(sentence, "경제"))
