# SJH
# [문항1]  ※ Open book, open note
# ※ 카페 내 ‘평가답안제출’ 게시판에 R code 업로드
# ※ R code안에 본인의 성명을 기입
# ※ R 파일명에 본인의 영문이니셜을 추가
# 
# ■ 아래 문제를 R code로 작성하여 제출하시오.
# 
# (텍스트데이터 빈도분석)
# 1. 제공된 데이터에서 빈도수가 2회 이상 단어를 이용하여 단어 구름으로 시각화 하시오
# 제출양식 : 젤렌스키_연설문_20220219.txt
# (1) 텍스트 데이터 가져오기
library(showtext)
library(wordcloud2)
library(ggwordcloud)
library(ggplot2)
library(stringr)
library(dplyr)
library(tidytext)
raw_moon <- readLines("젤렌스키_연설문_20220219.txt", encoding = "UTF-8")
head(raw_moon)

# [문항2]  (텍스트데이터 빈도분석)
# (2) 추출된 단어 대상 전처리


moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>%
  as_tibble() 

# [문항3]  (텍스트데이터 빈도분석)
# (3) 단어 출현 빈도수 산출

word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space <- word_space %>%
  count(word, sort = T)
word_space

# [문항4]  (텍스트데이터 빈도분석)
# (4) 단어 구름에 디자인 적용 (wordcloud2 패키지 사용)


wordcloud2(data = demoFreq)
wordcloud2(word_space)

# [문항5]  (텍스트데이터 빈도분석)
# (5) wordcloud2 패키지 사용하여 워드클라우드 결과 제출
# http://localhost:24399/session/viewhtml3f4bf25b04/index.html

# [문항6]  (텍스트 데이터 감성분석)
# 2. 다음 텍스트를 대상으로 감성분석을 실시하시오.
# 제출양식 : Itaewon_text1.txt
# (1) 단어별로 token화 하시오
library(dplyr)
library(readr)
library(textclean)
library(stringr)
library(tidytext)
library(tidyr)
df <- readLines("Itaewon_text1.txt", encoding = "UTF-8")
df <- tibble(sentence =df)

df <- df %>%
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F)
df
df <- df %>%
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))

# [문항7]  (텍스트 데이터 감성분석)
# (2) 문장별 감성점수를 산출하시오.
score_df <- df %>%
  group_by(sentence) %>%
  summarise(score = sum(polarity))
score_df

