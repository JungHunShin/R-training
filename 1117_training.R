# 감정분석
# 04-1 --------------------------------------------------------------------
# 감정 사전 불러오기
install.packages("readr")
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")
# -------------------------------------------------------------------------
library(dplyr)
# 긍정 단어
dic %>% 
  filter(polarity == 2) %>% 
  arrange(word)
# 부정 단어
dic %>% 
  filter(polarity == -2) %>% 
  arrange(word)
# -------------------------------------------------------------------------
dic %>% 
  filter(word %in% c("좋은", "나쁜"))
dic %>% 
  filter(word %in% c("기쁜", "슬픈"))
dic %>%

filter(word %in% c("행복하다", "좌절하다"))
# -------------------------------------------------------------------------
# 이모티콘
library(stringr)
dic %>% 
  filter(!str_detect(word, "[가-힣]")) %>% 
  arrange(word)
# -------------------------------------------------------------------------
dic %>% 
  mutate(sentiment = ifelse(polarity >= 1, "pos",
                            ifelse(polarity <= -1, "neg", "neu"))) %>% 
  count(sentiment)
# -------------------------------------------------------------------------
df <- tibble(sentence = c("디자인 예쁘고 마감도 좋아서 만족스럽다.",
                          "디자인은 괜찮다. 그런데 마감이 나쁘고 가격도 비싸다."))
df
library(tidytext)
df <- df %>% 
  unnest_tokens(input = sentence,
                output = word,
                token = "words",
                drop = F)
df %>% print(n = Inf)

# -------------------------------------------------------------------------
df <- df %>% 
  left_join(dic, by = "word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
df %>% print(n = Inf)
# -------------------------------------------------------------------------
score_df <- df %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))
score_df

# 04-2 --------------------------------------------------------------------
# 데이터 불러오기
raw_news_comment <- read_csv("news_comment_parasite.csv")
# -------------------------------------------------------------------------
# 기본적인 전처리
install.packages("textclean")
library(textclean)
news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))
# 데이터 구조 확인
glimpse(news_comment)
# -------------------------------------------------------------------------
# 토큰화
word_comment <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F)
word_comment %>%
  select(word, reply)
# 감정 점수 부여
word_comment <- word_comment %>%
  left_join(dic, by = "word") %>%

mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>%
  select(word, polarity)
# -------------------------------------------------------------------------
word_comment <- word_comment %>%
  mutate(sentiment = ifelse(polarity == 2, "pos",
                            ifelse(polarity == -2, "neg", "neu")))
word_comment %>%
  count(sentiment)
# -------------------------------------------------------------------------
top10_sentiment <- word_comment %>%
  filter(sentiment != "neu") %>%
  count(sentiment, word) %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10)
top10_sentiment
# -------------------------------------------------------------------------
# 막대 그래프 만들기
library(ggplot2)
ggplot(top10_sentiment, aes(x = reorder(word, n), 
                            y = n, 
                            fill = sentiment)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap(~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
# -------------------------------------------------------------------------
score_comment <- word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
score_comment %>% 
  select(score, reply)
# -------------------------------------------------------------------------
# 긍정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(-score)
# 부정 댓글
score_comment %>%
  select(score, reply) %>% 
  arrange(score)
# -------------------------------------------------------------------------
score_comment %>%
  count(score) %>%
  print(n = Inf)

# -------------------------------------------------------------------------
score_comment <- score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
# -------------------------------------------------------------------------
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
frequency_score
# -------------------------------------------------------------------------
# 막대 그래프 만들기
ggplot(frequency_score, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) + 
  scale_x_discrete(limits = c("pos", "neu", "neg"))
# -------------------------------------------------------------------------
df <- tibble(contry = c("Korea", "Korea", "Japen", "Japen"), # 축
             sex = c("M", "F", "M", "F"), # 누적 막대
             ratio = c(60, 40, 30, 70)) # 값
df
ggplot(df, aes(x = contry, y = ratio, fill = sex)) + geom_col()

# -------------------------------------------------------------------------
ggplot(df, aes(x = contry, y = ratio, fill = sex)) + 
  geom_col() +
  geom_text(aes(label = paste0(ratio, "%")), # % 표시
            position = position_stack(vjust = 0.5)) # 가운데 표시
# -------------------------------------------------------------------------
# 더미 변수 생성
frequency_score$dummy <- 0
frequency_score
# -------------------------------------------------------------------------
ggplot(frequency_score, aes(x = dummy, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = paste0(round(ratio, 1), "%")), 
            position = position_stack(vjust = 0.5)) + 
  theme(axis.title.x = element_blank(), # x축 이름 삭제
        axis.text.x = element_blank(), # x축 값 삭제
        axis.ticks.x = element_blank()) # x축 눈금 삭제
# 04-3 --------------------------------------------------------------------
comment <- score_comment %>%
  unnest_tokens(input = reply, # 단어 기준 토큰화
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") & # 한글 추출
           str_count(word) >= 2) # 두 글자 이상 추출

# -------------------------------------------------------------------------
# 감정 및 단어별 빈도 구하기
frequency_word <- comment %>%
  filter(str_count(word) >= 2) %>%
  count(sentiment, word, sort = T)
# -------------------------------------------------------------------------
# 긍정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "pos")
# 부정 댓글 고빈도 단어
frequency_word %>%
  filter(sentiment == "neg")
# -------------------------------------------------------------------------
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>% # 중립 제외
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
comment_wide
# -------------------------------------------------------------------------
# 로그 오즈비 구하기
comment_wide <- comment_wide %>%
 mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))
comment_wide
# -------------------------------------------------------------------------
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10)
top10 %>% print(n = Inf)
# -------------------------------------------------------------------------
top10 <- comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10 %>% print(n = Inf)
# -------------------------------------------------------------------------
# 막대 그래프 만들기
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 04-4 --------------------------------------------------------------------
# "소름"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "소름")) %>%
  select(reply)
# "미친"이 사용된 댓글
score_comment %>%
  filter(str_detect(reply, "미친")) %>%
  select(reply)
# -------------------------------------------------------------------------
dic %>% filter(word %in% c("소름", "소름이", "미친"))
# -------------------------------------------------------------------------
new_dic <- dic %>%
  mutate(polarity = ifelse(word %in% c("소름", "소름이", "미친"), 2, polarity))
new_dic %>% filter(word %in% c("소름", "소름이", "미친"))
# -------------------------------------------------------------------------
new_word_comment <- word_comment %>%
  select(-polarity) %>%
  left_join(new_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity))
# -------------------------------------------------------------------------
new_score_comment <- new_word_comment %>%

group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()
new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)
# -------------------------------------------------------------------------
# 1점 기준으로 긍정 중립 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >= 1, "pos",
                            ifelse(score <= -1, "neg", "neu")))
# -------------------------------------------------------------------------
# 원본 감정 사전 활용
score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
# 수정한 감정 사전 활용
new_score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
# -------------------------------------------------------------------------
word <- "소름|소름이|미친"
# 원본 감정 사전 활용
score_comment %>%

filter(str_detect(reply, word)) %>%
  count(sentiment)
# 수정한 감정 사전 활용
new_score_comment %>%
  filter(str_detect(reply, word)) %>%
  count(sentiment)
# -------------------------------------------------------------------------
df <- tibble(sentence = c("이번 에피소드 쩐다", 
                          "이 영화 핵노잼")) %>% 
  unnest_tokens(input = sentence, 
                output = word, 
                token = "words", 
                drop = F)
df %>% 
  left_join(dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))
# -------------------------------------------------------------------------
# 신조어 목록 생성
newword <- tibble(word = c("쩐다", "핵노잼"), 
                  polarity = c(2, -2))
# 사전에 신조어 추가
newword_dic <- bind_rows(dic, newword)
# 새 사전으로 감정 점수 부여

df %>% 
  left_join(newword_dic, by = "word") %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  group_by(sentence) %>% 
  summarise(score = sum(polarity))
# -------------------------------------------------------------------------
# 토큰화 및 전처리
new_comment <- new_score_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = "words",
                drop = F) %>%
  filter(str_detect(word, "[가-힣]") &
           str_count(word) >= 2)
# 감정 및 단어별 빈도 구하기
new_frequency_word <- new_comment %>%
  count(sentiment, word, sort = T)
# -------------------------------------------------------------------------
# Wide form으로 변환
new_comment_wide <- new_frequency_word %>%
  filter(sentiment != "neu") %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))
# 로그 오즈비 구하기
new_comment_wide <- new_comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                              ((neg + 1) / (sum(neg + 1)))))
# -------------------------------------------------------------------------
new_top10 <- new_comment_wide %>%
  group_by(sentiment = ifelse(log_odds_ratio > 0, "pos", "neg")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
# 막대 그래프 만들기
ggplot(new_top10, aes(x = reorder(word, log_odds_ratio),
                      y = log_odds_ratio,
                      fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
# -------------------------------------------------------------------------
# 원본 감정 사전 활용
top10 %>% 
  select(-pos, -neg) %>% 
  arrange(-log_odds_ratio) %>% 
  print(n = Inf)
# 수정한 감정 사전 활용
new_top10 %>%
  select(-pos, -neg) %>%
  arrange(-log_odds_ratio) %>%
  print(n = Inf)

# -------------------------------------------------------------------------
new_comment_wide %>%
  filter(word == "미친")
# -------------------------------------------------------------------------
# 긍정 댓글 원문
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "축하")) %>%
  select(reply)
new_score_comment %>%
  filter(sentiment == "pos" & str_detect(reply, "소름")) %>%
  select(reply)
# -------------------------------------------------------------------------
# 부정 댓글 원문
new_score_comment %>%
  filter(sentiment == "neg" & str_detect(reply, "좌빨")) %>%
  select(reply)
new_score_comment %>%
  filter(sentiment == "neg" & str_detect(reply, "못한")) %>%
  select(reply)

# 빈도분석
# 01-1 -------------------------------------------------------------------
library(here)
raw_moon <- readLines(here::here("speech_moon.txt"), encoding = "UTF-8")
head(raw_moon)
txt <- "치킨은!! 맛있다. xyz 정말 맛있다!@#"
txt
# install.packages("stringr")
library(stringr)
str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
# ------------------------------------------------------------------------
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ")
head(moon)
# ------------------------------------------------------------------------
# 파라미터명 입력
str_replace_all(string = txt, pattern = "[^가-힣]", replacement = " ")
# 파라미터명 생략
str_replace_all(txt, "[^가-힣]", " ")

# ------------------------------------------------------------------------
txt <- "치킨은 맛있다 정말 맛있다 "
txt
str_squish(txt)
# ------------------------------------------------------------------------
moon <- moon %>%
  str_squish()
head(moon)
# ------------------------------------------------------------------------
# install.packages("dplyr") #dply 패키지 없는 경우
library(dplyr)
moon <- as_tibble(moon)
moon
# ------------------------------------------------------------------------
moon <- raw_moon %>%
  str_replace_all("[^가-힣]", " ") %>% # 한글만 남기기
  str_squish() %>% # 연속된 공백 제거
  as_tibble() # tibble로 변환
# ------------------------------------------------------------------------
iris # data frame 출력
as_tibble(iris) # tibble 구조로 변환

# 01-2 --------------------------------------------------------------------
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든
권력은 국민으로부터 나온다.")
text
# ------------------------------------------------------------------------
# install.packages("tidytext")
library(tidytext)
# 문장 기준 토큰화
text %>%
  unnest_tokens(input = value, # 토큰화할 텍스트
                output = word, # 출력 변수명
                token = "sentences") # 문장 기준
# ------------------------------------------------------------------------
# 띄어쓰기 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "words") # 띄어쓰기 기준
# ------------------------------------------------------------------------
# 문자 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "characters") # 문자 기준

# ------------------------------------------------------------------------
word_space <- moon %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")
word_space

# 01-3 --------------------------------------------------------------------
word_space <- word_space %>%
  count(word, sort = T)
word_space
# ------------------------------------------------------------------------
str_count("배")
str_count("사과")
# ------------------------------------------------------------------------
# 두 글자 이상만 남기기
word_space <- word_space %>%
  filter(str_count(word) > 1)
word_space
# ------------------------------------------------------------------------
top20 <- word_space %>%
  head(20)
top20

# ------------------------------------------------------------------------
# install.packages("ggplot2")
library(ggplot2)
ggplot(top20, aes(x = reorder(word, n), y = n)) + # 단어 빈도순 정렬
  geom_col() +
  coord_flip() # 회전
# ------------------------------------------------------------------------
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + # 막대 밖 빈도 표시
  
  labs(title = "문재인 대통령 출마 연설문 단어 빈도", # 그래프 제목
       x = NULL, y = NULL) + # 축 이름 삭제
  
  theme(title = element_text(size = 12)) # 제목 크기
# ------------------------------------------------------------------------
# install.packages("ggwordcloud")
library(ggwordcloud)
ggplot(word_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), # 최소, 최대 단어 빈도
               range = c(3, 30)) # 최소, 최대 글자 크기
# ------------------------------------------------------------------------
ggplot(word_space, 
       aes(label = word, 
           size = n, 
           col = n)) + # 빈도에 따라 색깔 표현
  geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", # 최소 빈도 색깔
                       high = "#004EA1") + # 최고 빈도 색깔
  theme_minimal() # 배경 없는 테마 적용
# ------------------------------------------------------------------------
# install.packages("showtext")
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()
# ------------------------------------------------------------------------
ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "nanumgothic") + # 폰트 적용
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()
# ------------------------------------------------------------------------

font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()
ggplot(word_space,
       aes(label = word,
           size = n,
           col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "blackhansans") + # 폰트 적용
  scale_radius(limits = c(3, NA),
               range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2",
                       high = "#004EA1") +
  theme_minimal()
# ------------------------------------------------------------------------
font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext_auto()
ggplot(top20, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  
  labs(title = "문재인 대통령 출마 연설문 단어 빈도",
       x = NULL, y = NULL) +
  
  theme(title = element_text(size = 12),
        text = element_text(family = "gamjaflower")) # 폰트 적용

