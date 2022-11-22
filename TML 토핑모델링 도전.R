# sjh
# 연설문 불러오기
library(readr)
speeches_raw <- read_csv("speeches_roh.csv")
# 문장 기준 토큰화
library(dplyr)
library(tidytext)
speeches <- speeches_raw %>%
  unnest_tokens(input = content,
                output = sentence,
                token = "sentences",
                drop = F)
# 전처리
library(stringr)
speeches <- speeches %>%
  mutate(sentence = str_replace_all(sentence, "[^가-힣]", " "),
         sentence = str_squish(sentence))
# 명사 추출
library(tidytext)
library(KoNLP)
library(stringr)
nouns_speeches <- speeches %>%
  unnest_tokens(input = sentence,
                output = word,
                token = extractNoun,
                drop = F) %>%
  filter(str_count(word) > 1)
# 연설문 내 중복 단어 제거
nouns_speeches <- nouns_speeches %>%
  group_by(id) %>%
  distinct(word, .keep_all = T) %>%
  ungroup()
# 단어 빈도 100회 이하 단어 추출
nouns_speeches <- nouns_speeches %>%
  add_count(word) %>%
  filter(n <= 100) %>%
  select(-n)

stopword <- c("들이", "하다", "하게", "하면", "해서", "이번", "하네",
              "해요", "이것", "니들", "하기", "하지", "한거", "해주",
              "그것", "어디", "여기", "까지", "이거", "하신", "만큼")
# 불용어 제거
nouns_speeches <- nouns_speeches %>%
  filter(!word %in% stopword)

# 연설문별 단어 빈도 구하기
count_word_doc <- nouns_speeches %>%
  count(id, word, sort = T)
# DTM 만들기
dtm_comment <- count_word_doc %>%
  cast_dtm(document = id, term = word, value = n)
# 토픽 수 바꿔가며 LDA 모델 만들기
library(ldatuning)
models <- FindTopicsNumber(dtm = dtm_comment,
                           topics = 2:20,
                           return_models = T,
                           control = list(seed = 1234))
# 최적 토픽 수 구하기
FindTopicsNumber_plot(models)

lda_model <- models %>%
  filter (topics == 9) %>%
  pull(LDA_model) %>%
  .[[1]]

# beta 추출
term_topic <- tidy(lda_model, matrix = "beta")
# 토픽별 beta 상위 단어 추출
top_term_topic <- term_topic %>%
  group_by(topic) %>%
  slice_max(beta, n = 10)
top_term_topic
# 막대 그래프 만들기
library(ggplot2)
ggplot(top_term_topic,
       aes(x = reorder_within(term, beta, topic),
           y = beta,
           fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip () +
  scale_x_reordered() +
  labs(x = NULL)
# gamma 추출
doc_topic <- tidy(lda_model, matrix = "gamma")
# 문서별로 확률이 가장 높은 토픽 추출
doc_class <- doc_topic %>%
  group_by(document) %>%
  slice_max(gamma, n = 1)
# 변수 타입 통일
doc_class$document <- as.integer(doc_class$document)
# 연설문 원문에 확률이 가장 높은 토픽 번호 부여
speeches_topic <- speeches_raw %>%
  left_join(doc_class, by = c("id" = "document"))
speeches_topic %>%
  count(topic)
speeches_topic %>%
  filter(topic == 9) %>%
  arrange(-gamma) %>%
  select(content)
