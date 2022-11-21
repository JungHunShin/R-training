# 유의어 통일하기
comment <- comment %>%
  mutate(word = case_when(str_detect(word, "축하") ~ "축하",
                          str_detect(word, "방탄") ~ "자랑",
                          str_detect(word, "대단") ~ "대단",
                          str_detect(word, "자랑") ~ "자랑",
                          T ~ word))
library(readr)
library(dplyr)
raw_news_comment <- read_csv("news_comment_BTS.csv")
glimpse(raw_news_comment)

library(stringr)
library(textclean)
news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(id = row_number(),
         reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply))
news_comment %>%
  select(id, reply)

# 품사 기준 토큰화
library(tidytext)
library(KoNLP)
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)

library(tidyr)
comment_pos <- comment_pos %>%
  separate_rows(word, sep = "[+]")
comment_pos %>%
  select(word, reply)

# 명사, 동사, 형용사 추출
comment <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

comment %>%
  select(word, reply)

# 유의어 통일하기
comment <- comment %>%
  mutate(word = case_when(str_detect(word, "축하") ~ "축하",
                          str_detect(word, "방탄") ~ "자랑",
                          str_detect(word, "대단") ~ "대단",
                          str_detect(word, "자랑") ~ "자랑",
                          T ~ word))

# 단어를 댓글별 한 행으로 결합
line_comment <- comment %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))
line_comment

# 바이그램 토큰화
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment

# 바이그램 단어쌍 분리
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram

# 네트워크 그래프 데이터 만들기
library(tidygraph)
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 3) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_bigram

library(ggraph)
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5) +
  theme_graph()

# 그래프 꾸미기
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
set.seed(1234)
ggraph(graph_bigram, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph() # 배경 삭제
