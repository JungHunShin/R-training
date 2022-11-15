# SJH

# 1. tranExam.csv 파일을 대상으로 중복된 트랜잭션 없이 1-2컬럼만 single형식으로 트랜잭
# 션 객체를 생성하시오.
install.packages("arules")
library(arules)

# 1단계: 트랜잭션 객체 생성 및 확인
setwd("C:/Rwork/ ")
stran <- read.transactions("tranExam.csv", format = "single", sep = ",", cols = c(1, 2),rm.duplicates = TRUE)
stran

# 2단계: 각 items별로 빈도수 확인
inspect(stran)

# 3단계: 파라미터(supp = 0.3, conf = 0.1)를 이용하여 규칙(rule)생성
ar <- apriori(stran, parameter = list(supp = 0.3, conf = 0.1))
inspect(ar)

# 4단계: 연관규칙 결과 보기
# 4와 1의 향상도가 1.25로 가장 큰 연관성이 보여고 있다
# 2와 3의 향상도가 1.11로 가장 큰 연관성이 보이고 있다
# 그 밖에 4와 2,3은 비교해본 것이 없어 추가적인 확이이 필요하다


# 2. Adult데이터 셋을 대상으로 다음 조건에 맞게 연관분석을 수행하시오.
data("Adult")
# 1) 최소 support = 0.5, 최소 confidence = 0.9를 지정하여 연관규칙을 생성한다.
ar1 <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9))

# 2) 수행한 결과를 lift기준으로 정렬하여 상위 10개 규칙을 기록한다.
inspect(head(sort(ar1, by = "lift")))

# 3) 연관분석 결과를 LHS와 RHS의 빈도수로 시각화한다.
library(arulesViz)
plot(ar2,method = "grouped")

# 4) 연관분석 결과를 연관어의 네트워크 형태로 시각화한다.
plot(ar2, method = "graph")
Adult

# 5) 연관어 중심 단어를 해설한다.
redwine <- subset(ar2, rhs %in% 'sex=Male')
redwine
inspect(redwine)
plot(redwine, method = "graph")
# 성관계를 하는 남자는 남편일 확률이 높다
