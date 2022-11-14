# SJH

# 1. iris데이터 셋이 1-4번째 변수를 대상으로 유클리드 거리 매트릭스를 구하여 idist에 저
# 장한 후 계층적 클러스터링을 적용하여 결과를 시각화하시오.
library(cluster)
idist <- head(iris,n =4)
idist <- subset(idist, select=-Species)
idist
# 1단계: 유클리드 거리 계산
dist <- dist(idist, method = "euclidean")
dist

# 2단계: 계층형 군집분석(클러스터링)
hc <- hclust(dist)
hc

# 3단계: 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(hc, hang = -1)

# 4단계: 그룹 수를 4개로 지정하고 그룹별로 테두리 표시
rect.hclust(hc, k = 3, border ="red")

# 2. product_sales.csv 내 데이터를 이용하여 단계별로 비계층적 군집 분석을 수행하시오.
product <- read.csv("product_sales.csv")
t <- sample(1:nrow(product), 100)
test <- product[t, ]
dim(test)
head(test)

mydia <- test[c("tot_price", "visit_count", "buy_count", "avg_price")]
head(mydia)

# 1단계: 비계층적 군집분석: 3개의 군집으로 군집화
result2 <- kmeans(mydia, 3)
names(result2)
result2$cluster

# 2단계: 원형데이터에 군집 수 추가
mydia$cluster <- result2$cluster
head(mydia)

# 3단계: tot_price변수와 가장 상관계수가 높은 변수와 군집 분석 시각화
cor(mydia[ , -5], method = "pearson")
# 0.87262380 avg_price가 tot_price와 상관계수가 가장 높음
plot(mydia[ , -5])


library(mclust)
library(corrgram)

corrgram(mydia[ , -5], upper.panel = panel.conf)
corrgram(mydia[ , -5], lower.panel = panel.conf)

# 4단계: 군집의 중심점 표
plot(mydia$tot_price, mydia$avg_price, col = mydia$cluster)
points(result2$centers[, c("tot_price", "avg_price")],
       col = 1:4, pch = 8, cex = 4)


