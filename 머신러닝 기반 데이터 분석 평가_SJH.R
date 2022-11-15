# SJH
# [문항1]  ※ Open book, open note
# ※ 카페 내 ‘평가답안제출’ 게시판에 R code 업로드
# ※ R code안에 본인의 성명을 기입
# ※ R 파일명에 본인의 영문이니셜을 추가
# 
# ■ 아래 문제를 R code로 작성하여 제출하시오.
# 
# (의사결정트리- CART)
# 1. iris 데이터를 이용하여 CART 기법 적용(rpart()함수 이용)하여 분류분석 하시오.

# (1) 데이터 가져오기 & 샘플링
set.seed(1234)
idx <- sample(1:nrow(iris), nrow(iris) * 0.7)
train <- iris[idx, ]
test <- iris[-idx, ]

# (2) 분류모델 생성
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = train)
iris_ctree

# (3) 테스트 데이터를 이용하여 분류
pred <- predict(iris_ctree, test)

# (4) 예측정확도
table(pred, test$Species)
(16 + 15 + 12) / nrow(test)
# 정확도 0.9555556

# [문항2]  (의사결정나무 – 조건부 추론나무)
# 2. iris 데이터를 이용하여 조건부 추론나무 적용(ctree()함수 이용)하여 분류분석 하시오.

# 배점: 25점
library(party) 
# (1) 데이터 가져오기 & 샘플링
set.seed(1234)
samp <- sample(2,nrow(iris),replace = TRUE, prob=c(0.7,0.3))
trData <- iris[samp==1,] 
head(trData) 
teData <- iris[samp == 2, ] 
head(teData)

# (2) 분류모델 생성
citreeResult <- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=trData)
table(predict(citreeResult), trData$Species)
citreeResult2 <- ctree(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=teData)

# (3) 테스트 데이터를 이용하여 분류
forcasted2 <- predict(citreeResult2, data=teData)
table(forcasted2, teData$Species)

# (4) 시각화
plot(citreeResult2)

# [문항3]  (계층적 군집분석)
# 3. iris데이터 셋의 1~4번 변수를 대상으로 유클리드 거리 매트릭스를 구하여 idist에 저장한 후 계층적 클러스터링을 적용하여 결과를 시각화하시오.
# (1) 유클리드 거리 계산
idist <- dist(iris[1:4])
idist <- dist(idist, method = "euclidean")

# (2) 계층형 군집 분석(클러스터링)
hc <- hclust(idist)
hc
# (3) 분류결과를 대상으로 음수값을 제거하여 덴드로그램 시각화
plot(hc, hang = -1)

# (4) 그룹 수를 3개로 지정하여 그룹별로 테두리 표시
rect.hclust(hc, k = 3, border ="red")

# [문항4]  (K-Means 군집분석)
# 4. iris데이터에서 species 컬럼 데이터를 제거한 후 k-means clustering를 다음 단계별로 실행하시오

# (1) iris 데이터셋 로딩
idx <- sample(1:dim(iris)[1], 50) 
idx 
irisSample <- iris[idx, ] 
head(irisSample)

# (2) species 데이터 제거
irisSample$Species <- NULL 
head(irisSample)

# (3) k-means clustering 실행
hc_result <- hclust(dist(irisSample), method="ave")
hc_result

# (4) 군집 결과 시각화
plot(hc_result, hang=-1, labels = iris$Species[idx]) 
rect.hclust(hc_result, k=4)
