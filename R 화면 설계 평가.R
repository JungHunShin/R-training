# # sjh
# [문항1]  아래 문제를 R code로 작성하여 제출하시오.
# - Open book, open note
# - 대면/비대면 응시자는 카페 내 ‘평가답안제출’ 게시판에 R code 업로드
# - R code안에 본인의 성명을 기입
# - R 파일명에 본인의 영문이니셜을 추가

# (분류분석)
# 1. iris 데이터를 이용하여 인공신경망 기법 또는 xgboost 기법을 이용하여 분류분석 하시오.
# (1) 데이터 가져오기 및 샘플링 하시오. (샘플링 시 species별 데이터가 같게 하시오)
library(xgboost)
set.seed(1234)

data(iris)
iris
iris_label <- ifelse(iris$Species == 'setosa', 0,
                     ifelse(iris$Species == 'versicolor', 1, 2))
table(iris_label)
iris$label <- iris_label
idx <- sample(nrow(iris), 0.7 * nrow(iris))
train <- iris[idx, ] 
test <- iris[-idx, ]

# [문항2]  (분류분석)
# (2) 분류모델을 생성하시오.
train
train_mat <- as.matrix(train[-c(5:6)])
train_lab <- train$label
dtrain <- xgb.DMatrix(data = train_mat, label = train_lab)
xgb_model <- xgboost(data = dtrain, max_depth = 2, eta = 1,
                     nthread = 2, nrounds = 2,
                     objective = "multi:softmax", 
                     num_class = 3,
                     verbose = 0)
xgb_model
test_mat <- as.matrix(test[-c(5:6)])


# [문항3]  (분류분석)
# (3) 테스트 데이터를 이용하여 분류하시오.
pred_iris <- predict(xgb_model, test_mat)

# [문항4]  (분류분석)
# (4) 예측정확도를 산출하시오.
table(pred_iris, test_lab)
(15+9+13)/length(test_lab)
# 0.8222222

# [문항5]  (분석결과 시각화)
# 2. iris 데이터를 대상으로 다음 조건에 맞게 시각화 하시오.
# (1) 1번 컬럼을 x축으로 하고 3번 컬럼을 y축으로 하고 5번 컬럼을 색상 지정하시오.
plot(iris[,1], iris[,3], col = iris[,5])

# [문항6]  (분석결과 시각화)
# (2) 차트 제목을 “Scatter plot for iris data”로 설정하시오.
plot(iris[,1], iris[,3], col = iris[,5], main = "Scatter plot for iris data")

# [문항7]  (분석결과 시각화)
# (3) 작성한 차트를 파일명이 “iris_(본인 영문이니셜).jpg”인 파일에 저장하고 제출하시오
jpeg(filename="iris_sjh.jpg")
plot(iris[,1], iris[,3], col = iris[,5], main = "Scatter plot for iris data")
dev.off()
# [문항8]  (분석결과 시각화)
# 3. diamonds 데이터 셋을 대상으로
# (1) x축에 carat변수, y축에 price변수를 지정하고, clarity변수를 선 색으로 지정하여 미적 요소 맵핑 객체를 생성하시오.
library(ggplot2)
qplot(carat,price, data = diamonds, color = clarity)

# [문항9]  (분석결과 시각화)
# (2) 산점도 그래프 주변에 부드러운 곡선이 추가되도록 레이아웃을 추가하시오.
qplot(carat,price, data = diamonds, color = clarity)+geom_smooth()

# [문항10]  (분석결과 시각화)
# (3) 작성한 차트를 파일명이 “diamonds_(본인 영문이니셜).jpg”인 파일에 저장하고 제출하시오.
ggsave(file = "C:/Rwork/diamond_sjh.jpg")
