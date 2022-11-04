# 1. product.csv 파일의 데이터를 이용하여 다음의 단계별로 다중 회귀분석을 수행하시오.
a <- read.csv("product.csv")
a
# 1단계: 학습데이터(train), 검정데이터(test)를 7:3비율로 샘플링
x <-sample(1:nrow(a), 0.7 * nrow(a))
x
train <- a[x, ]
train # 학습데이터
test <- a[-x, ]
test # 검정데이터

# 변수모델링: y변수는 제품_만족도, x변수는 제품_적절성과 제품_친밀도
y = a$제품_만족도
x1 = a$제품_적절성
x2 = a$제품_친밀도

# 2단계: 학습데이터 이용 회귀모델 생성
library(car)
model <- lm(formula = y ~ x1 + x2, data = train)# train 으로 학습 데티어 생성
model
vif(model) # 10보다 작아서 다중공선성에 문제가없다
summary(model)
head(train, 1)

# 3단계: 검정데이터 이용 모델 예측치 생성
model <- lm(formula = 제품_만족도 ~ 제품_적절성 + 제품_친밀도, data = train)
model
pred <- predict(model, test) # 합습이된 모델에 검정데티어를 넣고
pred

# 4단계: 모델 평가: cor()함수 이용
cor(pred, test$제품_만족도) # 상관관계 분석



# 2. ggplot2 패키지에서 제공하는 diamonds 데이터 셋을 대상으로 carat, table, depth 변
# 수 중에서 다이아몬드의 가격(price)에 영향을 미치는 관계를 다중회귀 분석을 이용하여
# 예측하시오.
library(ggplot2)
library(car)
diamonds
# 조건1: 다이아몬드 가격 결정에 가장 큰 영향을 미치는 변수는?
x <-sample(1:nrow(diamonds), 0.7 * nrow(diamonds))
x
train <- diamonds[x, ]
train # 학습데이터
test <- diamonds[-x, ]
test # 검정데이터

# 조건2: 다중회귀 분석 결과를 정(+)과 부(-)관계로 해설
modle <- lm(formula = price ~ carat + table + depth, data = train)
vif(modle) # 10이상이 아니라서 다중공선성에 문제가 없다
summary(modle)
# carat 양의 상관관계
# table, depth 음의 상관관계

pred <- predict(modle, test) # 훈련시킨모델(예측모델)에 test데이터 넣기 

cor(pred,test$price) # 훈련모델과 예측모델의 상관관계