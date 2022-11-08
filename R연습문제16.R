# SJH

# [문항1]  ※ Open book, open note
# ※ 카페 내 ‘평가답안제출’ 게시판에 R code 업로드
# ※ R code안에 본인의 성명을 기입
# ※ R 파일명에 본인의 영문이니셜을 추가
# 
# ■ 아래 문제를 R code로 작성하여 제출하시오.
# 
# 평가제목 : 동일성 검정
# 1. 제공된 response.csv 파일 내 데이터에서 작업 유형에 따른 응답 정도에 차이가 있는가를 단계별로 검정하시오.
# 

# (1) 파일 가져오기 (파일 내 데이터 저장)
data<- read.csv("response.csv")
data
# (2) 코딩 변경 – 리코딩
# Job 컬럼: 1: 학생,  2: 직장인,  3: 주부
# Response 컬럼: 1: 무응답,  2: 낮음,  3: 높음
data$job2[data$job==1] <- "학생"
data$job2[data$job==2] <- "직장인"
data$job2[data$job==3] <- "주부"
data$response2[data$response ==1] <- "무응답"
data$response2[data$response ==2]<- "낮음"
data$response2[data$response ==3] <- "높음"

# (3) 교차 분할표 작성
library(gmodels)
library(ggplot2)
CrossTable(x=data$job2, y=data$response2)

# (4) 동질성 검정
chisq.test(data$job2, data$response2)

# (5) 검정 결과
# 직업과 상관없이 응답정도의 차이가 무응답이 10%미만으로 유사하다는 결과가 나왔다
# 그리고 응답낮음이 140명중 44.3%(62명)직장인이 가장 높았고,
# 응답높음이 120명중 6.7%(8명)으로 학생이 가장 낮았다.
# 동질성 검정 결과p-value(6.901e-12)< 0.05 직업 유형에 따른 응답 정도에 차이가 있다


# ============================================

# [문항2]  평가제목 : 변수선택법

# 2. attitude 데이터를 이용하여 등급(rating)에 영향을 미치는 요인을 회귀를 이용해 식별하고 후진제거법을 이용하여 적절한 변수 선택을 하여 최종 회귀식을 구하시오.


# (1) 데이터 가져오기
library(mlbench)
data("attitude")

# (2) 회귀분석 실시
data <- lm(rating ~ .,data=attitude)

# (3) 수행결과 산출 및 해석
data

# complaints, learning, raises, critical은 양의 영향,
# privileges, advance은 음의 연향을 준다

# (4) 후진제거법을 이용하여 독립변수 제거
data2 <- step(data, direction = "backward")
formula(data2)

# (5) 최종 회귀식
summary(data2)

#=============================================

# [문항3]  평가제목 : 독립성 검정
# 3. 제공된 cleanData.csv 파일 내 데이터에서 나이(age3)와 직위(position)간이 관련성을 단계별로 분석하시오.

# 1) 파일 가져오기(파일 내 데이터 저장)
data<- read.csv("cleanData.csv")
data <- na.omit(data)
# 2) 코딩 변경(변수 리코딩)
# x <- data$position # 행 – 직위변수 이용
# y <- data$age3 #열 – 나이 리코딩 변수 이용
x <- data$position
y <- data$age3
x
y
# 3) 산점도를 이용한 변수간의 관련성 보기(plot(x,y)함수 이용)
plot(x, y)

# 4) 독립성 검정
chisq.test(x, y)

# 5) 결과 해석
# p-value(2.2e-16) < 0.05 나이와 직위는 차이가 있다

# [문항4]  평가제목 : 로지스틱 회귀분석
# 4. mtcars 데이터에서 엔진(vs)을 종속변수로, 연비(mpg)와 변속기종류(am)를 독립변수로 설정하여 로지스틱 회귀분석을 실시하시오.

# (1) 데이터 가져오기
data("mtcars")
mtcars

# (2) 로지스틱 회귀분석 실행하고 회귀모델 확인
mtcars_df <- mtcars[ , c(1, 8, 9)]
mtcars_df

# (3) 로지스틱 회귀모델 요약정보 확인
str(mtcars_df)

# (4) 로지스틱 회귀식
# 학습모델(7:3)
idx <- sample(1:nrow(mtcars_df ), nrow(mtcars_df) * 0.7)
train <- mtcars_df[idx, ]
test <- mtcars_df[-idx, ]
test <- 
mtcars_model <- glm(vs ~ ., data = train, family = 'binomial', na.action=na.omit)
mtcars_model
summary(mtcars_model)

pred <- predict(mtcars_model,newdata = test, type = "response")
pred

# (5) mpg가 30이고 자동변속기(am=0)일 때 승산(odds)?
result_pred <- ifelse(pred >= 30,1,0)
result_pred
table(result_pred)

#   [문항5]  평가제목 : 분산분석
# 5. 새롭게 제작된 자동차의 성능(주행거리(마일)/갤런)을 -30도, 0도, 30도의 기온 하에 성능을 측정하였다. 각 기온당 측정된 성능데이터의 수는 4개였다. 성능데이터로부터 다음의 ANOVA 테이블을 구성하였다. 빈칸에 들어갈 숫자(정수)와 숫자를 계산한 식을 제시하시오.
# 1) (a) 값 36.00072x2=72.00144
# 2) (b) 값 3-1=2
# 3) (c) 값 10.8x3.3334=36.00072
# 4) (d) 값 3.3334x9=30.0006
# 5) (e) 값 12-3=9
# 6) (f) 값 72.00144+30.0006=102.00204
# 7) (g) 값 12-1=11