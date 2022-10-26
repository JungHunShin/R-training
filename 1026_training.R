# SJH
# 1. 교육 방법에 따라 시험성적에 차이가 있는지 검정하시오
# (힌트. 두 집단 평균 차이 검정)
# 1) 데이터셋: twomethod.csv 
# 2) 변수: method(교육방법), score(시험성적)
# 3) 모델: 교육방법(명목) -> 시험성적(비율)
# 4) 전처리, 결측치 제거

setwd("C:/Rwork/ ")
data <- read.csv("twomethod.csv", header = TRUE)
head(data)
data<- na.omit(data) #na  제거
a <- subset(data, method ==1)
b <- subset(data, method ==2)
a <- a$score
b <- b$score

#동절성 확인
var.test(a, b)

#두 집단 평균 차이 검정(양측검정)
t.test(a, b, altr = "two.sided", conf.int = TRUE, conf.level = 0.95)
#교육 방법에 따라 시험성적에 차이가 있다

#방형성 갖는 단측 검정
t.test(a, b, alter = "greater", 
       conf.int = TRUE, conf.level = 0.95)
t.test(a, b, alter = "less", 
       conf.int = TRUE, conf.level = 0.95)


# 2. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서 만족도에 차이가 있는가를
# 검정하시오.
# (힌트. 두 집단 비율 차이 검정)
# 1) 데이터셋: two_sample.csv 
# 2) 변수: gender(1,2), survey(0, 1)
setwd("C:/Rwork/ ")
data_two <- read.csv("two_sample.csv", header = TRUE)
head(data)
data_two<- na.omit(data_two)
x <- data_two$gender
y <- data_two$survey
table(x)
table(y)

#교차분석
table(x, y, useNA = "ifany")

#두 집단 비율 차이 검정
prop.test(c(138, 107), c(174, 126),
          alternative = "two.sided", conf.level = 0.95)
# 대학에 대해서 만족도 차이가 없다

#방형성 갖는 단측 검정
prop.test(x, y, alter = "greater", 
       conf.int = TRUE, conf.level = 0.95)
prop.test(x, y, alter = "less", 
       conf.int = TRUE, conf.level = 0.95)

# 3. 우리나라 전체 중학교 2 학년 여학생 평균 키가 148.5cm 로 알려진 상태에서 A 중학교 2 학년
# 전체 500 명을 대상으로 10%인 50 명을 표본으로 선정하여 표본평균 신장을 계산하고 모집단의
# 평균과 차이가 있는 지를 단계별로 분석을 수행하여 검정하시오.
# 1) 데이터셋: student_height,csv
# 2) height <- stheight$height
setwd("C:/Rwork/ ")
stheight <- read.csv("student_height.csv", header = TRUE)
head(stheight)
stheight <- na.omit(stheight)
height <- stheight$height

# 3) 기술통계량 평균 계산
mean(height)

# 4) 정규성 검정
shapiro.test(height)

# 5) 가설 검정
t.test(height, mu = 148.5, alter = "two.side", conf.level = 0.95)
# 모집단의 평균과 차이가 단계별로 차이가 없다

#방형성 갖는 단측 검정
t.test(height, mu = 148.5, alter = "greater", conf.level = 0.95)
t.test(height, mu = 148.5, alter = "less", conf.level = 0.95)


# 4. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 기존
# 구매비율보다 15% 향상되었는지를 단계별로 분석을 수행하여 검정하시오.
# 귀무가설(H0):HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 기존 구매비율보다 15% 향상 되지 않는다 
# 연구가설(H1):HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 기존 구매비율보다 15% 향상 되지 된다
# 1) 구매여부 변수: buy (1: 구매하지 않음, 2: 구매)
# 2) 데이터셋: hdtv.csv
setwd("C:/Rwork/")
data <- read.csv("hdtv.csv", header = TRUE)
head(data)
data <- na.omit(data)
buy <- data$buy
library(prettyR)

# 3) 빈도수와 비율 계산
freq(buy)

# 4) 가설 검정
setwd("C:/Rwork/")
data <- read.csv("hdtv.csv", header = TRUE)
head(data)
data <- na.omit(data)
buy <- data$buy
library(prettyR)

# 빈도수와 비율계산
freq(buy)
table(buy)

# 가설검정 
binom.test(10, 50, p = 0.15, alternative = "two.sided", conf.level = 0.95)
# 구매비율보다 15% 향상 되지 않았다

#방형성 갖는 단측 검정
binom.test(c(10, 50), p = 0.15, 
           alternative = "greater", conf.level = 0.95)
binom.test(c(10, 50), p = 0.15, 
           alternative = "less", conf.level = 0.95)