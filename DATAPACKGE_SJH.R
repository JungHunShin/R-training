# [문항1]  연관 능력단위명 : 빅데이터 품질관리시스템 개발 / 난이도 : 3(중)
# 평가 제목 : 패키지 설치
# 
# ■ data.table 패키지와 아래 데이터를 이용하여 조건에 맞게 R로 coding하시오.
# 
# - 데이터: 2015년 Programme for International Student Assessment (PISA) 데이터셋
# https://www.mediafire.com/file/c2kesbkmdezzpg0/pisa2015.rar/file

# 1. data.table package 설치 및 loading 하시오.
 
install.packages("data.table")
library(data.table)

# 2. pisa2015.csv파일을 data.table 패키지 내 함수를 이용하여 읽어 변수 pisa에 저장하시오.
pisa <- fread("pisa2015.csv", na.strings = "")

# 3. pisa2015.csv파일 내 데이터의 size를 보이시오.
print(object.size(pisa), unit = "GB")

# 4. pisa데이터 중 Korea과 Japan 데이터만 eastasia2.csv 파일로 저장하시오.
region6 <- subset(pisa, CNT %in%  c("Japan", "Korea"))
fwrite(region6, file = "eastasia2.csv")

# 5. pisa데이터 첫 5줄과 마지막 5줄을 한꺼번에 보이시오.
pisa

# 6. pisa데이터에서 CNTRYID 맨 마지막 10줄 데이터를 보이시오
tail(pisa$CNTRYID,n=10)

# 7. 한국과 일본에서 물리시험(ST063Q01NA)에 참가한 학생수를 보이시오.
pisa[CNTRYID %in% c("Korea", "Japan"),table(ST063Q01NA)]

# 8. 한국과 일본 학생들의 과학 자기효능감(Science Self-efficacy scale) 항목(SCIEEFF)의 평균, 표준편차, 최소값, 중간값(median), 최대값을 구하여 보이시오. (NA는 제외하시오)
summary(na.omit(pisa[,.(CNTRYID, SCIEEFF)]))

# 9. 한국과 일본 학생들의 긴장여부(ST118Q04NA)를 tense라는 중간변수(intermediate variable)를 생성하며 화면에 출력하시오.
# 긴장여부는 "Strongly disagree", "Disagree", "Agree", "Strongly agree")로 구분.
pisa[CNTRYID %in% c("Mexico", "Japan"),
     .(tense = factor(ST118Q04NA, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree")))
][,table(tense)]

# 10. 과학 자기효능감 지수(SCIEEFF)와 과학 흥미지수(JOYSCIE) 데이터를 이용하여 산포도 (scatter plot)를 그리시오. (NA는 제외하시오)
pisa[CNTRYID %in% c("Mexico","Japan"),
     .(plot(y = SCIEEFF, x = JOYSCIE, 
            col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3)), 
       xbar.joyscie = mean(JOYSCIE, na.rm = T))]