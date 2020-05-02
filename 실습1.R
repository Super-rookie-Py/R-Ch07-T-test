###  Ch07.t-검정

###  2020/05/02 keonwoo park

## 실습1

## K대학에서는 재학생 만족도 조사를 실시하였다.
## 재학생 만족도가 50점이라고 할 수 있는가?

satis <-read.csv('Ch0702.satisfaction.csv',
                 header = TRUE,
                 na.strings = '.')
satis
length(satis$no) # 자료 200개

library(psych)
describe(satis$satis)


opar <- par(no.readonly =TRUE) #디폴트 par 값을 미리 할당--> par(opar)

par(mfrow=c(1,2)) #화면 분할 행, 열로 화면을 쪼갠다.
boxplot(satis$satis)
hist(satis$satis,
     breaks=10,
     col='red',
     xlab='점수',
     ylab='개수',
     main = '만족도점수')

par(opar) #분할 복구

options("scipen" = 20) # 지수 표기법 수정: 2.2e-4 = 0.00022 소수점20자리까지는 보여줘라
t.test(satis$satis,
       alternative= c("two.sided"), #양측검정
       mu = 50.0, #평균 mu 값 입력(귀무가설)검정값
       conf.level = 0.95) # 신뢰도 설정

# p-value 0.9898 -> 귀무가설이 참이다.

mu=50
se=1.21 # 표본이므로 표준편차sd 대신 표준오차 se사용
inter = qt(p=0.025, df=199) # 95퍼 신뢰구간 df=자유도 n-1
data <- rnorm(1000, mu, se)
data <- sort(data) 
plot(data,
     dnorm(data, mu, se),
     type = 'l',
     main = '만족도 검정',
     xlim=c(30,70))
abline(v=mu, col='green', lty=5) #수직라인 초록색평균
abline(v=mu+inter*se,col='blue',lty=5) #신뢰구간 상한
abline(v=mu-inter*se,col='blue',lty=5) #신뢰구간 하한
abline(v=49.98, col='red', lty=5) #표본의 평균


