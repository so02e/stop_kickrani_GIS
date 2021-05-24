library(car)
library(corrplot)
library(lmtest)
library(sf)
library(tmap)
library(dplyr)
library(sp)
library(sf)
library(RANN)
library(spdep)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(maptools)
library(gvlma)
library(car)
library(tidyr)
library(lmtest)
library(leaps)
library(ggplot2)
library(broom)
library(AER)
library(MASS)
library(rsq)
library(gridExtra)
library(spgwr)
library(spatialreg)
library(geojsonio)


final_data <- st_read('output/그리드 전처리 완료/analysis1_dataset.geojson')
# final_data <- st_read('output/그리드 전처리 완료/analysispo_dataset.geojson')

final_data %>% head(5)


dat <- data.frame(final_data) %>% dplyr::select(-c(gid, geometry))
dat %>% head()



options(repr.plot.width = 10, repr.plot.height = 10)

tmap_mode("plot")
tm_shape(final_data) + tm_fill('acci_cnt', style='pretty', palette='Reds') + tm_borders(alpha=.1)



## OLS 회귀분석
# (1) 모든 변수
lm_ols <- lm(acci_cnt ~ . , data = dat)
summary(lm_ols)

# (2) 단계적 선택법
lm_ols.step <- step(lm_ols, direction = 'both')
summary(lm_ols.step)
## 2030val, 무인카메라, 신호등, 버스아침승차, 버스저녁하차, 생활서비스,소매점,음식점, 교육,퇴근시간자전거, 지하철아침승차, 지하철아침하차

## 다중공산성 확인 : 10 이하
vif(lm_ols.step)

bptest(lm_ols.step)

## 기본가정 1 확인
qqnorm(lm_ols.step$residuals)





## 포아송회귀분석
# (1) 모든 변수
lm_poisson <- glm(acci_cnt~., family=poisson(), data=dat)
summary(lm_poisson)

# (2) 단계적 선택법
lm_poisson.step <- step(lm_poisson, direction='both')
summary(lm_poisson.step)



dispersiontest(lm_poisson.step)

exp(lm_poisson.step$coefficients)




## 음이항회귀분석

lm_nb <- glm.nb(acci_cnt~., data=dat)
summary(lm_nb)
# warnings()

## 2030val 신호등 버스저녁하차 생활서비스 소매점 음식점 퇴근시간자전거 
lm_nb.step <- step(lm_nb, direction='both')
summary(lm_nb.step)

exp(coef(lm_nb.step))

exp(lm_nb.step$coefficients)



set = read.csv('output/그리드 전처리 완료/final4_0524.csv',sep = ",",header = TRUE)
set

set <- data.frame(set) %>% dplyr::select(-c(gid))

#set <- data.frame(set) %>% dplyr::select(-c(acci_cnt))
set








pre <- predict(lm_nb.step, newdata=set,interval="prediction")
pre <- as.data.frame(pre)
head(pre)


final_data$lm_nb.step<-lm_nb.step$fitted.values 

write.csv(final_data,"output/그리드 전처리 완료/negativeregression_result.csv")


write.csv(pre,"output/그리드 전처리 완료/negativeregression.csv")





nb1 <- glm(acci_cnt~., data=dat, family = quasipoisson())
summary(nb1)
# warnings()

## 2030val 신호등 버스저녁하차 생활서비스 소매점 음식점 퇴근시간자전거 
nb1.step <- step(nb1, direction='both')
summary(nb1.step)

exp(coef(nb1.step))

exp(nb1.step$coefficients)

