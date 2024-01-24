library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

raw_savData <- read.spss(file = "Koweps_hpc16_2021_beta1.sav", to.data.frame = T)
savData <- raw_welfare 
View(savData)
head(savData)
dim(savData)
tail(savData)

savData <- rename(savData,
                  healthStatus = h10_med2, # 건강 상태
                  birth = h10_g4, # 태어난 년도
                  workAbility = h10_eco2, # 근로 능력 정도
                  region = h10_g11) # 종교 유무
class(savData$healthStatus)
class(savData$birth)
class(savData$workAbility)
class(savData$region)

table(savData$healthStatus)
table(savData$birth)
table(savData$workAbility)
table(savData$region)

# 전처리
# 태어난 년도 -> 연령대별로 전처리
savData$birth <- ifelse(savData$birth == 9999, NA, savData$birth)
savData$age <- 2021 - savData$birth + 1
savData <- savData %>%
  filter(!is.na(age)) %>%
  mutate(ageg = ifelse(age < 10, "0대", 
                ifelse(age < 20, "10대",
                ifelse(age < 30, "20대",
                ifelse(age < 40, "30대",
                ifelse(age < 50, "40대",
                ifelse(age < 60, "50대",
                ifelse(age < 70, "60대",
                ifelse(age < 80, "70대",
                ifelse(age < 90, "80대", "90대 이상"))))))))))
qplot(savData$ageg)
# 건강 상태 전처리
savData$healthStatus <- ifelse(savData$healthStatus == 1, "아주 건강하다",
                        ifelse(savData$healthStatus == 2, "건강한 편이다",
                        ifelse(savData$healthStatus == 3, "보통이다",
                        ifelse(savData$healthStatus == 4, "건강하지 않은 편이다",
                        ifelse(savData$healthStatus == 5, "건강이 아주 안 좋다", NA)))))
qplot(savData$healthStatus) + coord_flip()
# 근로 능력 전처리
savData$workAbility <- ifelse(savData$workAbility == 1, "근로 가능",
                        ifelse(savData$workAbility == 2, "단순 근로 가능",
                        ifelse(savData$workAbility == 3, "단순 근로 미약자",
                        ifelse(savData$workAbility == 4, "근로 능력 없음", NA))))
qplot(savData$workAbility) + coord_flip()
# 종교 유무 전처리
savData$region <- ifelse(savData$region == 1, "있음", "없음")
qplot(savData$region)

# 관계 분석
# 1. 연령대에 따른 건강 상태 비율
ageg_healthStatus <- savData %>%
                      group_by(ageg, healthStatus) %>%
                      summarise(statusNumber = n()) %>%
                      mutate(tot_group = sum(statusNumber)) %>%
                      mutate(pct = round(statusNumber/tot_group*100, 1))
ageg_healthStatus
ggplot(data = ageg_healthStatus, aes(x = ageg, y = pct, fill = healthStatus)) 
+ geom_col() + coord_flip()
# 2. 연령대에 따른 근로 능력 정도
ageg_workAbility <- savData %>% 
                      filter(!is.na(workAbility)) %>%
                      group_by(ageg, workAbility) %>%
                      summarise(statusNumber = n()) %>%
                      mutate(tot_group = sum(statusNumber)) %>%
                      mutate(pct = round(statusNumber/tot_group*100, 1))
ageg_workAbility
ggplot(data = ageg_workAbility, aes(x = ageg, y = pct, fill = workAbility)) + 
  geom_col() + coord_flip()
# 3. 연령대에 따른 종교 유무
ageg_region <- savData %>%
                group_by(ageg, region) %>%
                summarise(statusNumber = n()) %>%
                mutate(tot_group = sum(statusNumber)) %>%
                mutate(pct = round(statusNumber/tot_group*100, 1))
ageg_region
ggplot(data = ageg_region, aes(x = ageg, y = statusNumber, fill = region)) 
+ geom_col(position = "dodge") + coord_flip()
