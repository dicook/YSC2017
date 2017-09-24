# Tidying tb data - example 4
tb <- tb %>%
  gather(var, count, m_04:f_u) %>%
  separate(var, c("gender", "age")) %>%
  filter(!(age %in% c("014", "04", "514", "u")))
head(tb)

# Numbers in Australia
tb %>%
  filter(iso2 == "AU") %>%
  ggplot(aes(x = year, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(~ age)

# Tidying pew survey data - example 5
pew %>%
  gather(income, count, -religion) %>%
  group_by(religion) %>%
  mutate(prop = count / sum(count)) %>%
  head()

# Relationship between income and religion

pew %>%
  gather(income, count, -religion) %>%
  group_by(religion) %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(aes(income, prop)) +
  geom_line(aes(group = religion)) +
  facet_wrap(~religion, ncol=6) +
  scale_x_discrete(labels=c("<$10k"="10", "$10-20k"="20", "$20-30k"="30",
                            "$30-40k"="40", "$40-50k"="50", "$50-75k"="75",
                            "$75-100k"="100", "$100-150k"="150", ">150k"="200",
                            "Don't know/refused"="N"))

# PISA

load("pisa_au.rda")
pisa_au <- pisa_au %>% mutate(state=as.character(substr(STRATUM, 4, 5)),
                              schtype_yr=as.character(substr(STRATUM, 6, 7))) %>%
  mutate(state=recode(state, "01"="ACT", "02"="NSW", "03"="VIC",
                      "04"="QLD", "05"="SA", "06"="WA", "07"="TAS", "08"="NT")) %>%
  mutate(schtype_yr=recode(schtype_yr,
                           "01"="Catholic_Y10", "02"="Catholic_noY10",
                           "03"="Gov_Y10", "04"="Gov_noY10",
                           "05"="Ind_Y10", "06"="Ind_noY10",
                           "07"="Catholic_Y10", "08"="Catholic_noY10",
                           "09"="Gov_Y10", "10"="Gov_noY10",
                           "11"="Ind_Y10", "12"="Ind_noY10",
                           "13"="Catholic_Y10", "14"="Catholic_noY10",
                           "15"="Gov_Y10", "16"="Gov_noY10",
                           "17"="Ind_Y10", "18"="Ind_noY10",
                           "19"="Catholic_Y10", "20"="Catholic_noY10",
                           "21"="Gov_Y10", "22"="Gov_noY10",
                           "23"="Ind_Y10", "24"="Ind_noY10",
                           "25"="Catholic_Y10", "26"="Catholic_noY10",
                           "27"="Gov_Y10", "28"="Gov_noY10",
                           "29"="Ind_Y10", "30"="Ind_noY10",
                           "31"="Catholic_Y10", "32"="Catholic_noY10",
                           "33"="Gov_Y10", "34"="Gov_noY10",
                           "35"="Ind_Y10", "36"="Ind_noY10",
                           "37"="Catholic_Y10", "38"="Catholic_noY10",
                           "39"="Gov_Y10", "40"="Gov_noY10",
                           "41"="Ind_Y10", "42"="Ind_noY10",
                           "43"="Catholic_Y10", "44"="Catholic_noY10",
                           "45"="Gov_Y10", "46"="Gov_noY10",
                           "47"="Ind_Y10", "48"="Ind_noY10")) %>%
  separate(schtype_yr, c("schtype","yr")) %>%
  rename(birthmonth=ST003D02T, birthyr=ST003D03T,
         gender=ST004D01T, desk=ST011Q01TA,
         room=ST011Q02TA, computer=ST011Q04TA, internet=ST011Q06TA,
         solarpanels=ST011D17TA, tvs=ST012Q01TA, cars=ST012Q02TA,
         music_instr=ST012Q09NA, books=ST013Q01TA, birthcnt=ST019AQ01T,
         mother_birthcnt=ST019BQ01T, father_birthcnt=ST019CQ01T,
         test_anxiety=ST118Q01NA, ambitious=ST119Q04NA,
         prefer_team=ST082Q01NA, make_friends_easy=ST034Q02TA,
         tardy=ST062Q03TA, science_fun=ST094Q01NA, breakfast=ST076Q01NA,
         work_pay=ST078Q10NA, sport=ST078Q11NA, internet_use=IC006Q01TA,
         install_software=IC015Q02NA,
         outhours_study=OUTHOURS, math_time=MMINS, read_time=LMINS,
         science_time=SMINS, belong=BELONG,
         anxtest=ANXTEST, motivat=MOTIVAT, language=LANGN,
         home_edres=HEDRES, home_poss=HOMEPOS, wealth=WEALTH,
         stuweight=W_FSTUWT) %>%
  mutate(math=(PV1MATH+PV2MATH+PV3MATH+PV4MATH+PV5MATH+
                 PV6MATH+PV7MATH+PV8MATH+PV9MATH+PV10MATH)/10,
         science=(PV1SCIE+PV2SCIE+PV3SCIE+PV4SCIE+PV5SCIE+
                    PV6SCIE+PV7SCIE+PV8SCIE+PV9SCIE+PV10SCIE)/10,
         read=(PV1READ+PV2READ+PV3READ+PV4READ+PV5READ+
                 PV6READ+PV7READ+PV8READ+PV9READ+PV10READ)/10) %>%
  select(state, schtype, yr, birthmonth, birthyr, gender, desk, room,
         computer, internet, solarpanels, tvs, cars, music_instr, books,
         birthcnt, mother_birthcnt, father_birthcnt, test_anxiety,
         ambitious, prefer_team, make_friends_easy, tardy, science_fun,
         breakfast, work_pay, sport, internet_use, install_software,
         outhours_study, math_time, read_time, science_time, belong,
         anxtest, motivat, language, home_edres, home_poss, wealth,
         stuweight, math, science, read) %>%
  mutate(gender=factor(gender, levels=1:2, labels=c("female", "male"))) %>%
  mutate(birthmonth=factor(birthmonth, levels=1:12,
                           labels=c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug",
                                    "sep", "oct", "nov", "dec")))

pisa_au %>% group_by(state) %>% summarise(m=weighted.mean(math, stuweight))

pisa_au %>% group_by(state, gender) %>%
  summarise(m=weighted.mean(math, stuweight)) %>%
  spread(gender, m) %>%
  mutate(dif=female-male)

fit <- lm(math~anxtest, weights=stuweight, data=pisa_au)
summary(fit)


