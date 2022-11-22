##作业1
iri <- read.csv("iri.csv")
iri_sub <- iri |>
  filter(STATE_CODE == 6 & stringr::str_detect(SHRP_ID, "050"))

iri_sub1 <- iri |>
  group_by(STATE_CODE, SHRP_ID) |>
  summarise(
    iri_max = max(IRI),
    iri_min = min(IRI),
    iri_mean = mean(IRI),
  )

iri_sub2 <- iri_sub1 |>
   arrange(desc(iri_mean))

iri_sub3 <- iri |>
  filter(VISIT_DATE == "1/10/96, 12:00:00 AM" )
ggplot(iri_sub3, aes(x=VISIT_DATE, y=IRI))+
  geom_point()

##作业2
accident <- read.csv("ACCIDENT.csv")
person <- read.csv("PERSON.csv")
vehicle <- read.csv("VEHICLE.csv")
acc_per <- inner_join(accident, person, by=c("CASENUM", "PSU"))

accident_sub <- accident |>
  group_by(REGION) |>
  summarise(
    observation = n()
  )

acc_veh <- left_join(accident, vehicle, by=c("CASENUM", "PSU"))
dim(acc_veh)
sum(is.na(acc_veh))


