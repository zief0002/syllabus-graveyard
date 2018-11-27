library(readr)
library(dplyr)
library(mlmRev)

head(bdf)

#summary(lmer(langPOST ~ 1 + (1 | schoolNR), data = bdf, REML = FALSE))


bdf_l1 = bdf %>%
  select(
    student_id = pupilNR, 
    school_id = schoolNR,
    language_pre = langPRET,
    language_post = langPOST,
    ses,
    verbal_iq = IQ.ver.cen,
    female = sex,
    minority = Minority
    ) %>%
  mutate(minority = ifelse(minority == "Y", 1, 0))

head(bdf_l1)


bdf_l2 = bdf %>%
  select(
    school_id = schoolNR,
    school_type = denomina,
    school_ses = schoolSES,
    school_verbal_iq = avg.IQ.ver.cen,
    school_minority = percmino
  ) %>%
  group_by(school_id) %>%
  filter(row_number() == 1) %>%
  mutate(
    school_type = case_when(
      school_type == 1 ~ "Public",
      school_type == 2 ~ "Protestant",
      school_type == 3 ~ "Catholic",
      school_type == 4 ~ "Non-denominational"
    )
  )

bdf_l2


write_csv(bdf_l1, "~/Desktop/netherlands-level-1.csv")
write_csv(bdf_l2, "~/Desktop/netherlands-level-2.csv")
