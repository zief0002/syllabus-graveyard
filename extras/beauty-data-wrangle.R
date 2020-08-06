beauty = readr::read_csv(file = "~/Dropbox/epsy-8252/data/beauty.csv")
head(beauty)


evaluations_l1 = beauty %>%
  select(
    prof_id = prof,
    avg_eval = avgeval,
    num_students = students,
    perc_evaluating = percentevaluating
    ) %>%
  arrange(prof_id)

evaluations_l1


evaluations_l2 = beauty %>%
  mutate(native_english = if_else(nonenglish == 1, 0, 1)) %>%
  select(
    prof_id = prof,
    beauty = btystdave,
    tenured, native_english, age, female
  ) %>%
  arrange(prof_id) %>%
  group_by(prof_id) %>%
  filter(row_number() == 1)


evaluations_l2
nrow(evaluations_l2)



write_csv(evaluations_l1, "~/Desktop/evaluation-level-1.csv")
write_csv(evaluations_l2, "~/Desktop/evaluation-level-2.csv")


########## Create OLS regression data

my_means = evaluations_l1 %>%
  group_by(prof_id) %>%
  summarise(
    avg_eval = sum(num_students * avg_eval)/sum(num_students),
    num_students = sum(num_students),
    avg_perc_evaluating = mean(perc_evaluating)
    )


evaluations = left_join(evaluations_l2, my_means, by = "prof_id") %>%
  select(prof_id, avg_eval, beauty, num_students, avg_perc_evaluating, female, age)


write_csv(evaluations, "~/Desktop/evaluations.csv")


#### Bias
summary(lm(avg_eval ~ 1 + avg_perc_evaluating + num_students, data = evaluations))
summary(lm(avg_eval ~ 1 + avg_perc_evaluating + num_students + female + age, data = evaluations))
summary(lm(avg_eval ~ 1 + avg_perc_evaluating + num_students + female + beauty, data = evaluations))
summary(lm(avg_eval ~ 1 + avg_perc_evaluating + num_students + female + beauty + female:beauty, data = evaluations))



