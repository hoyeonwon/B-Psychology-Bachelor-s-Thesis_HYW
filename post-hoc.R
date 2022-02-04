#After adjustment for MQmeans, there wasn't a statistically significant three-way interaction between reward frequency, control condition, and half on the PCvar score, F(3, 101) = 2.292, p = 0.064. 

### post-hoc test
## Simple main effect for Reward Frequency
final_exp2 %>%
  group_by(Control.Condition,Half) %>%
  anova_test(PCvar ~ MQmeans+Reward.Frequency)

# Pairwise comparisons
pwc2 <- final_exp2 %>% 
  group_by(Control.Condition,Half) %>%
  emmeans_test(
    PCvar ~ Reward.Frequency, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2 %>% filter(Control.Condition == "CTRL" & Half == "Half2var")
pwc2 %>% filter(Control.Condition == "YOK" & Half == "Half2var")

## Simple main effect for Control Condition
final_exp2 %>%
  group_by(Reward.Frequency,Half) %>%
  anova_test(PCvar ~ MQmeans+Control.Condition)

# Pairwise comparisons
pwc2_1 <- final_exp2 %>% 
  group_by(Reward.Frequency,Half) %>%
  emmeans_test(
    PCvar ~ Control.Condition, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2_1 %>% filter(Reward.Frequency == "1" & Half == "Half2var")

## Simple main effect for Half
final_exp2 %>%
  group_by(Reward.Frequency,Control.Condition) %>%
  anova_test(PCvar ~ MQmeans+Half)

# Pairwise comparisons
pwc2_2 <- final_exp2 %>% 
  group_by(Reward.Frequency,Control.Condition) %>%
  emmeans_test(
    PCvar ~ Half, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc2_2 %>% filter(Reward.Frequency == "1" & Control.Condition == "CTRL")



### post-hoc test
## Simple main effect for Reward Frequency
final_exp3 %>%
  group_by(Control.Condition,Half) %>%
  anova_test(DP ~ MQmeans+Reward.Frequency)

# Pairwise comparisons
pwc3 <- final_exp3 %>% 
  group_by(Control.Condition,Half) %>%
  emmeans_test(
    DP ~ Reward.Frequency, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3 %>% filter(Control.Condition == "CTRL" & Half == "Half1Direct")
pwc3 %>% filter(Control.Condition == "CTRL" & Half == "Half2Direct")
pwc3 %>% filter(Control.Condition == "YOK" & Half == "Half1Direct")
pwc3 %>% filter(Control.Condition == "YOK" & Half == "Half2Direct")

## Simple main effect for Control Condition
final_exp3 %>%
  group_by(Reward.Frequency,Half) %>%
  anova_test(DP ~ MQmeans+Control.Condition)

# Pairwise comparisons
pwc3_1 <- final_exp3 %>% 
  group_by(Reward.Frequency,Half) %>%
  emmeans_test(
    DP ~ Control.Condition, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3_1 %>% filter(Reward.Frequency == "1" & Half == "Half2Direct")

## Simple main effect for Half
final_exp3 %>%
  group_by(Reward.Frequency,Control.Condition) %>%
  anova_test(DP ~ MQmeans+Half)

# Pairwise comparisons
pwc3_2 <- final_exp3 %>% 
  group_by(Reward.Frequency,Control.Condition) %>%
  emmeans_test(
    DP ~ Half, covariate = MQmeans,
    p.adjust.method = "bonferroni"
  )
#conditions that showed reward frequency was significant in main effect analysis
pwc3_2 %>% filter(Reward.Frequency == "0.4" & Control.Condition == "CTRL")
pwc3_2 %>% filter(Reward.Frequency == "1" & Control.Condition == "CTRL")

