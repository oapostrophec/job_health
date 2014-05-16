string = data$minimum_requirements

score = str_extract(string, "(min_score:\\d)")
score_num = str_extract(score, "\\d")
score_num = as.double(score_num)
skills_strip = str_replace_all(string, "(\\{skill_scores:\\{|\\}.+)", "")

##add skills_simple to active worker intersect query
skills_simple = str_replace_all(skills_strip, "(:\\d)", "")
skill_split = str_split(skills_simple, ",")
skill_unlist = unlist(skill_split[[1]])
skill_unlist = skill_unlist[skill_unlist!=""]
skills_for_psql = paste(paste0("'",skill_unlist,"'"), collapse=",")



skills_as_factors = as.factor(skill_unlist)

skill_listed = paste(skills_as_factors, collapse="\', \'")

for(i in 1:length(skills_as_factors){
  skills_as_factors[i] = append(skills_as_factors[i])
  
}

#country extraction
countries = 
  data_country$country[!(str_detect(data_country$country,"(\\d rows)"))]
countries = paste(paste0("'", countries, "'"), collapse=",")