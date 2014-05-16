get_worker_intersect_skills_countries <- function(skills, skill_score, countries){
  #countries need to be formatted here as 'US', 'CA', etc and bucketed correctly
  countries_format = 
    countries$country[!(str_detect(countries$country,"(\\d rows)"))]
  countries_format = paste(paste0("'", countries, "'"), collapse=",")
  
  start = paste('select count(*) from (select worker_id, count(*) from builder_conversions')
  where_active = paste('where finished_at > GETDATE() - INTERVAL \'2 weeks\' ')
  begin = paste(start, where_active)
  
  if(length(countries_format) > 0){
    if(data_country$excluded[1] == "t"){
      select_countries = paste('and NOT country IN (', countries_format,') ')
    } else {
      select_countries = paste('and country IN (', countries_format,') ')
    }
  }
  
  if(length(skills) > 0){
    grab_worker_id = paste('and worker_id IN ')
    select_skills = paste('(select profiles_contributors.builder_contributor_id as contributor_id ')
    join_profiles = paste('from profiles_profile_elements join profiles_contributors on profiles_profile_elements.contributor_id = profiles_contributors.id ')
    
    ## this part is only in play if you actually have skills
    matching_skill_ids = paste('where skill_id in (select id from profiles_skills ')
    skills = paste('where name in (', skills,'))', sep="")
    
    remove_skill_nulls = paste('and profiles_contributors.builder_contributor_id IS NOT NULL ')
    skill_score = paste('group by profiles_contributors.builder_contributor_id having count(skill_id) >= 2) ')
    skill_query = paste(grab_worker_id, select_skills, join_profiles, matching_skill_ids, skills,
                        remove_skill_nulls, skill_score, sep="")
  }
  end = paste('group by worker_id)')
  
  if(length(skills) > 0 && length(countries_format) > 0){
    command=paste(begin, select_countries, skill_query, end)
  } else if (length(skills) == 0 && length(countries_format) > 0){
    command=paste(begin, select_countries, end)
  } else if (length(countries_format) == 0 && length(skills) > 0){
    command=paste(begin, skill_query, end)
  } else {
    command=paste(end)
  }
  
  return(command)
}