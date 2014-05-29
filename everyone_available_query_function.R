# select count(*) from (
#   -- BEGIN BLOCK get contributor ids with country and date restrictions
#   select worker_id, 
#   count(*)
#   from builder_conversions
#   where finished_at > GETDATE() - INTERVAL '2 weeks' -- we define an active worker as someone who has converted in the past 2 weeks
#   -- lines 7 and 8 are special: you will need to include or exclude a whole line if country restrictions are absent
#   and country IN ('CA','US','FR', 'BR','PT') -- SUPPLY country inclusion list here
#   and NOT country IN ('CA') -- SUPPLY country exclusion list here
#   and worker_id IN
#   -- BEGIN BLOCK get contributors with skills
#   (select profiles_contributors.builder_contributor_id as contributor_id
#    from profiles_profile_elements
#    join profiles_contributors 
#    on profiles_profile_elements.contributor_id = profiles_contributors.id
#    where skill_id in (select id from profiles_skills
#                       where name in ('level_1_contributors','portuguese_translation') -- SUPPLY your skill names here, comma separated
#    ) 
#    and profiles_contributors.builder_contributor_id IS NOT NULL
#    group by profiles_contributors.builder_contributor_id
#    having count(skill_id) >= 2 -- you would SUPPLY this based on the number 
#    -- of skills you provide; 1 if "at least one" is required
#    -- END BLOCK
#   )
#   group by worker_id
#   -- END BLOCK
# )

everyone_available_query <- function(countries_include, countries_exclude, skills, min_score) {
  if (length(countries_include) > 0) {
    open = " and country IN ("
    country_vector = paste(paste0("'", countries_include, "'"), collapse=",")
    close= ")"
    countries_include_line = paste0(open,country_vector,close)
  } else {
    countries_include_line = ""
  }
  if (length(countries_exclude) > 0) {
    open = " and NOT country IN ("
    country_vector = paste(paste0("'", countries_exclude, "'"), collapse=",")
    close = ")"
    countries_exclude_line = paste0(open,country_vector,close)
  } else {
    countries_exclude_line = ""
  }
  if (length(skills) > 0) {
    part1 = " and worker_id IN (select profiles_contributors.builder_contributor_id as contributor_id
   from profiles_profile_elements
   join profiles_contributors 
   on profiles_profile_elements.contributor_id = profiles_contributors.id
   where skill_id in (select id from profiles_skills
                      where name in ("
    skill_level = paste(paste0("'", skills, "'"), collapse=",")
    part2 = ") 
   ) 
   and profiles_contributors.builder_contributor_id IS NOT NULL
   group by profiles_contributors.builder_contributor_id
   having count(skill_id) >= "
    skill_counts = min_score
    skill_restriction_block = paste0(part1, skill_level, part2, skill_counts, "  ) ")
  } else {
    skill_restriction_block = ""
  }
  part1 = "select count(*) from (
  select worker_id, 
  count(*)
  from builder_conversions
  where finished_at > GETDATE() - INTERVAL '2 weeks' "
  last_part = "
  group by worker_id
  )"
  query = paste(part1, countries_include_line, countries_exclude_line,
                skill_restriction_block, last_part)
  return(query)
}