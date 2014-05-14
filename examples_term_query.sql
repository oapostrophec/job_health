psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c "select job_id, channel_name 
    from builder_job_channels where builder_job_countries.job_id = 427237" > '~/Desktop/channels_427237.csv'

psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c 
   "select count(*) as num_conversions, max(uid) as uid, max(profiles_profile_elements.skill_id) as skill, max(country) as country, worker_id 
   from builder_conversions, profiles_contributors, profiles_profile_elements 
   where profiles_contributors.builder_contributor_id = builder_conversions.worker_id and profiles_contributors.id = profiles_profile_elements.contributor_id and profiles_profile_elements.skill_id in (259, 260, 261) and finished_at > '2014-03-01'group by worker_id order by max(uid)" > '~/all_active_leveled_workers_active_since_march_1st.csv'

   psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c "select job_id, channel_name 
    from builder_job_channels where builder_job_channels.job_id = 427237"

    psql -h cf-redshift.etleap.com -U matt_crowdflower -d matt_crowdflower -p 5439 -A -F"," -c "select job_id, channel_name 
    from builder_job_channels where builder_job_channels.job_id = 427237" > '~/Desktop/channels_427237.csv'