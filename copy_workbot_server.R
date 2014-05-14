# WORKBOT
source('../.Rprofile.apps')

system('mkdir -p /tmp/workbot')

require('shiny')
require('plyr')
library('ggplot2')
require('zoo')
require('scales')

options(StringAsFactors=F)

shinyServer(function(input, output) {
  cat('\n****************************************************\n',file=stderr())

  worker_id = reactive({ return(input$worker_id) })

  data = reactive({
    cat("getting data\n",file=stderr())
    id= worker_id()

    if( id==0){
      return(data.frame(log_duration={},top_jobs={}))
    }else{
      builder_ro = pgConnectionInfo("builder_readonly")
      psql = paste("psql", builder_ro["database"], "-U", builder_ro["user"], "-h", builder_ro["host"], "-p", builder_ro["port"], 
      	     		   "-A -F\',\' -c ")
      q1 = "select created_at, started_at, job_id, external_type, ip, country, region, tainted, missed from judgments where worker_id="
      q2 = " and created_at > now() - interval'2 months' and golden='t';"
      file = paste("/tmp/workbot_",id,".csv",sep="")
      command = paste(psql, '"', q1, id, q2, '"', " > ", file, sep="")
      system(command)
      df = read.csv(file,stringsAsFactors=FALSE)

      cat(as.character(df$created_at[1]),file=stderr())
      cat("\n",file=stderr())
      df$created_at = as.POSIXct(strptime(df$created_at,"%Y-%m-%d %H:%M:%S"))
      df$started_at = as.POSIXct(strptime(df$started_at,"%Y-%m-%d %H:%M:%S"))
      df$duration = as.numeric(difftime(df$created_at, df$started_at,units="secs"))
      df$log_duration=log(df$duration)
      df=df[order(df$started_at),]

      mov_avg = function(x,n=5){filter(x,rep(1/n,n), sides=1)}
      df$var_10 = rollapply(df$duration, width = 10, FUN = var, fill = NA)
      df$trust_5 = as.factor(mov_avg(as.numeric(df$missed!='t')))
      df$std_10=ifelse(sqrt(df$var_10)<480,sqrt(df$var_10),480)

      jobs=unique(df$job_id)
      djobs=ddply(df,.(job_id),summarize,count=length(job_id))
      djobs=djobs[order(djobs$count,decreasing=T),]
      top_jobs=djobs$job_id[1:10]
      df$top_jobs = ifelse(is.element(df$job_id,top_jobs),df$job_id,"other")
      df$index=1:nrow(df)
      write.csv(df,file=paste("/tmp/workbot/worker_",id,".csv",sep=""),row.names=F)
      cat(nrow(df),file=stderr())
      return(df)
    }
  })

output$duration_indexed=renderPlot({
  df=data()
  if(nrow(df)>0){
    seconds=c(5,15,30,60,120,240,480,2*480)
    p_dix=ggplot(df,aes(x=index, y=log_duration,color=top_jobs))+geom_point()+
    theme(text=element_text(size=18))+xlab('Number of gold units seen')+ylab('log-duration (seconds)\n')+
    labs(color="Top jobs by judgments")+
    scale_y_continuous(breaks=log(seconds),labels=seconds)+theme(legend.position="bottom")
    png(filename=paste("/tmp/workbot/worker_",worker_id(),"_dur.png",sep=""),width=1000,height=300,units="px")
    print(p_dix)
    dev.off()
    print(p_dix)
  }
},height = 300,width=1000)

output$duration_time=renderPlot({
  df=data()
  if(nrow(df)>0){
    seconds=c(5,15,30,60,120,240,480,2*480)
    p_time=ggplot(df,aes(x=created_at, y=log_duration,color=top_jobs))+geom_point()+
    theme(text=element_text(size=18))+ylab('Seconds per assignment)\n')+
    theme(legend.position="bottom")+
    scale_y_continuous(breaks=log(seconds),labels=seconds)+scale_x_datetime(breaks = date_breaks("5 day"), minor_breaks=date_breaks("1 day"))
    png(filename=paste("/tmp/workbot/worker_",worker_id(),"_durtime.png",sep=""),width=1000,height=300,units="px")
    print(p_time)
    dev.off()
    print(p_time)
  }
},height = 300, width=1000)

output$variance=renderPlot({
  df=data()
  if(nrow(df)>0){
    seconds=c(5,15,30,60,120,240,480,2*480)
    p_var=ggplot(df,aes(x=index, y=log(var_10)))+geom_point(aes(color=trust_5), size=3)+geom_line()+
    theme(text=element_text(size=18), legend.position="bottom")+ylab('log-variance per assignment\n')+labs(color="Rolling trust (5 gold)")+
    xlab('Number of gold units seen')
    png(filename=paste("/tmp/workbot/worker_",worker_id(),"_var.png",sep=""),width=1000,height=300,units="px")
    print(p_var)
    dev.off()
    print(p_var)
  }
},height = 300, width=1000)

})