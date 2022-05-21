library(tidyverse)
data_generator <- function(Number_of_subjects,
                           Number_of_statements,
                           Number_of_evils,
                           expected_results){
  N.subj=Number_of_subjects
  N.stat=Number_of_statements
  N.evil=5

  # This is not necessary in the context of shiny due to 'step=2'.
  if(N.subj%%2!=0){N.subj <- N.subj+1}
  if(N.stat%%2!=0){N.stat <- N.stat+1}

  # Helper function to sample 5 evil people
  person_temp <- c("Hitler","Arafat","Nasrala","Binladen","Sadam")
  rep_evil=rep(person_temp,(N.stat/2)/N.evil)
  rep_unkn=rep("unknown",N.stat/2)
  while(length(rep_evil)+length(rep_unkn)!=N.stat){
    rep_evil <- c(rep_evil,sample(rep_evil,1))

  }

  # Fake data generator:
  trial <- data.frame(
    # choose 10 subjects (using the names from 'babynames' package):
    subject=rep(babynames::babynames$name[1:N.subj],each=N.stat),
    age=unlist(map(1:N.subj,~rep(sample(20:30,1),N.stat))),
    sex=unlist(map(1:N.subj,~rep(sample(c("Male","Female"),1),N.stat))),
    # sample 10 sentences randomly:
    statement=unlist(purrr::map(1:N.subj,~sample(paste("statement",1:N.stat),N.stat,F))),
    # specific person:
    person=purrr::map(1:N.subj,~sample(c(rep_evil,
                                         rep_unkn),N.stat,F)) %>% unlist(),
    score=sample(1:5,N.subj*N.stat,TRUE)

  )
  # If the person is an Evil, TRUE. else, (unknown) FALSE:
  trial <- trial %>% mutate(evil=person!="unknown",
                            # manipulation test:
                            binladen=unlist(map(1:N.subj,~rep(sample(1:3,1),N.stat))),
                            hitler=unlist(map(1:N.subj,~rep(sample(1:3,1),N.stat))),
                            nasrala=unlist(map(1:N.subj,~rep(sample(1:3,1),N.stat))),
                            arafat=unlist(map(1:N.subj,~rep(sample(1:3,1),N.stat))),
                            sadam=unlist(map(1:N.subj,~rep(sample(1:3,1),N.stat))))
  # Sample expected results: when known, more likely low agreement scores
  if(expected_results){
    trial$score[trial$evil==T] <- map(1:N.stat/2,~sample(1:5,N.subj/2,
                                                         replace=T,prob=c(.3,.35,.2,.1,.05))) %>% unlist()
    trial$score[trial$evil==F] <- map(1:N.stat/2,~sample(1:5,N.subj/2,
                                                         replace=T,prob=c(.02,.05,.08,.45,.4))) %>% unlist()
  }

  #cat("Number of Subjects:",N.subj,"\nNumber of statements:",N.stat)
  trial <- trial %>% select(everything(),binladen)
  return(trial %>%
           purrr::map_if(is.character, as.factor)  %>%
           data.frame())
}
reg_model <- function(model,plot){
  SST = round(var(model$model[, 1]) * (length(model$model[,
                                                          1]) - 1), 3)
  SSRes = round(sum(resid(model)^2), 3)
  SSReg = round(SST - SSRes, 3)
  Rsq = round(SSReg/SST, 4)
  lm_m <- summary(model)
  adrsq = round(lm_m$adj.r.squared, 4)
  datatemp <- data.frame(SST = SST, SSRes = SSRes, SSReg = SSReg,
                         Rsq = Rsq, Adj.Rsq = adrsq)
  if(plot){
    print(datatemp %>% select(SSRes,SSReg) %>%
            pivot_longer(cols = everything()) %>%
            ggplot(aes(x="",y=value,fill=name,label=name))+theme_void()+
            geom_bar(stat="identity",color="white",show.legend = F)+
            coord_polar("y",start=0)+
            labs(subtitle =paste("R squared:",datatemp$Rsq))+
            theme(text = element_text(color="#56b1f7",face = "bold",size = 15))+
            scale_fill_manual(values = c("#56b1f7","#132b43"))
    )}


  return(datatemp %>% pivot_longer(cols=everything(),names_to = "Estimate"))
}
options(DT.options = list(pageLength = 30))

#previous version of reg_model:
# reg_model <- function(model,plot){
#   SST = round(var(model$model[, 1]) * (length(model$model[,
#                                                           1]) - 1), 3)
#   SSRes = round(sum(resid(model)^2), 3)
#   SSReg = round(SST - SSRes, 3)
#   Rsq = round(SSReg/SST, 4)
#   lm_m <- summary(model)
#   adrsq = round(lm_m$adj.r.squared, 4)
#
#   if(plot){
#     xlabel=c("Predictor(s):",paste(names(model$model)[-1],collapse=", "))
#     pie(c(SSRes, SSReg), labels = c("SSRes", "SSReg"),
#         col = c("#f8766d", "#6da1f8"),
#         main = c("R squared:",  Rsq), xlab = xlabel,radius = .5)
#   }
#
#   datatemp <- data.frame(SST = SST, SSRes = SSRes, SSReg = SSReg,
#                          Rsq = Rsq, Adj.Rsq = adrsq)
#   return(datatemp %>% pivot_longer(cols=everything(),names_to = "Estimate"))
# }
