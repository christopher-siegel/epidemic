#Metapopulation SIS model on network

read_data <- function(){
  #===================================
  # Read in infection data
  #===================================
  infections =read.csv("infections.csv", header = TRUE, sep = ";")
  
  pop =read.csv("country_codes_population.csv", header = TRUE, sep = ";")
  pop <- pop[which(pop$population>0),]
  
  flights =read.csv("flights.csv", header = TRUE, sep = ";")
  
  nrow(infections)
  infections <- infections[which(infections$year>=2016),]
  #infections[which(infections$week==53),]
  infections <- infections[which(infections$week<=52),]
  infections <- infections[which(infections$code %in% pop$code_3),]
  
  countries  <- data.frame(unique(as.character(infections$code)))
  countries$node_number <- c(1:nrow(countries))
  names(countries) <- c('code','node_number')
  countries$code_2 <- rep(NA,nrow(countries))
  
  countries$code <- as.character(countries$code)
  pop$code_3 <- as.character(pop$code_3)
  pop$code_2 <- as.character(pop$code_2)
  
  for (i in 1:nrow(countries)){
    for (j in 1:nrow(pop)){
      if (countries$code[i] == pop$code_3[j]) countries$code_2[i] <- pop$code_2[j]
    }
  }
  
  #countries<- countries[which(countries$node_number<=5),]
  infections <- infections[which(infections$code %in% countries$code),]
  
  infections$code <- as.character(infections$code)
  countries$code <- as.character(countries$code)
  
  time_index <- rep(NA,(max(infections$year)-min(infections$year)+1)*(max(infections$week)-min(infections$week)+1))
  i <- 1
  for (year in min(infections$year):max(infections$year)){
    for (week in min(infections$week):max(infections$week)){
      if (week <= 9){
        time_index[i] <- paste(year,'_0',week, sep='')
      } else {
        time_index[i] <- paste(year,'_',week, sep='')
      }
      i <- i+1
    }
  }
  
  infections$time_index <-rep(NA, nrow(infections))
  
  for (i in 1:nrow(infections)){
    if (infections$week[i] <=9){
      infections$time_index[i] <- paste(infections$year[i],'_0',infections$week[i], sep='')
      print(infections$time_index[i])
    }else{
      infections$time_index[i] <- paste(infections$year[i],'_',infections$week[i], sep='')
      print(infections$time_index[i])
    }
  }
  
  timedf <- data.frame(time_index)
  timedf$time_stamp <- c(1:nrow(timedf))
  infections$node_number <- rep(NA, nrow(infections))
  infections$time_stamp <-rep(NA, nrow(infections))
  infections$time_index <- as.character(infections$time_index)
  timedf$time_index <- as.character(timedf$time_index)
  
  for (i in 1:nrow(infections)){
    for (j in 1:nrow(countries)){
      if (infections$code[i] == countries$code[j]) infections$node_number[i] <- countries$node_number[j]
    }
    for (j in 1:nrow(timedf)){
      if (infections$time_index[i] == timedf$time_index[j]) infections$time_stamp[i] <- timedf$time_stamp[j]
      print(paste(i,' ',j, sep=''))
    }
  }
  
  table(infections$week)
  table(infections$time_index)
  head(infections)
  
  #===================================
  # add initial population data
  #===================================
  infections$pop <- rep(NA, nrow(infections))
  for (i in 1:nrow(infections)){
    for (j in 1:nrow(pop)){
      if (infections$code[i] == pop$code_3[j]) infections$pop[i] <- pop$population[j]
    }
  }
  
  infections <- infections[complete.cases(infections),]
  infections <- infections[which(infections$pop>0),]
  
  save(infections, file='infections.Rda')
  
  n_t <- max(infections$time_stamp)-min(infections$time_stamp)+1
  n_nodes <- max(infections$node_number)-min(infections$node_number)+1
  #===================================
  # Read in flights data
  #===================================
  flight_data <- function(){
    for (i in 1:nrow(flights)){
      if (flights$week[i] <=9){
        flights$time_index[i] <- paste(flights$year[i],'_0',flights$week[i], sep='')
        print(flights$time_index[i])
      }else{
        flights$time_index[i] <- paste(flights$year[i],'_',flights$week[i], sep='')
        print(flights$time_index[i])
      }
    }  
    
    flights$node_from <- rep(NA, nrow(flights))
    flights$node_to <- rep(NA, nrow(flights))
    flights$time_stamp <- rep(NA, nrow(flights))
    
    flights$from <- as.character(flights$from)
    flights$to <- as.character(flights$to)
    
    for (i in 1:nrow(flights)){
      for (j in 1:nrow(countries)){
        if (flights$from[i] == countries$code_2[j]) flights$node_from[i] <- countries$node_number[j]
        if (flights$to[i] == countries$code_2[j]) flights$node_to[i] <- countries$node_number[j]
      }
      for (j in 1:nrow(timedf)){
        if (flights$time_index[i] == timedf$time_index[j]) flights$time_stamp[i] <- timedf$time_stamp[j]
        print(paste(i,' ',j, sep=''))
      }
    }
    
    flights <- flights[complete.cases(flights),]
    
    save(flights, file='flights.Rda')
  }
  
  
  #F<-array(0,dim=c(n_nodes,n_nodes,n_t+n_t_predict))
  
  #for (i in 1:nrow(flights)){
  #  node_from <- flights$node_from[i]
  #  node_to <- flights$node_to[i]
  #  t <- flights$time_stamp[i]
  #  F[node_from, node_to, t] <- flights$passengers[i]
  #}
  
  F<-array(0,dim=c(n_nodes,n_nodes))
  for (i in 1:nrow(flights)){
    node_from <- flights$node_from[i]
    node_to <- flights$node_to[i]
    F[node_from, node_to] <- flights$passengers[i]
  }
 
}


#===================================
# Modelling
#===================================
load(file='infections.Rda')
load(file='flights.Rda')
n_t_predict <- 52-36
#===================================
# Build empty nodes
#===================================
create_node <- function() {
  #s_m: susceptible modelled
  #i_m: susceptible modelled
  #n_m: susceptible modelled
  #i_o: infected observed
  #data.frame(s_m=rep(0,n_t+n_t_predict), i_m=rep(0,n_t+n_t_predict), n_m=rep(0,n_t+n_t_predict), i_o=rep(0,n_t+n_t_predict), t=c(min(infections$time_stamp):max(infections$time_stamp)))
  data.frame(s_m=rep(0,n_t+n_t_predict), i_m=rep(0,n_t+n_t_predict), n_m=rep(0,n_t+n_t_predict), i_o=rep(0,n_t+n_t_predict), t=c(1:(n_t+n_t_predict)))  
}

state1 <- replicate(n_nodes, create_node(), simplify=FALSE)
state1
#===================================
# distribute observed infected data into nodes
#===================================
 time_offset <- min(infections$time_stamp) -1

 for (i in 1:nrow(infections)){
   node_number <- infections$node_number[i]
   t <-  infections$time_stamp[i] - time_offset
   state1[[node_number]]$i_o[t] <- infections$infected[i]
   if (is.na(print(state1[[node_number]]$i_o[t]))) print(i)
 }
state1

#===================================
# distribute population data into nodes
#===================================
time_offset <- min(infections$time_stamp) -1

for (i in 1:nrow(infections)){
  node_number <- infections$node_number[i]
  #t <-  infections$time_stamp[i] - time_offset
  t <- 1
  state1[[node_number]]$n_m[t] <- infections$pop[i]
  state1[[node_number]]$i_m[t] <- state1[[node_number]]$i_o[t]
  state1[[node_number]]$s_m[t] <- state1[[node_number]]$n_m[t] - state1[[node_number]]$i_m[t]
  print(i)
}
state1


#===================================
# Compute modelled states
#===================================
update_state <- function(state, t, F, alpha, beta){
  for (j in 1:n_nodes){
     #j - node
     #t - time instance 
    
    #state[[j]]$n_m[t] <-  state[[j]]$n_m[t-1] + sum(F[,j,t-1]) - sum(F[j,,t-1])
    state[[j]]$n_m[t] <-  state[[j]]$n_m[t-1] + sum(F[,j]) - sum(F[j,])
    
    help_sum <- 0
    for (k in 1:n_nodes){
       help_sum <- help_sum + F[k,j]*state[[k]]$i_m[t-1]/state[[k]]$n_m[t-1]
    }
    
   state[[j]]$i_m[t] <- state[[j]]$i_m[t-1]+
                        alpha* state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]*(state[[j]]$i_m[t-1]+help_sum) -
                        beta * state[[j]]$i_m[t-1] - 
                        sum(F[j,])*state[[j]]$i_m[t-1]/state[[j]]$n_m[t-1]

   state[[j]]$s_m[t] <- state[[j]]$s_m[t-1]-
                        alpha* state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]*(state[[j]]$i_m[t-1]+help_sum) +
                        beta * state[[j]]$i_m[t-1] -
                        sum(F[j,])*state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]
  
  state[[j]]$n_m[t]  <- max(state[[j]]$n_m[t], 0)
  state[[j]]$i_m[t]  <- max(state[[j]]$i_m[t], 0)
  state[[j]]$s_m[t]  <- max(state[[j]]$s_m[t], 0)
  }
  return(state)
}

#state <- update_state(state1, t = 2, F, alpha = 1e-06, beta = 1e-06)
#state

#===================================
# compute parameters on the grid
#===================================
length.out = 2
alpha=seq(from=0.000001, to=0.001, length.out=length.out)
beta=seq(from=0.000001, to=0.001, length.out=length.out)
param_grid <- expand.grid( alpha,  beta)
names(param_grid) <- c('alpha','beta')
param_grid$score <- rep(NA,length.out)

cost_function <- function(state1, alpha, beta){
  #starting from state1 update according to the dynamics
  state <- state1
  for (t in 2:(n_t+n_t_predict)){
    state <- update_state(state, t, F, alpha, beta)
  }
  
  cost <- 0
  for (j in 1:n_nodes){
    for (t in 1:n_t){
      cost <- cost +  ifelse(is.na((state[[j]]$i_m[t] - state[[j]]$i_o[t])^2),0,(state[[j]]$i_m[t] - state[[j]]$i_o[t])^2)
    }
  }
  return(cost)
}

for (i in 1:nrow(param_grid)){
  param_grid$score[i] <- cost_function(state1, alpha= param_grid$alpha[i], beta = param_grid$beta[i])
}

alpha_opt <- param_grid$alpha[which(param_grid$score == min(param_grid$score))]
beta_opt <- param_grid$beta[which(param_grid$score == min(param_grid$score))]
alpha_opt
beta_opt

cost_function(state1, alpha= alpha_opt, beta = beta_opt)

state <- state1
for (t in 2:(n_t+n_t_predict)){
  state <- update_state(state, t, F, alpha= alpha_opt, beta = beta_opt)
}

state
#===================================
# extract prediction
#===================================
options(scipen = 999)
country_num = countries$node_number
#week = c((n_t+1):(n_t+n_t_predict))
week = c(1:(n_t+n_t_predict))

output <- expand.grid(country_num, week)
names(output) <- c('country_num','week')
output$infected_number <- rep(NA, nrow(output))
output$infected_percentage <- rep(NA, nrow(output))
output$infected_percentage <- rep(NA, nrow(output))
output$code <- rep(NA, nrow(output))

#output <- data.frame(country = rep(NA, nro), week= sample(NA,20, replace=T), infected_number= sample(100,20, replace=T), infected_percentage= sample(10,20, replace=T)/1000)


for (i in 1:nrow(output)){
    node_num <- output$country[i]
    t <- output$week[i]
    output$infected_number[i] <-  state[[node_num]]$i_m[t]
    output$infected_percentage[i] <-  state[[node_num]]$i_m[t]/state[[node_num]]$n_m[t]*100   
}

for (i in 1:nrow(output)){
  for (j in 1:nrow(countries)){
    if (output$country_num[i]==countries$node_number[j])  output$code[i] <- countries$code[j]
  }
}

output1 <- data.frame(output$code, output$week, output$infected_number, output$infected_percentage)
output <- output1
names(output) <- c('country','week','infected_number','infected_percentage')
write.table(output, file = "/Users/semenova/Dropbox/HackZurich2016/comm/epidemic/magic/R_output2.csv")
