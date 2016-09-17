#Metapopulation SIS model on network

read_data <- function(){
  #===================================
  # Read in infection data
  #===================================
  infections =read.csv("infections.csv", header = TRUE, sep = ";")
  nrow(infections)
  infections <- infections[which(infections$year>=2016),]
  #infections[which(infections$week==53),]
  infections <- infections[which(infections$week<=52),]
  
  countries  <- data.frame(unique(as.character(infections$code)))
  countries$node_number <- c(1:nrow(countries))
  names(countries) <- c('code','node_number')
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
  pop =read.csv("country_codes_population.csv", header = TRUE, sep = ";")
  
  infections$pop <- rep(NA, nrow(infections))
  for (i in 1:nrow(infections)){
    for (j in 1:nrow(pop)){
      if (infections$code[i] == pop$code_3[j]) infections$pop[i] <- pop$population[j]
    }
  }
  
  save(infections, file='infections.Rda')
}

load(file='infections.Rda')
n_t <- max(infections$time_stamp)-min(infections$time_stamp)+1
n_nodes <- max(infections$node_number)-min(infections$node_number)+1

#===================================
# Read in flights data
#===================================
F<-array(0,dim=c(n_nodes,n_nodes,n_t))
for (i in 1:n_nodes){
  for (j in 1:n_nodes){
    for (k in 1:n_t){
      F[i,j,k] <- sample(50,1,replace = TRUE)
    } 
  }
}

#===================================
# Build empty nodes
#===================================
create_node <- function() {
  #s_m: susceptible modelled
  #i_m: susceptible modelled
  #n_m: susceptible modelled
  #i_o: infected observed
  #list(s_m=rep(NA,n_t), s_m=rep(NA,n_t), s_m=rep(NA,n_t), s_o=rep(NA,n_t))
  #data.frame(s_m=rep(1,n_t), i_m=rep(1,n_t), n_m=rep(1,n_t), i_o=rep(NA,n_t))
  data.frame(s_m=rep(0,n_t), i_m=rep(0,n_t), n_m=rep(0,n_t), i_o=rep(0,n_t), t=c(min(infections$time_stamp):max(infections$time_stamp)))
  #data.frame(s_m=c(min(infections$time_stamp):max(infections$time_stamp)), i_m=rep(1,n_t), n_m=rep(1,n_t), i_o=rep(NA,n_t))
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
    
    #if (t==1){
    #state[[j]]$n_m[1] <-
    #  state[[j]]$s_m[1] <-
    #  state[[j]]$i_m[1] <-
    #} else {}
    
    state[[j]]$n_m[t] <-  state[[j]]$n_m[t-1] + sum(F[,j,t-1]) - sum(F[j,,t-1])
    
    help_sum <- 0
    for (k in 1:n_nodes){
       help_sum <- help_sum + F[k,j,t-1]*state[[k]]$i_m[t-1]/state[[k]]$n_m[t-1]
       print(paste(k, ' ',F[k,j,t-1]*state[[k]]$i_m[t-1]/state[[k]]$n_m[t-1], sep=''))
    }
   
    
   state[[j]]$i_m[t] <- state[[j]]$i_m[t-1]+
                        alpha* state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]*(state[[j]]$i_m[t-1]+help_sum) -
                        beta * state[[j]]$i_m[t-1] - 
                        sum(F[j,,t-1])*state[[j]]$i_m[t-1]/state[[j]]$n_m[t-1]

   state[[j]]$s_m[t] <- state[[j]]$s_m[t-1]-
                        alpha* state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]*(state[[j]]$i_m[t-1]+help_sum) +
                        beta * state[[j]]$i_m[t-1] -
                        sum(F[j,,t-1])*state[[j]]$s_m[t-1]/state[[j]]$n_m[t-1]
  
  state[[j]]$n_m[t]  <- max(state[[j]]$n_m[t], 0)
  state[[j]]$i_m[t]  <- max(state[[j]]$i_m[t], 0)
  state[[j]]$s_m[t]  <- max(state[[j]]$s_m[t], 0)
  }
  return(state)
}

state <- update_state(state1, t = 2, F, alpha = 0.1, beta = 0.1)
state

#===================================
# compute parameters on the grid
#===================================
length.out = 10
alpha=seq(from=0.00001, to=100, length.out=length.out)
beta=seq(from=0.00001, to=100, length.out=length.out)
param_grid <- expand.grid( alpha,  beta)
names(param_grid) <- c('alpha','beta')
param_grid$score <- rep(NA,length.out)

cost_function <- function(state1, alpha, beta){
  #starting from state1 update according to the dynamics
  for (t in 2:n_t){
    state <- update_state(state1, t, F, alpha, beta)
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
  param_grid$score[i] <- cost_function(state1, param_grid$alpha[i], param_grid$beta[i])
}

alpha_opt <- param_grid$alpha[which(param_grid$score == min(param_grid$score))]
beta_opt <- param_grid$beta[which(param_grid$score == min(param_grid$score))]
alpha_opt
beta_opt
