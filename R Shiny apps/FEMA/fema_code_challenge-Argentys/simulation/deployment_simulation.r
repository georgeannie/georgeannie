
generate_deployments <- function(
    num_years = 5,
    lambda = 0.0262557077625571, 
    rate =  0.0009,

    max_exec_time = 5000.0,
    min_exec_time = 1.0
) {
    set.seed(1)
    hr_in_year = 24*365
    
    # how many hours of the process will be simulated 
    # adding one year for the process to set up
    num_hours_year <- (num_years + 1)*hr_in_year

    
    ## generating the start times of the deployments
    ts_sim <-rpois(num_hours_year,lambda=lambda)
    l <- list()
    for (i in 1:max(ts_sim))(for (k in 1:i) (l <- c(unlist(l),(1:num_hours_year)[ts_sim == i])))
    depl_start <- sort(l) + runif(length(l), min = 0, max = 1)
    
    #total number of deployments
    depl_num <- sum(ts_sim)
    
    # generating the lengths of the deployments
    depl_len <- rexp(depl_num, rate = rate)


    depl_len[depl_len > max_exec_time] <- max_exec_time
    depl_len[depl_len < min_exec_time] <- min_exec_time
    
    return(
        list(
            request_received_time = depl_start,
            depl_length = depl_len,
            depl_num = depl_num,
            num_years = num_years
        )
    )
}

process_deployments <- function(
    responder_num = 30, 
    deployments
){
    
    depl_start <-  deployments$request_received_time
    depl_len <- deployments$depl_length
    depl_num <- deployments$depl_num
    
    active <- integer(depl_num)
    shift <- integer(depl_num)
    responder_id <- integer(depl_num)
    idle_time <- replicate(depl_num, 0.0)
    
    active_depl_num = 0
    delayed_depl = 0
    not_delayed_depl = 0
 
    for (i in 1:depl_num){
        start <- depl_start[i]
        depl_end <- depl_start + shift + depl_len
        active[active == 1 & depl_end < start] <- 0
    
        if (sum(active) < responder_num){
            # if total active deployments are less than the number of responders
            # the deployment starts right away
            active[i] <- 1
            not_delayed_depl <- not_delayed_depl + 1
        }
        else {
            # if not, shift the start of the deployment to the time slot 
            # right after the closest end of the active deployment
            
            argm = which.min(depl_end[active == 1])
            shift[i] <- min(depl_end[active == 1]) - start
            active[active == 1][argm] <- 0
            active[i] <- 1
            delayed_depl <- delayed_depl + 1
        }
    }

    depl_end = depl_start + depl_len + shift
    
    # distributing responder_ids
    responder_id[1:responder_num] <- 1:responder_num
    ordered_index <- order(depl_end)
    
    for (i in (responder_num+1):depl_num){
        prev_i = ordered_index[i-responder_num]
        responder_id[i] <- responder_id[prev_i]
        idle_time[i] <- depl_start[i] + shift[i] - depl_end[prev_i]
    }
          
    df <- data.frame(
        request_received_time = depl_start,
        deployment_start = depl_start + shift,
        responder_id = responder_id,
        deployment_length = depl_len,
        deployment_delay = shift,
        deployment_end = depl_end,
        idle_time = idle_time
    )

    return(df)
}

calculate_metrics <- function(
    depl,
    skip_hours = 8760
){
    #hr_in_year = 8760
    df <- depl
    responder_num = length(unique(depl$responder_id))
    
    sim_df <- df[df$request_received_time > skip_hours,]
    
    # ff === fulfillment
    tot_exec_time <- sum(sim_df$deployment_length)
    avg_ff_time <- mean(sim_df$deployment_start - sim_df$request_received_time)
    tot_ff_time <- sum(sim_df$deployment_start - sim_df$request_received_time)
    max_ff_time <- max(sim_df$deployment_start - sim_df$request_received_time)
    tot_idle_time <- sum(sim_df$idle_time)
    max_idle_time <- max(sim_df$idle_time)
    avg_idle_time <- mean(sim_df$idle_time)
    util_rate = tot_exec_time/(tot_exec_time+tot_idle_time)
    

    return(list(
        total_exec_time = tot_exec_time,
        avg_fulfillment_time = avg_ff_time,
        total_fulfillment_time = tot_ff_time,
        total_idle_time = tot_idle_time,
        maximum_fulfillment_time = max_ff_time,
        maximum_idle_time = max_idle_time,
        avg_idle_time = avg_idle_time,
        util_rate = util_rate
        
    ))
    
}
                             

simulate_deployments <- function(
    num_years = 5,
    resp_num_start = 20, 
    resp_num_end = 50, 

    mean_exec_time = 1107.5, 
    depl_rate =  230.0,
    skip_hours = 8760,
    max_exec_time = 5000,
    min_exec_time = 2
) {
    
    hr_in_year = 24*365
    rate = 1.0/mean_exec_time
    lambda = depl_rate/hr_in_year
    
    # how many hours of the process will be simulated
    num_hours_year <- num_years*hr_in_year
    
    ## generating the deployments
    
    simulated_deployments <- generate_deployments(
        num_years = num_years,
        lambda = lambda, 
        rate = rate,
        max_exec_time = max_exec_time,
        min_exec_time = min_exec_time  
    )
    
    # processing the deployments
    
    resp_nums <- resp_num_start:resp_num_end
    tot_ff_time <- list()
    tot_idle_time <- list()
    avg_idle_time <- list()
    avg_ff_time <- list()
    max_ff_time <- list()
    max_idle_time <- list()
    util_rate <- list()


    for (resp_num in resp_nums){
      
        df <- process_deployments(
            responder_num = resp_num,
            deployments = simulated_deployments
        )

        metrics <- calculate_metrics(df, skip_hours)
        
        tot_idle_time <- append(unlist(tot_idle_time), metrics$total_idle_time)
        avg_idle_time <- append(unlist(avg_idle_time), metrics$avg_idle_time)
        max_idle_time <- append(unlist(max_idle_time), metrics$maximum_idle_time)
        tot_ff_time <- append(unlist(tot_ff_time), metrics$total_fulfillment_time)
        avg_ff_time <- append(unlist(avg_ff_time), metrics$avg_fulfillment_time)
        max_ff_time <- append(unlist(max_ff_time), metrics$maximum_fulfillment_time)
        util_rate <- append(unlist(util_rate), metrics$util_rate)

    } 
    
    metrics_df <- data.frame(
      avg_fulfillment_time = avg_ff_time,
      responder_num = resp_nums,
      total_fulfillment_time = tot_ff_time,
      total_idle_time = tot_idle_time,
      avg_idle_time = avg_idle_time,
      maximum_fulfillment_time = max_ff_time,
      maximum_idle_time = max_idle_time,
      util_rate = util_rate
    )
    
    return(
        list(
            data = simulated_deployments,
            metrics = metrics_df
        )
    )
}


suggest_responder_num <- function(sim,cost_ratio){
  metrics <- sim$metrics

  total_cost <- metrics$total_idle_time + cost_ratio*metrics$total_fulfillment_time
  suggected_responder_num <- metrics$responder_num[which.min(total_cost)]
  
  return(suggected_responder_num)
}


