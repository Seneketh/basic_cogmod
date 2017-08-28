#All ACT-R functions are defined here:

# act-r noise
noise <- function(number)
{
    random <- runif(1, min = 0.0001, max = 0.9999)
    number * log( (1 - random ) / random)
}

# function which converts time in ms to an internal representation in pulses
msecs_to_pulses <- function(msecs)
{
    pulse_counter = 0 # number of accumulated pulses
    time_counter <- 0 # passed time
    pulse <- exp_dict$tick_base # length of the initial pulse
    
    while (time_counter < msecs) 
    {
        pulse <- exp_dict$tick_increment * pulse + noise(exp_dict$b * exp_dict$tick_increment * pulse) # generate next pulse
        pulse_counter <- pulse_counter + 1
        time_counter <- time_counter + pulse
    }
    
    pulse_counter
}


# function which produces time in ms based on an internal representation in pulses
pulse_to_msces <- function(pulses_input)
{
    
    time_counter <- 0 # passed time
    pulse_counter <- 0 # number of pulses
    pulse <- exp_dict$tick_base # length of the initial pulse
    
    while (pulse_counter < pulses_input)
    {
        pulse <- exp_dict$tick_increment * pulse + noise(exp_dict$b * exp_dict$tick_increment * pulse) # generate next pulse
        pulse_counter <- pulse_counter + 1
        time_counter <- time_counter + pulse
    }
    
    time_counter
}


encounter_add <- function(subject_setup, sample_int_pulse)
{
    tmp <- sum(!is.na(subject_setup$DecMem[sample_int_pulse, ])) + 1
    subject_setup$DecMem[sample_int_pulse, tmp] <- subject_setup$real_time
    subject_setup$real_time <- subject_setup$real_time + 100
    subject_setup
}

encounters_get <- function(subject_setup, current_row) 
{
    tmp <- subject_setup$DecMem[current_row,]
    
    if (length(tmp[!is.na(tmp)]) == 0){
        return(NA)
    }else{
        return(tmp[!is.na(tmp)])
    }
    
}

chunk_activation <- function(encounters, curtime) 
{
    if (curtime < min(encounters)) {
        return(NA)
    } else {
        sum((curtime - encounters[encounters<curtime])^exp_dict$decay)
    }
}


prob_calc <- function(current_activation, total_activation)
{
    dividend = sum(current_activation) / exp_dict$parm_t 
    divisor = total_activation / exp_dict$parm_t
    
    retrieval_prob = dividend / divisor
}

blending <- function(subject_setup)
{
    total_activation <- 0
    blended_response <- 0
    
    for (current_row in seq(1, nrow(subject_setup$DecMem))) 
    {
        amount_presentations <- sum(!is.na(subject_setup$DecMem[current_row, ]))
        
        if (amount_presentations > 0)
        {
            encounters <- encounters_get(subject_setup, current_row)
            
            current_activation <- chunk_activation(encounters, subject_setup$real_time)
            
            total_activation <- total_activation + current_activation # ACTIVATION OF THE DECLARATIVE MEMORY 
        }
    }
    
    for (current_row in seq(1, nrow(subject_setup$DecMem))) 
    {
        amount_presentations <- sum(!is.na(subject_setup$DecMem[current_row, ]))
        
        if (amount_presentations > 0)
        {
            
            encounters <- encounters_get(subject_setup, current_row)
            
            current_activation <- chunk_activation(encounters, subject_setup$real_time)
            
            retrieval_prob <- prob_calc(current_activation, total_activation)
            blended_response <- blended_response + (retrieval_prob * current_row)
            
        }
    }
    
    blended_response
}
