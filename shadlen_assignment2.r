#       __  ___                              
#      /  |/  /___ _______________           
#     / /|_/ / __ `/ ___/ ___/ __ \          
#    / /  / / /_/ / /  / /__/ /_/ /          
#   /_/  /_/\__,_/_/   \___/\____/           
#  _       ___      __  __    ___          
#  | |     / (_)____/ /_/ /_  / (_)___      
#  | | /| / / / ___/ __/ __ \/ / / __ \     
#  | |/ |/ / / /  / /_/ / / / / / / / /     
#  |__/|__/_/_/   \__/_/ /_/_/_/_/ /_/-------------------------------------------------------------------------

#S2876507

# Running instructions: Press Ctrl+A, then Ctrl+Enter. "plyr" package must be installed.
# If you zoom in on the graph, drag the window a bit to the right, until the plots are roughly of quadratic shape (nicer that way).

library("plyr")

# exp_dict according to Taatgen, van Rijn and Anderson (2004)
exp_dict <- list(
    tick_increment = 1.1, 
    b = 0.015, 
    tick_base = 11,
    decay = -0.75, #decay parameter, default 0.5
    amount_subjects = 5, # subjects per interval
    amount_trials = 500, # amount of trials per subject and per interval
    subject_data = data.frame(matrix(ncol = 4, nrow = 0)),
    parm_t = 20
)

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



experiment_setup <- function(current_subject) #I want setup the experiment parameters for each subject, once
{
    real_time <- 0 #this is the subjects objective time in ms. this will stay with the subject for the duration of the experiment
    DecMem <- matrix(NA, 35, 900)
    #simuli for the trials get determined:
    short_condition_values <- seq(494, 847, length=11) #values of the short interval get determined
    short_condition_sample <- sample(short_condition_values, exp_dict$amount_trials, replace = TRUE)
    medium_condition_values <- seq(671, 1023, length=11) #values of the short interval get determined
    medium_condition_sample <- sample(medium_condition_values, exp_dict$amount_trials, replace = TRUE)
    long_condition_values <- seq(847, 1200, length=11) #values of the short interval get determined
    long_condition_sample <- sample(long_condition_values, exp_dict$amount_trials, replace = TRUE)
    
    condition_order <- sample(c(1, 2, 3)) #randomization of experimental condition order (1=short, 2=medium, 3=long)
    
    list(real_time = real_time, subject_nr = current_subject,
        DecMem = DecMem,
        short_condition_sample = short_condition_sample,
        medium_condition_sample = medium_condition_sample,
        long_condition_sample = long_condition_sample,
        condition_order = condition_order
    )
}

encounter_add <- function(subject_setup, sample_int_pulse)
{
    tmp <- sum(!is.na(subject_setup$DecMem[sample_int_pulse, ])) + 1
    subject_setup$DecMem[sample_int_pulse, tmp] <- subject_setup$real_time
    subject_setup$real_time <- subject_setup$real_time + 50
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

trial <- function(current_condition, subject_setup)
{
    if(current_condition == 1){
        current_condition_sample <- subject_setup$short_condition_sample} else if(current_condition == 2){
            current_condition_sample <- subject_setup$medium_condition_sample} else if(current_condition == 3){
                current_condition_sample <- subject_setup$long_condition_sample}
    
    for (.. in seq(1, length(current_condition_sample))) #length = amount_trials (default: 500)
    {
        ts <- current_condition_sample[..] # sample interval (ts) for the current trial
        subject_setup$real_time <- subject_setup$real_time + 1.000 + sample(seq(0.250, 0.850), 1) + 0.100 + ts + 0.50
        
        # presentation of central fixation point (+1000), + variable delay ranging from 0.25–0.85s
        # + "READY?" flash for 100 ms + actual time of the sample interval 
        # + "SET!" flash (lasted 100ms, but tp was recorded alrady after 50ms)
        
        sample_int_pulse <- msecs_to_pulses(ts) # the sample interval gets converted into pulses
        
        subject_setup <- encounter_add(subject_setup, sample_int_pulse)
        
        blended_response <- blending(subject_setup)
        tp <- pulse_to_msces(blended_response)

        subject_setup$subject_data <- rbind(subject_setup$subject_data, c(subject_setup$subject_nr, current_condition, ts, tp))
    }
    
    subject_setup
}

train <- function(current_condition, subject_setup)
{
    train_amount <- 250
    if(current_condition == 1){
        current_condition_sample <- subject_setup$short_condition_sample} else if(current_condition == 2){
            current_condition_sample <- subject_setup$medium_condition_sample} else if(current_condition == 3){
                current_condition_sample <- subject_setup$long_condition_sample}
    
    for (.. in seq(1, train_amount)) 
    {
        ts <- current_condition_sample[..] # sample interval (ts) for the current trial
        subject_setup$real_time <- subject_setup$real_time + 1 + sample(seq(0.250, 0.850), 1) + 0.100 + ts + 0.50
        sample_int_pulse <- msecs_to_pulses(ts) # the sample interval gets converted into pulses
        subject_setup <- encounter_add(subject_setup, sample_int_pulse)
    }
    subject_setup
}



main <- function()
{
    simulated_data <- data.frame(matrix(ncol = 4, nrow = 0))
        
    for (.. in seq(1, exp_dict$amount_subjects)) #loop for subjects
    {
        current_subject <- ..
        subject_setup <- experiment_setup(current_subject)
        
        for (.. in subject_setup$condition_order) # loop containing all conditions
        {
            current_condition <- ..
            subject_setup$DecMem <- matrix(NA, 35, 900) #new DM each day(condition)
            subject_setup <- train(current_condition, subject_setup)
            subject_setup$real_time <- subject_setup$real_time + 1
            subject_setup <- trial(current_condition, subject_setup) #here the trials happen (another loop inside)

        }
        
        simulated_data <- rbind(simulated_data, subject_setup$subject_data)
    }
    
    colnames(subject_setup$subject_data) <- c("Subject_Nr ", "Cond", "Ts", "Tp")
    subject_data <- data.frame(subject_setup$subject_data)
    subject_data$Response[is.nan(subject_data$Response)] <- subject_data$Prior[is.nan(subject_data$Response)]
    subject_data
}



plotting <- function(dataframe)
{

    brown <- "#8b4513";
    red <- "#ff1100";
    black <- "#000000";
    brownT <- "#8b451322";
    redT <- "#ff110022";
    blackT <- "#00000022";
    
    ## ---
    datJS <- dataframe
    par(mfrow=c(1,1))
    
    plotDatJS <- with(datJS,aggregate(list(Tp=Tp),list(Ts=Ts,Cond=Cond),mean))
    agregJS <- ddply(datJS, c("Cond", "Ts"), summarise, Ts_mean = mean(Ts), Tp_mean = mean(Tp))
    yrange <- range(plotDatJS$Ts)*c(.95,1.05)
    
    with(plotDatJS[plotDatJS$Cond==3,],plot(Ts,Tp,type="b",col=red,lwd=2,ylim=yrange,xlim=yrange,main="J&S All"))
    with(plotDatJS[plotDatJS$Cond==2,],lines(Ts,Tp,type="b",col=brown,lwd=2,ylim=yrange,xlim=yrange))
    with(plotDatJS[plotDatJS$Cond==1,],lines(Ts,Tp,type="b",col=black,lwd=2,ylim=yrange,xlim=yrange))
    
    lines(c(yrange[1],yrange[2]),c(yrange[1],yrange[2]),col="darkgrey",lty=2)
    
    with(datJS[datJS$Cond==3,],points(jitter(Ts),Tp,col=redT,pch=".",cex=3))
    with(datJS[datJS$Cond==2,],points(jitter(Ts),Tp,col=brownT,pch=".",cex=3))
    with(datJS[datJS$Cond==1,],points(jitter(Ts),Tp,col=blackT,pch=".",cex=3))    
}

plotting(main())
