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

# Running instructions: Press Ctrl+A, then Ctrl+Enter. "plyr" and "ggplot2" packages must be installed.
# If you zoom in on the graph, drag the window a bit to the right, until the plots are roughly of quadratic shape (nicer that way).


library("plyr")
library("ggplot2")
#library("lme4")

# Parameters according to Taatgen, van Rijn and Anderson (2004)
exp_dict <- list(
    tick_increment = 1.1, 
    b = 0.015, 
    tick_base = 11,
    expdata3_6 = c(0.08, 0.1, 0.2, 0.45, 0.74, 0.86, 0.95), #data from real experiment
    expdata2_8 = c(0.02, 0, 0.12, 0.5, 0.84, 0.91, 1),
    expdata4_12 = c(0, 0.07, 0.22, 0.46, 0.69, 0.86, 0.92),
    amount_subjects = 20, # subjects PER INTERVAL
    amount_training = 10, # amount of training PER ANCHOR POINT in the current interval
    amount_trials = 100 # amount of trials per subject and per interval
)

intervals <- list(
    Interval_3_6 = c(3, 3.37, 3.78, 4.24, 4.76, 5.34, 6), #interval 3 to 6 secs. timepoints presented to the subjects 
    Interval_2_8 = c(2, 2.52, 3.18, 4, 5.04, 6.35, 8),
    Interval_4_12 = c(4, 4.8, 5.77, 6.93, 8.32, 9.99, 12))

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
    pulse <- param$t0 # length of the initial pulse
    
    while (pulse_counter < pulses_input)
    {
        pulse <- exp_dict$tick_increment * pulse + noise(exp_dict$b * exp_dict$tick_increment * pulse) # generate next pulse
        pulse_counter <- pulse_counter + 1
        time_counter <- time_counter + pulse
    }
    
    time_counter
}

#each subjects gets a feeling for the short/long intervals (this is why here we convert from msecs to pulses)
training <- function(current_interval)
{
    lower <- min(unlist(intervals[current_interval])) #lower anchor of current interval
    lower <- rep(lower*1000, times = exp_dict$amount_training) 
    #multiplication with 1000 because miliseconds, creates vector length of amount_training of element lower
    lower <- sapply(lower, msecs_to_pulses) #applies msecs_to_pulses to vector elementwise
    lower <- mean(lower) #mean of elements of vector
    
    upper <- max(unlist(intervals[current_interval])) #same for upper as for lower
    upper <- rep(upper*1000, times=exp_dict$amount_training) 
    upper <- sapply(upper, msecs_to_pulses)
    upper <- mean(upper)
    
    list(lower = lower, upper = upper)# save the training info for usage in the trials
}


trial <- function(current_interval, trained_intervals)
{
    stimuli <- sample(unlist(intervals[current_interval]), 1, prob = c(0.3, 0.7, 0.7, 0.7, 0.7, 0.7, 0.3))*1000 #current stimuli in ms
    #random sampling with 30% chance for anchor points and 70% chance for intermediate timepoints
    stimuli_pulse <- msecs_to_pulses(stimuli) # mental converstion to pulses during the trial
    
    if (abs(stimuli_pulse - trained_intervals$upper) < abs(stimuli_pulse - trained_intervals$lower)) #decision phase
        response = 1 # the stimuli is perceived to be nearer to the long interval
    else response =  0
    
    list(stimuli = stimuli/1000, response = response)
}

plot <- function(simulation_df) #ggplot is nice, but requires a lot of code. Returns plot and aggregated data
{
    aggreg_simul_data <- ddply(simulation_df, c("Interval", "Stimuli"), summarise, Proportion_long = mean(ChosenLong))
    aggreg_simul_data <- cbind(aggreg_simul_data, c(rep("Simulation Data", times = 21), rep("Experimental Data", times = 21)))
    aggreg_simul_data$Proportion_long[22:42] <- c(exp_dict$expdata3_6, exp_dict$expdata2_8, exp_dict$expdata4_12) 
    colnames(aggreg_simul_data) <- c("Interval", "Stimuli", "ProportionLong", "Origin")
    
    aggreg_simul_data$Interval[aggreg_simul_data$Interval == "1"] <- "Interval 3 to 6 Seconds" 
    aggreg_simul_data$Interval[aggreg_simul_data$Interval == "2"] <- "Interval 2 to 8 Seconds" 
    aggreg_simul_data$Interval[aggreg_simul_data$Interval == "3"] <- "Interval 4 to 12 Seconds" 
    
    simulated.data.plot <- ggplot(aggreg_simul_data, aes(x = Stimuli, y = ProportionLong, group = Origin, color = Origin)) + 
        geom_line() +  geom_point() + facet_grid(. ~ Interval, scales = "free") + theme_bw() + 
        theme(legend.position = "bottom") +
        scale_colour_brewer(palette="Set1", name = "Groups:") +
        labs(title = "Bisection Intervals: Simulated and Experimental Data") + 
        xlab("Timepoints [s]") + 
        ylab("Proportion long") + 
        theme(strip.background = element_rect(fill = "lightblue"))  + coord_fixed(5)
    
    list(plot = simulated.data.plot, aggreg_simul_data = aggreg_simul_data)
}

main <- function()
{
    simulation_df <- data.frame(matrix(ncol = 5, nrow = 0)) #empty dataframe for data gathering
    
    for (.. in seq(1, length(intervals))) #loop for the intervals
    {
        current_interval <- ..
        
        for (.. in seq(1, exp_dict$amount_subjects)) #loop for the subjects
        {
               
            current_subject <- ..
            trained_intervals <- training(current_interval) #the current subject gets trained on the interval
                
            for (.. in seq(1, exp_dict$amount_trials)) #loop for the trials
            {
                current_trial <- ..
                results <- trial(current_interval, trained_intervals) #the current subject gets exposed to the current interval-trials
                    
                simulation_df <- rbind(simulation_df, c(current_interval, current_subject, current_trial, results$response, results$stimuli))
                
            }
        }
    }
    
    colnames(simulation_df) <- c("Interval", "Subject", "Trial", "ChosenLong", "Stimuli")
    plot(simulation_df) 
    
}

main()