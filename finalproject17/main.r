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

#sets working directory to source file location (RSTUDIO ONLY! comment out if not rstudio.)
library("rstudioapi")
library("plyr")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("actr_functions.r")
source("plotting_functions.r")



# exp_dict according to Taatgen, van Rijn and Anderson (2004)
exp_dict <- list(
    tick_increment = 1.1, # (default: 1.1)
    b = sample(0.0125:0.0126, 1, replace = TRUE), # (default 0.015)
    tick_base = 11, # (default: 11)
    decay = -1.5, #decay parameter, (default: 0.5)
    amount_subjects = 5, # subjects per interval
    amount_trials = 50, # amount of trials per subject and per interval (default: 50)
    amount_training = 5,
    subject_data = data.frame(matrix(ncol = 7, nrow = 0)), #tmp data storage
    parm_t = 60, # (default: 1)
    point_gain = 5, # (default: 5)
    no_penalty = 0,
    low_penalty = -5, # (default: -5)
    high_penalty = -30, # (default: -30)
    yellowDot_time = sample(1000:2000, 1, replace = TRUE),
    base_interval = 700, # (default: 750)   sample(725:775, 1, replace = TRUE)
    leeway = 0.15, #percentage of space around base interval that counts as correct (default: 0.15)
    # adaption_rate = 50,
    condition_order = c(1, 2, 3, 1)
)

experiment_setup <- function(current_subject) #I want setup the experiment parameters for each subject, once
{
    list(real_time = 0, #this is the subjects objective time in ms. this will stay with the subject for the duration of the experiment
         subject_nr = current_subject,
         points = 0,
         recalled_interval = 0,
         trialnr = 0,
         DecMem = matrix(NA, 100, 500),
         cond = 0
    )
}

trial <- function(current_condition, subject_setup)
{
    subject_setup$cond <- subject_setup$cond + 1
    if(current_condition == 1){
        penalty <- exp_dict$no_penalty} else if(current_condition == 2){
            penalty <- exp_dict$low_penalty} else if(current_condition == 3){
                penalty <- exp_dict$high_penalty}

    for (.. in seq(1, exp_dict$amount_trials)) #amount_trials (default: 50)
    {
        subject_setup$trialnr <- subject_setup$trialnr + 1
        ts <- exp_dict$base_interval
        subject_setup$real_time <- subject_setup$real_time + exp_dict$yellowDot_time #time yellow dot flashes
        sample_int_pulse <- msecs_to_pulses(ts) # the sample interval gets converted into pulses
        subject_setup <- encounter_add(subject_setup, sample_int_pulse)
        blended_response <- blending(subject_setup)
        rt <- pulse_to_msces(blended_response)

        if (rt > (1 - exp_dict$leeway)*exp_dict$base_interval & rt < (1 + exp_dict$leeway)*exp_dict$base_interval) {
            correct <- 1
            points_increment <- exp_dict$point_gain
            subject_setup <- encounter_add(subject_setup, (25))

        } else {
            correct <- 0
            points_increment <- exp_dict$point_gain + penalty

            subject_setup <- encounter_add(subject_setup, (13 - penalty*1.5))
        }

        subject_setup$points <- subject_setup$points + points_increment

        subject_setup$subject_data <- rbind(subject_setup$subject_data,
        c(subject_setup$subject_nr,
          subject_setup$trialnr,
          subject_setup$cond,
          correct,
          subject_setup$points,
          points_increment,
          rt))
    }

    subject_setup
}

# training <- function(subject_setup)
# {
#     subject_setup$recalled_interval <- 750
#     for (.. in seq(1, exp_dict$amount_training)) #training was 5 times exposition to 750 ms interval
#     {
#         train_pulses <- msecs_to_pulses(subject_setup$recalled_interval)
#         subject_setup <- encounter_add(subject_setup, train_pulses)
#         blended_training <- blending(subject_setup)
#         subject_setup$recalled_interval <- pulse_to_msces(blended_training)
#     }
#     subject_setup
# }

# adaption <- function(subject_setup, penalty)
# {
#     adaption_pulses <- msecs_to_pulses(subject_setup$recalled_interval)
#
#     subject_setup$recalled_interval <- pulse_to_msces(subject_setup$recalled_interval + adaption)
#
#     subject_setup
# }

main <- function()
{
    simulated_data <- data.frame(matrix(ncol = 7, nrow = 0)) #main data storage

    for (.. in seq(1, exp_dict$amount_subjects)) #loop for subjects
    {
        current_subject <- ..
        subject_setup <- experiment_setup(current_subject) #initializing current subject

        for (.. in exp_dict$condition_order) # loop containing all conditions
        {
            current_condition <- ..
            # subject_setup <- training(current_condition)
            subject_setup <- trial(current_condition, subject_setup) #here the trials happen (another loop inside)

        }

        simulated_data <- rbind(simulated_data, subject_setup$subject_data)
    }

    colnames(simulated_data) <- c("Subject_Nr ", "Trial_Nr", "Cond", "Correct", "Points", "Points_Increment", "Rt")

    simulated_data

    plotdat <- ddply(simulated_data, c("Trial_Nr", "Cond"), summarise,
                     mRT = mean(Rt),
                     sdRT = sd(Rt),
                     mScore = mean(Points),
                     mCurPoints = mean(Points_Increment),
                     mCorrect = mean(Correct))

    plotdat

    print("Run successfull!")
    #
    rt_plot <- plot_rt(plotdat)
    #sd_plot <- plot_sd(plotdat)
    # mScore_plot <- plot_mScore(plotdat)
    # mCurPoints_plot <- plot_mCurPoints(plotdat)
    #mCorrect_plot <- plot_mCorrect(plotdat)


}

main()
