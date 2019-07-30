# this is a rather random example of how you could analyze the example data

### basics ----
library("neatStats")
library("data.table")
setwd(script_path('saccade_detection'))
source("microsacc.R")
source("smoothdata.R")
source("vecvel.R")
setwd(script_path('asc'))
mon = mon_neat(
    distance = 57,
    mon_width_cm = 50,
    mon_width_pixel = 1920
)
plot_saccade = function() {
    # Plot trajectory
    par(mfrow = c(1, 2))
    plot(
        smoothed[, 1],
        smoothed[, 2],
        type = 'l',
        asp = 1,
        xlab = expression(x[l]),
        ylab = expression(y[l]),
        main = "Position"
    )
    j <- saccades$table[1]:saccades$table[2]
    lines(
        smoothed[j, 1],
        smoothed[j, 2],
        type = 'l',
        col = 'red',
        lwd = 1
    )
    points(smoothed[saccades$table[2], 1], smoothed[saccades$table[2], 2], col =
               'red')
    
    # Plot trajectory in 2D velocity space
    vls <- vecvel(eye_single, SAMPLING)
    plot(
        vls[, 1],
        vls[, 2],
        type = 'l',
        asp = 1,
        xlab = expression(v[x]),
        ylab = expression(v[y]),
        main = "Velocity"
    )
    j <- saccades$table[1]:saccades$table[2]
    lines(vls[j, 1],
          vls[j, 2],
          type = 'l',
          col = 'red',
          lwd = 1)
}
SAMPLING = 1000
MINDUR = 15
VFAC = 10

######

# 'example.txt' is a hypothetical behavioral data file
file_names = list('example.txt') # could contain multiple files too

#
add_indeterminates = T
options(warn = 1)
### data ----
if (exists("data_merged")) {
    rm(data_merged)
}
for (name_txt in file_names) {
    #name_txt = file_names[[1]]
    
    name_msg = sub('\\.txt', '.msg', name_txt)
    name_dat = sub('\\.txt', '.dat', name_txt)
    
    con = file(name_msg, open = "r")
    
    trial_list = list()
    while (length(event_line <- readLines(con, n = 1)) > 0) {
        event_line = strsplit(event_line, " ")[[1]]
        if (event_line[2] == 'TRIALID') {
            # exclude practice block (event_line[3]: blocknum)
            trial_name = paste0('block', event_line[3], 'trial', event_line[4])
            if (is.null(trial_list[[event_line[4]]])) {
                # (event_line[4]: trialnum)
                trial_list[[event_line[4]]] = c()
            }
            trial_list[[trial_name]][event_line[5]] = as.numeric(substr(event_line[1], 5, 19)) # (event_line[5]: event title)
        }
    }
    close(con)
    tracking_msgs = trial_list
    
    tracking_data = read.table(name_dat, stringsAsFactors = F)
    
    trials_full = list()
    for (trial_name in names(tracking_msgs)) {
        #trial_name = 'block1trial44'
        cat(trial_name, fill = T)
        current_trial = tracking_msgs[[trial_name]]
        
        if ('no_arrival' %in% names(current_trial) ||
            'too_slow' %in% names(current_trial)) {
            next
        }
        
        trial_num = strsplit(as.character(trial_name), "trial")[[1]][2]
        
        t_start = current_trial['target_shown'] - 10
        
        s_start = current_trial['saccade_start'] - t_start # saccade_start is online detected start
        trial_data = tracking_data[tracking_data$V1 > t_start &
                                       tracking_data$V1 < (current_trial['saccade_start'] + 150) , ]
        if (trial_data$V2[trial_data$V1 == current_trial['saccade_start']] == -1) {
            warning('missing tracking data (sacc start) - skipping trial num ',
                    trial_num)
            next
        }
        # Select epoch from trial, transform to matrix
        eye_single <- as.matrix(trial_data[, 2:3])
        # Apply running average
        smoothed <- smoothdata(eye_single)
        # you can plot this dataframe with plot_saccade()
        
        # Detection of microsaccades
        saccades <- microsacc(smoothed, VFAC, MINDUR, SAMPLING)
        if (is.null(saccades))  {
            warning('no saccade detected - skipping trial num ', trial_num)
            next
        }
        
        sacc_table = saccades$table
        jump_length = saccades$table[, 4] - (trial_data[saccades$table[, 1], 2] - mon$mon_width_pixel / 2)
        filtered = saccades$table[abs(saccades$table[, 5]) < mon_conv(mon, 4, from = 'deg', to = 'pix') &
                                      (s_start - saccades$table[, 1]) >= 0 &
                                      (s_start - saccades$table[, 1]) < 25, ]
        
        if (class(filtered) != "numeric") {
            if ((!is.na(trial_data[saccades$table[2], 2])) &
                trial_data[saccades$table[2], 2] == -1) {
                warning('--- missing tracking data (sacc end)',
                        trial_num)
            }
            if (all((s_start - saccades$table[, 1]) >= 0) == FALSE)  {
                warning(
                    'saccade before py detected ',
                    list(abs(
                        saccades$table[, 1] -  s_start
                    )),
                    ' - skipping trial num ',
                    trial_num
                )
                next
            } else if (all((s_start - saccades$table[, 1]) < 25) == FALSE)  {
                warning('saccade nowhere near ',
                        list(abs(
                            saccades$table[, 1] -  s_start
                        )),
                        ' - skipping trial num ',
                        trial_num)
                next
            } else if (all(abs(saccades$table[, 5]) < mon_conv(mon, 4, from = 'deg', to = 'pix')) == FALSE) {
                warning(
                    'vertical move ',
                    mon_conv(
                        mon,
                        saccades$table[, 5],
                        from = 'pix',
                        to = 'deg'
                    ),
                    ' - skipping trial num ',
                    trial_num
                )
                next
            } else {
                stop('Check - nrow(filtered): ', nrow(filtered))
            }
        } else {
            saccades$table = filtered
        }
        
        sacc_start = saccades$table[1] + t_start
        sacc_end = saccades$table[2] + t_start
        
        current_trial = current_trial[c('target_shown', 'blank')]
        
        current_trial['sacc_start'] = sacc_start
        current_trial['sacc_start_py'] = s_start
        current_trial['sacc_end'] = sacc_end
        current_trial['sacc_end_x'] = trial_data[saccades$table[2], 2]
        current_trial['sacc_end_y'] = trial_data[saccades$table[2], 3]
        
        current_trial['sacc_amp'] = mon_conv(mon,
                                             (saccades$table[4] - saccades$table[5]),
                                             from = 'pix',
                                             to = 'deg')
        
        trials_full[[trial_name]] = c(
            subject_id = name_txt,
            # normally you should get the ID from the data file or sth
            trial_num = as.numeric(trial_num),
            current_trial
        )
    }
    
    subj_data = as.data.frame(do.call(rbind, trials_full))
    if (exists("data_merged")) {
        # add subject aggregations
        data_merged = rbind(data_merged, subj_data)
    } else {
        data_merged = subj_data
    }
}

# data_merged is ready for further analysis
