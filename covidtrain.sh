## set working_dir
cd '/home/jmr/Dropbox/Current projects/other_projects/covidnews-ml-pt' 
LOGFILE="covidtrain.log"

notify-send "Running covidtrain.sh"

# run it
/usr/lib/R/bin/Rscript './train/train_model.R' 2>&1 | tee "$LOGFILE"

notify-send "Finished running covidtrain.sh"

