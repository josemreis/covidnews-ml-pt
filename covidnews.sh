## set working_dir
cd '/home/jmr/Dropbox/Current projects/other_projects/covidnews-ml-pt' 
LOGFILE="covidnews.log"
# check if connected to wifi
wget -q --tries=10 --timeout=20 --spider http://google.com

notify-send "Running covidenews.sh"

## if yes, go on...
if [[ $? -eq 0 ]]; then

         # run it
	/usr/lib/R/bin/Rscript './scrape-parse-classify/4_get_covid19_news.R' 2>&1 | tee "$LOGFILE"

else
        # wait a minute
	sleep 60

	# run it
	/usr/lib/R/bin/Rscript './scrape-parse-classify/4_get_covid19_news.R' 2>&1 | tee "$LOGFILE"

fi

notify-send "Finished running covidenews.sh"


