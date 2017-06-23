.PHONY: export 

export: 2017-05-24-soil_equilibrium4.csv
	mongoexport -h 192.168.1.177 -d mqtt -c tele --fields topic,timestamp,value, -o bla.csv --csv
	mv bla.csv 2017-05-24-soil_equilibrium4.csv
