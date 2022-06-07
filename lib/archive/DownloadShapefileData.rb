# Pull down the required TIGER Shapefile data from the US Census FTP server
# Requires a directory that already exists to save downloaded files.
# This directory must have a subdirectory called /data where the
# extracted files are saved.
#
# Check to ensure that in the futures, files on the FTP server will have
# the same naming convention (tl_2015_53_FACES.zip, etc.).
# The tl_ part has been hard coded.
#
# At some point it would be cool to rescue timeouts, a common cause of failure
# rescue Timeout::Error =>
# 	"Hey, fail gracefully, will ya?"
# end

require 'net/ftp'
require 'logger'
require 'fileutils'

dataYear = "2018"
ftpURL = "ftp2.census.gov"
ftpDir = "//geo/tiger/TIGER#{dataYear}/"
stateFIPS = "53" # WA is 53
tempDirectory = "local/MakeShapeFiles"
unzip = "//local/7za.exe"

def download_shapefile(ftpURL, ftpDir, stateFIPS, tempDirectory, dataYear, unzip)
	FileUtils.cd(tempDirectory)
	dir = File.dirname("#{tempDirectory}/DownloadShapefile.log")
	FileUtils.mkdir_p(dir) unless File.directory?(dir)
	log = Logger.new("#{tempDirectory}/DownloadShapefile.log")
	puts "Connecting to #{ftpURL} ..."
	log.debug "Downloading TIGER files from #{ftpURL}"
	Net::FTP.open(ftpURL) do |ftp|
		ftp.login
		# Filter the FTP results to match a pattern for our year & state
		ftpFilter = "tl_#{dataYear}_#{stateFIPS}*.zip"
		# Download the Place file
		n = 0
		filesPlace = ftp.chdir(ftpDir + "PLACE")
		filesPlace = ftp.nlst(ftpFilter)
		log.debug "Downloading PLACE file: #{filesPlace}"
		filesPlace.each do |f|
			n += 1
			log.debug "#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf *.shp *.shx -aoa"
			ftp.getbinaryfile(f)
			system("#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf *.shp *.shx -aoa")
		end
		if n > 1
			log.debug "WARNING: Multiple PLACE files found for a single state!"
		else
			log.debug "Downloaded #{n} PLACE file for State #{stateFIPS}"
		end
		# Download the FACES files
		n = 0
		filesFace = ftp.chdir(ftpDir + "FACES")
		filesFace = ftp.nlst(ftpFilter)
		log.debug "Downloading the following FACE files:"
		log.debug filesFace
		filesFace.each do |f|
			n += 1
			ftp.getbinaryfile(f)
			log.debug "#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf -aoa"
			system("#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf -aoa")
		end
		log.debug "Downloaded #{n} FACES files for State #{stateFIPS}"
		# Download the EDGES files
		n = 0
		filesEdge = ftp.chdir(ftpDir + "EDGES")
		filesEdge = ftp.nlst(ftpFilter)
		log.debug "Downloading the following EDGE files:"
		log.debug filesEdge
		filesEdge.each do |f|
			n += 1
			log.debug "#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf *.shp *.shx -aoa"
			ftp.getbinaryfile(f)
			system("#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf *.shp *.shx -aoa")
		end
		log.debug "Downloaded #{n} FACES files for State #{stateFIPS}"
		# Download the FEATNAMES files
		n = 0
		filesFeat = ftp.chdir(ftpDir + "FEATNAMES")
		filesFeat = ftp.nlst(ftpFilter)
		log.debug "Downloading the following FEATNAME files:"
		log.debug filesFeat
		filesFeat.each do |f|
			n += 1
			log.debug "#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf -aoa"
			ftp.getbinaryfile(f)
			system("#{unzip} x #{tempDirectory}/#{f} -o#{tempDirectory}/data/ *.dbf -aoa")
		end
		log.debug "Downloaded #{n} FEATNAMES files for State #{stateFIPS}"
	end
	puts "Download complete. Delete temporary files from #{tempDirectory}."
end

download_shapefile(ftpURL, ftpDir, stateFIPS, tempDirectory, dataYear,unzip)
