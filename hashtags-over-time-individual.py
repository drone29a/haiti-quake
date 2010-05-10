##
#
# Output data in the form:
# time.interval, tweet.id, hash.tag
#
##

import sys, csv, re

top_tags = ['haiti', 'earthquake', 'helphaiti', 'cnn', 'haitiquake', 'news', 'fb', 'oxfam', 'haitian', 'tcot', 'omgfacts', 'yele', 'help', 'wyclefwarriors', 'p2', 'worldwednesday', 'ireport', 'aid', 'msf', 'vols']

tablefilename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/quakedata_intervals.csv'
outfilename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/hashtags_over_time_individual.csv'

tablereader = csv.reader(open(tablefilename), delimiter=',', quotechar='"')

# read the headers in the table
headers = tablereader.next()
d = dict()
for index, item in enumerate(headers):
	d[item] = index

# open the output file and write headers
writer = csv.writer(open(outfilename, 'w'), delimiter=',')
writer.writerow(['time.interval', 'tweet.id', 'hash.tag'])

# read through the table and for each interval calculate counts for each of the top tags
hash_regex = re.compile(r'#[0-9a-zA-Z+_]*',re.IGNORECASE)
for row in tablereader:
	row_interval = row[d['interval']]
	# loop through the hashtags and count up the top tags 		
	for ht in hash_regex.findall(row[d['text']]):
		tag = ht.lstrip('#').lower()
		if tag in top_tags:
			writer.writerow([row_interval, row[d['id']], tag])
