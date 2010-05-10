
import sys, csv, re

top_tags = ['haiti', 'earthquake', 'helphaiti', 'cnn', 'haitiquake', 'news', 'fb', 'oxfam', 'haitian', 'tcot', 'omgfacts', 'yele', 'help', 'wyclefwarriors', 'p2', 'worldwednesday', 'ireport', 'aid', 'msf', 'vols']

def reset_tag_counts():
	tag_counts = dict()
	for tag in top_tags:
		tag_counts[tag] = 0;
	return tag_counts
	
def ordered_values(d, l):
	a = []
	for item in l:
		a.append(d[item])
	return a

tablefilename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/quakedata_intervals.csv'
#tweetfilename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/tweet.groups.csv'
outfilename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/hashtags_over_time.csv'

tablereader = csv.reader(open(tablefilename), delimiter=',', quotechar='"')
#tweetreader = csv.reader(open(tweetfilename), delimiter=',', quotechar='"')

# read the headers in the table
headers = tablereader.next()
d = dict()
for index, item in enumerate(headers):
	d[item] = index

# open the output file and write headers
writer = csv.writer(open(outfilename, 'w'), delimiter=',')
writer.writerow(['interval'] + top_tags)

# read through the table and for each interval calculate counts for each of the top tags
hash_regex = re.compile(r'#[0-9a-zA-Z+_]*',re.IGNORECASE)
current_interval = ''
counts = reset_tag_counts()
for row in tablereader:
	row_interval = row[d['interval']]
	if row_interval != current_interval:
		# write the row and reset the counters and current interval
		if current_interval != '' and current_interval != 'NA':
			writer.writerow([current_interval] + [counts[key] for key in top_tags])
		current_interval = row_interval
		counts = reset_tag_counts()
	# loop through the hashtags and count up the top tags 		
	for ht in hash_regex.findall(row[d['text']]):
		tag = ht.lstrip('#').lower()
		if tag in top_tags:
			counts[tag] += 1

# write the last row
writer.writerow([current_interval] + [counts[key] for key in top_tags])