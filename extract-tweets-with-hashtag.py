##
#
# Read in a .csv file containing twitter data and output the hashtags and the
# tweet IDs under which they were tweeted.
#
##

import sys, csv, re

def print_usage():
    print "python extract-hashtags.py <filename> <hashtag>"

def main(argv=None):
	argv = sys.argv if argv is None else argv
    
	if len(argv) < 2:
		print_usage()
		return
		
	filename = argv[1]
	hashtag = argv[2]
	reader = csv.reader(open(filename), delimiter=',', quotechar='"')
	
	headers = reader.next()
	d = dict()
	for index, item in enumerate(headers):
		d[item] = index
	
	hash_regex = re.compile(r'#[0-9a-zA-Z+_]*',re.IGNORECASE)
	user_regex = re.compile(r'@[0-9a-zA-Z+_]*',re.IGNORECASE)
	
	for row in reader:
		if hashtag in hash_regex.findall(row[d['text']]):
			print row[d['text']]

if __name__ == '__main__':
	sys.exit(main())