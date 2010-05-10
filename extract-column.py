##
#
# Read in a .csv file containing twitter data and output the hashtags and the
# tweet IDs under which they were tweeted.
#
##

import sys, csv

def print_usage():
    print "python extract-column.py <filename> <column-name>"

def main(argv=None):
	argv = sys.argv if argv is None else argv
    
	if len(argv) < 2:
		print_usage()
		return
		
	filename = argv[1]
	column = argv[2]
	reader = csv.reader(open(filename), delimiter=',', quotechar='"')
	
	headers = reader.next()
	d = dict()
	for index, item in enumerate(headers):
		d[item] = index
		
	if not column in headers:
		print "Error: " + column + " not among headers."
		return
	
	for row in reader:
		print row[d[column]]

if __name__ == '__main__':
	sys.exit(main())