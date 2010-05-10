##
#
# Read in a .csv file containing twitter data and output the network hashtag groups
#
##

import sys, csv, re

# Remove duplicates without preserving order
def remove_duplicates(seq):
    # Not order preserving
    keys = {}
    for e in seq:
        keys[e] = 1
    return keys.keys()

#def class TweetRecord:
#	str name

# def print_usage():
#     print "python extract-column.py <filename>"
# 
# def main(argv=None):
# 	argv = sys.argv if argv is None else argv
#     	
# 	if len(argv) < 1:
# 		print_usage()
# 		return
# 	
# 	filename = argv[1]
	
filename = '/Users/jharrison/classes/css692/project/code/haiti-quake/data/quakedata_intervals.csv'
reader = csv.reader(open(filename), delimiter=',', quotechar='"')

# build the dictionary of headers
headers = reader.next()
d = dict()
for index, item in enumerate(headers):
	d[item] = index

# dictionary to map hashtags to the group of users
usersOfTag = dict()

# dictionary to map users to the hashtags they've used
tagsByUser = dict()

hash_regex = re.compile(r'#[0-9a-zA-Z+_]*',re.IGNORECASE)
user_regex = re.compile(r'@[0-9a-zA-Z+_]*',re.IGNORECASE)

for row in reader:
	user = row[d['user.screen_name']]
	tags = tagsByUser[user] if user in tagsByUser else list()
	# loop through all hashtags in this tweet
	for ht in hash_regex.findall(row[d['text']]):
		tag = ht.lstrip('#').lower()
		if tag is "":
			continue
		users = usersOfTag[tag] if tag in usersOfTag else list()
		users.append(user)
		usersOfTag[tag] = users
		tags.append(tag)
	if tags:
		tagsByUser[user] = tags
		
#print the nodes
print "nodedef>name,mention_count INT,member_count INT"
for tag in usersOfTag.keys():
	users = usersOfTag[tag]
	print "%s,%d,%d" % (tag, len(users), len(remove_duplicates(users)))

#print the edges
print ""
print "edgedef>node1 VARCHAR,node2 VARCHAR,weight INT"
edgeDict = dict()
for user in tagsByUser.keys():
	tags = sorted(remove_duplicates(tagsByUser[user]))
	for i,val1 in enumerate(tags):
		for j,val2 in enumerate(tags):
			if j > i:
				s = val1+','+val2
				if s in edgeDict:
					edgeDict[s] += 1
				else:
					edgeDict[s] = 1
					
				#print "%s,%s,1" % (val1, val2)
		
for pair in edgeDict.keys():
	print "%s,%d" % (pair,edgeDict[pair])


# if __name__ == '__main__':
# 	sys.exit(main())