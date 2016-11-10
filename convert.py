import numpy
import pandas
import re
import logging

fields = [
	"businessName"
  , "violation"
  , "description"
  , "comment"
  , "date"
  , "result"
  , "risk" 
  , "address"
  , "city"
  , "state"
  , "latitude"
  , "longitude"
]

def get_group(exp, x, g):
	if type(x) is not str:
		return ''
	m = re.search(exp, x)
	return m.group(g)

''' Boston Food Establishment Inspections '''

### Open file

input = pandas.read_csv("Data/Food_Establishment_Inspections.csv")

### Conversion

output = pandas.DataFrame()
output["businessName"] = input["BusinessName"]
output["violation"] = input["Violation"]
output["description"] = input["ViolDesc"]
output["comment"] = input["Comments"]
exp = r"([0-9]{2}\/[0-9]{2}\/[0-9]{4}) [0-9]{2}:[0-9]{2}:[0-9]{2} (A|P)M"
output["date"] = map(lambda x: get_group(exp, x, 1), input["VIOLDTTM"] )
output["result"] = input["ViolStatus"]
output["risk"] = map(lambda x: ["Low", "Medium", "High", "High"][len(str(x)) - 1], input["ViolLevel"])
output["address"] = input["Address"]
output["city"] = input["City"]
output["state"] = input["State"]
exp = r"\(([-0-9]+\.[0-9]+), ([-0-9]+\.[0-9]+)\)"
output["latitude"] = map(lambda x: get_group(exp, x, 1), input["Location"])
output["longitude"] = map(lambda x: get_group(exp, x, 2), input["Location"])

### Cleaning

output = output.drop([i for (i, x) in enumerate(output["result"]) if type(x) is not str])

### Saving

output.to_csv("Data/Food_Establishment_Inspections_(converted).csv")

''' Chicago Food Inspections '''

### Open file

input = pandas.read_csv("Data/Food_Inspections.csv")

### Conversion

output = {}
for f in fields: output[f] = [] 

for i, viol in enumerate(input["Violations"]):
	# pre-emptive cleaning
	if type(viol) is not str:
		continue
	for m in viol.split("|"):
		exp = r"([0-9]+)\. (.+?(?= - Comments:)) - Comments: *([^|]+)"
		output["violation"].append(get_group(exp, viol, 1))
		output["description"].append(get_group(exp, viol, 2))
		output["comment"].append(get_group(exp, viol, 3))
		output["businessName"].append(input["DBA Name"][i])
		output["date"].append(input["Inspection Date"][i])
		output["result"].append(input["Results"][i])
		exp = r"Risk ([0-9]) \((\w+)\)|(All)"
		risk = get_group(exp, input["Risk"][i], 2)
		output["risk"].append(risk if risk else "High")
		output["address"].append(input["Address"][i])
		output["city"].append(input["City"][i])
		output["state"].append(input["State"][i])
		output["latitude"].append(input["Latitude"][i])
		output["longitude"].append(input["Longitude"][i])
		
### Cleaning

output = pandas.DataFrame(output)

### Saving

output.to_csv("Data/Food_Inspections_(converted).csv")
