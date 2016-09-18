import json
import csv

#def main()


# infections : [
#     y2016w1: {
#         DEU: {
#             infected: int,
#             percent: double
#         },
#     },
# ]



JSON_HEADER_TOP = "infections"
JSON_HEADER_INFECTED = "infected"
JSON_HEADER_PERCENTAGE = "percent"


CSV_HEADER_COUNTRY = "country"
CSV_HEADER_PERCENTAGE = "infected_percentage"
CSV_HEADER_INFECTIONS = "infected_number"
CSV_HEADER_WEEK = "week"
CSV_HEADER_YEAR = "Year"


FILE_NAME = "R_output"
FILE_CSV_NAME = FILE_NAME + ".csv"
FILE_JSON_NAME = FILE_NAME + ".json"

def main():
    csvToJson()


def csvToJson():
    with open(FILE_CSV_NAME) as source:
        reader = csv.DictReader(source, delimiter=" ")
        jsonResult = {}
        for line in reader:
            countryDict = {}
            countryDict[JSON_HEADER_INFECTED] = line[CSV_HEADER_INFECTIONS]
            countryDict[JSON_HEADER_PERCENTAGE] = line[CSV_HEADER_PERCENTAGE]
            weekDict = {}
            weekDict[line[CSV_HEADER_COUNTRY]] = countryDict
            weekNo = str(line[CSV_HEADER_WEEK])
            jsonResult["y2016w" + weekNo] = weekDict
            print(jsonResult)
        with open(FILE_JSON_NAME,'w') as dest:
            encoded = json.dumps(jsonResult, ensure_ascii=False)
            dest.write(encoded)

main()