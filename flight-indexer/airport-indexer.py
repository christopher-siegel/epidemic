import requests
import json
import os
####################### GLOBALS
#Skyscanner
SKYSCANNER_API_KEY = "prtl6749387986743898559646983194"
SKYSCANNER_SERVICE_URL = "http://partners.api.skyscanner.net/apiservices/browseroutes/v1.0/"#GB/GBP/en-GB/UK/anywhere/anytime/anytime?apiKey=" + apiKey;
CSV_FILE_NAME = "flights.csv"

class Flight:
    fromDest = ""
    toDest = ""
    week = 0
    year = 0
    passengers = 0

#### Main

HEADER_CODE2 = 2
COUNTRY_NAME = 0

#IDX_START = 1
#IDX_START = 86
IDX_START = 241
IDX_JUMP = 85

CSV_FILE_COUNTRIES = "country_codes_population.csv"
def main():
    with open(CSV_FILE_COUNTRIES) as flightsCSV:
        reader = csv.reader(flightsCSV, delimiter=';')
        flights = list(reader)
        currentIDX = IDX_START
        MAX_FLIGHTS = len(flights)

        IDX_END = min(IDX_START + IDX_JUMP, MAX_FLIGHTS)
        print(IDX_END)
        while currentIDX < IDX_END:
            fromDestCountry = fromDest = flights[currentIDX][COUNTRY_NAME]
            print("#################  Started indexing....")
            fromDest = flights[currentIDX][HEADER_CODE2]
            for toDestIDX in range(1, IDX_END):
                if toDestIDX != currentIDX:
                    toDest = flights[toDestIDX][HEADER_CODE2]
                    flightData = getFlightData(fromDest, toDest, '2016-09-17', '2016-09-24')
                    computeFlightData(fromDest, toDest, flightData)
            currentIDX+=1    
            print("^^^^^^^^^^^^^^^^^^^^^^^^^^finished indexing for country: " + fromDestCountry)
        

def computeFlightData(fromDest, toDest, flightData):
    from random import randint
    if checkJsonKeyExists(flightData, "Quotes"):
        quotes = flightData["Quotes"]
        print("Indexing Flights - from: " + fromDest + "  -> " + toDest + "     | " + str(len(quotes)))
        if quotes != None:
            passengersSpace = 105
            flight = Flight()
            flight.passengers = len(quotes) * passengersSpace 
            flight.fromDest = fromDest
            flight.toDest = toDest
            flight.year = 2016
            for week in range(1, 53):
                flight.week = week
                writeModelToCSV(flight)



####################### Skyscanner stuff
def getFlightData(fromCountry, toCountry, fromDate, toDate):
    serviceURL = createServiceURL(fromCountry, toCountry, fromDate, toDate)
    jsonResp = responseForRequestService(serviceURL)
    jsonObject = json.loads(jsonResp)
    return jsonObject

def createServiceURL(fromCountry, toCountry, fromDate, toDate):
    return (SKYSCANNER_SERVICE_URL +  "GB/GBP/en-GB/" + fromCountry + "/" + toCountry + "/" + fromDate + "/" + toDate + "?apiKey=" + SKYSCANNER_API_KEY)

def responseForRequestService(serviceURL):
    r = requests.get(serviceURL)
    r.encoding = 'utf-8'
    return r.content


#Storing stuff
import csv

CSV_HEADER_FROM = "from"
CSV_HEADER_TO = "to"
CSV_HEADER_WEEK = "week"
CSV_HEADER_YEAR = "year"
CSV_HEADER_PASSENGER = "passengers"

CSV_HEADER = [
    CSV_HEADER_FROM, 
    CSV_HEADER_TO,
    CSV_HEADER_WEEK, 
    CSV_HEADER_YEAR, 
    CSV_HEADER_PASSENGER
    ]

CSVFile = open(CSV_FILE_NAME, 'w')
writer = csv.DictWriter(CSVFile, fieldnames=CSV_HEADER, delimiter=";")
writer.writeheader()



def writeModelToCSV(model):
    jsonModel = makeJSONModel(model)
    writer.writerow(jsonModel)    
    


###################### HELPERS
def makeJSONModel(model):
    return {
        CSV_HEADER_FROM: model.fromDest, 
        CSV_HEADER_TO: model.toDest, 
        CSV_HEADER_WEEK : model.week,
        CSV_HEADER_YEAR: model.year,
        CSV_HEADER_PASSENGER : model.passengers
    }


def checkJsonKeyExists(json, key):
    return True if key in json else False



main()