import requests
import json
import os
####################### GLOBALS
#Skyscanner
SKYSCANNER_API_KEY = "ha949236875329385519213435129503"
SKYSCANNER_SERVICE_URL = "http://partners.api.skyscanner.net/apiservices/browseroutes/v1.0/"#GB/GBP/en-GB/UK/anywhere/anytime/anytime?apiKey=" + apiKey;

class Flight:
    fromDest = ""
    toDest = ""
    week = 0
    year = 0
    passengers = 0

#### Main

HEADER_CODE2 = 2
COUNTRY_NAME = 0

IDX_JUMP = 10
NUMBER_OF_THREADS = 25

CSV_FILE_NAME = "bam2.csv"
CSV_FILE_COUNTRIES = "country_codes_population.csv"

def main(startIdx):
    import time
    start_time = time.time()

    with open(CSV_FILE_COUNTRIES) as flightsCSV:
        reader = csv.reader(flightsCSV, delimiter=';')
        flights = list(reader)
        currentIDX = startIdx
        MAX_FLIGHTS = len(flights)

        IDX_END = min(startIdx + IDX_JUMP, MAX_FLIGHTS)
        while currentIDX < IDX_END:
            fromDestCountry = fromDest = flights[currentIDX][COUNTRY_NAME]
            # print(threading.currentThread().getName() + "#################  Started indexing...." + fromDestCountry)
            fromDest = flights[currentIDX][HEADER_CODE2]
            for toDestIDX in range(1, IDX_END):
                if toDestIDX != currentIDX:
                    toDest = flights[toDestIDX][HEADER_CODE2]
                    flightData = getFlightData(fromDest, toDest, '2016-09-22', '2016-09-23')
                    computeFlightData(fromDest, toDest, flightData)
            currentIDX+=1    
            # print("^^^^^^^^^^^^^^^^^^^^^^^^^^finished indexing for country: " + fromDestCountry)
        print(threading.currentThread().getName() + "--- %s seconds ---" % (time.time() - start_time))

        

def computeFlightData(fromDest, toDest, flightData):
    from random import randint
    if checkJsonKeyExists(flightData, "Quotes"):
        quotes = flightData["Quotes"]
        #print("Indexing Flights - from: " + fromDest + "  -> " + toDest + "     | " + str(len(quotes)))
        if quotes != None:
            passengersSpace = 105
            flight = Flight()
            flight.passengers = len(quotes) * passengersSpace * 7
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
    return (SKYSCANNER_SERVICE_URL +  "GB/GBP/en-GB/" + fromCountry + "/" + toCountry + "/" + fromDate + "?apiKey=" + SKYSCANNER_API_KEY)

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



# main()

import threading 
def threadedMain():
    myThreadPool = []
    start = 1 

    t1 = threading.Thread(name="t1", target = main, args = (1,))
    t2 = threading.Thread(name="t2", target = main, args = (11,))
    t3 = threading.Thread(name="t3", target = main, args = (21,))
    t4 = threading.Thread(name="t4", target = main, args = (31,))
    t5 = threading.Thread(name="t5", target = main, args = (41,))
    t6 = threading.Thread(name="t6", target = main, args = (51,))
    t7 = threading.Thread(name="t7", target = main, args = (61,))
    t8 = threading.Thread(name="t8", target = main, args = (71,))
    t9 = threading.Thread(name="t9", target = main, args = (81,))
    t10 = threading.Thread(name="t10", target = main, args = (91,))
    t11 = threading.Thread(name="t11", target = main, args = (101,))
    t12 = threading.Thread(name="t12", target = main, args = (111,))
    t13 = threading.Thread(name="t13", target = main, args = (121,))
    t14 = threading.Thread(name="t14", target = main, args = (131,))
    t15 = threading.Thread(name="t15", target = main, args = (141,))
    t16 = threading.Thread(name="t16", target = main, args = (151,))
    t17 = threading.Thread(name="t17", target = main, args = (161,))
    t18 = threading.Thread(name="t18", target = main, args = (171,))
    t19 = threading.Thread(name="t19", target = main, args = (181,))
    t20 = threading.Thread(name="t20", target = main, args = (191,))
    t21 = threading.Thread(name="t21", target = main, args = (201,))
    t22 = threading.Thread(name="t22", target = main, args = (211,))
    t23 = threading.Thread(name="t23", target = main, args = (221,))
    t24 = threading.Thread(name="t24", target = main, args = (231,))
    t25 = threading.Thread(name="t25", target = main, args = (241,))
    

    t1.start()
    t2.start()
    t3.start()
    t4.start()
    t5.start()
    t6.start()
    t7.start()
    t8.start()
    t9.start()
    t10.start()
    t11.start()
    t12.start()
    t13.start()
    t14.start()
    t15.start()
    t16.start()
    t17.start()
    t18.start()
    t19.start()
    t20.start()
    t21.start()
    t22.start()
    t23.start()
    t24.start()
    t25.start()


    t1.join()
    t2.join()
    t3.join()
    t4.join()
    t5.join()
    t6.join()
    t7.join()
    t8.join()
    t9.join()
    t10.join()
    t11.join()
    t12.join()
    t13.join()
    t14.join()
    t15.join()
    t16.join()
    t17.join()
    t18.join()
    t19.join()
    t20.join()
    t21.join()
    t22.join()
    t23.join()
    t24.join()
    t25.join()
    
threadedMain()
