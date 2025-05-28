import requests
import csv
import os
from dotenv import load_dotenv


def write_csv(file_path, data):
    """
    Writes a dictionary to a CSV file
    """
    with open(file_path, "w", newline="") as csvFile:
        if len(data) > 0:
            csvWriter = csv.DictWriter(csvFile, fieldnames=data[0].keys())
            csvWriter.writeheader()
            for each_location in data:
                csvWriter.writerow(each_location)


def get_cqc_ratings(cqc_data, API_URL, AUTH_TOKEN):
    """
    Returns cqc ratings for each care home in the cqc_data param
    """
    for each_location in cqc_data:
        rating = "NULL"
        if each_location["CQC_Location_ID"] != "NULL":
            rating = get_location_by_id(
                each_location["CQC_Location_ID"], API_URL, AUTH_TOKEN
            )

        each_location["Overall_CQC_Rating"] = rating

    return cqc_data


def get_location_by_id(cqc_id, API_URL, AUTH_TOKEN):
    """
    Returns a location ID's rating
    """
    myHeaders = {
        "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.82 Safari/537.36",
        "Ocp-Apim-Subscription-Key": AUTH_TOKEN,
    }
    api_response = requests.get(
        API_URL + f"/locations/{cqc_id}", headers=myHeaders)
    res = api_response.status_code
    if res == 200:
        data = api_response.json()

        try:
            return data["currentRatings"]["overall"]["rating"]
        except:
            return "NULL"
    else:
        print(f"Response code not good: {res}")
        return "NULL"


def read_csv(file_path):
    """
    Reads and returns all data from a CSV file
    """
    file_contents = []
    try:
        with open(file_path, newline="") as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                file_contents.append(row)
    except:
        print("Can't find file")
    return file_contents


def main():
    """
    Main method entry point for the application
    """
    # Read in CQC location IDs from a csv file
    cqc_data = read_csv("Location_IDs.csv")

    # Get URL and API key from .env file
    load_dotenv()
    API_URL = os.getenv("API_URL")
    AUTH_TOKEN = os.getenv("AUTH_TOKEN")

    # Get all data for a given CQC location ID
    cqc_ratings = get_cqc_ratings(cqc_data, API_URL, AUTH_TOKEN)

    # Write results to output CSV file
    write_csv("CQC_Locations_And_Ratings.csv", cqc_ratings)


main()
