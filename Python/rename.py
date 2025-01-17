import os
import csv
from datetime import datetime, timedelta
import glob

rootdir = './'
listdir = os.listdir(rootdir)
if '.idea' in listdir:
    listdir.remove('.idea')
for folder in listdir:
    path = os.path.join(rootdir, folder)
    if os.path.isdir(path):
        date = ''
        time = ''
        try:
            with open(path + "/gazecalibration/000/info.csv", 'r', encoding="utf8") as info_file:
                datareader = csv.reader(info_file)
                for row in datareader:
                    if row[0] == 'Start Date':
                        date = row[1]
                    elif row[0] == 'Start Time':
                        time = row[1]
            day, month, year = date.split(".")
            hour, minute, second = time.split(":")
            date_time = datetime(int(year), int(month), int(day), int(hour), int(minute), int(second))
            date_time = date_time + timedelta(minutes=10)
            filename = str(date_time.year) + str(date_time.month).zfill(2) + str(date_time.day).zfill(2) + str(
                date_time.hour).zfill(
                2) + str(date_time.minute).zfill(2) + str(date_time.second).zfill(2)
            files_to_rename = [os.path.basename(x) for x in glob.glob(path + "/spectra/*.csv")]
            for file in files_to_rename:
                old_filename, file_extension = file.split('.')
                os.rename(path + "/spectra/" + old_filename + "." + file_extension,
                          path + "/spectra/" + filename + '.' + file_extension)
                if os.path.exists(path + "/spectra/" + old_filename + ".jpg"):
                    os.rename(path + "/spectra/" + old_filename + ".jpg", path + "/spectra/" + filename + '.jpg')
                date_time = date_time + timedelta(seconds=10)
                filename = str(date_time.year) + str(date_time.month).zfill(2) + str(date_time.day).zfill(2) + str(
                    date_time.hour).zfill(
                    2) + str(date_time.minute).zfill(2) + str(date_time.second).zfill(2)

        except Exception as e:
            print(path + "/gazecalibration/000/info.csv not found: " + e)
