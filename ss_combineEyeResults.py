import pandas
import sys
df1 = pandas.read_csv(sys.argv[1]+'correspondence_table.csv')
df1['Frame #']+1
df2 = pandas.read_csv(sys.argv[1]+'exports/000/pupil_positions.csv')
pupildata = df2['diameter_3d'][df1['Frame #']+1]

pupildata = pupildata.reset_index(drop=True)
df = pandas.concat([df1, pupildata], axis=1)
df.to_csv(sys.argv[1]+'results.csv')
