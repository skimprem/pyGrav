from data_objects import Campaign

fn = 'test_case/input_data/raw/Atest_Raw_data.txt'
# fn = 'test_case/input_data/raw/CG-6_0458_1089-1253.dat'

campdata = Campaign()
campdata.readRawDataFile(fn)

