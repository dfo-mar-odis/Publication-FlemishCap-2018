# Python Script: concatenate_qat_files_2013021.py
#
# Author: Jeff Jackson
# Last Updated: 29-AUG-2013

import glob

# Get the list of files to concatenate.
filelist = glob.glob('021*.qat')

# Open the output file.
fout = open('HUD2013021qat.csv', 'w+')

# Iterate through the list of input files.
for i in filelist:

    # Get the file to be opened.
    filename = i

    # Get the operation number from the filename.
    operation = filename[4:7]
    
    print("Processing operation: ", operation)

    # Open the current input file.
    fin = open(filename, 'r')

    # Read in each line of the file and process it.
    for line in fin:

        # Parse the current line using the comma as the token .
        params = line.split(',')

        # Change the second list parameter to the operation number because
        #  some of the files are missing this value.
        params[1] = str(int(operation))

        # Delete the cruise name from the list.
        print(params[0])
        print('Deleting the cruise name from the list')
        params[0:0] = []
        print(params[0])
        
        # Insert the filename in as the first parameter in the list.
        file_minus_ext = filename[0:7]
        params[:0] = [file_minus_ext]
				
        # Iterate through the list and remove all leading and trailing spaces.
        for j,v in enumerate(params):
            params[j] = v.strip(' ')
        
        #  Rejoin the comma parameters separating the value with commas.
        newline = ", ".join(params)
        fout.write(newline)
    fin.close()
fout.close()
