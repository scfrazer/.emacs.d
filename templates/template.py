#!/router/bin/python-2.7.4

import sys
import re
from argparse import ArgumentParser

def main():

    parser = ArgumentParser(description='TBD description')

    parser.add_argument('-q', '--quit', action='store_true', help='Immediately quit')
    parser.add_argument('filename', nargs='+', help='File to process')

    args = parser.parse_args()

    for filename in args.filename:

        try:
            file_obj = open(filename, 'r')
        except IOError:
            print "*** ERROR: Couldn't open file '%%s'" %% (filename)
            sys.exit(1)

        file_obj.close()

################################################################################

if __name__ == '__main__':
    main()
