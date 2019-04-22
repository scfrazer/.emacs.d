#!/router/bin/python3
# -*- mode: python -*-

import sys
import re
from argparse import ArgumentParser


################################################################################

def main():

    parser = ArgumentParser(description='TBD description')

    parser.add_argument('-q', '--quit', action='store_true', help='Immediately quit')
    parser.add_argument('filename', nargs='+', help='File to process')

    args = parser.parse_args()

    some_re = re.compile(r'(?P<foo>blah)')
    for filename in args.filename:

        try:
            file_obj = open(filename, 'r')
        except IOError:
            print("*** ERROR: Couldn't open file '{}'".format(filename))
            sys.exit(1)

        for line in file_obj:
            match = some_re.search(line)
            if match:
                print(line.rstrip(), end='')
                print('->{}'.format(match.group('foo')))

        file_obj.close()


################################################################################

if __name__ == '__main__':
    main()
