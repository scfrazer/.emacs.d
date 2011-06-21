#!/router/bin/python-2.7.1

import sys
import re
from optparse import OptionParser

def main():

    opt_parser = OptionParser(usage="usage: %%prog [options] filename(s)")

    opt_parser.add_option('-q', '--quit',
                          action='store_true', dest='quit',
                          help="Immediately quit")

    (options, filenames) = opt_parser.parse_args()

    if not filenames:
        opt_parser.print_help()
        sys.exit(1)

    for filename in filenames:

        try:
            file_obj = open(filename, 'r')
        except IOError:
            print "*** ERROR: Couldn't open file '%%s'" %% (filename)
            sys.exit(1)

        file_obj.close()

################################################################################

if __name__ == '__main__':
    main()
