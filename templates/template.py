#!/router/bin/python-2.4.3

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
            thing = open(filenames[0], 'r')
        except IOError:
            print "*** ERROR: Couldn't open file '%%s'" %% (filenames[0])
            sys.exit(1)

        thing.close()

################################################################################

if __name__ == '__main__':
    main()
%@
