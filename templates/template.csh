#!/bin/csh -f

set opts=(`getopt -s csh -o h --long help -- $argv:q`)
if ($? != 0) then
    echo "Terminating..." >/dev/stderr
    exit 1
endif

eval set argv=\($opts:q\)
while (1)
    switch($1:q)
    case -h:
    case --help:
        shift
        goto print_help
        breaksw
    case --:
        shift
        break
    default:
        break
    endsw
end

set files=($argv)

%@

exit 0

# Print help and exit

print_help:
echo ""
echo "usage: %b [options] <files>"
echo ""
echo "options:"
echo "  -h, --help        This message"
echo ""
exit 0
