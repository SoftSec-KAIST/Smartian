import os, sys

if len(sys.argv) != 3:
    print("Usage: python %s <log file> <bugs(comma-seperated)>" % sys.argv[0])
    exit(1)

logfile = sys.argv[1]
bugs = sys.argv[2].split(",")

f = open(logfile, "r")
buf = f.read()
f.close()

success = True

for bug in bugs:
    if bug.startswith("-"):
        if ("%s at" % bug[1:]) in buf:
            print("Unexpected %s found from %s" % (bug[1:], logfile))
            success = False
    else:
        if ("%s at" % bug) not in buf:
            print("Failed to find %s from %s" % (bug, logfile))
            success = False


if success:
    print("Passed check on %s" % logfile)
