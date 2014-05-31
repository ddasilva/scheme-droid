#!/usr/bin/python2
"""
A script to the automate the running of Scheme Droid tests.

This script is necessary because a wrapper around adb is needed to ensure that
the correct exit code is returned.
"""

import os
import subprocess
import sys

# FAILURE_WORDINGS is the set of words in adb's stdout/stderr that indicate
# a testing failute
FAILURE_WORDINGS = set(['FAILURES!!!'])
PACKAGE = 'net.meltingwax.schemedroid'


def main():
    # Construct the command to run the tests
    if 'ANDROID_HOME' in os.environ:
        android_home = os.environ['ANDROID_HOME']
    else:
        print 'Environmental variable ANDROID_HOME is not defined.'
        sys.exit(1)

    args = [
        '{0}/platform-tools/adb'.format(android_home),
        'shell',
        'am',
        'instrument',
        '-w',
        '{0}/android.test.InstrumentationTestRunner'.format(PACKAGE)
    ]

    # Run the command through a pipe, and keep track of its stdout/stderr.
    # Use the stdout/stderr to check for failure, using FAILURE_WORDINGS.
    process = subprocess.Popen(args,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.PIPE)
    exit_code = 0
    stdout_accum = ''
    stderr_accum = ''

    while True:
        comm = process.communicate()
        stdout_accum += comm[0]
        stderr_accum += comm[1]

        print comm[0]
        print comm[1]

        for wording in FAILURE_WORDINGS:
            if wording in stdout_accum or wording in stderr_accum:
                exit_code = 1
                break

        # Poll the process, terminate if complete
        if not process.poll():
            break        

    sys.exit(exit_code)

if __name__ == '__main__':
    main()
