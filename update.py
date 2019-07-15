#!/usr/bin/python

import subprocess
import sys

sys.stdout.write('Content-Type: text/plain\n\n')
sys.stdout.flush()
subprocess.call(["git","pull"])

