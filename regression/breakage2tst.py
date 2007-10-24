#! /usr/bin/env python

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU Go, a Go program.        #
#                                                               #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/ #
# for more information.                                         #
#                                                               #
# Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005 and 2006   #
# by the Free Software Foundation.                              #
#                                                               #
# This program is free software; you can redistribute it and/or #
# modify it under the terms of the GNU General Public License   #
# as published by the Free Software Foundation - version 3      #
# or (at your option) any later version.                        #
#                                                               #
# This program is distributed in the hope that it will be       #
# useful, but WITHOUT ANY WARRANTY; without even the implied    #
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       #
# PURPOSE.  See the GNU General Public License in file COPYING  #
# for more details.                                             #
#                                                               #
# You should have received a copy of the GNU General Public     #
# License along with this program; if not, write to the Free    #
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,   #
# Boston, MA 02111, USA.                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

import string
import sys, getopt
import re

help_string = """
Usage:
breakage2tst.py [--pike] <BREAKAGE_FILE 
	This creates a command line invoking regress.pike to run all tes
	cases that appear as unexpected PASS or FAIL in BREAKAGE_FILE.
breakage2tst.py [--pike] --update <BREAKAGE_FILE
	This changes all .tst files so that the expected results match
	the behaviour of the version that produced BREAKAGE_FILE.
In both cases, it needs to be run from the regression test directory.
"""

# This prints out the list of tests from testfile in the format
# <tstfile>:number
def write_tests(tstfilename, tests):
	for number, expected in tests:
		print "%s:%d" % (tstfilename, number),


def toggled_result(resultline, expected):
	if (re.search(r"\]$", resultline)):
		if (not expected == 0):
			print "Result line doesn't match 'unexpected FAIL':",
			print resultline
			sys.exit(2)
		return (re.sub(r"\]$", "]*", resultline))
	elif (re.search(r"\]\*$", resultline)):
		if (not expected == 1):
			print "Result line doesn't match 'unexpected PASS':",
			print resultline
			sys.exit(2)
		return (re.sub(r"\]\*$", "]", resultline))
	else:
		print "Couldn't parse alleged result line:", resultline
		sys.exit(2)


# This toggles the expected result in the .tst-file "tstfilename" for
# all tests whose id is listed in "tests"
def update_tstfile(tstfilename, tests):
	if len(tests) == 0:
		print tstfilename, "unchanged."
		return
	print "Updating", tstfilename
	tstfile = open(tstfilename, 'r')
	tstlines = tstfile.readlines()
	tstfile.close
	new_tstfile = ''

	for number, expected in tests:
		current_line = tstlines.pop(0)
		command_pattern = re.compile("^%d " % number)

		# Look for the line containing the command with matching id,
		# while keeping recent commands and comments
		while (not command_pattern.match(current_line)):
			new_tstfile = new_tstfile + current_line
			current_line = tstlines.pop(0)

		# Found match. Now look for the result line:
		while (not re.match(r"^#\?", current_line)):
			new_tstfile = new_tstfile + current_line
			current_line = tstlines.pop(0)

		new_tstfile = new_tstfile + toggled_result(current_line,
							   expected)
		
	# Now copy the rest of the file without change. 
	new_tstfile = new_tstfile + string.join(tstlines, '')
				
	tstfile = open(tstfilename, 'w')
	tstfile.write(new_tstfile)
	tstfile.close

def parse_input(do_work):
	tests = []
	filename = ''
	while 1:
		try:
			inputline = raw_input()
		except EOFError:
			do_work(filename, tests)
			break
		else:
			s = string.split(inputline)
			if len(s) == 0:
				continue
			if (re.search(r"regress\.sh", s[0])
			    or re.search(r"eval\.sh", s[0])):
			        if (filename != ''):
					do_work(filename, tests)
				filename = re.search(r"[^\s]+\.tst", inputline).group()
				tests = []
			elif (re.search("PASS", string.join(s[1:3]))):
				tests.append([int(s[0]), 1])
			elif (re.search("FAIL", string.join(s[1:3]))):
				tests.append([int(s[0]), 0])

def parse_pike_input(do_work):
	tests = []
	filename = ''
	while 1:
		try:
			inputline = raw_input()
		except EOFError:
			if (filename != ''):
				do_work(filename, tests)
			break
		else:
			s = string.split(inputline)
			if (not re.search(r"\:", s[0])):
				continue
			new_filename = re.search(r"[^:]+", s[0]).group() \
				       + ".tst"
			if (filename != new_filename):
				if (filename != ''):
					do_work(filename, tests)
				filename = new_filename
				tests = []
			number = int(re.search(r"[\d]+$", s[0]).group())
			if (s[1] == "PASS"):
				tests.append([number, 1])
			elif (s[1] == "FAIL"):
				tests.append([number, 0])
			else:
				print "Inconsistent input line:", inputline
				sys.exit(2)


def main():
	mode = 0
	pike = 0
	try:
		opts, args = getopt.getopt(sys.argv[1:], "",
					   ["update", "help", "pike"])
	except getopt.GetoptError:
		print "breakage2tst: Unrecognized option."
		print help_string
		sys.exit(2)
	if (args != []):
		print "I know nothing about arguments", args
		print help_string
		sys.exit(2)

	for o, a in opts:	
		if (o == "--help"):
			print help_string
			sys.exit()
		if (o == "--update"):
			mode = 1
		if (o == "--pike"):
			pike = 1
	
	if (mode == 0):
		print "./regress.pike ",
		do_work = write_tests
	else:
		do_work = update_tstfile


	if (pike):
		parse_pike_input(do_work)
	else:
		parse_input(do_work)

	if (mode == 0):
		print
	else:
		print "Done."


main()
