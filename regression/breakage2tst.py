#! /usr/bin/env python

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU Go, a Go program.        #
#                                                               #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/ #
# for more information.                                         #
#                                                               #
# Copyright 2002 and 2003 by the Free Software Foundation.      #
#                                                               #
# This program is free software; you can redistribute it and/or #
# modify it under the terms of the GNU General Public License   #
# as published by the Free Software Foundation - version 2.     #
#                                                               #
# This program is distributed in the hope that it will be       #
# useful, but WITHOUT ANY WARRANTY; without even the implied    #
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       #
# PURPOSE.  See the GNU General Public License in file COPYING  #
# for more details.                                             #
#                                                               #
# You should have received a copy of the GNU General Public     #
# License along with this program; if not, write to the Free    #
# Software Foundation, Inc., 59 Temple Place - Suite 330,       #
# Boston, MA 02111, USA.                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

import string
import sys, getopt
import re

help_string = """
Usage:
breakage2tst.py [--pike] <BREAKAGE_FILE >testfile.tst
	This creates an excerpt of all test cases that appear as unexpected
	PASS or FAIL in BREAKAGE_FILE and writes these test to testfile.tst.
breakage2tst.py [--pike] --update <BREAKAGE_FILE
	This changes all .tst files so that the expected results match
	the behaviour of the version that produced BREAKAGE_FILE.
In both cases, it needs to be run from the regression test directory.
"""

start_tst_string = """
reset_owl_node_counter
reset_reading_node_counter
reset_connection_node_counter
"""
		
finish_tst_string = """
########### end of tests #####################

# Report number of nodes visited by the tactical reading
100000 get_reading_node_counter
#? [0]&

# Report number of nodes visited by the owl code
100001 get_owl_node_counter
#? [0]&

# Report number of nodes visited by connection reading
100002 get_connection_node_counter
#? [0]&
"""


# This function scans the .tst-file <tstfilename> for tests with numbers in
# the list testnumbers. These have to be ordered in the same way as they
# are occuring in the .tst-file

# We copy all non-numbered commands starting from the last loadsgf leading
# up to each test to be copied, plus all following non-numbered commands
# until the next loadsgf.
# This is to cope with trymove/popgo's or increase/decrease_depth_values
def write_tests(tstfilename, tests):
	global command_id

	if len(tests) == 0:
		return

	print
	print '# Tests from', tstfilename+':',
	tstfile = open(tstfilename, 'r')
	commands = ''
	state = 0
	for number, expected in tests:
		comment = ''
		current_line = tstfile.readline()
		command_pattern = re.compile("^%d " % number)

		# Look for the line containing the command with matching id,
		# while keeping recent commands and comments
		while (not command_pattern.match(current_line)):
			if re.match(r"#[^\?]", current_line):
				comment = comment + current_line
			elif (re.match(r"$", current_line)
			      or re.match(r" +$", current_line)):
				comment = ''
			elif re.search("loadsgf", current_line):
				if state == 1:
					print commands,
					state = 0
				commands = current_line
			elif (not re.match(r"[0-9]|#|^ *$", current_line)):
				commands = commands + current_line
			current_line = tstfile.readline()
		# Found match.
		state = 1
		print
		print '#', tstfilename+':', number
		print comment + commands, command_id, \
			re.sub(command_pattern, '', current_line),
		commands = ''

		# Now look for the result line:
		while (not re.match(r"^#\?", current_line)):
			current_line = tstfile.readline()
		print current_line,
		command_id = command_id + 10

	# We need to do a final scan for some popgo's or similar until
	# the next loadsgf or EOF:
	current_line = tstfile.readline()
	while (current_line != '' and not re.search(" *loadsgf", current_line)):
		if (not re.match(r"[0-9]|#| *$", current_line)):
			print current_line
		current_line = tstfile.readline()

	tstfile.close
	print


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
	global command_id
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
		print start_tst_string
		command_id = 10
		do_work = write_tests
	else:
		do_work = update_tstfile


	if (pike):
		parse_pike_input(do_work)
	else:
		parse_input(do_work)

	if (mode == 0):
		print finish_tst_string
	else:
		print "Done."


main()
