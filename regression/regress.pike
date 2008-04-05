#!/usr/bin/env pike

static int debug = 0;
static int verbose = 0;
Thread.Queue testsuite_queue;

/* General class to manage a high-score list (e.g. of slow tests, tests
 * with many nodes, ..)
 */
class Highscorelist
{
  array(float) scores;
  array(string) names;
  int max;
  
  void create(int m)
  {
    max = m;
    scores = ({});
    names = ({});
  }

  void add_score(float score, string name)
  {
    int num = sizeof(scores);
    if (num != sizeof(names)) {
      write("This should not happen!!");
      return;
    }
    if (num < max) {
      scores += ({score});
      names += ({name});
      sort(scores, names);
    }
    else if (scores[0] < score) {
      scores[0] = score;
      names[0] = name;
      sort(scores, names);
    }
    return;
  }

  void report(string s)
  {
    for (int i = 0; i < sizeof(scores); i++)
      write(s, names[i], scores[i]);
  }
}

Highscorelist slow_moves;
int report_slow = 0;

class Testsuite
{
  string name;
  mapping(int:string) correct_results = ([]);
  multiset expected_failures = (<>);
  int reading_nodes;
  int owl_nodes;
  int connection_nodes;
  float walltime;
  float cputime;
  float uncertainty;
  Process.create_process engine;
  int quit_has_been_sent;
  
  array(int) pass;
  array(int) fail;
  array(int) PASS;
  array(int) FAIL;

  Thread.Queue writing_finished;
  Thread.Queue reading_finished;
  Thread.Queue write_queue;
  int timebase;
  float last_time;

  void create(string s)
  {
    name = s;
    reading_nodes = 0;
    owl_nodes = 0;
    connection_nodes = 0;
    uncertainty = 0.0;
    walltime = 0.0;
    pass = ({});
    fail = ({});
    PASS = ({});
    FAIL = ({});
  }

  static void finish()
  {
    // Write nothing if no test from the file was run at all.
    if (sizeof(pass) + sizeof(fail) + sizeof(PASS) + sizeof(FAIL) > 0)
      write("%-37s %7.2f %9d %7d %8d\n", name, cputime, reading_nodes,
	    owl_nodes, connection_nodes);
  }

  static void program_reader(object f)
  {
    int test_number;
    
    if (debug)
      werror("Waiting for writing to be finished.\n");
    writing_finished->read();
    if (debug)
      werror("Finished waiting for writing to be finished.\n");
    
    while (1) {
      string s = f->gets() - "\r";
      float current_time = time(timebase);
      if (!s)
	break;
      if (debug)
	werror("Recv: " + s + "\n");
      
      int number;
      string answer;
      if (sscanf(s, "=%d %s", number, answer)) {
	if (number < 10000 || number > 10005) {
	  test_number = (int) number;
	  string correct = correct_results[test_number];
	  if (!correct) {
	    correct = "correct result missing, check the test suite";
	    correct_results[test_number] = correct;
	  }
	  int negate = 0;
	  if (correct[0] == '!') {
	    correct = correct[1..];
	    negate = 1;
	  }
	  correct = "^" + correct + "$";
	  object re = Regexp(correct);
	  string result = (negate ^ re->match(answer)) ? "pass" : "fail";
	  
	  if (result == "pass" && expected_failures[test_number])	{
	    result = "PASS";
	  }
	  if (result == "fail" && !expected_failures[test_number]) {
	    result = "FAIL";
	  }
	  this_object()[result] += ({test_number});
	  walltime += (current_time - last_time);
	  if (report_slow)
	    slow_moves->add_score(current_time - last_time,
				  name + ":" + test_number);
	  
	  if (result == "PASS" || result == "FAIL" || verbose)
	    write("%-15s %s %s [%s]\n", name + ":" + test_number,
		  result, answer, correct_results[test_number]);
	  last_time = current_time;
	}
	else if (number == 10000)
	  reading_nodes += (int) answer;
	else if (number == 10001)
	  owl_nodes += (int) answer;
	else if (number == 10002)
	  connection_nodes += (int) answer;
	else if (number == 10003)
	  cputime = (float) answer;
	else if (number == 10005)
	  uncertainty += (float) answer;
	else if (number == 10004)
	  break;
      }
      else if (sscanf(s, "?%s", answer)) {
	number = -1;
	sscanf(answer, "%d", number);
	write("%-15s ?%s\n", name + ":", answer);
      }
    }
    if (debug)
      werror("Reader closing down.\n");
    finish();
    f->close();
    reading_finished->write("\n");
  }

  static void program_writer(object f)
  {
    while (1) {
      string s = write_queue->read();
      if (has_value(s, "quit"))
	quit_has_been_sent = 1;
      if (s == "")
	break;
      f->write(s);
    }
    f->close();
    if (debug)
      werror("Writer closed down\n");
  }

  static void program_monitor()
  {
    while (!quit_has_been_sent) {
      sleep(1);
      if (engine->status() != 0 && !quit_has_been_sent) {
       write("engine crashed in test suite %s.\n", name);
       exit(1);
      }
    }
  }
  
  void send(string|void s)
  {
    if (!s) {
      if (debug)
	werror("Finishing sending.\n");
    }
    else {
      if (debug)
	werror("Sent: " + s + "\n");
      write_queue->write(s + "\n");
    }
  }
  
  void run_testsuite(string suite_name, string command,
		     array(string) engine_options,
		     mapping(string:mixed) options,
		     array(int)|void test_numbers)
  {
    array(string) program_start_array = ({command}) + engine_options;
    
    string testsuite = Stdio.read_file(suite_name);
    if (!testsuite) {
      werror("Couldn't find " + suite_name + "\n");
      exit(1);
    }
    
    if (options["valgrind"])
      program_start_array = ({"valgrind"}) + program_start_array;
    
    if (options["check-unoccupied-answers"])
      testsuite = modify_testsuite(testsuite);
    
    writing_finished = Thread.Queue();
    reading_finished = Thread.Queue();
    write_queue = Thread.Queue();
    object f1 = Stdio.File();
    object pipe1 = f1->pipe();
    object f2 = Stdio.FILE();
    object pipe2 = f2->pipe();
    engine = Process.create_process(program_start_array,
				    (["stdin":pipe1, "stdout":pipe2]));
    thread_create(program_reader, f2);
    thread_create(program_writer, f1);
    thread_create(program_monitor);
    
    int number;
    string answer;
    string expected;
    
    timebase = time();
    last_time = time(timebase);
    
    correct_results = ([]);
    expected_failures = (<>);
    
    if (test_numbers && sizeof(test_numbers) == 0)
      test_numbers = 0;
    
    foreach (testsuite/"\n", string s) {
      string command;
      if (sscanf(s, "%d %s", number, command) == 2) {
	command = (command / " ")[0];
	if (number >= 10000 && number <= 10003)
	  continue;
	if (test_numbers && !has_value(test_numbers, number))
	  continue;
	if (sizeof(allowed_commands) > 0 && !allowed_commands[command])
	  continue;
	if (correct_results[(int) number])
	  write("Repeated test number " + number + ".\n");
	send("reset_reading_node_counter");
	send("reset_owl_node_counter");
	send("reset_connection_node_counter");
	send(s);
	if (sscanf(s, "%*sreg_genmove%*s") == 2)
	  send("10005 move_uncertainty");
	send("10000 get_reading_node_counter");
	send("10001 get_owl_node_counter");
	send("10002 get_connection_node_counter");
	send("10003 cputime");
      }
      else if (sscanf(s, "#? [%[^]]]%s", answer, expected)) {
	correct_results[(int)number] = answer;
	if (expected == "*")
	  expected_failures[(int)number] = 1;
      }
      else
	send(s);
    }
    
    if (debug)
      werror("Signalling finish of writing\n");
    writing_finished->write("\n");
    send("10004 cputime");
    reading_finished->read();
    send("10004 quit");
  }
}

array(Testsuite) testsuites = ({});
multiset(string) allowed_commands = (<>);

// Replace all tests in the testsuite with new tests checking whether
// the given answers are unoccupied vertices.
string modify_testsuite(string testsuite)
{
  string s = "";
  int test_number = 0;
  Regexp re = Regexp("[^A-T]([A-T][0-9]+)(.*)");
  foreach (testsuite / "\n", string row) {
    if ((int) row != 0)
      test_number = (int) row;
    else if (row[0..1] != "#?")
      s += row + "\n";
    else {
      string coord;
      int n = 11;
      while (re->split(row)) {
	[coord, row] = re->split(row);
	s += sprintf("%d color %s\n", 100 * test_number + n, coord);
	s += "#? [empty]\n";
	n++;
      }
    }   
  }
  return s;
}

void final_report()
{
  float total_time = 0.0;
  float total_cputime = 0.0;
  float total_uncertainty = 0.0;
  int reading_nodes = 0;
  int owl_nodes = 0;
  int connection_nodes = 0;
  int number_unexpected_pass = 0;
  int number_unexpected_fail = 0;

  foreach (testsuites, Testsuite t) {
    total_time       += t->walltime;
    total_cputime    += t->cputime;
    total_uncertainty += t->uncertainty;
    reading_nodes    += t->reading_nodes;
    owl_nodes        += t->owl_nodes;
    connection_nodes += t->connection_nodes;
    number_unexpected_pass += sizeof(t->PASS);
    number_unexpected_fail += sizeof(t->FAIL);
  }
  write("Total nodes: %d %d %d\n", reading_nodes, owl_nodes,
	connection_nodes);
  write("Total time: %.2f (%.2f)\n", total_cputime, total_time);
  write("Total uncertainty: %.2f\n", total_uncertainty);
  if (number_unexpected_pass > 0)
    write("%d PASS\n", number_unexpected_pass);
  if (number_unexpected_fail > 0)
    write("%d FAIL\n", number_unexpected_fail);
  if (report_slow) {
    write("Slowest moves:\n");
    slow_moves->report("%s: %f seconds\n");  
  }
}

string parse_tests(mapping(string:array(int)) partial_testsuites,
		   string tests)
{
  string suite, numbers;
  if (sscanf(tests, "%[^ :]:%s", suite, numbers) != 2) {
    suite = tests;
    numbers = "";
  }

  if (has_value(suite, " "))
    return "";
  
  if (!has_suffix(suite, ".tst"))
    suite += ".tst";

  if (numbers != "") {
    if (!partial_testsuites[suite])
      partial_testsuites[suite] = ({});
    else if (sizeof(partial_testsuites[suite]) == 0)
      return suite;
    
    foreach (numbers / ",", string interval) {
      int start, stop;
      if (sscanf(interval, "%d-%d", start, stop) == 2)
	for (int k = start; k <= stop; k++)
	  partial_testsuites[suite] |= ({k});
      else
	partial_testsuites[suite] |= ({(int) interval});
    }
  }
  else
    partial_testsuites[suite] = ({});
  
  return suite;
}

int main(int argc, array(string) argv)
{
  array(string) testsuites = ({});
  mapping(string:array(int)) partial_testsuites = ([]);

  array(array(mixed)) all_options;

  all_options = ({ ({"help", Getopt.NO_ARG, ({"-h", "--help"})}),
		   ({"verbose", Getopt.NO_ARG, ({"-v", "--verbose"})}),
		   ({"valgrind", Getopt.NO_ARG, "--valgrind"}),
		   ({"check-unoccupied-answers", Getopt.NO_ARG,
		     "--check-unoccupied"}),
		   ({"slow_moves", Getopt.HAS_ARG, ({"-s", "--slow-moves"})}),
		   ({"engine", Getopt.HAS_ARG, ({"-e", "--engine"})}),
		   ({"options", Getopt.HAS_ARG, ({"-o", "--options"})}),
		   ({"file", Getopt.HAS_ARG, ({"-f", "--file"})}),
		   ({"jobs", Getopt.HAS_ARG, ({"-j", "--jobs"})}),
		   ({"limit-commands", Getopt.HAS_ARG, ({"-l", "--limit-commands"})})});

  mapping(string:mixed) options = ([]);
  string engine = "";
  array(string) engine_options = ({});
  int jobs = 1;
  
  foreach (Getopt.find_all_options(argv, all_options), array(mixed) option) {
    [string name, mixed value] = option;
    switch (name) {
      case "help":
	write(help_message, basename(argv[0]));
	return 0;
	break;
      
      case "valgrind":
	options["valgrind"] = 1;
	break;
	
      case "check-unoccupied-answers":
	options["check-unoccupied-answers"] = 1;
	break;
	
      case "verbose":
	verbose = 1;
	break;
	
      case "engine":
	engine = value;
	break;
	
      case "options":
	engine_options += value / " ";
	break;

      case "slow_moves":
        report_slow = 1;
        slow_moves = Highscorelist((int) value);
        break;
	
      case "file":
	string testlist = Stdio.read_file(value);
	if (!testlist) {
	  werror("Couldn't find %s\n", value);
	  continue;
	}
	foreach ((testlist / "\n") - ({""}), string tests)
	  testsuites |= ({parse_tests(partial_testsuites, tests)});
	break;

      case "jobs":
	jobs = (int) value;
	break;

      case "limit-commands":
	foreach (value / ",", string command)
	  allowed_commands[command] = 1;
	break;
    }
  }

  if (engine == "") {
    engine = "../interface/gnugo";
    engine_options |= "--quiet --mode gtp" / " ";
  }

  argv = Getopt.get_args(argv)[1..];
  foreach (argv, string tests)
    testsuites |= ({parse_tests(partial_testsuites, tests)});

  testsuites -= ({""});
  
  if (sizeof(testsuites) == 0) {
    string makefile = Stdio.read_file("Makefile.am");
    foreach (makefile / "\n", string s) {
      string filename;
      if (sscanf(s, "%*sregress.sh $(srcdir) %s ", filename) == 2)
	testsuites += ({filename});
    }
  }

  if (jobs < 1)
    jobs = 1;

  testsuite_queue = Thread.Queue();
  
  for (int j = 0; j < jobs; j++)
    thread_create(run_testsuites, engine, engine_options,
		  options, partial_testsuites);
  
  foreach(testsuites, string testsuite)
    testsuite_queue->write(testsuite);

  for (int j = 0; j < jobs; j++)
    testsuite_queue->write("");

  while (testsuite_queue->size() > 0)
    sleep(1);

  final_report();
}

void run_testsuites(string engine, array(string) engine_options,
		    mapping(string:mixed) options,
		    mapping(string:array(int)) partial_testsuites)
{
  while (1) {
    string suite_name = testsuite_queue->read();
    if (suite_name == "")
      break;
    
    Testsuite current_testsuite = Testsuite(suite_name - ".tst");
    testsuites += ({current_testsuite});
    
    current_testsuite->run_testsuite(suite_name, engine, engine_options,
				     options, partial_testsuites[suite_name]);
  }
}

string help_message =
"Usage: %s [OPTIONS]... [TESTS]...\n"
"\n"
"Run all regressions or a selection of them.\n"
"Options:\n"
"  -h, --help                    Display this help and exit.\n"
"  -v, --verbose                 Show also expected results.\n"
"      --valgrind                Run regressions under valgrind (very slow).\n"
"      --check-unoccupied        Do not run regressions. Instead check that\n"
"                                the listed answers are not occupied.\n"
"  -e, --engine=ENGINE           Engine to run regressions on. Default is\n"
"                                ../interface/gnugo.\n"
"  -o, --options=OPTIONS         Options passed to the engine.\n"
"  -f, --file=FILE               File containing a list of tests to run.\n"
"  -j, --jobs=JOBS               Number of testsuites being run in parallel.\n"
"  -l, --limit-commands=COMMANDS Only run tests having certain GTP commands.\n"
"\n"
"Tests are listed on the command line in one of the following forms:\n"
"reading           Run all tests in the testsuite reading.tst.\n"
"reading:4         Run test number 4 in reading.tst.\n"
"reading:4,17,30   Run tests with numbers 4, 7, and 30 in reading.tst\n"
"reading:4-17      Run tests with numbers between 4 and 17 in reading.tst\n"
"\n"
"It is also allowed to include the suffix \".tst\" above and more complex\n"
"lists like \"reading.tst:1-3,15,17,30-50,52\" are also understood.\n"
"The format of files used with --file is the same, with one testsuite on\n"
"each line.\n"
"\n"
"If no test suite is listed on the command line or read from file, then all\n"
"regressions listed in Makefile.am will be run.\n"
"\n"
"The --limit-commands option takes a comma separated list of GTP commands,\n"
"e.g. '--limit-commands=attack,defend' to only run tactical reading tests.\n";

/*
 * Local Variables:
 * tab-width: 8
 * c-basic-offset: 2
 * End:
 */
