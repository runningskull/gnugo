#!/usr/bin/env pike

static Thread.Queue write_queue;
static Thread.Condition cond;
static Thread.Mutex condmutex;
static Thread.MutexKey condmutexkey;
static int debug = 0;
static object machine;
static mapping(int:string) correct_results = ([]);
static multiset expected_failures = (<>);
static int timebase;
static float last_time;
static float total_time = 0.0;
static int total_pass = 0;
static int total_fail = 0;
static int report_all_tests = 0;

class Testsuite
{
  string name;
  int reading_nodes;
  int owl_nodes;
  int connection_nodes;
  float time;
  float cputime;
  
  array(int) pass;
  array(int) fail;
  array(int) PASS;
  array(int) FAIL;

  void create(string s)
  {
    name = s;
    reading_nodes = 0;
    owl_nodes = 0;
    connection_nodes = 0;
    time = 0.0;
    pass = ({});
    fail = ({});
    PASS = ({});
    FAIL = ({});
  }
}

array(Testsuite) testsuites = ({});
Testsuite current_testsuite;

void Send(string|void s)
{
  if (!s) {
    if (debug)
      werror("Finishing sending.\n");
    write_queue->write("");
  }
  else {
    if (debug)
      werror("Sent: " + s + "\n");
    write_queue->write(s + "\n");
  }
}

static void Finish()
{
  write("%-37s %7.2f %9d %7d %8d\n", current_testsuite->name,
	current_testsuite->cputime,
	current_testsuite->reading_nodes, current_testsuite->owl_nodes,
	current_testsuite->connection_nodes);
  cond->signal();
}

static void ProgramReader(object f)
{
  int move;
  string stored = "";
  array(int) nodes = ({0, 0, 0});
  array(int) total_nodes = ({0, 0, 0});
  array(int) unexpected_failures = ({});
  array(int) unexpected_passes = ({});
  int test_number;
  while (1) {
    string s = f->gets();
    float current_time = time(timebase);
    if (!s)
      break;
    if (debug)
      werror("Recv: " + s + "\n");
    
    int number;
    string answer;
    if (sscanf(s, "=%d %s", number, answer)) {
      if (number < 10000 || number > 10004) {
	test_number = (int) number;
	string correct = correct_results[test_number];
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
	current_testsuite[result] += ({test_number});
	current_testsuite->time += (current_time - last_time);
	
	if (result == "PASS" || result == "FAIL" || report_all_tests)
	  write("%-15s %s %s [%s]\n",
		current_testsuite->name + ":" + test_number, result, answer,
		correct_results[test_number]);
	last_time = current_time;
      }
      else if (number == 10000)
	current_testsuite->reading_nodes += (int) answer;
      else if (number == 10001)
	current_testsuite->owl_nodes += (int) answer;
      else if (number == 10002)
	current_testsuite->connection_nodes += (int) answer;
      else if (number == 10003)
	current_testsuite->cputime = (float) answer;
      else if (number == 10004)
	break;
    }
    else if (sscanf(s, "?%s", answer))
      write("%-15s ?%s\n", current_testsuite->name + ":", answer);
  }
  f->close();
  if (debug)
    werror("Reader closed down.\n");
  Finish();
}

static void ProgramWriter(object f)
{
  while (1) {
    string s = write_queue->read();
    if (s == "")
      break;
    f->write(s);
  }
  f->close();
  if (debug)
    werror("Writer closed down\n");
}

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

void run_testsuite(string suite_name, array(string) program_args,
		   array(int)|void test_numbers)
{
  array(string) program_start_array = ({"../interface/gnugo", "--quiet",
					"--mode", "gtp"});

  string testsuite = Stdio.read_file(suite_name);
  if (!testsuite) {
    werror("Couldn't find " + suite_name + "\n");
    exit(1);
  }

  if (has_value(program_args, "valgrind")) {
    program_start_array = ({"valgrind"}) + program_start_array;
    program_args -= ({"valgrind"});
  }

  if (has_value(program_args, "check-unoccupied-answers")) {
    program_args -= ({"check-unoccupied-answers"});
    testsuite = modify_testsuite(testsuite);
  }

  write_queue = Thread.Queue();
  object f1 = Stdio.File();
  object pipe1 = f1->pipe();
  object f2 = Stdio.FILE();
  object pipe2 = f2->pipe();
  machine = Process.create_process(program_start_array + program_args,
				   (["stdin":pipe1, "stdout":pipe2]));
  thread_create(ProgramReader, f2);
  thread_create(ProgramWriter, f1);

  int number;
  string answer;
  string expected;
  
  timebase = time();
  last_time = time(timebase);
  
  correct_results = ([]);
  expected_failures = (<>);
  current_testsuite = Testsuite(suite_name - ".tst");
  testsuites += ({current_testsuite});
  
  foreach (testsuite/"\n", string s) {
    if (sscanf(s, "%d", number) == 1) {
      if (number >= 10000 && number <= 10003)
	continue;
      if (test_numbers && !has_value(test_numbers, number))
	continue;
      Send("reset_reading_node_counter");
      Send("reset_owl_node_counter");
      Send("reset_connection_node_counter");
      Send(s);
      Send("10000 get_reading_node_counter");
      Send("10001 get_owl_node_counter");
      Send("10002 get_connection_node_counter");
      Send("10003 cputime");
    }
    else if (sscanf(s, "#? [%[^]]]%s", answer, expected)) {
      correct_results[(int)number] = answer;
      if (expected == "*")
	expected_failures[(int)number] = 1;
    }
    else
      Send(s);
  }
  
  Send("10004 quit");
  Send();
  cond->wait(condmutexkey);
}

void final_report()
{
  float total_time = 0.0;
  float total_cputime = 0.0;
  int reading_nodes = 0;
  int owl_nodes = 0;
  int connection_nodes = 0;
  int number_unexpected_pass = 0;
  int number_unexpected_fail = 0;

  foreach (testsuites, Testsuite t) {
    total_time       += t->time;
    total_cputime    += t->cputime;
    reading_nodes    += t->reading_nodes;
    owl_nodes        += t->owl_nodes;
    connection_nodes += t->connection_nodes;
    number_unexpected_pass += sizeof(t->PASS);
    number_unexpected_fail += sizeof(t->FAIL);
  }
  write("Total nodes: %d %d %d\n", reading_nodes, owl_nodes,
	connection_nodes);
  write("Total time: %.2f (%.2f)\n", total_cputime, total_time);
  if (number_unexpected_pass > 0)
    write("%d PASS\n", number_unexpected_pass);
  if (number_unexpected_fail > 0)
    write("%d FAIL\n", number_unexpected_fail);
}

// There is support for various fancy options in the code but
// those are not considered stable yet. The only official way
// to run the script is without any options at all.
int main(int argc, array(string) argv)
{
  cond = Thread.Condition();
  condmutex = Thread.Mutex();
  condmutexkey = condmutex->lock();
  array(string) testsuites = ({});
  mapping(string:array(int)) partial_testsuites = ([]);
  argv = argv[1..];

  testsuites = glob("*.tst", argv);
  argv -= testsuites;

  foreach (glob("+f*", argv), string filename) {
    argv -= ({filename});
    filename = filename[2..];
    string testlist = Stdio.read_file(filename);
    if (!testlist) {
      werror("Couldn't find %s\n", filename);
      continue;
    }
    string name, numbers;
    foreach (testlist / "\n", string tests) {
      if (sscanf(tests, "%s:%s", name, numbers) == 2) {
	name += ".tst";
	testsuites |= ({name});
	partial_testsuites[name] = (array(int)) (numbers / ",");
      }
    }
  }
  
  if (sizeof(testsuites) == 0) {
    string makefile = Stdio.read_file("Makefile.am");
    foreach (makefile / "\n", string s) {
      string filename;
      if (sscanf(s, "%*sregress.sh $(srcdir) %s ", filename) == 2)
	testsuites += ({filename});
    }
  }

  foreach(testsuites, string testsuite)
    run_testsuite(testsuite, argv, partial_testsuites[testsuite]);

  final_report();
}
