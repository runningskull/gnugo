#invoke with gnugo --mode gtp < reading.tst | awk -f regress.awk tst=reading.tst

# store results of gnugo in array gtpout

# store test numbers in array test to be able
# to retrieve test results in same order as in gtp file

BEGIN {
  ntest = 0;
  nexpect = 0;
  passes = 0;
  unexpected_pass = 0;
  unexpected_fail = 0;
  failures = 0;
}

/^[=?][0-9]+ /{
  sub(/\r/, "");
  sub(/^[=?]/, "");
  num = $1;
  sub(/^[0-9]+ */, "");
  gtpout[num] = $0;
  test[ntest] = num;
  ntest++;
}

/^;/{
  print
}

END {

# store test results in tst file in array expect

  while (getline < tst) {
    if (match ($0, /^#\?/)) {
      sub(/^#\? */, "");
      expect[num] = $0;
      nexpect++;
    } else {
      num = $1;
    }
  }

  if (nexpect != ntest) {
    if (ntest > 0) {
      print "Possible crash!: expected " nexpect " results, got " ntest ". Last successful test was " test[ntest-1] ".";
    } else {
      print "Possible crash!: expected " nexpect " results, got " ntest ". Crash in first test.";
    }
  }

# check results of gnugo --mode gtp

  for (i=0; i<ntest; i++) {

    num = test[i];
    correct_prn = expect[num];
    sub(/^\[/, "", correct_prn);
    sub(/\][&*]*$/, "", correct_prn);

    ignore = 0;
    fail = 0;
    negate = 0;
    if (match(expect[num], /&$/)) {
      ignore = 1;
    }
    if (match(expect[num], /\*$/)) {
      fail = 1;
    }
    correct_re = correct_prn;
    if (match(correct_re, /^!/)) {
      negate = 1;
      sub(/^!/, "", correct_re);
    }
    sub(/^/, "^", correct_re);
    sub(/$/, "$", correct_re);

    result = gtpout[num];
    if (!ignore) {
      match_result = match(result, correct_re);
      if (negate)
	match_result = !match_result;
      if (match_result) {
	passes++;
	if (fail) {
	  unexpected_pass++;
	  if (verbose)
	    print num " PASSED";
	  else
	    print num " unexpected PASS!";
         if (url)
           print "   "url""tst":"num;

	} else {
	  if (verbose) {
	    print num " passed";
	  }
	}
      } else {
	failures++;
	if (!fail) {
	  unexpected_fail++;
	  if (verbose)
	    print num " FAILED: Correct '" correct_prn "', got '" result "'";
	  else  
	    print num " unexpected FAIL: Correct '" correct_prn "', got '" result "'";
          if (url)
            print "   "url""tst":"num;
	} else {
	  if (verbose) {
	    print num " failed: Correct '" correct_prn "', got '" result "'";
	  }
	}
      }
    } else {
      if (verbose) {
        print num " " result;
      }
    }
  }
  if (verbose) {
    total = passes + failures;
    
    if (unexpected_pass == 1)
      pass_string = "pass";
    else
      pass_string = "passes";
    
    if (unexpected_fail == 1)
      fail_string = "failure";
    else
      fail_string = "failures";
    
    print "Summary: " passes "/" total " passes. " unexpected_pass " unexpected " pass_string ", " unexpected_fail " unexpected " fail_string;
  }
}
