#!/usr/bin/perl

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU GO, a Go program.        #
#                                                               #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/ #
# for more information.                                         #
#                                                               #
# Copyright 1999, 2000, 2001 by the Free Software Foundation.   #
# Copyright 2001 by Code Bus, Inc.                              #
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
#
# Here is a perlscript regress.pl. Its purpose is to run
# the regression tests that are currently implemented with
# shells and awk scripts.
#
# Run with:
#
# regress.pl --help 
#

package REGRESS;

use IPC::Open2;
use IPC::Open3;
use IO::Handle;
use Getopt::Long;
use FileHandle;

use FindBin;

use strict;
use warnings;


STDOUT->autoflush(1);

my $helpstring = "

Run with:

regress.pl --goprog \'<path to program> --mode gtp [program options]\' \\
           --testfile \'<path to gtp test file>\' \\
           --all_batches  Ignores --testfile, gets test files from Makefile.in
           --numbers \'regexp of test numbers the next test after which won\'t be run\'
           [options]

Possible options:

  --verbose 1 (to list moves) or --verbose 2 (to draw board)
      [FIXME: verbose levels not well defined.]
  --html 0 (to not generate html) or --html 1 (default - generate html file w/ results)

";


my %categories = 
 ("JOSEKI_DATABASE", "",
  "JOSEKI_PATTERN", "", 
  "FUSEKI_CONCEPT", "",
  "DYNAMIC_CONNECTION", "Dynamic Connection Reading",
  "TACTICAL_READING", "",
  "OWL_TUNING", "",
  "PATTERN_TUNING", "",
  "CONNECTION_TUNING", "",
  "MOVE_VALUATION", "",
  "ATARI_ATARI", "",
  "SEMEAI_MODULE", "",
  "KO_READING", ""
 );


my $cur_passed;
my $result;
my $correct_re;
my $bang;
my $top_moves;
my $handicap_stones;
my $sgfmove;
my $vertex;
my @vertices;
my $first;
my $second;
my $resultb;
my $resultw;
my $scriptfile;
my $pidt;
my $pidg;
my $testdir;
my $goprog;
my $verbose = 0;
my $old_whole_gtp = "";
my $html_whole_gtp = "";
my $testfile;
my $num;
my $filepos;
my $goprog_in ;		# stdin of computer player
my $goprog_out;		# stdout of computer player
my $passes;
my $unexpected_pass;
my $failures;
my $unexpected_fail;
my $numbers = "";
my $boardsize = 19;  #current boardsize
my $testfile_out;
my $all_batches;
my $make_images;
my $cputime;

my $goprog_name = "unknown";
my $goprog_version = "0";
my $goprog_timestamp = 0;

my $do_topmove = 0;
my $one_gg_process = 0;

my @failed_links;
my @FAILED_links;


my $next_cmd = "";
my $prev_cmd = "";


GetOptions(
           "goprog|g=s"         => \$goprog,
           "verbose|v=i"        => \$verbose,
           "numbers|n=s"	=> \$numbers,
           "all_batches|all-batches|a=i" => \$all_batches,
           "make_images|m=i"    => \$make_images
);

if ($make_images) {
  make_images();
  exit;
}

die $helpstring unless defined $goprog;


if (!-e "html") {
  mkdir "html";
}


# if $numbers matches the current test number, then read it to mean:
# "inhibit all gtp commands AFTER the matching number, until the next 
# numbered test, then resume."
if ($numbers) {
  $numbers = "^($numbers)\$";
}

use File::stat;


# create FileHandles
$goprog_in  = new FileHandle;		# stdin of computer player
$goprog_out = new FileHandle;		# stdout of computer player
$pidg = open2($goprog_out, $goprog_in, $goprog);
print "goprog pid: $pidg\n" if $verbose > 1;
my ($goprog_exe) = split (" ", $goprog);
$goprog_timestamp = (stat $goprog_exe)->mtime;

go_command("name");
#print $goprog_in "name\n";
$_ = <$goprog_out>;
if (/^=\s*(.*)/) {
  ($goprog_name = $1) =~ s/\s*$//;
}
<$goprog_out>;
go_command("version");
#print $goprog_in "version\n";
$_ = <$goprog_out>;
if (/^=\s*(.*)/) {
  ($goprog_version = $1) =~ s/\s*$//;
}
<$goprog_out>;

print "Name: " .  $goprog_name ." ". $goprog_version . "\n" if $verbose > 1;

if ($one_gg_process) {
  go_command("quit");#print $goprog_in "quit\n";
  print "waiting\n" if $verbose > 2;
  waitpid $pidg, 0;
  print "done waiting\n" if $verbose > 2;
}


if ($all_batches) {
  @ARGV = allTargets();
}
  
  
my $curtstfile = "";
my $file_count = 0;
while ($file_count <= $#ARGV) {
  $curtstfile = $ARGV[$file_count];
  unlink "html/index.html";
  unlink "html/$curtstfile/index.html";
  print "regressing file $ARGV[$file_count]\n" if $verbose > 1;
  unlink "html/$curtstfile/index.html";
  regress_file ($ARGV[$file_count]);
  $file_count++;
  @failed_links = @FAILED_links = ();
};


go_command("quit");
print "waiting\n" if $verbose > 1;
waitpid $pidg, 0;
print "done waiting\n" if $verbose > 1;

exit;


sub regress_file {
  unless ($one_gg_process) {
    $goprog_in  = new FileHandle;		# stdin of computer player
    $goprog_out = new FileHandle;		# stdout of computer player
    $pidg = open2($goprog_out, $goprog_in, $goprog);
    print "goprog pid: $pidg\n" if $verbose > 1;
  }
  
  
  $testfile = shift;
  print "$testfile\n" if $verbose;

  #main bit.
  $pidt = open ($testfile_out,"<$testfile");
  print "testfile pid: $pidt\n" if $verbose > 1;

  my $negate;
  my $ignore;
  my $fail;
  $passes=0;
  $unexpected_pass=0;
  $failures=0;
  $unexpected_fail=0;
  $result = "";
  $next_cmd = "";
  $num = 0;
  $filepos = 0;
  go_command("cputime");
  $cputime = <$goprog_out>;
  print "cputime: $cputime\n" if $verbose > 1 or 1;
  ($cputime) = ($cputime =~ /((\d|\.)+)/);
  print "cputime: $cputime\n" if $verbose > 1 or 1;
  <$goprog_out>;  
  my $skipping = 0;
  while (defined($next_cmd))
  {
    $filepos++;
    my $force_read = 1;
    while ($force_read) {
      $prev_cmd = $next_cmd;
      $next_cmd = <$testfile_out>;
      $force_read = 0;
      if (defined($next_cmd)) {
        chop($next_cmd);
        print "$next_cmd\n" if ($verbose > 1);
        if (($next_cmd =~ /^\s*#\?\s+\[(\!*)(.*)\]\s*(\*)*(\&)*\s*$/)) {
          $bang = $1;
          if ($1) { $negate = 1} else {$negate = 0};
          $correct_re = $2;
          if ($3) { $fail = 1} else { $fail = 0};
          if ($4) {$ignore = 1} else {$ignore = 0};

          my $old_skipping = $skipping;
          $skipping = $numbers && ($num =~ $numbers);
          if ($old_skipping) {
            print "$num skipped.\n";
            tally_result ($num, "skipped", "&nbsp;", "&nbsp;");
          } else {
 	    print "TST: $negate - $correct_re - $fail - $ignore\n" if $verbose > 1;
	    if (!$ignore) {
	      my $match_result = $result =~ /^$correct_re$/ ;
	      if ($negate) {
		$match_result = ! $match_result;
	      }
	      if ($match_result) {
		if ($fail) {
		  tally_result ( $num, "PASSED", "$bang$correct_re", "$result");
		} else {
	          tally_result ( $num, "passed", "$bang$correct_re", "$result");
		}
	      } else {
		if (!$fail) {
		  tally_result ( $num, "FAILED", "$bang$correct_re", "$result");
		} else {
      		  tally_result ( $num, "failed", "$bang$correct_re", "$result");
		}
	      }
	    }
          }
	  $old_whole_gtp = $html_whole_gtp;
	  $html_whole_gtp = "";
	} else {
	  if (!($next_cmd =~ /^\s*$/)) {
	    $html_whole_gtp .= "   " . html_encode($next_cmd) . "<BR>\n";
	  }
	}
 	$next_cmd =~ s/^\s*$//;  $next_cmd =~ s/^#.*$//;
  	$force_read = $next_cmd eq ""
      }
    }
    if (defined($next_cmd)) {
      if (!$skipping) {
	$top_moves = "";
	if ($do_topmove) {
	  if ($next_cmd =~ /gg_genmove\s+([blackwhite])+/) {
	    $next_cmd =~ s/gg_genmove\s+([blackwhite]+)/top_moves_$1/;
	    $top_moves = 1;
	  }
	}
 	go_command($next_cmd); #print $goprog_in "$next_cmd\n";
 	if ($top_moves) {
 	  $top_moves = eat_move($goprog_out);
 	  if ($top_moves) {
 	    ($result, $_) = split(/ /, $top_moves, 2);
 	  } else {
 	    $result = "PASS";
 	    $top_moves = "";
 	  }
 	  print "TopMoves:$top_moves\n" if $verbose > 1;
 	} else {
	  $result = eat_move($goprog_out);
	  if (!defined($result)) {$result="";}
	}
	print "RES: $result\n" if $verbose > 1;
      }
      $next_cmd =~ /^([0-9]+)/;
      if ($1) {$num = $1;}
    }
  }
  my $pass_string;
  my $fail_string;
  if ($unexpected_pass == 1) {
    $pass_string = "pass";
  } else {
    $pass_string = "passes";
  }
  if ($unexpected_fail == 1) {
    $fail_string = "failure";
  } else {
    $fail_string = "failures";
  }

  print "Summary: $passes/" . ($passes + $failures) . 
        " passes. $unexpected_pass unexpected $pass_string, "
       . "$unexpected_fail unexpected $fail_string\n";

  unless ($one_gg_process) {
    go_command("quit");#print $goprog_in "quit\n";
    print "waiting\n" if $verbose > 2;
    waitpid $pidg, 0;
    print "done waiting\n" if $verbose > 2;
  }
}

sub tally_result {
  (my $number, my $status, my $correct, my $incorrect) = @_;
  my $showboard = $status ne "skipped";
  $passes++ if $status eq "passed";
  $unexpected_pass++ if $status eq "PASSED";
  $failures++ if $status eq "failed";
  $unexpected_fail++ if $status eq "FAILED";
  
  if ($status ne "skipped") {
    print "$number: $status: correct: $correct  incorrect: $incorrect\n";
  }
  
  $cur_passed = ($status =~ /pass/i);
  if ($showboard) {
    mkdir ("html/$testfile");# die quietly - probably already exists.
    my $brd = new FileHandle;
    open ($brd, "> html/$testfile/$num.xml") || die "ERROR: couldn't crate xml board: $!\n";
    my $brdout = eat_board();
    print $brd "<GOPROB filepos=$filepos number=$num file=\"$testfile\" status=\"$status\">\n";
    print $brd qq@<ENGINE version="$goprog_version" name="goprog_name" timestamp="goprog_timestamp">\n@;
    print $brd "<CORRECT>$correct</CORRECT>\n";
    print $brd "<ANSWER>$incorrect</ANSWER>\n";
    if ($html_whole_gtp !~ /^\s*loadsgf/m) {
      $old_whole_gtp .= $html_whole_gtp;
      $html_whole_gtp = $old_whole_gtp;
    }
    print $brd "<GTP_ALL>\n$html_whole_gtp\n</GTP_ALL>";
    foreach my $listval ("DESCRIPTION", "CATEGORY", "SEVERITY") {
      my $astxt;
      $html_whole_gtp =~ /$listval=(.*?)<BR>/;
      if (defined($1)) {$astxt = $1;} else {$astxt = "";};
      print $brd "<$listval>$astxt</$listval>\n";
    }
    print $brd "<NODES owl=123 reading=123>\n";

    go_command("cputime");
    my $new_cputime = <$goprog_out>;
    ($new_cputime) = ($new_cputime =~ /((\d|\.)+)/);
    print "cputime: ".$new_cputime."\n" if $verbose > 1 or 1;
    <$goprog_out>;  
    print $brd "<TIME wall=0.123 CPU=" . sprintf("%.5f", $new_cputime - $cputime) .  ">\n";
    $cputime = $new_cputime;

    print $brd "<GTP_COMMAND>$prev_cmd</GTP_COMMAND>\n";
    print $brd $brdout;
    print $brd "</GOPROB>\n";
    close $brd;
  }
}

sub html_encode {
  my $r = shift;
  $r =~ s/&/&amp;/g;
  $r =~ s/</&lt;/g;
  $r =~ s/>/&gt;/g;
  return $r;
}


sub eat_board {
  go_command("query_boardsize");
  my $line = "";
  while ($line eq "") {
    chop($line = <$goprog_out>) or die "No response!";
  }
  <$goprog_out>;
  (undef, $boardsize) = split(' ', $line, 2);
  $boardsize = $boardsize + 0;
  my $linesleft = $boardsize + 2;

  my $xboard = "";

  my $cur_point = 0;
  my $cur_color = 0;
  my $cur_matcher_status = 0;
  my $cur_dragon_status=0;
  my $cur_owl_status=0;
  my $cur_color_letter=0;
  my %dragons;
  my $white_letter = chr(ord('z')+1);
  my $black_letter = chr(ord('A')-1);
  my $iline = 1;
  my $no_dragon_data = 0;
  my %stones;

  if ($prev_cmd =~ /gg_genmove/) {
    #FIXME: There may be other commands that won't require dragon_data
    #to be regenerated.  Better might be to provide a way to query the
    #engine whether dragon_data is currently available w/out regenerating.
    go_command("dragon_data\n");
    while ($iline) {
      $iline = $_ = <$goprog_out>;
      if ($iline =~ /^\?(.*)/) {
        $no_dragon_data = $1;
        $iline = $_ = <$goprog_out>;
        last;
      }
      $iline =~ s/\s*$//mg;
      if ($iline =~ /^=?\s*([A-Z][0-9][0-9]?):\s*$/ || !$iline) {
  	if ($cur_point) {
	  if ($cur_color eq "white") {
	    $_ = $white_letter = chr(ord($white_letter)-1);
	    $cur_color_letter = "O";
	  } elsif ($cur_color eq "black"  || die "invalid color $cur_color") {
	    $_ = $black_letter = chr(ord($black_letter)+1);
	    $cur_color_letter = "X";
	  }
  	  $dragons{$cur_point} = $_ . ";status=" . $cur_dragon_status . 
  				      ";owl_status=" . $cur_owl_status . 
            			      ";color_letter=" . $cur_color_letter.
            			      ";";
          $cur_color = 0;
          $cur_matcher_status = 0;
          $cur_dragon_status=0;
          $cur_owl_status=0;
          $cur_color_letter=0;
	}
  	$cur_point = $1;
      } elsif ($iline =~ /^color:?\s+([blackwhite]*)\s*$/) {
        $cur_color = $1;
      } elsif ($iline =~ /^matcher_status:?\s+(\S*)\s*$/) {
        $cur_matcher_status = $1;
      } elsif ($iline =~ /^status:?\s+(\S*)\s*$/) {
        $cur_dragon_status = $1;
      } elsif ($iline =~ /^owl_status:?\s+(\S*)\s*$/) {
        $cur_owl_status = $1;
      } else {
        #we ignore lots of dragon data!
      }
    }
  } else {
    $no_dragon_data=1;
    foreach $cur_color ("white", "black") {
      $iline = 1;
      go_command("worm_stones $cur_color");
      if ($cur_color eq "white") {
        $cur_color_letter = "O";
      } elsif ($cur_color eq "black"  || die "invalid color $cur_color") {
        $cur_color_letter = "X";
      }
      while ($iline) {
        $iline = <$goprog_out>;
        $iline =~ s/^[=]\s*//;
        $iline =~ s/\s*$//mg;
        foreach (split (/\s+/,$iline)) {
          $stones{$_} =";color_letter=" . $cur_color_letter.
  	    	       ";";
        }
      }
    }
  }
  
  if ($prev_cmd =~ /^[0-9]*\s*gg_genmove/) {
    if (! ($next_cmd =~ /^#\?\s*\[(!)?\(?(.*)\)?\]\*?\s*$/)) {
      print "BAD TEST: $next_cmd\n";
    }
    #$1 and $2 are just $bang and $correct_re, right?
    #print "Genmove test:\n";
    #print "  $1;$2\n";
    foreach (split(/\|/,$2)) {
      if ($1) {
        $stones{$_} .= ";known_wrong;";
      } else {
        $stones{$_} .= ";known_right;";
      }
    }
    if ($cur_passed) {
      $stones{$result} .= ";try_right;";
    } else {
      $stones{$result} .= ";try_wrong;";
    }
  } else {
    # Experimental - should work for gg_genmove too!
    if (! ($next_cmd =~ /^#\?\s*\[(!)?\(?(.*)\)?\]\*?\s*$/)) {
      print "BAD TEST: $next_cmd\n";
    }  #see commend on this regex above.
    my $known = $2;
    #Here, look for something that looks like a move!
    while ($known =~ s/([A-Z]\d\d?)//) {
      if ($bang) {
        $stones{$1} .= ";known_wrong;";
      } else {
        $stones{$1} .= ";known_right;";
      }
    }
    my $try = $result;
    while ($try =~ s/([A-Z]\d\d?)//) {
      if ($cur_passed) {
        $stones{$1} .= ";try_right;";
      } else {
        $stones{$1} .= ";try_wrong;";
      }
    }
  }
    
  {
    my $pc = $prev_cmd;
    while ($pc =~ s/([A-Z]\d\d?)//) {
      $stones{$1} .= ";question;";
    }
  }

    

  unless ($no_dragon_data) {	
    #FIXME: This data is available via the strings line from dragon_data.
    go_command("dragon_stones");
    $iline = 1;
    while ($iline) {
      $iline = <$goprog_out>;
      $iline =~ s/\s*$//mg;
      $iline =~ s/^=?\s*//;
      $iline = " " . $iline . " ";
      foreach (keys(%dragons)) {
        my $k = $_;
        my $label = $dragons{$k};
        if ($iline =~ (" ".$k." ")) {
          $iline =~ s/^\s*//;   
          $iline =~ s/\s*$//;
          foreach (split(/ /,$iline)) {
            $stones{$_} = $label;
          }
        }
      }
      $iline =~ s/\s*//mg;
    }
  }

  my %tmarr;
  if ($prev_cmd =~ /.*gg_genmove\s+([whiteblack]+)/) {
    go_command ("top_moves");
    my $top_moves = <$goprog_out>;
    <$goprog_out>;
    if ($top_moves) {
      $top_moves =~ s/^=\s*//;
      $top_moves =~ s/\s*$//mg;
      print "TOP_MOVES:'$top_moves'\n" if $verbose > 1;
      if ($top_moves =~ /^\s*(.*)\s*/) { #i.e. always!
        my $t = $1;
        %tmarr = split(/\s+/,$t);
        foreach my $k (keys(%tmarr)) {
          $stones{$k} .=  ";move_value=$tmarr{$k};";
        }
      }
    }
  }
        
  my $j;
  my $i;

  for ($j = $boardsize; $j > 0; $j--) {
    my $jA = $j;
    if ($j <= 9) {
      $jA .= " ";
    }
    for ($i = 1; $i <= $boardsize; $i++) {
      my $iA = ord('A') + $i - 1;
      if ($iA >= ord('I')) { $iA++; }
      $iA = chr($iA);
      my $point = "";
      if ($stones{$iA.$j}) {
        $point .= qq/   coord="$iA$j"\n/;
        my $status = $stones{$iA.$j};
        if ($status =~ /(.).*;owl_status=([^;]*);/) {
          $point .= qq/   owl_status="$2"\n/;
        }
        if ($status =~ /(.).*;status=([^;]*);/) {
          $point .= qq/   dragon_letter="$1"\n/;
          $point .= qq/   dragon_status="$2"\n/;
        }
        if ($status =~ /;color_letter=([^;]*);/) {
          $point .= qq/   stone="/ . (($1 eq 'X') ? 'black' : 'white') . qq/"\n/;
        }
        if ($status =~ /;move_value=([^;]*);/) {
          $point .= qq/   move_value="$1"\n/;
        }
        $point .= qq/   known="wrong"\n/ if ($status =~ /;known_wrong;/);
        $point .= qq/   known="right"\n/ if ($status =~ /;known_right;/);
        $point .= qq/   try="right"\n/   if ($status =~ /;try_right;/);
        $point .= qq/   try="wrong"\n/   if ($status =~ /;try_wrong;/);
        
        $point .=  qq/   question="1"\n/  if ($status =~/;question;/);
      }
      if ($point) {
        $xboard .= " <POINT\n" . $point . " ></POINT>\n";
      }
    }
  }
  
  return "<BOARD size=$boardsize>\n" . $xboard . "</BOARD>\n";
}
  

sub eat_move {
    my $h = shift;
# ignore empty lines
    my $line = "";
    while ($line eq "") {
	chop($line = <$h>) or die "No response!";
    }
    $line =~ s/\s*$//;
    my ($equals, $move) = split(' ', $line, 2);
    $line = <$h>;
    return $move;
}

sub go_command {
  $_ = shift;
  print $goprog_in "$_\n";
  print "CMD:$_\n" if $verbose > 1;
}


my %images;
sub extract_images {
  my $line = shift;
  #i.e.: <TD><IMG HEIGHT=25 WIDTH=25 SRC="../images/B25x43_green.png"></TD>
  if ($line =~ /SRC=.*images.(.*)\"><.TD>.*/) {
    if ($verbose) {
      print "  found: $1\n" unless ($images{$1});
    }
    $images{$1} = 1;
  }
}

our $curdir;
our $curfile;  
our $CURDIR;
sub extract_image_dir {
  local $curdir = shift;
  local $CURDIR;
  opendir $CURDIR, $curdir;
  while (local $curfile = readdir $CURDIR) {
    $_ = "$curdir/$curfile";
    #print -d."\n";
    #print "X:".($curfile=~/^\.+$/)."\n";
    if ((-d ) && !($curfile=~/^\.{1,2}$/)) {
      print "diving into: $curdir/$curfile\n" if $verbose>2;
      extract_image_dir ("$curdir/$curfile");
    } elsif (($curfile =~ /\.html$/) && ($curdir =~ /d2/)) {
      print "processing: $curdir/$curfile\n" if $verbose;
      open IMGFILE, "<$curdir/$curfile" or die "Couldn't open: $curdir/$curfile" ;
      while (<IMGFILE>) {
        extract_images($_);
      }
      close IMGFILE;
    } else {
      #print "no match: $curdir/$curfile\n" if $verbose;
    }
  }
  closedir CURDIR;
}
 

sub make_images {
  print "Starting processing\n" if $verbose;
  extract_image_dir (".") ;
  print "Processed files, generated ".((scalar keys(%images))/2)
         ." unique images:\n" if $verbose;
  foreach (keys(%images)) {
    parseFileName($_);
  }
  print "Done.\n" if $verbose;
}


sub allTargets {
  open (MAKEFILE, "< Makefile.in");
  my @targets = "";
  while (<MAKEFILE>) {
    if (s/^all_batches://) {
      @targets = split;
      last;
    }
  }
  my $target_reg = "^" . join ("|", @targets) . ":" ;
  close MAKEFILE;
  
  open  (MAKEFILE, "< Makefile.in");
  my @files;
  while (<MAKEFILE>) {
    if ($_ =~ $target_reg) {
      chop($_ = <MAKEFILE>);
      while ($_) {
        push @files, $_ =~ /\s+(\w+\.tst)/;
        chop($_ = <MAKEFILE>);
      }
    }
  }
  close MAKEFILE;
  
  return @files;
}
