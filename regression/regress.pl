#!/usr/bin/perl -I../interface

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
use Getopt::Long;
use FileHandle;

use GoImage::Stone;

use strict;
use warnings;
#
#GTPtoSGF("H1");
#GTPtoSGF("J2");
#exit;
#
my $helpstring = "

Run with:

regress.pl --goprog \'<path to program> --mode gtp [program options]\' \\
           --testfile \'<path to gtp test file>\' \\
           --numbers \'regexp of test numbers the next test after which won\'t be run\'
           [options]

Possible options:

  --verbose 1 (to list moves) or --verbose 2 (to draw board)
      [FIXME: verbose levels not well defined.]
  --html 0 (to not generate html) or --html 1 (default - generate html file w/ results)

";

my @categories = 
 {"JOSEKI_DATABASE", "",
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
 };

  my %colors = ("ALIVE", "green",
	"DEAD", "cyan",
	"CRITICAL", "red",
	"UNKNOWN", "yellow",
	"UNCHECKED", "magenta");

my $top_moves;
my $html = 1;
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
my $htmlfile_out;
my $html_whole_gtp = "";
my $testfile;
my $num;
my $goprog_in ;		# stdin of computer player
my $goprog_out;		# stdout of computer player
my $passes;
my $unexpected_pass;
my $failures;
my $unexpected_fail;
my $numbers = "";
my $boardsize = 19;  #current boardsize
my $testfile_out;

my $goprog_name = "unknown";
my $goprog_version = "0";

my @failed_links;
my @FAILED_links;


GetOptions(
           "goprog|g=s"         => \$goprog,
           "verbose|v=i"        => \$verbose,
           "html|h=i"		=> \$html,
           "numbers|n=s"	=> \$numbers
);

die $helpstring unless defined $goprog;

if ($html > 0  && !-e "html") {
	mkdir "html";
}


# if $numbers matches the current test number, then read it to mean:
# "inhibit all gtp commands AFTER the matching number, until the next 
# numbered test, then resume."
if ($numbers) {
  $numbers = "^($numbers)\$";
}


# create FileHandles
$goprog_in  = new FileHandle;		# stdin of computer player
$goprog_out = new FileHandle;		# stdout of computer player
$pidg = open2($goprog_out, $goprog_in, $goprog);
print "goprog pid: $pidg\n" if $verbose > 1;



print $goprog_in "name\n";
$_ = <$goprog_out>;
if (/^=\s*(.*)/) {
  ($goprog_name = $1) =~ s/\s*$//;
}
<$goprog_out>;
print $goprog_in "version\n";
$_ = <$goprog_out>;
if (/^=\s*(.*)/) {
  ($goprog_version = $1) =~ s/\s*$//;
}
<$goprog_out>;

print "Name: " .  $goprog_name ." ". $goprog_version . "\n" if $verbose > 1;


my $index_out = new FileHandle;
open($index_out, "> html/index.html");
print $index_out 
"<HTML><HEAD><TITLE>Regression test summary - $goprog_name $goprog_version</TITLE></HEAD>
 <BODY>
 <H3> Regression test summary - $goprog_name $goprog_version</H3>
 Program: " . html_encode($goprog) . "<BR>
 <TABLE border=1>
 <TR><TD>file</TD><TD>passed</TD><TD>PASSED</TD><TD>failed</TD><TD>FAILED</TD>
 </TR>\n";

my $file_count = 0;
while ($file_count <= $#ARGV) {
	print $index_out "<TR><TD><A href=\"" . $ARGV[$file_count] . ".html\">" . $ARGV[$file_count] . "</A>";
	print "regressing file $ARGV[$file_count]\n" if $verbose > 1;
	regress_file ($ARGV[$file_count]);
	print $index_out "<TD>".($passes-$unexpected_pass)."</TD><TD>$unexpected_pass</TD>" .
	      "<TD>".($failures-$unexpected_fail)."</TD><TD>$unexpected_fail</TD></TR>\n"; 
	print $index_out "<TR><TD colspan=3</TD><TD>";
	foreach (@failed_links) {
	  print $index_out "<A HREF=\"$ARGV[$file_count]\/$_.html\">$_</A> ";
	}
	print $index_out "</TD><TD>";
	foreach (@FAILED_links) {
	  print $index_out "<A HREF=\"$ARGV[$file_count]\/$_.html\">$_</A> ";
	}
	$file_count++;
	@failed_links = @FAILED_links = ();
};
print $index_out "</Table></body></html>\n";


print $goprog_in "quit\n";
print "waiting\n" if $verbose > 2;
waitpid $pidg, 0;
print "done waiting\n" if $verbose > 2;



exit;


my $prev_cmd = "";


sub regress_file {
$testfile = shift;

#my $testfile_in  = new FileHandle;		# stdin of test file
#my $testfile_out = new FileHandle;		# stdout of white player

if ($html > 0) {
  $htmlfile_out = new FileHandle;
}

#main bit.
$pidt = open ($testfile_out,"<$testfile");
print "testfile pid: $pidt\n" if $verbose > 1;


if ($html > 0) {
	open($htmlfile_out, "> html/$testfile\.html");
#	open($sgffile_out,  "> html/$testfile\.sgf");
	print $htmlfile_out 
"<HTML><HEAD><TITLE>$testfile regression results - $goprog_name $goprog_version</TITLE>
<BODY>
<H3>$testfile regression results - $goprog_name $goprog_version</H3>
<TABLE border=1>
<tr>
  <TH>number </TH>
  <!--  <TH>game </TH>-->
  <TH>result</TH>
  <TH>expected </TH>
  <TH>got</TH>
  <!-- <TH>to play </TH> -->
  <TH>problem view</TH>
  <TH>gtp</TH>
</TR>
";
}


my $negate;
my $correct_re;
my $ignore;
my $fail;
$passes=0;
$unexpected_pass=0;
$failures=0;
$unexpected_fail=0;
my $result= "";
my $next_cmd = "";
$num = 0;
my $skipping = 0;
while (defined($next_cmd))
{
	my $force_read = 1;
	while ($force_read) {
	  $prev_cmd = $next_cmd;
	  $next_cmd = <$testfile_out>;
	  $force_read = 0;
	  if (defined($next_cmd)) {
	    chop($next_cmd);
	    print "$next_cmd\n" if ($verbose > 1);
	    if (($next_cmd =~ /^\s*#\?\s+\[(\!*)(.*)\]\s*(\*)*(\&)*\s*$/)) {
  	      my $bang = $1;
	      if ($1) { $negate = 1} else {$negate = 0};
	      $correct_re = $2;
	      if ($3) { $fail = 1} else { $fail = 0};
	      if ($4) {$ignore = 1} else {$ignore = 0};

	      my $old_skipping = $skipping;
	      $skipping = $numbers && ($num =~ $numbers);
	      if ($old_skipping) {
	      	  print "$num skipped.\n";
	      	  html_results ($num, "skipped", "&nbsp;", "&nbsp;", 0);
	      } else {
   	        print "TST: $negate - $correct_re - $fail - $ignore\n" if $verbose > 1;
   	      
		    if (!$ignore) {
		      my $match_result = $result =~ $correct_re ;
		      if ($negate) {
			$match_result = ! $match_result;
		      }
		      if ($match_result) {
			$passes++;
			if ($fail) {
			  $unexpected_pass++;
			  print "$num PASSED\n";
			  html_results (  $num, "<B>PASSED</B>", "$bang$correct_re", "$result", 1 );
			} else {
			  if ($verbose) {
			    print "$num passed\n";
			    html_results  ( $num, "passed", "$bang$correct_re", "$result",  1);
			  }
			}
		      } else {
		        $failures++;
			if (!$fail) {
			  $unexpected_fail++;
			  push @FAILED_links, $num;
			  print "$num FAILED: Correct '$bang$correct_re', got '$result'\n";
			  html_results ( $num, "<B>FAILED</B>", "$bang$correct_re", "$result",  1);
			} else {
			  push @failed_links, $num;
			  if ($verbose) {
			    print "$num failed: Correct '$bang$correct_re', got '$result'\n";
			    html_results ($num, "failed", "$bang$correct_re", "$result", 1);
			  }
			}
		      }
		    }
	      }
	      $html_whole_gtp = "";
	    } else {
	    	if (!($next_cmd =~ /^\s*$/)) {
	    	  $html_whole_gtp .= "   " . html_encode($next_cmd) . "<BR>\n";
	    	}
	    }
 	    $next_cmd =~ s/^#.*$//;
  	    $next_cmd =~ s/^\s*$//;
  	    $force_read = $next_cmd eq ""
	  }
	}

   	if (defined($next_cmd)) {
   	  if (!$skipping) {
	    print "CMD: $next_cmd\n" if $verbose > 1;
	    $top_moves = "";
	    if ($next_cmd =~ /gg_genmove\s+([blackwhite])+/) {
	      $next_cmd =~ s/gg_genmove\s+([blackwhite]+)/top_moves_$1/;
	      $top_moves = 1;
	    }
 	    print $goprog_in "$next_cmd\n";
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

  print "Summary: $passes/" . ($passes + $failures) . " passes. $unexpected_pass unexpected $pass_string, "
       . "$unexpected_fail unexpected $fail_string\n";

  if ($html) {
  	print $htmlfile_out "</TABLE></BODY></HTML>\n";
  }

}

sub html_results {
  if ($html) {
	  (  my $number, my $status, my $correct, my $incorrect, my $showboard) = @_;
	  my $htmllink = "&nbsp;";
	  if ($showboard > 0) {
	  	$htmllink = "<A href=\"$testfile/$num.html\">link</A>";
	  	mkdir ("html/$testfile");# die quietly || die "ERROR: couldn't create output dir\n.";
	  	my $brd = new FileHandle;
	  	my $sbrd = new FileHandle;
	  	open ($brd, "> html/$testfile/$num.html") || die "ERROR: couldn't output board\n";
	  	open ($sbrd, "> html/$testfile/$num.sgf") || die "ERROR: couldn't create sgf board\n";
	  	my $brdout = eat_board(0);
	  	my $sgfout;
	  	($brdout, $sgfout) = split(/<HR>/, $brdout);
	  	print $brd "<HTML><HEAD><TITLE>$testfile test $num details.</TITLE></HEAD>\n";
	  	print $brd "<BODY><TABLE border=1>\n";
	  	print $brd "<TR><TD>number:</TD><TD>$number<BR></TD></TR>\n";
	  	print $brd "<TR><TD>status:</TD><TD>$status<BR></TD></TR>\n";
	  	print $brd "<TR><TD>correct:</TD><TD>$correct<BR></TD></TR>\n";
	  	print $brd "<TR><TD>answer:</TD><TD>$incorrect</TD></TR>\n";
	  	print $brd "<TR><TD>gtp:</TD><TD>$html_whole_gtp</TD></TR>\n";
	  	print $brd "</TABLE>\n\n<HR>\n";
	  	print $brd "<A HREF=\"$num.sgf\">sgf board</A><HR>\n";
	  	print $brd $brdout;
	  	print $brd "</BODY></HTML>\n";
	  	my $sgfappend = "C[number: $number\nstatus: $status\ncorrect: $correct\n"
	  			."answer: $incorrect\ngtp: \n$html_whole_gtp\n";
	  	$sgfappend =~ s/]/\\]/mg;
	  	$sgfappend =~ s/<BR>//mg;
	  	$sgfout =~ s/;/;$sgfappend\]/mg;
	  	print $sbrd $sgfout;
	  }
	  print $htmlfile_out "<TR><TD>$number</TD><TD>$status</TD><TD>$correct</TD>" .
	    "<TD>$incorrect</TD><TD>$htmllink</TD><TD>$html_whole_gtp</TD></TR>\n";
#	  print $sgffile_out $sgfout;
  }
}

sub html_encode {
  #print shift;
  my $r = shift;
  $r =~ s/&/&amp;/g;
  $r =~ s/</&lt;/g;
  $r =~ s/>/&gt;/g;
  return $r;
}

sub eat_no_response {
    my $h = shift;

# ignore empty lines
    my $line = "";
    while ($line eq "") {
        $line = <$h>;
        $line =~ s/\s*//mg;
	#chop($line = <$h>) or die "No response!";
    }
}

sub eat_board {
  print $goprog_in "query_boardsize\n";
    my $line = "";
    while ($line eq "") {
	chop($line = <$goprog_out>) or die "No response!";
    }
    <$goprog_out>;
    my $equals;
    ($equals, $boardsize) = split(' ', $line, 2);
    $boardsize = $boardsize + 0;
    my $linesleft = $boardsize + 2;

  my $preboard = "";
 
  print $goprog_in "dragon_data\n";
  my $cur_point = 0;
  my $cur_color = 0;
  my $cur_matcher_status = 0;
  my $cur_dragon_status=0;
  my $cur_owl_status=0;
  my $cur_color_letter=0;
  my $cur_color_sgf = 0;
  my %dragons;
  my $white_letter = chr(ord('z')+1);
  my $black_letter = chr(ord('A')-1);
  my $iline = 1;
  my $no_dragon_data = 0;
  while ($iline) {
        $iline = $_ = <$goprog_out>;
        #print $iline;
        if ($iline =~ /^\?/) {
          $no_dragon_data = 1;
          $iline = $_ = <$goprog_out>;
          last;
        }
 	$iline =~ s/\s*$//mg;
  	if ($iline =~ /^=?\s*([A-Z][0-9][0-9]?):\s*$/ || !$iline) {
  	  if ($cur_point) {
	    if ($cur_color eq "white") {
	      $_ = $white_letter = chr(ord($white_letter)-1);
	      $cur_color_letter = "O";
	      $cur_color_sgf = "W";
	    } elsif ($cur_color eq "black"  || die "invalid color $cur_color") {
	      $_ = $black_letter = chr(ord($black_letter)+1);
	      $cur_color_letter = "X";
	      $cur_color_sgf = "B";
	    }
  	    $dragons{$cur_point} = $_ . ";status=" . $cur_dragon_status . 
  	    				";owl_status=" . $cur_owl_status . 
  	    				";color_letter=" . $cur_color_letter.
  	    				";color_sgf=" . $cur_color_sgf .
  	    				";";
  	    $cur_color = 0;
  	    $cur_matcher_status = 0;
	    $cur_dragon_status=0;
  	    $cur_owl_status=0;
  	    $cur_color_letter=0;
  	    $cur_color_sgf=0;
  	  }
  	  $cur_point = $1;
  	  if (defined($cur_point)) {
  	  	 $preboard .= "POINT:$cur_point\n";
  	  }
  	} elsif ($iline =~ /^color:?\s+([blackwhite]*)\s*$/) {
  	  $cur_color = $1;
  	  $preboard .= "COLOR: $cur_color\n";
  	} elsif ($iline =~ /^matcher_status:?\s+(\S*)\s*$/) {
  	  $cur_matcher_status = $1;
  	} elsif ($iline =~ /^status:?\s+(\S*)\s*$/) {
  	  $cur_dragon_status = $1;
  	} elsif ($iline =~ /^owl_status:?\s+(\S*)\s*$/) {
  	  $cur_owl_status = $1;
  	} else {
  	  $preboard .= "no match: $iline\n";
  	}
    }
  my $owlboard = "";
  my $dragonboard = "";
  my $colorboard = "";
  my $sgfboard = "(;FF[4]GM[1]GN[1]SZ[$boardsize]\n";
          

  if ($no_dragon_data) {
     $owlboard = $dragonboard = $colorboard = "No dragon data available."
  } else {	
        print $goprog_in "dragon_stones\n";
        $iline = 1;
        my %stones;
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


  my %tmarr;
  #my $topmoves = "top moves unavailable";
  #if ($prev_cmd =~ /.*gg_genmove\s+([whiteblack]+)/) {
  #  print $goprog_in "top_moves_$1\n";
  #  my $tm = <$goprog_out>;
  #  <$goprog_out>;
  if ($top_moves) {
    print "$top_moves\n" if $verbose > 1;
    if ($top_moves =~ /^\s*(.*)\s*/) { #i.e. always!
      my $t = $1;
      %tmarr = split(/\s+/,$t);
      #Skip putting topmove labels in SGF board, for now.
      #$sgfboard .= "\n;\nLB";
      foreach (keys(%tmarr)) {
      	my $k = $_;
      	my $coord = GTPtoSGF($k);
        #$sgfboard .= "[$coord:$tmarr{$k}]";
      }
      $sgfboard .= "\n";
    }
  }
  # else {
  #    print "NOTOP:$tm\n";
  #  }
  #}


        
        my $j;
        $owlboard = "  ";
        my $i;
        my $colorboard_letter_row = " <TR>\n  <TD>&nbsp;</TD>\n";
        for ($i = 1; $i <= $boardsize; $i++) {
            my $iA = ord('A') + $i - 1;
            if ($iA >= ord('I')) { $iA++; }
            $iA = chr($iA);
            $owlboard .= " $iA";
            $colorboard_letter_row .= "  <TD align=center valign=center>$iA</TD>\n"; 
        }
        $colorboard_letter_row .= "  <TD>&nbsp;</TD>\n </TR>";
        $owlboard .= "\n";
        $dragonboard = $owlboard;  
        my $letterrow = $owlboard;  
        
        my $img_pix_size = 25;
        $colorboard .= "dragon_status and top_moves\n<TABLE  border=0 cellpadding=0 cellspacing=0>\n"
             . "$colorboard_letter_row\n";
        for ($j = $boardsize; $j > 0; $j--) {
          my $jA = $j;
          if ($j <= 9) {
            $jA .= " ";
          }
          $owlboard .= $jA;
          $dragonboard .=$jA;
          $colorboard .= " <TR>\n  <TD align=center valign=center>&nbsp;$j&nbsp;</TD>\n";
          for ($i = 1; $i <= $boardsize; $i++) {
            my $iA = ord('A') + $i - 1;
            if ($iA >= ord('I')) { $iA++; }
            $iA = chr($iA);
            my $colorboard_imgsrc;
            if ($stones{$iA.$j}) {
            	$_ = $stones{$iA.$j};
            	/(.).*;owl_status=([^;]*);/;
            	my $label = $1;
            	$owlboard .= " <B><FONT color=\"$colors{$2}\">$1</FONT></B>";
            	$_ = $stones{$iA.$j};
            	/(.).*;status=([^;]*);/;
            	my $dragonletter = $1;
            	my $dragoncolor = $colors{$2};
            	$dragonboard .= " <B><FONT color=\"$dragoncolor\">$dragonletter</FONT></B>";
            	/(.).*;color_letter=([^;]*);/;
            	my $bw = $2;
            	if ($bw eq 'X') {$bw = 'black';} else {$bw = 'white';}
            	#print "$bw\n";
            	
            	$colorboard_imgsrc = createPngFile($bw, $img_pix_size, "", $dragonletter, $dragoncolor);
            	
            	$_ = $stones{$iA.$j};
            	my $sgfcoord = chr(ord('a') + $i -1) .  chr(ord('a') + $boardsize  - $j) ;
            	/(.).*;color_sgf=([^;]*);/;
            	$sgfboard .= "A$2\[$sgfcoord]LB[$sgfcoord:$label]\n";
            } else {
            	$owlboard .= " " . ".";
            	$dragonboard .= " " . ".";
            	my $top_label = $tmarr{$iA.$j};
            	if ($top_label) {
            	  $top_label = sprintf "%f", $top_label;
            	  ($top_label = substr($top_label, 0, 3)) =~ s/\.$//;
            	  print "$top_label\n";
            	}
            	$colorboard_imgsrc = createPngFile("", $img_pix_size, "", $top_label, "blue"); #TOP MOVE CHECK GOES HERE!!!!!!!!
            }
            $colorboard .= "  <TD><IMG HEIGHT=$img_pix_size WIDTH=$img_pix_size SRC=\"../images/$colorboard_imgsrc\"></TD>\n";
          }
          $owlboard .= " $jA\n";
          $dragonboard .= " $jA\n";
          $colorboard .= "  <TD align=center valign=center>&nbsp;$j&nbsp;</TD>\n </TR>\n";
        }
        $owlboard .= $letterrow;
        $dragonboard .= $letterrow;
        $colorboard .= "$colorboard_letter_row\n";
        $colorboard .= "</TABLE>\n";
  }

#  my %tmarr;
#  my $topmoves = "top moves unavailable";
#  if ($prev_cmd =~ /.*gg_genmove\s+([whiteblack]+)/) {
#    print $goprog_in "top_moves_$1\n";
#    my $tm = <$goprog_out>;
#    <$goprog_out>;
#    if ($tm =~ /^=\s*(.*)\s*/) {
#      $topmoves = $1;
#      %tmarr = split(/\s+/,$topmoves);
#      $sgfboard .= "\n;\nLB";
#      foreach (keys(%tmarr)) {
#      	my $k = $_;
#      	my $coord = GTPtoSGF($k);
#        #$sgfboard .= "[$coord:$tmarr{$k}]";
#      }
#      $sgfboard .= "\n";
#    } else {
#      print "NOTOP:$tm\n";
#    }
#  }
#

  $sgfboard .= ")";
  $preboard = "$colorboard<BR>\ntopmoves:  $top_moves<BR>\n" . #\ndragon_status:\n$dragonboard\n
              "<PRE>owl_status:\n$owlboard</PRE><P>";
  my $legend ="\nLegend:\n";
  foreach (keys(%colors)) {
    $legend .= "<B><FONT color=\"$colors{$_}\">$colors{$_}=$_</FONT></B>\n";
  }
  
  $preboard .= $legend;
    
  "" . $preboard . "</PRE>" .
  "<HR>" . $sgfboard;
}
  
    
sub GTPtoSGF {
  $_ = shift;
  if (! /([A-Z])([0-9]{1,2})/) {
    return ;
  }
  $_ = ord($1) - ord("A") + 1;
  if ($_ > (ord("I") - ord("A") + 1)) { $_--; }
  chr(ord("a") + $_ - 1) . chr(ord("a") + $boardsize - $2);
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
#    $num = $equals;  
#    $num =~ s/^[=?]//;#ACK - modifying global $num here! FIXME.
# $num is now set in the caller by matching on the request instead of the response.
    $line = <$h>;
    return $move;
}

sub eat_handicap {
    my $h = shift;
    my $sgf_handicap = "AB";
# ignore empty lines, die if process is gone
    my $line = "";
    while ($line eq "") {
	chop($line = <$h>) or die "No response!";
    }
    @vertices = split(" ", $line);
    foreach $vertex (@vertices) {
	if (!($vertex eq "=")) {
	    $vertex = standard_to_sgf($vertex);
	    $sgf_handicap = "$sgf_handicap\[$vertex\]";
	}
    }		
    return "$sgf_handicap;";
}

sub eat_score {
    my $h = shift;
# ignore empty lines, die if process is gone
    my $line = "";
    while ($line eq "") {
	chop($line = <$h>) or die "No response!";
    }
    $line =~ s/\s*$//;
    my ($equals, $result) = split(' ', $line, 2);
    $line = <$h>;
    return $result;
}

sub rename_sgffile {
    my $nogames = int shift(@_);
    $_ = shift(@_);
    s/\.sgf$//;
    return "$_.sgf" if ($nogames == 1);
    return "$_$nogames.sgf";
}


