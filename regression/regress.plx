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
# Here is a perlscript regress.plx. 
#
# It parses the XML files created by regress.pl and generates HTML.
# It is designed to be run as a CGI script.


use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../interface";

use GoImage::Stone;

use CGI qw/:standard/;


print "Content-type: text/html\n\n";

my %colors = ("ALIVE", "green",
	"DEAD", "cyan",
	"CRITICAL", "red",
	"UNKNOWN", "yellow",
	"UNCHECKED", "magenta");

my $query = new CGI;

my $tstfile = $query->param("tstfile");
if ($tstfile && !($tstfile =~ /^[a-zA-Z0-9_]+\.tst$/)) {
  print "bad test file: $tstfile\n";
}

my $num = $query->param("num");
my $sortby = $query->param("sortby");

my $reset = $query->param("reset");

if ($reset) {
  unlink glob("html/*.html");
  unlink glob("html/*/*.html");
  print "Cleaned up!<HR>\n";
}

my %points;
my %attribs;



unless ($tstfile) {
#CASE 1 - main index
  if (!-e "html/index.html") {
    createIndex();
  } else {
    print STDERR "Cached!\n";
  }
  open (TESTFILE, "html/index.html") or (print "$! ".__LINE__, die);
  while (<TESTFILE>) {
    print;
  }
  close TESTFILE;
  exit;
}
 
sub createIndex {
  open I, ">html/index.html";
  my %hha;
  foreach my $file (glob("html/*.tst/*.xml")) {
    my ($tst, $prob) = $file =~ m@html.(.*.tst).(.*).xml@;
    open F, $file or print "Couldn't open file: $file; $_", die;
    while (<F>){
      if (m@.*status="([passedfailed]+?)"@i) {
        push @{$hha{$tst}{$1}}, $prob;
        last;
      }
    }
    close F;
  }
  
  print I '<HTML><HEAD><TITLE>Regression test summary - _VERSION-TBD_</TITLE></HEAD>
 <BODY>
 <H3> Regression test summary - _VERSION-TBD_</H3>
 Program: _CMDLINE_TBD_ <BR>
 <TABLE border=1>
 <TR><TD>file</TD><TD>passed</TD><TD>PASSED</TD><TD>failed</TD><TD>FAILED</TD>
 </TR>';
  


  foreach my $k1 (sort keys %hha) { #$k1 = filename
    print I qq@<TR><TD><A href="regress.plx?tstfile=$k1&sortby=result">$k1</A></TD>\n@;
    foreach my $k2 ("passed", "PASSED", "failed", "FAILED") {
      my $c = 0;
      $c = @{$hha{$k1}{$k2}} if $hha{$k1}{$k2};
      if (!($k2 =~ /passed/) and $c) {
        print I "<TD>$c:<BR>\n";
        foreach (sort {$a<=>$b} @{$hha{$k1}{$k2}}) {
          print I qq@<A HREF="regress.plx?tstfile=$k1&num=$_">$_</A>\n@;
        }
        print I "</TD>\n";
      } else {
        print I "<TD>$c</TD>\n";
      }
      #print I "$k1: $k2: ". (join ";", @{$hha{$k1}{$k2}}) . "\n";
    }
  }
  print I " </TABLE></BODY></HTML>\n";
  close I;
} 

sub bypPfF {
  pPfFtonum($a) <=> pPfFtonum($b);
}

sub pPfFtonum {
  $_ = shift;
  s/FAILED/4/;  s/failed/3/; s/PASSED/2/; s/passed/1/;
  $_;
}

sub fptonum {
  $_ = shift;
  s/FAILED/1/;  s/failed/3/; s/PASSED/2/; s/passed/4/; s/<B>//; s@</B>@@;
  $_;
}
 

if ($num) {
#CASE 2 - problem detail.
  open (FILE, "html/$tstfile/$num.xml");
  local $/; undef($/);
  my $content = <FILE>;
  close FILE;
  game_parse($content);
  
  print qq@<HTML><HEAD><TITLE>$tstfile test $attribs{"num"} details.</TITLE></HEAD>\n@;
  print qq@<BODY><TABLE border=1>\n@;
  print qq@<TR><TD>number:</TD><TD>$attribs{"num"}</TD></TR>\n@;
  print qq@<TR><TD>status:</TD><TD>$attribs{"status"}</TD></TR>\n@;
  print qq@<TR><TD>correct:</TD><TD>$attribs{"correct"}</TD></TR>\n@;
  print qq@<TR><TD>answer:</TD><TD>$attribs{"answer"}</TD></TR>\n@;
  print qq@<TR><TD>gtp:</TD><TD>$attribs{"gtp_command"}</TD></TR>\n@;
  print qq@<TR><TD>description:</TD><TD>$attribs{"description"}</TD></TR>\n@;
  print qq@<TR><TD>category:</TD><TD>$attribs{"category"}</TD></TR>\n@;
  print qq@<TR><TD>severity:</TD><TD>$attribs{"severity"}</TD></TR>\n@;
  print qq@</TABLE>\n\n@;
  print qq@<HR>\n\n@;

  my $boardsize = $attribs{"boardsize"};  #need to add to export.

  my $colorboard;

  $colorboard .= "<TABLE border=0 cellpadding=0 cellspacing=0>\n"
             . colorboard_letter_row($boardsize). "\n";
    
  for (my $j = $boardsize; $j > 0; $j--) {
    my $jA = $j;
    $jA .= " " if ($j <= 9);
    $colorboard .= " <TR>\n  <TD align=center valign=center>&nbsp;$j&nbsp;</TD>\n";
    for (my $i = 1; $i <= $boardsize; $i++) {
      my $iA = ord('A') + $i - 1;
      if ($iA >= ord('I')) { $iA++; }
      $iA = chr($iA);  
      my $coord = $iA.$j;
      my $bw = pval($coord, "stone");
      my $img_pix_size = 25;
      my $dragonletter = pval($coord, "dragon_letter");
      my $dragoncolor = $colors{pval($coord, "dragon_status")};
      
      my ($markcolor, $known, $try) = ("", pval($coord, "known"), pval($coord, "try"));
      $markcolor = "magenta" if ($known and $known eq "wrong");
      $markcolor = "green"  if ($known and $known eq "right");
      $markcolor = "cyan" if ($try and $try eq "right");
      $markcolor = "red" if ($try and $try eq "wrong");
      
      my $question = pval($coord, "question");
      if ($question) {
        $dragonletter .= "*";
        $dragoncolor = "blue" unless $dragoncolor;
      }

      my $colorboard_imgsrc = createPngFile($bw, $img_pix_size, "", $dragonletter, $dragoncolor, $markcolor);
      $colorboard .= "  <TD><IMG HEIGHT=$img_pix_size WIDTH=$img_pix_size SRC=\"html/images/$colorboard_imgsrc\"></TD>\n";
    }
    $colorboard .= "  <TD align=center valign=center>&nbsp;$j&nbsp;</TD>\n </TR>\n";
  }
  $colorboard .= colorboard_letter_row($boardsize);
  $colorboard .= "\n</TABLE>\n";

  print $colorboard;
  print qq@<HR>\n\n@;
  print qq@<TABLE border=1><TR><TD>Full GTP:</TD><TD>$attribs{gtp_all}</TD></TR></TABLE>@;
  print qq@<HR>\nSGF board not generated - does anybody care?@;
  
  print "\n\n</HTML>";
 # print %attribs;
  
} else {
#CASE 3 - test file summary.
#  if (!-e "html/$tstfile/index.html") {
    summarizeTestFile();
#  } else {
#    print "Cached:<HR>";
#  }
#  open (TESTFILE, "html/$tstfile/index.html") or (print "$! ".__LINE__, die);
#  while (<TESTFILE>) {
#    print;
#  }
#  close TESTFILE;
}


my %files;
sub summarizeTestFile {

  unless ($sortby) { $sortby = "filepos"; }
  
 # open (TF, "> html/$tstfile/index.html")
 #   or print "couldn't open for output; $!\n", die;
 *TF = *STDOUT;
  
  print TF "<HTML><HEAD><TITLE>$tstfile regression results - _VERSION_</TITLE>\n";
  print TF "<BODY>\n";
  print TF "<H3>$tstfile regression results - _VERSION_</H3>\n";
  print TF qq@<TABLE border=1>
<tr>
  <TH><A HREF="regress.plx?tstfile=$tstfile&sortby=filepos">line</A></TH>
  <TH><A href="regress.plx?tstfile=$tstfile&sortby=num">number</A></TH>
  <TH><A href="regress.plx?tstfile=$tstfile&sortby=result">result</A></TH>
  <TH>expected </TH>
  <TH>got</TH>
  <TH>gtp</TH>
</TR>\n@;

  my @files = glob("html/$tstfile/*.xml");
  foreach my $curfile (@files) {
    $curfile =~ s/html.$tstfile.(.*xml)/$1/;
    local $/;
    undef($/);
    open(FILE, "html/$tstfile/$curfile");
    my $content = <FILE>;
    close FILE;
    my $gtp_all = $1
      if $content =~ m@<GTP_ALL>(.*?)</GTP_ALL>@s;
    my $gtp = escapeHTML($1)
      if $content =~ m@<GTP_COMMAND>(.*?)</GTP_COMMAND>@s;
    my $result = $1 
      if $content =~ m@<GOPROB.*?status="(.*?)"@s; 
    my $num = $1
      if $content =~ m@<GOPROB.*?number=(\d*)@s;
    my $filepos = $1
      if $content =~ m@<GOPROB.*?filepos=(\d*)@s;
    my $expected = $1
      if $content =~ m@<CORRECT>(.*?)</CORRECT>@s;
    my $got = $1
      if $content =~ m@<ANSWER>(.*?)</ANSWER>@s;
    $files{$curfile} = {
      gtp_all => $gtp_all,
      gtp => $gtp,
      filepos => $filepos,
      num => $num,
      expected => $expected,
      got => $got,
      result => $result
    }
  }
 
  sub byfilepos {  $files{$a}{"filepos"} <=> $files{$b}{"filepos"};   }
  sub bynum  {  $files{$a}{"num"} <=> $files{$b}{"num"};   }
  sub byresult {
    fptonum($files{$a}{"result"}) <=> fptonum($files{$b}{"result"})
    or byfilepos();
  }
  
  sub filesby {
    $_ = shift;
    return byfilepos if /filepos/;
    return bynum if /num/;
    return byresult if /result/;
    $files{$a}{$_} <=> $files{$b}{$_};   
  }
  
  foreach my $curfile (sort {filesby($sortby)} keys %files) {
    my %h = %{$files{$curfile}};
    my $numURL = qq@<A HREF="regress.plx?tstfile=$tstfile&num=$h{num}">$h{num}</A>@;
    my $r = $h{result};
    $r =~ s@^([A-Z]*)$@<B>$1</B>@;
    print TF "<TR><TD>$h{filepos}</TD><TD>$numURL</TD><TD>$r</TD><TD>$h{expected}</TD>"
        . "<TD>$h{got}</TD><TD>$h{gtp}</TD></TR>\n";
  }

  #close TF;
}



sub pval {
  my ($coord, $attrib) = @_;
  if ($points{$coord}) {
    $points{$coord} =~ m@$attrib="(.*?)"@;
    if ($1) {
      return $1;
    } else {
      return "";
    }
  } else {
    return "";
  }
}



sub game_parse {
  my $content = shift;
  $attribs{"num"} = $1
    if $content =~ m@<GOPROB.*?number=(\d*)@s;
  $attribs{"file"} = $1
    if $content =~ m@<GOPROB.*?file="(.*?)"@s;
  $attribs{"status"} = $1
    if $content =~ m@<GOPROB.*?status="(.*?)"@s; 
  $attribs{"correct"} = $1
    if $content =~ m@<CORRECT>(.*?)</CORRECT>@s;
  $attribs{"answer"} = $1
    if $content =~ m@<ANSWER>(.*?)</ANSWER>@s;
  $attribs{"gtp_all"} = $1
    if $content =~ m@<GTP_ALL>(.*?)</GTP_ALL>@s;
  $attribs{"description"} = $1
    if $content =~ m@<DESCRIPTION>(.*?)</DESCRIPTION>@s;
  $attribs{"category"} = $1
    if $content =~ m@<CATEGORY>(.*?)</CATEGORY>@s;
  $attribs{"severity"} = $1
    if $content =~ m@<SEVERITY>(.*?)</SEVERITY>@s;
  $attribs{"gtp_command"} = $1
    if $content =~ m@<GTP_COMMAND>(.*?)</GTP_COMMAND>@s;
  $attribs{"boardsize"} = $1
    if $content =~ m@<BOARD[^>]*size=(\d+)@s;
  $content =~ s@.*?<POINT@<POINT@s;
  while ($content =~ s@<POINT(.*?)></POINT>@@s) {
    my $pattr = $1;
    if ($pattr =~ m@coord="(.*?)"@s) {
      $points{$1} = $pattr;
    } else {
      print "<P>MISSING coord: " . encodeHTML($content) . "<P>" . 
          encodeHTML($pattr);
      die;
    }
  }
}
    
    

sub colorboard_letter_row {
  my $boardsize = shift;
  my $ret = " <TR>\n  <TD>&nbsp;</TD>\n";
  for (my $i = 1; $i <= $boardsize; $i++) {
      my $iA = ord('A') + $i - 1;
      if ($iA >= ord('I')) { $iA++; }
      $iA = chr($iA);
      $ret .= "  <TD align=center valign=center>$iA</TD>\n"; 
  }
  $ret .= "  <TD>&nbsp;</TD>\n </TR>";
}



#sub GTPtoSGF {
#  $_ = shift;
#  if (! /([A-Z])([0-9]{1,2})/) {
#    return ;
#  }
#  $_ = ord($1) - ord("A") + 1;
#  if ($_ > (ord("I") - ord("A") + 1)) { $_--; }
#  chr(ord("a") + $_ - 1) . chr(ord("a") + $boardsize - $2);
#}
#
#