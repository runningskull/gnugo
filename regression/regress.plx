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



#BEGIN {
#  use CGI::Carp qw(carpout);
#  my $errfile = "C:/temp/web.err";
#  #open (WEBERR, ">$errfile") or die "Couldn't open $errfile.";
#  carpout(STDOUT);
#}
#

use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../interface";

use GoImage::Stone;

use CGI::Carp;
use CGI qw/:standard/;

my $debug=2;

my %colors = ("ALIVE", "green",
	"DEAD", "cyan",
	"CRITICAL", "red",
	"UNKNOWN", "yellow",
	"UNCHECKED", "magenta");

my $query = new CGI;
my ($tstfile, $num, $sortby, $sgf, $reset, $trace, $bycat);

($tstfile, $num) = ($query->query_string() =~ /keywords=(.*)%3A(.*)/);

if (!$tstfile) {
  $tstfile = $query->param("tstfile");
  $num = $query->param("num");
  $sortby = $query->param("sortby");
  $sgf = $query->param("sgf");
  $reset = $query->param("reset");
  $trace = $query->param("trace");
  $bycat = $query->param("bycat");
}

sub sgfFile(%);


print "HTTP/1.0 200 OK\r\n";
print "Content-type: " .
        do {
          my $plain = $trace;
          if ($sgf) { "application/x-go-sgf" }
          elsif ($plain) { "text/plain" }
          else {"text/html"; }
        } . "\r\n\r\n";

if ($tstfile) {
  $tstfile .= '.tst' if $tstfile !~ /.tst$/;
}
if ($tstfile && !($tstfile =~ /^[a-zA-Z0-9_]+\.tst$/)) {
  print "bad test file: $tstfile\n";
  exit;
}

if ($reset) {
  unlink glob("html/*.html");# or die "couldn't delete html files: $!";
  unlink glob("html/*/*.html");# or die "couldn't delete html/* files: $!"; 
  unlink "html/one.perldata";# or die "couldn't delete data file";
  print "Cleaned up!<HR>\n";
}

if ($trace) {
  open (TRACER, "html/$tstfile/$num.trace") or
    do {print "Couldn't find trace file: $!";  exit;};
  while (<TRACER>) {
    print;
  }
  close TRACER;
  exit;
}




my %points;

unless ($tstfile) {
#CASE 1 - main index
  if (!-e "html/index.html") {
    createIndex();
  } else {
    print STDERR "Cached!\n";
  }
  
  if ($bycat) {
    printbycategory();
    exit;
  }
  
  if (-z "html/index.html") {
    print "Yikes - index missing - please reset!";
    exit;
  }
  
  open (TESTFILE, "html/index.html") or do {print "$! ".__LINE__; confess "$!"};
  while (<TESTFILE>) {
    print;
  }
  close TESTFILE;
  exit;
}
 

my %fullHash;
use Data::Dumper;

sub createIndex {
  my %h;
  foreach my $file (glob("html/*.tst/*.xml")) {
    my ($tst, $prob) = $file =~ m@html.(.*).tst.(.*).xml@;
    open (FILE, "$file");
    local $/; undef($/);
    my $content = <FILE>;
    close FILE;
    $h{"$tst:$prob"} = game_parse($content);
    delete $h{"$tst:$prob"}->{gtp_all};
  }
  
  open (FILE, ">html/one.perldata") or confess "can't open";
  print FILE Dumper([\%h]) or confess "couldn't print";
  close FILE or confess "can't close";
  

  #our $VAR1;
  #do "html/one.perldata" or confess "can't do perldata";
  #my %h = %{$VAR1->[0]};
  

  open I, ">html/index.html";

  print I '<HTML><HEAD><TITLE>Regression test summary - </TITLE></HEAD>
 <BODY>
 <H3> Regression test summary - </H3>
 Program: _CMDLINE_TBD_ <BR>
 <A href="?bycat=1">View by category</A><BR>
 <TABLE border=1>
 <TR><TD>file</TD><TD>passed</TD><TD>PASSED</TD><TD>failed</TD><TD>FAILED</TD>
 </TR>';
  
  my @pflist = ("passed", "PASSED", "failed", "FAILED");
  my %totHash;
  @totHash{@pflist} = (0,0,0,0);

  sub byfilebynum {
    my ($fileA,$numA) = $a =~ /(.*):(.*)/;
    my ($fileB,$numB) = $b =~ /(.*):(.*)/;
    $fileA cmp $fileB or $numA <=> $numB;
  }    

  my $curfile = "";
  my %subTotHash;
  foreach my $k1 (sort byfilebynum keys %h) { #$k1 = filename
    if ($k1 !~ /^$curfile:/) {
      if ($curfile ne "") {
        #New file = print old totals
        print I qq@<TR>\n <TD><A href="?tstfile=$curfile&sortby=result">$curfile</A></TD>\n@;
        foreach my $k2 (@pflist) {
          my $c = @{$subTotHash{$k2}};  #i.e. length of array.
          if ($k2 !~ /passed/ and $c) {
            print I " <TD>$c:<BR>\n";
            foreach (sort {$a<=>$b} @{$subTotHash{$k2}}) {
              print I qq@  <A HREF="?$curfile:$_">$_</A>\n@;
            }
            print I " </TD>\n";
          } else {
            print I " <TD>$c</TD>\n";
          }
        }
        print I qq@</TR>@;
      }
      #prepare for next file.
      ($curfile) = $k1 =~ /(.*):/;
      @subTotHash{@pflist} = ([],[],[],[]);
    }
    push @{$subTotHash{$h{$k1}{status}}}, $h{$k1}{num};
  }

      #direct copy from above - don't miss last time through - HACK!  
        if ($curfile ne "") {
        #New file = print old totals
        print I qq@<TR>\n <TD><A href="?tstfile=$curfile&sortby=result">$curfile</A></TD>\n@;
    foreach my $k2 (@pflist) {
          my $c = @{$subTotHash{$k2}};  #i.e. length of array.
          if ($k2 !~ /passed/ and $c) {
        print I " <TD>$c:<BR>\n";
            foreach (sort {$a<=>$b} @{$subTotHash{$k2}}) {
              print I qq@  <A HREF="?$curfile:$_">$_</A>\n@;
        }
        print I " </TD>\n";
      } else {
        print I " <TD>$c</TD>\n";
      }
    }
        print I qq@</TR>@;
  }

  
  print I "<TR>\n <TD><B>Total</B></TD>\n";
  foreach (@pflist) {
    print I " <TD>$totHash{$_}</TD>\n";
  }
  print I "</TR>\n";
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
 
my @counters = qw/life_node owl_node reading_node trymove/;

if ($num) {
#CASE 2 - problem detail.
  open (FILE, "html/$tstfile/$num.xml");
  local $/; undef($/);
  my $content = <FILE>;
  close FILE;
  my %attribs = %{game_parse($content)};
  
  if ($sgf) {
    sgfFile(%attribs);
    exit;
  }
  
  print qq@<HTML><HEAD><TITLE>$tstfile test $attribs{"num"} details.</TITLE></HEAD>\n@;
  print qq@<BODY><TABLE border=1>\n@;
  print qq@
 <TR>
   <TD>number:</TD><TD>$attribs{"num"}</TD><TD>&nbsp;</TD>
   <TD>cputime:</TD><TD>$attribs{"cputime"}</TD>
 </TR><TR>
   <TD>status:</TD><TD>$attribs{"status"}</TD><TD>&nbsp;</TD>
   <TD>$counters[0]:</TD><TD>$attribs{"$counters[0]_counter"}</TD>
 <TR>
   <TD>correct:</TD><TD>$attribs{"correct"}</TD><TD>&nbsp;</TD>
   <TD>$counters[1]:</TD><TD>$attribs{"$counters[1]_counter"}</TD>
 <TR>
   <TD>answer:</TD><TD>$attribs{"answer"}</TD><TD>&nbsp;</TD>
    <TD>$counters[2]:</TD><TD>$attribs{"$counters[2]_counter"}</TD>
 <TR>
   <TD>gtp:</TD><TD>$attribs{"gtp_command"}</TD><TD>&nbsp;</TD>
   <TD>$counters[3]:</TD><TD>$attribs{"$counters[3]_counter"}</TD>
 </TR><TR><TD>category:</TD><TD>$attribs{"category"}</TD>
 </TR><TR><TD>severity:</TD><TD>$attribs{"severity"}</TD>
 </TR><TR><TD>description:</TD><TD>$attribs{"description"}</TD>
 </TR>
</TABLE>\n\n@;
  print qq@<HR>\n\n@;
  print qq@
<TABLE border=0>
<TR><TD><A HREF="?tstfile=$tstfile&num=$num&sgf=1">SGF File</A>
</TD><TD>&nbsp;&nbsp;&nbsp;<A HREF="?tstfile=$tstfile&num=$num&trace=1">Trace File</A>
</TD></TR></TABLE>
@;

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
  
  my $gtpall = $attribs{gtp_all};
  $gtpall  =~ s/<BR>//mg;
  $gtpall  =~ s/\s+$//mg;
  $gtpall  =~ m@loadsgf\s+ ((?:\w|[-.\\/])+)  \s* (\d*) @xm 
    or $gtpall =~m/(.*?)/;  #Problems!!!!  
  
  my $cmdline = "gq -l $1 " . ($2 ? "-L $2 " : "");
  if ($gtpall =~ m@ .* (owl_(?:attack|defend)*) \s* ([A-Z]\d{1,2}) \s* $ @x) {
    $cmdline .= "--decide-dragon $2 -o x.sgf" ;
  } elsif ($gtpall =~ m@ .* (gg_genmove\s+[whiteblack]*)  \s* $@x) {
    $cmdline .= "-t";
  } elsif ($gtpall =~ m@ .* (attack|defend) \s* ([A-Z]\d{1,2}) \s* $ @x) {
    $cmdline .= "--decide-string $2 -o x.sgf";
  } else {
    $cmdline .= " <BR> (directive unrecognized)";
  }
  print qq@<HR>\n\n@;
  print qq@<TABLE border=1>\n@;
  print qq@ <TR><TD>CMD Line Hint:</TD><TD>$cmdline</TD></TR>\n@;
  print qq@ <TR><TD>Full GTP:</TD><TD>$attribs{gtp_all}</TD></TR>\n</TABLE>\n@;
  
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
  <TH><A HREF="?tstfile=$tstfile&sortby=filepos">line</A></TH>
  <TH><A href="?tstfile=$tstfile&sortby=num">number</A></TH>
  <TH><A href="?tstfile=$tstfile&sortby=result">result</A></TH>
  <TH>expected </TH>
  <TH>got</TH>
  <TH>gtp</TH>
  <TH><A href="?tstfile=$tstfile&sortby=cputime">cputime</A></TH>
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
    my $cputime = $1
      if $content =~ m@<TIME.*?CPU=((\d|\.)*)@s;
    $cputime =~ s/0*$//;
    $files{$curfile} = {
      gtp_all => $gtp_all,
      gtp => $gtp,
      filepos => $filepos,
      num => $num,
      expected => $expected,
      got => $got,
      result => $result,
      cputime => $cputime
    }
  }
 
  sub byfilepos {  $files{$a}{"filepos"} <=> $files{$b}{"filepos"};   }
  sub bynum  {  $files{$a}{"num"} <=> $files{$b}{"num"};   }
  sub byresult {
    fptonum($files{$a}{"result"}) <=> fptonum($files{$b}{"result"})
    or byfilepos();
  }
  sub bycputime {
    $files{$b}{cputime} <=> $files{$a}{cputime}
    or byfilepos();
  }
  
  sub filesby {
    $_ = shift;
    return byfilepos if /filepos/i;
    return bynum if /num/i;
    return byresult if /result/i;
    return bycputime if /cputime/i;
    $files{$a}{$_} <=> $files{$b}{$_};   
  }
  
  foreach my $curfile (sort {filesby($sortby)} keys %files) {
    my %h = %{$files{$curfile}};
    my $numURL = qq@<A HREF="?tstfile=$tstfile&num=$h{num}">$h{num}</A>@;
    my $r = $h{result};
    $r =~ s@^([A-Z]*)$@<B>$1</B>@;
    print TF "<TR><TD>$h{filepos}</TD><TD>$numURL</TD><TD>$r</TD><TD>$h{expected}</TD>"
        . "<TD>$h{got}</TD><TD>$h{gtp}</TD><TD>$h{cputime}</TD></TR>\n";
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
  my %attribs;
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
  $attribs{"cputime"} = $1
    if $content =~ m@<TIME.*?CPU=((\d|\.)*)@s;
  $attribs{"boardsize"} = $1
    if $content =~ m@<BOARD[^>]*size=(\d+)@s;
  foreach (@counters) {
    $attribs{$_."_counter"} = $1
      if $content =~ m@<COUNTER[^>]*$_="?(\d+)@s;
  }
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
  return \%attribs;
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


sub sgfFile(%) {
  my %attribs = shift;
  my $boardsize = $attribs{"boardsize"};  #need to add to export.

  my $ret="";
  $ret .= "(;\nFF[4]GM[1]SZ[$boardsize]\nAP[regress.plx]\n";
   
  for (my $j = $boardsize; $j > 0; $j--) {
    my $jA = $j;
    $jA .= " " if ($j <= 9);
    for (my $i = 1; $i <= $boardsize; $i++) {
      my $iA = ord('A') + $i - 1;
      if ($iA >= ord('I')) { $iA++; }
      $iA = chr($iA);  
      my $coord = $iA.$j;
      my $bw = pval($coord, "stone");
      
      if ($bw eq "black") {
        $ret .= "AB\[" . GTPtoSGF($coord, $boardsize) . "]";
      } elsif ($bw eq "white") {
        $ret .= "AW\[" . GTPtoSGF($coord, $boardsize) . "]";
      }
    }
  }
  $ret.=")";
  
  $ret =~ s/((A[BW]\[..\]){12})/$1\n/g;
  
  print $ret;
}


sub GTPtoSGF {
  local $_ = shift;
  my $boardsize = shift;
  if (! /([A-Z])([0-9]{1,2})/) {
    return ;
  }
  $_ = ord($1) - ord("A") + 1;
  if ($_ > (ord("I") - ord("A") + 1)) { $_--; }
  chr(ord("a") + $_ - 1) . chr(ord("a") + $boardsize - $2);
}





sub printbycategory {

    our $VAR1;
    do "html/one.perldata" or confess "can't do perldata";
    my %hash = %{$VAR1->[0]};
    
    my %fails;
    
    foreach my $k (keys %hash) {
      my $status = $hash{$k}{status};
      $fails{$k} = $hash{$k} if $status =~ /failed/i;
    }
    
    sub bycat {
      defined $fails{$a}{file}
        or do {
          print '$a:'."$a\n";
          confess "missing file";
        };
      
      my $ca = $fails{$a}{category};
      my $cb = $fails{$b}{category};
      defined $ca or $ca = 0;
      defined $cb or $cb = 0;
        
      if ($ca ne "" and $cb eq "") { return -1; }
      if ($ca eq "" and $cb ne "") { return 1; }
      
      $ca ne "" or $ca = $fails{$a}{file};
      $cb ne "" or $cb = $fails{$b}{file};
      
      uc ($ca) cmp uc($cb) 
      or
        do {
          my $sa = $fails{$a}{severity};
          my $sb = $fails{$b}{severity};
          #print '$sa <=> $sb :' . "$sa <=> $sb  ($ca, $cb)" , "\n"
          #  if defined $sa and defined $sb and ($sa ne "") and ($sb ne "");
          defined $sa or $sa = 5;
          defined $sb or $sb = 5;
          if ($sa eq "") {$sa = 5};
          if ($sb eq "") {$sb = 5};
          -($sa <=> $sb); 
        }
      or
        do {
          my $fa = $fails{$a}{file};
          my $fb = $fails{$b}{file};
          $fa cmp $fb;
        }
      or
        do {
          my $na = $fails{$a}{num};
          my $nb = $fails{$b}{num};
          $na <=> $nb;
        }
    }
    
    sub getcat(%) {
      my %h = %{shift()};
      $h{category} or $h{file};
    }
    sub getsev(%) {
      my %h = %{shift()};
      my $s = $h{severity};
      defined $s or do {return 5};
      $s ne "" or do {return 5};
      no warnings qw/numeric/;
      $s+0;
    }
    
    print "<HTML><HEAD><TITLE>Failures by category - GNU Go</TITLE></HEAD>\n";
    print "<BODY><H4>Failures by category</H4>";
    print qq@<A href="?">main index</A>@;
    
    
    print "<TABLE border=1>";
    print "<TR><TD><B>Category</B></TD><TD><B>Severity</B></TD><TD><B>Problem</B></TD>\n";
    my $cat = "";
    my $sev = "";
    foreach my $k (sort bycat keys %fails) {
      if (uc(getcat($fails{$k})) ne $cat) {
        $cat = uc(getcat($fails{$k}));
        print "</TD></TR>\n";
        print "<TR><TD>$cat</TD>\n";
        $sev = "";
      }
      if (($sev eq "") or $sev != getsev($fails{$k})) {
        print "</TD></TR>\n<TR><TD>&nbsp;</TD>" if ($sev ne "");
        $sev = getsev($fails{$k});
        print "<TD>$sev</TD><TD>\n";
      }
      print qq@<A href="?$k">$k</A>&nbsp&nbsp</A>\n@;
    }
    print "</TABLE>\n";
    print "</body></html>\n";
    
}
