#!/usr/bin/perl

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU Go, a Go program.            #
#                                                                   #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/     #
# for more information.                                             #
#                                                                   #
# Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007,   #
# 2008, 2009 and 2010 by the Free Software Foundation.              #
#                                                                   #
# This program is free software; you can redistribute it and/or     #
# modify it under the terms of the GNU General Public License       #
# as published by the Free Software Foundation - version 3          #
# or (at your option) any later version.                            #
#                                                                   #
# This program is distributed in the hope that it will be           #
# useful, but WITHOUT ANY WARRANTY; without even the implied        #
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           #
# PURPOSE.  See the GNU General Public License in file COPYING      #
# for more details.                                                 #
#                                                                   #
# You should have received a copy of the GNU General Public         #
# License along with this program; if not, write to the Free        #
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor,       #
# Boston, MA 02111, USA.                                            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
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

use CGI qw/:standard/;
use CGI::Carp 'fatalsToBrowser';

use FindBin;
use lib "$FindBin::Bin/../interface";

use GoImage::Stone;

use HTML::Entities ;#qw/encode_entity/;
  

#set $name to whatever this script is called in the URL.
#eg, if you access it from http://example.com/regress/
#then set $name = ""

my $name = "regress.plx";

my $debug=2;

my %colors = ("ALIVE", "green",
	"DEAD", "cyan",
	"CRITICAL", "red",
	"UNKNOWN", "yellow",
	"UNCHECKED", "magenta");

my $query = new CGI;
my ($tstfile, $num, $sortby, $sgf, $reset, $trace, $bycat,
     $unexpected, $slow, $special, $move, $small);

($tstfile, $num) = ($query->query_string() =~ /keywords=(.*)%3A(.*)/);

if (!$tstfile) {
  $tstfile = $query->param("tstfile");
  $num = $query->param("num");
  $sortby = $query->param("sortby");
  $sgf = $query->param("sgf");
  $reset = $query->param("reset");
  $trace = $query->param("trace");
  $bycat = $query->param("bycat");
  $unexpected = $query->param("unexpected");
  $slow = $query->param("slow");
  $special = $query->param("special");
  $move = $query->param("move");
  $small = $query->param("small");
}

sub sgfFile(%);


#print "HTTP/1.0 200 OK\r\n";
print "Content-type: " .
        do {
          my $plain = $trace;
          if ($sgf) { "application/x-go-sgf" }
          elsif ($plain) { "text/plain" }
          else {"text/html"; }
        } . "\r\n\r\n";

if ($tstfile) {
  $tstfile = $1 if $tstfile =~ /(.*)\.tst$/;
}
if ($tstfile && !($tstfile =~ /^[a-zA-Z0-9_]+$/)) {
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
  open (TRACER, "html/$tstfile.tst/$num.trace") or
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
  
  if ($unexpected) {
    printunexpected();
    exit;
  }
  
  if ($slow) {
    printslow();
    exit;
  }
  
  if ($special) {
    printspecial();
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
#use Data::Dumper;

sub insinglequote {
  my $s = shift;
  $s =~ s@\\@\\\\@g;
  $s =~ s@'@\\'@g;
  return "'$s'";
}

sub FastDump {
  my ($h) = @_;

  open (FILE, ">html/one.perldata.new") or confess "can't open";
  print  FILE "\$VAR1 = [\n {\n";
  


  #print FILE Dumper([\%h]) or confess "couldn't print";

  foreach my $k1 (sort keys %{$h}) {
    print FILE "  '$k1' =>\n   {\n";
    foreach my $k2 (sort keys %{%{$h}->{$k1}}) {
      print FILE "     '$k2' => " . insinglequote(%{$h}->{$k1}->{$k2}) . ",\n";
    }
    print FILE "   },\n";
  }
  
  print FILE "  }\n ];";

  close FILE or confess "can't close";
}

sub createIndex {
  my %h;
  foreach my $file (glob("html/*.tst/*.xml")) {
    my ($tst, $prob) = $file =~ m@html.(.*).tst.(.*).xml@;
    open (FILE, "$file");
    local $/; undef($/);
    my $content = <FILE>;
    close FILE;
    $h{"$tst:$prob"} = game_parse($content, 0);
    delete $h{"$tst:$prob"}->{gtp_all};
  }
  
  FastDump(\%h);
  
  #print "DONE!\n";
  #return;  

  #our $VAR1;
  #do "html/one.perldata" or confess "can't do perldata";
  #my %h = %{$VAR1->[0]};


  open I, ">html/index.html";

  print I qq@<HTML>
 <HEAD>
  <TITLE>Regression test summary - </TITLE>
  <META NAME="ROBOTS" CONTENT="NOFOLLOW">
 </HEAD>
 <BODY>
 <H3> Regression test summary - </H3>
 Program: _CMDLINE_TBD_ <BR>
 <A href="$name?bycat=1">View by category</A><BR>
 <A href="$name?unexpected=1">View unexpected results</A><BR>
 <TABLE border=1>
 <TR><TD>file</TD><TD>passed</TD><TD>PASSED</TD><TD>failed</TD><TD>FAILED</TD>
 </TR>@;
  
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
        print I qq@<TR>\n <TD><A href="$name?tstfile=$curfile&sortby=result">$curfile</A></TD>\n@;
        foreach my $k2 (@pflist) {
          my $c = @{$subTotHash{$k2}};  #i.e. length of array.
          $totHash{$k2} += $c;
          if ($k2 !~ /passed/ and $c) {
            print I " <TD>$c:<BR>\n";
            foreach (sort {$a<=>$b} @{$subTotHash{$k2}}) {
              print I qq@  <A href="$name?$curfile:$_">$_</A>\n@;
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
        print I qq@<TR>\n <TD><A href="$name?tstfile=$curfile&sortby=result">$curfile</A></TD>\n@;
        foreach my $k2 (@pflist) {
          my $c = @{$subTotHash{$k2}};  #i.e. length of array.
          $totHash{$k2} += $c;
          if ($k2 !~ /passed/ and $c) {
            print I " <TD>$c:<BR>\n";
            foreach (sort {$a<=>$b} @{$subTotHash{$k2}}) {
              print I qq@  <A href="$name?$curfile:$_">$_</A>\n@;
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
 
my @counters = qw/connection_node owl_node reading_node trymove/;

if ($move) {
#CASE 2a - move detail - extract interesting info from trace file.
  if (!$num) {
    print "Must provide num if providing move.<BR>";
    exit;
  }
  
  print qq@<HTML>
  <HEAD>
  <TITLE>$tstfile:$num move $move</TITLE>
  <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
  </HEAD><BODY>\n@;
  
  open (FILE, "html/$tstfile.tst/$num.trace") or die "couldn't open trace file $tstfile, $num: $!.";
  #local $/; undef($/);
  #my $content = <FILE>;
  #close FILE;
  
  my $blank=1;
  my $inpattern=0;
  $move = uc($move);
  print "<PRE>\n";
  while (<FILE>) {
    if (/^$move[^0-9]/ || 
        /[^A-Za-z0-9]$move[^0-9]/ || 
        $inpattern && /^\.\.\./) {
      print encode_entities($_);
      $blank=0;
      $inpattern ||= /^pattern.*at $move/;
    } else {
      print "\n" unless $blank;
      $blank++;
      $inpattern=0;
    }
  }
  print "</PRE></BODY></HTML>\n";
  exit;
}
  

if ($num) {
#CASE 2 - problem detail.

  if ($sgf && -e "html/$tstfile.tst/$num.sgf") {
    open (SGFFILE, "html/$tstfile.tst/$num.sgf") or confess "couldn't open file";
    while (<SGFFILE>) {
      print;
    }
    close SGFFILE;
    exit;
  }

  open (FILE, "html/$tstfile.tst/$num.xml") or die "couldn't open xml file\n";
  local $/; undef($/);
  my $content = <FILE>;
  close FILE;
  my %attribs = %{game_parse($content, 1)};

  if ($sgf) {
    foreach (sort keys %attribs) {
    #  print "$_: $attribs{$_}\n";
    }
    sgfFile(%attribs);
    exit;
  }
  
  print qq@<HTML><HEAD>
    <TITLE>$tstfile:$num details.</TITLE>
    <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
    </HEAD>\n@;
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
<TR><TD><A href="$name?tstfile=$tstfile&num=$num&sgf=1">SGF File</A>
</TD><TD>&nbsp;&nbsp;&nbsp;<A href="$name?tstfile=$tstfile&num=$num&trace=1" target=tracefile>Trace File</A>
</TD></TR></TABLE>
@;

  print qq@<TABLE><TR><TD> dragon_status | owl_status\n@;

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
      my $owlcolor = $colors{pval($coord, "owl_status")};
      my $owlletter = $dragonletter;
      my $alt = "";
      
      my ($markcolor, $known, $try) = ("", pval($coord, "known"), pval($coord, "try"));
      $markcolor = "magenta" if ($known and $known eq "wrong");
      $markcolor = "green"  if ($known and $known eq "right");
      $markcolor = "cyan" if ($try and $try eq "right");
      $markcolor = "red" if ($try and $try eq "wrong");
      
      my $question = pval($coord, "question");
      if ($question) {
        $dragonletter .= "*";
        $owlletter = "";
        $dragoncolor = "blue" unless $dragoncolor;
      }
      
      my $score = pval($coord, "move_value");
      if ($score) {
        # FIXME: Should round this, not truncate it.
        #     Also, should remove trailing "." if not necessary.
        $dragonletter = substr($score, 0,3);
        $dragoncolor = "blue";
        $owlletter="";
        $alt = "whack";
      }

      my $colorboard_imgsrc = createPngFile($bw, $img_pix_size, "", $dragonletter, $dragoncolor, $owlletter, $owlcolor, $markcolor);
      $colorboard .= qq@  <TD><A href="$name?tstfile=$tstfile&num=$num&move=$coord" target=movewin>@ .
                     qq@<IMG border=0 HEIGHT=$img_pix_size WIDTH=$img_pix_size @ . 
                     qq@SRC="html/images/$colorboard_imgsrc"></A></TD>\n@;
    }
    $colorboard .= "  <TD align=center valign=center>&nbsp;$j&nbsp;</TD>\n </TR>\n";
  }
  $colorboard .= colorboard_letter_row($boardsize);
  $colorboard .= "\n</TABLE>\n";

  print $colorboard;
 
  print qq@</TD><TD valign=top>
<PRE>\n\n\n\n
<FONT color=green>green=alive</FONT>
<FONT color=cyan>cyan=dead</FONT>
<FONT color=red>red=critical</FONT>
<FONT color=yellow>yellow=unknown</FONT>
<FONT color=magenta>magenta=unchecked</FONT>
</PRE>
</TD></TR>
</TABLE>@;
 
  my $gtpall = $attribs{gtp_all};
  $gtpall  =~ s/<BR>//mg;
  $gtpall  =~ s/\s+$//mg;
  $gtpall  =~ m@loadsgf\s+ ((?:\w|[-+.\\/])+)  [ \t]* (\d*) @x
    or $gtpall =~m/(.*?)/;  #Problems!!!!  
  
  my $cmdline = "gq -l $1 " . ($2 ? "-L $2 " : "");
  if ($gtpall =~ m@ .* (owl_attack|owl_defend|dragon_status) \s* ([A-Z]\d{1,2}) \s* $ @x) {
    $cmdline .= "--decide-dragon $2 -o x.sgf" ;
  } elsif ($gtpall =~ m@ .* (reg_genmove\s+[whiteblack]*)  \s* $@x) {
    $cmdline .= "-t -w -d0x101800";
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
  
  if ($small) {
    summaryDiagrams();
  }
#CASE 3 - test file summary.
#  if (!-e "html/$tstfile.tst/index.html") {
    summarizeTestFile();
#  } else {
#    print "Cached:<HR>";
#  }
#  open (TESTFILE, "html/$tstfile.tst/index.html") or (print "$! ".__LINE__, die);
#  while (<TESTFILE>) {
#    print;
#  }
#  close TESTFILE;
}


sub summaryDiagrams {
  my $content;
  foreach my $curfile (glob("html/$tstfile.tst/*.xml"))
  {
    %points = {};
    $curfile =~ s/html.$tstfile.tst.(.*xml)/$1/;
    local $/;
    undef($/);
    open(FILE, "html/$tstfile.tst/$curfile");
    $content = <FILE>;
    close FILE;

    my %attribs = %{game_parse($content, 1)};

    print qq@<HR><A href="$name?$tstfile:$attribs{num}">$tstfile:$attribs{num}</A>\n@;

    my $boardsize = $attribs{"boardsize"};  #need to add to export.
    my $colorboard;
    $colorboard .= "<TABLE border=0 cellpadding=0 cellspacing=0>\n"
             . "\n";

  my $img_pix_size = 9;
    
  for (my $j = $boardsize; $j > 0; $j--) {
    my $jA = $j;
    $jA .= " " if ($j <= 9);
    $colorboard .= "<TR>\n";
    for (my $i = 1; $i <= $boardsize; $i++) {
      my $iA = ord('A') + $i - 1;
      if ($iA >= ord('I')) { $iA++; }
      $iA = chr($iA);  
      my $coord = $iA.$j;
      my $bw = pval($coord, "stone");
      my $alt = "";
      
      my $colorboard_imgsrc = createPngFile($bw, $img_pix_size, "", "","","","", "");
      $colorboard .= qq@  <TD>@ .
                     qq@<IMG border=0 HEIGHT=$img_pix_size WIDTH=$img_pix_size @ . 
                     qq@SRC="html/images/$colorboard_imgsrc"></A></TD>\n@;
    }
    $colorboard .= "</TR>\n";
  }
  #$colorboard .= colorboard_letter_row($boardsize);
  $colorboard .= "\n</TABLE>\n";  
  
  print $colorboard;
  }

  exit;
}



my %files;
sub summarizeTestFile {

  unless ($sortby) { $sortby = "filepos"; }
  
 # open (TF, "> html/$tstfile.tst/index.html")
 #   or print "couldn't open for output; $!\n", die;
 *TF = *STDOUT;
  
  print TF qq@<HTML><HEAD>
        <TITLE>$tstfile regression results - _VERSION_</TITLE>
        <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
      </HEAD>\n@;
  print TF "<BODY>\n";
  print TF "<H3>$tstfile regression results - _VERSION_</H3>\n";
  print TF qq@<TABLE border=1>
<tr>
  <TH><A href="$name?tstfile=$tstfile&sortby=filepos">line</A></TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=num">number</A></TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=result">result</A></TH>
  <TH>expected </TH>
  <TH>got</TH>
  <TH>gtp</TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=cputime">cputime</A></TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=owl_node">owl_node</A></TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=reading_node">reading_node</A></TH>
  <TH><A href="$name?tstfile=$tstfile&sortby=msperowl">1000*time/owl_node</A></TH>
</TR>\n@;

  my @files = glob("html/$tstfile.tst/*.xml");
  foreach my $curfile (@files) {
    $curfile =~ s/html.$tstfile.tst.(.*xml)/$1/;
    local $/;
    undef($/);
    open(FILE, "html/$tstfile.tst/$curfile");
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
    my $owl_node = $1
      if $content =~ m@<COUNTER[^>]*owl_node="?(\d+)@s;
    my $reading_node = $1
      if $content =~ m@<COUNTER[^>]*reading_node="?(\d+)@s;
    $cputime =~ s/0*$//;
    $files{$curfile} = {
      gtp_all => $gtp_all,
      gtp => $gtp,
      filepos => $filepos,
      num => $num,
      expected => $expected,
      got => $got,
      result => $result,
      cputime => $cputime,
      owl_node => $owl_node,
      reading_node => $reading_node,
      msperowl => ($owl_node ? 1000*$cputime/ $owl_node : 0),
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
  sub byowl_node {
    $files{$b}{owl_node} <=> $files{$a}{owl_node}
    or byfilepos();
  }

  sub byreading_node {
    $files{$b}{reading_node} <=> $files{$a}{reading_node}
    or byfilepos();
  }
  sub bymsperowl  {
    $files{$b}{msperowl} <=> $files{$a}{msperowl}
    or byfilepos();
  }
  
  sub filesby {
    $_ = shift;
    return byfilepos if /filepos/i;
    return bynum if /num/i;
    return byresult if /result/i;
    return bycputime if /cputime/i;
    return byowl_node if /owl_node/i || /owlnode/i;
    return bymsperowl if /msperowl/i;
    return byreading_node if /reading_node/i || /readingnode/i;
    $files{$a}{$_} <=> $files{$b}{$_};   
  }
  
  my %totals = (cputime=>0, owl_node=>0);
  
  foreach my $curfile (sort {filesby($sortby)} keys %files) {
    my %h = %{$files{$curfile}};
    my $numURL = qq@<A href="$name?$tstfile:$h{num}">$h{num}</A>@;
    my $r = $h{result};
    $r =~ s@^([A-Z]*)$@<B>$1</B>@;
    print TF "<TR><TD>$h{filepos}</TD><TD>$numURL</TD><TD>$r</TD><TD>$h{expected}</TD>"
        . "<TD>$h{got}</TD><TD>$h{gtp}</TD><TD>$h{cputime}</TD><TD>$h{owl_node}</TD>"
        . "<TD>$h{reading_node}</TD>"
        . "<TD>".sprintf("%.2f",$h{msperowl})."</TD></TR>\n";
    $totals{cputime} += $h{cputime};
    $totals{owl_node} += $h{owl_node};
    $totals{reading_node} += $h{reading_node};
  }
  print TF "<TR><TD>Total</TD><TD>&nbsp;</TD><TD>&nbsp;</TD><TD>&nbsp;</TD>"
    . "<TD>&nbsp;</TD><TD>&nbsp;</TD><TD>$totals{cputime}</TD><TD>$totals{owl_node}</TD>"
    . "<TD>$totals{reading_node}</TD>"
    ." <TD>".sprintf("%.2f",1000*$totals{cputime}/($totals{owl_node}+.0001))."</TD></TR>\n";
  print TF "</TABLE>";
  #close TF;
}



sub pval {
  my ($coord, $attrib) = @_;
  if ($points{$coord}) {
#    print "$coord $attrib<BR>\n";
    if ($points{$coord} =~ m@$attrib="(.*?)"@) {
     # if ($attrib eq 'stone') {
 #       print "$attrib=$1<BR>\n";
      #}
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
  my $details = shift;
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
  
  return \%attribs  unless $details;
  
  $content =~ s@.*?<POINT@<POINT@s;
  while ($content =~ s@<POINT(.*?)></POINT>@@s) {
    my $pattr = $1;
    if ($pattr =~ m@coord="(.*?)"@s) {
      $points{$1} = $pattr;
    } else {
      print "<P>MISSING coord: " . encode($content) . "<P>" . 
          encode($pattr);
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


sub printslow {
  our $VAR1;
  do "html/one.perldata.new" or confess "can't do perldata";
  my %h = %{$VAR1->[0]};
  my $by_cputime = 
    sub {
      $h{$b}->{cputime} <=> $h{$a}->{cputime}
      or $a cmp $b;
    };
  

  print qq@<HTML>
  <HEAD>
    <TITLE>Slow results - GNU Go</TITLE>
    <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
  </HEAD>\n@;
  print "<BODY><H4>Slow results</H4>";
  print "<TABLE border=1>";
  print "<TR><TD><B>Problem</B></TD><TD><B>Status</B></TD><TD>CPU Time</TD></TR>\n";

  my $i = 0;
  foreach my $k (sort $by_cputime keys %h) {
    $i++;
    last if $i > 50;
    print qq@<TR><TD><A href="$name?$k">$k</TD><TD>$h{$k}->{status}</TD>@;
    print qq@    <TD>$h{$k}->{cputime}</TD></TR>@;
    my ($p, $n) = $k =~ /(\w+):(\d+)/;
    open (F, "html/$p.tst/$n.trace") or do {print "Missing trace file for $k<BR>"; next;};
    my $first=1;
    while (<F>) {
      my $line = $_;
      if ($line =~ /^owl_.*\d{6} nodes/) {
        print qq@<TR><TD>&nbsp;</TD><TD>&nbsp;</TD><TD>@ if $first-- > 0;
        print qq@$line<BR>@;
      }
    }
    print qq@</TD></TR>@ if $first < 1;
    close F;
  }
  print "</TABLE></BODY></HTML>\n";
}

sub printspecial {
  our $VAR1;
  do "html/one.perldata.new" or confess "can't do perldata";
  my %h = %{$VAR1->[0]};

  my (%special);
  my $sfile = "special";

    print qq@<HTML>
         <HEAD><TITLE>Special results - GNU Go</TITLE>
         <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
       </HEAD>\n@;
    print "<BODY><H4>Special results</H4>";

    print "<TABLE border=1>";
    print "<TR><TD><B>Problem</B></TD><TD><B>Status</B></TD><TD>cputime</TD></TR>\n";

  if (-e $sfile) {
    open (BF, $sfile);
    while (<BF>) {
      if (/^((\w+):(\d+))/) {
        print qq@<TR><TD><A href="$name?$1">$1</A></TD><TD>$h{$1}->{status}</TD>@ .
              qq@<TD>$h{$1}->{cputime}</TD></TR>\n@;
      }
    }
    close(BF);
  }
  print qq@</TABLE></BODY></HTML>@;
}


sub printunexpected{
    my (%breakage);
    if (-e 'BREAKAGE.local') {
      open (BF, 'BREAKAGE.local');
      while (<BF>) {
        if (my ($bfile, $bpf) = $_ =~ /^(\w+:\d+)\s+(FAILED|PASSED)/i) {
          $breakage{lc $bfile} = $bpf;
        }
      }
      close(BF);
    }


    our $VAR1;
    do "html/one.perldata.new" or confess "can't do perldata";
    my %h = %{$VAR1->[0]};
 
    my @fails;  my @ufails;      
    my @passes; my @upasses;     


    print qq@<HTML><HEAD>
            <TITLE>Unexpected results - GNU Go</TITLE>
            <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
          </HEAD>\n@;
    print "<BODY><H4>Unexpected results</H4>";

    sub bynamenumber {
      my ($aname, $anumber) = $a =~ /(.*):(.*)/;
      my ($bname, $bnumber) = $b =~ /(.*):(.*)/;
      $aname cmp $bname or
        $anumber <=> $bnumber or
        $a cmp $b;
    }

    foreach my $k (sort bynamenumber keys %h) {
      my $status = %{$h{$k}}->{status};
      defined $status or do { warn "missing status for $k"; next;};
      if ($status eq 'FAILED') {
        unless (defined ($breakage{lc $k}) and $breakage{lc $k}eq 'FAILED') {
          push @ufails, $k;
        }
      } elsif ($status eq 'PASSED') {
        unless (defined ($breakage{lc $k}) and $breakage{lc $k} eq 'PASSED') {
          push @upasses, $k;
        }
      } elsif ($status eq 'passed') {
        if (defined ($breakage{lc $k})) {
          push @passes, $k;
        }
      } elsif ($status eq 'failed') {
        if (defined ($breakage{lc $k})) {
          push @fails, $k;
        }
      }
    }

    print "<TABLE border=1>\n";
    print qq@<TR><TD>FAILS</TD><TD>@.scalar(@ufails).qq@</TD></TR>\n@;
    print qq@<TR><TD>fails</TD><TD>@.scalar(@fails).qq@</TD></TR>\n@;
    print qq@<TR><TD>PASSES</TD><TD>@.scalar(@upasses).qq@</TD></TR>\n@;
    print qq@<TR><TD>passes</TD><TD>@.scalar(@passes).qq@</TD></TR>\n@;
    print qq@<TR><TD>pass : fail</TD><TD>@.
              sprintf("%.2f : 1", ((@upasses + @passes) / (@ufails + @fails + .001))).
              qq@</TD></TR>\n@;
    print "</TABLE><BR>\n";

    print "<TABLE border=1>";
    print "<TR><TD><B>Problem</B></TD><TD><B>Status</B></TD></TR>\n";
    foreach (@ufails) {
      print qq@<TR><TD><A href="$name?$_">$_</A></TD><TD>FAILED</TD></TR>\n@;
    }
    foreach (@fails) {
      print qq@<TR><TD><A href="$name?$_">$_</A></TD><TD>failed</TD></TR>\n@;
    }
    foreach (@upasses) {
      print qq@<TR><TD><A href="$name?$_">$_</A></TD><TD>PASSED</TD></TR>\n@;
    }
    foreach (@passes) {
      print qq@<TR><TD><A href="$name?$_">$_</A></TD><TD>passed</TD></TR>\n@;
    }
    print "</TABLE>\n";
    print "</body></html>\n";
    

}



sub printbycategory {

    our $VAR1;
    do "html/one.perldata.new" or confess "can't do perldata";
    my %hash = %{$VAR1->[0]};
    
    my %fails;
    
    foreach my $k (keys %hash) {
      my $status = $hash{$k}{status};
      $fails{$k} = $hash{$k} if $status =~ /failed/i;
    }
    
    my $by_cat = 
    sub {
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
    };
    
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
    
    print qq@<HTML><HEAD>
       <TITLE>Failures by category - GNU Go</TITLE>
       <META NAME="ROBOTS" CONTENT="NOINDEX, NOFOLLOW">
      </HEAD>\n@;
    print "<BODY><H4>Failures by category</H4>";
    print qq@<A href="$name?">main index</A>@;
    
    
    print "<TABLE border=1>";
    print "<TR><TD><B>Category</B></TD><TD><B>Severity</B></TD><TD><B>Problem</B></TD>\n";
    my $cat = "";
    my $sev = "";
    foreach my $k (sort $by_cat keys %fails) {
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
      print qq@<A href="$name?$k">$k</A>&nbsp&nbsp</A>\n@;
    }
    print "</TABLE>\n";
    print "</body></html>\n";
    
}
