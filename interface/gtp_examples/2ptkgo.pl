#!/usr/bin/perl -w

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU Go, a Go program.        #
#                                                               #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/ #
# for more information.                                         #
#                                                               #
# Copyright 1999, 2000, 2001 by the Free Software Foundation.   #
#                                                               #
# This program is free software; you can redistribute it and/or #
# modify it under the terms of the GNU General Public License   #
# as published by the Free Software Foundation - version 3,     #
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

use Tk;
use ttgo;
use FileHandle;
use IPC::Open2;

# use strict;

$| = 1;

my $boardsize = 11;

my $autoplay = 1;

my @program = ();
my @cur_id = ();


my $Aprg_in = new FileHandle;
my $Aprg_out = new FileHandle;
$program[0] = 'gnugo --mode gtp --quiet';
$cur_id[0] = 1;   # starting id


my $Bprg_in = new FileHandle;
my $Bprg_out = new FileHandle;
$program[1] = 'gnugo --mode gtp --score aftermath --capture-all-dead --chinese-rules --quiet';
$cur_id[1] = 1;   # starting id


my $state = 'start';    # first initialization state

open2($Aprg_out, $Aprg_in, $program[0]);
$flags = 
    fcntl( $Aprg_out, F_GETFL, 0) 
    or die "Error with fcntl\n";
$flags = 
    fcntl( $Aprg_out, F_SETFL, $flags | O_NOBLOCK) 
    or die "Error with fcntl\n";


open2($Bprg_out, $Bprg_in, $program[1]);
$flags = 
    fcntl( $Bprg_out, F_GETFL, 0) 
    or die "Error with fcntl\n";
$flags = 
    fcntl( $Bprg_out, F_SETFL, $flags | O_NOBLOCK) 
    or die "Error with fcntl\n";



my $flags = 0;
my $consecutive_passes = 0;
my $ctm = 'B';   # who's turn to move?
my $cc = 'W';    # computers color


my $msgstr = '';


# This handles up to 25 size boards
# =================================
my @letter = qw ( A B C D E F G H J K L M N O P Q R S T U V W X Y Z );


# color definitions
# =================
my %cstr = ( 'b' => '#604040', 'B' => '#604040', 
	     'w' => '#ffffff', 'W' => '#ffffff' 
	     );

my $bkc = '#eeeeee';




# get command line arguments start with defaults
# ==============================================
my $sqwh = 26;
my $sqwh2 = 12;   # 1/2 of sqwh




my %toix = ();
foreach my $ix (0 .. $#letter) {
    $toix{ $letter[$ix] } = $ix;
}




# initialize graphics and such
# ----------------------------
my $top = MainWindow->new; 
$top->title("ptkgo.pl");
$top->resizable(0,0);
my $geox = ($boardsize-1) * $sqwh + 80;
my $geoy = ($boardsize-1) * $sqwh + 140;

$top->geometry( $geox . 'x' . $geoy );
$top->configure( background => $bkc );



# build the background go board

my $backing = $top->Canvas(
			   -width => $sqwh * $boardsize + 80,
			   -height => $sqwh * $boardsize + 80,
			   -background => $bkc
			   )->place(
				    -x => 0,
				    -y => 0,
				    );


foreach my $x ( 0 .. $boardsize-1 ) {

    $backing->createText( 40 + $x * $sqwh, 
			  25,
			  -text => $letter[$x],
			  -fill => 'black',
			  -justify => 'center',
			  -font => '-b&h-*-bold-r-*-*-11-*-*-*-*-*-*-*'
			  );

    $backing->createText( 40 + $x * $sqwh, 
			  ($boardsize-1)*$sqwh + 55,
			  -text => $letter[$x],
			  -fill => 'black',
			  -justify => 'center',
			  -font => '-b&h-*-bold-r-*-*-11-*-*-*-*-*-*-*'
			  );


    $backing->createLine( $x*$sqwh + 40,
			  40,
			  $x*$sqwh+40,
			  ($boardsize-1)*$sqwh + 40,
			  -fill => 'black',
			  -width => 1 );
}




foreach my $y ( 0 .. $boardsize-1 ) {

    $backing->createText( 25,
			  $y * $sqwh + 40,
			  -text => $boardsize - $y,
			  -fill => 'black',
			  -justify => 'center',
			  -font => '-b&h-*-bold-r-*-*-11-*-*-*-*-*-*-*'
			  );

    $backing->createText( ($boardsize-1) * $sqwh + 55,
			  $y * $sqwh + 40,
			  -text => $boardsize - $y,
			  -fill => 'black',
			  -justify => 'center',
			  -font => '-b&h-*-bold-r-*-*-11-*-*-*-*-*-*-*'
			  );


    $backing->createLine( 40,
			  $y*$sqwh+40,
			  ($boardsize-1)*$sqwh+40,
			  $y*$sqwh + 40,
			  -fill => 'black',
			  -width => 1 );
}


ttNewGame($boardsize);
ttShowBoard();




# pass button
# -----------
my $pass = $top->Button(
			-text => 'Pass',
			-command => sub {   },
			-width => 2,
			-height => 1,
			-font => '5x7',
			-borderwidth => 1,
			-highlightcolor => 'black',
			-highlightthickness  => 1,
			-highlightbackground => 'black',
			-relief => 'flat'
			)->place(
				 -x => 40 + 0 * 40,
				 -y => ($boardsize + 2) * $sqwh,
				 );



# undo button
# -----------
my $undo = $top->Button(
			-text => 'Undo',
			-command => sub {   },
			-width => 2,
			-height => 1,
			-font => '5x7',
			-borderwidth => 1,
			-highlightcolor => 'black',
			-highlightthickness  => 1,
			-highlightbackground => 'black',
			-relief => 'flat'
			)->place(
				 -x => 40 + 1 * 40,
				 -y => ($boardsize + 2) * $sqwh,
				 );




$top->bind( "<Button-1>", [ \&drop_stone, Ev('x'), Ev('y') ] );


$top->fileevent( $Aprg_out, 'readable', [ \&getmessage, 0] );
$top->fileevent( $Bprg_out, 'readable', [ \&getmessage, 1] );


$state = 'start';    # first initialization state
control();


MainLoop();




my $tmpstr;

sub getmessage
{
    my ($pi) = @_;


    if ($pi == 0) {
	$tmpstr = <$Aprg_out>;
    } else {
	$tmpstr = <$Bprg_out>;	
    }
    
    if (defined $tmpstr) {
	chomp($tmpstr);

	if ($tmpstr eq '') {   # eat the line, update id
	    $cur_id[$pi] ++; 
	    control( $msgstr );
	}  else {
	    $msgstr = $tmpstr;
	    print "Came up with $msgstr\n";
	}

    }
}




sub xputstone
{
    my ($color, $x, $y) = @_;
    

    my $xx = $x * $sqwh + 40;
    my $yy = $y * $sqwh + 40;

    $backing->createOval( $xx-$sqwh2, $yy-$sqwh2, 
			  $xx+$sqwh2, $yy+$sqwh2,
			  -tags => $x . '_' . $y,
			  -outline => 'black',
			  -fill => $cstr{$color} );
}


# This routine clears all empty squares, it does 
# not actually draw board
sub xfixboard
{
    my @vis = ttGetBoard();
    my $st;

    foreach my $y (0 .. $boardsize -1) {
	foreach my $x (0 .. $boardsize -1) {

	    $st = shift @vis;

	    if ($st eq '+') {
		$backing->delete( $x . '_' . $y );
	    }
	}
    }
}


sub pass
{


}


sub drop_stone
{
    my ( $w, $x, $y) = @_;

    $x = -1 + int(($x-3) / 26);
    $y = -1 + int(($y-3) / 26);

    if ($x < 0) { return 1; }
    if ($y < 0) { return 1; }
    if ($x >= $boardsize) { return 1; }
    if ($y >= $boardsize) { return 1; }


    my $gn = $letter[$x] . ($boardsize - $y);

    if ( !ttPlaceStone( $ctm, $gn ) ) {
	xputstone( $ctm, $x, $y );
	xfixboard();
	ttShowBoard();
    } else {  return 1; }


    if ($ctm eq 'W') { 
	$state = 'white';
    } else {
	$state = 'black';
    } 

    swap_ctm();    

}




# This routine is called after each message is recieved
# -----------------------------------------------------

# How the control loop works:
#
# the '$state' variable determines where to jump in.
# control is called when a program responds to a message

sub  control
{
    my ($msg) = @_;


    # send boardsize 0    (prgA)
    # send boardsize 1    (prgB)
    # xxx
    # send genmove_black  (prgA);
    # send black          (prgB);
    # send genmove_white  (prgB);
    # white               (prgA)
    # goto                xxx
    

    if (defined $msg) {
	print STDERR "state/msg = $state $msg\n";
    } else { print STDERR "state/msg = $state NULL\n"; }

    if ($state eq 'start') {
	snd( 0, "$cur_id[0] boardsize $boardsize" );
	$state = 'startb';
	return;	
    }

    if ($state eq 'startb') {
	snd( 1, "$cur_id[1] boardsize $boardsize" );
	$state = 'genmove_black';
	return;
    }

    if ( $state eq 'genmove_black' ) {
	snd( 0, "$cur_id[0] genmove_black" );
	$state = 'black';
	return;
    }

    if ( $state eq 'black' ) {
	my $y;
	my $x;
	my $gn;

	print "msg ---> $msg\n";

	$msg =~ /^=\d+\s+(.)(.*)/;    # parse out move components

	if ( $msg =~ /PASS/ ) {
	    $consecutive_passes++;
	    $gn = 'PASS';
	} else {
	    $consecutive_passes = 0;
	    $y = $boardsize - $2;
	    $x = $toix{$1};
	    $gn = $letter[$x] . ($boardsize - $y);
	}


	# show blacks move to the interface 
	# ---------------------------------
	if ( !ttPlaceStone( $ctm, $gn ) ) {
	    xputstone( $ctm, $x, $y ) if $gn ne 'PASS';
	    xfixboard();
	    ttShowBoard();
	    swap_ctm();
	} else {  return 1; }

	# send the move along to WHITE
	# ----------------------------
	snd( 1, "$cur_id[1] black $gn" );
	$state = 'genmove_white';

	if ($consecutive_passes == 2) {
	    $state = 'gameover';
	}

	return;
    }


    if ( $state eq 'genmove_white' ) {
	snd( 1, "$cur_id[1] genmove_white" );
	$state = 'white';
	return;
    }


    if ( $state eq 'white' ) {
	my $y;
	my $x;
	my $gn;

	print "msg ---> $msg\n";

	$msg =~ /^=\d+\s+(.)(.*)/;    # parse out move components

	if ( $msg =~ /PASS/ ) {
	    $consecutive_passes++;
	    $gn = 'PASS';
	} else {
	    $consecutive_passes = 0;
	    $y = $boardsize - $2;
	    $x = $toix{$1};
	    $gn = $letter[$x] . ($boardsize - $y);
	}


	# show blacks move to the interface 
	# ---------------------------------
	if ( !ttPlaceStone( $ctm, $gn ) ) {
	    xputstone( $ctm, $x, $y ) if $gn ne 'PASS';
	    xfixboard();
	    ttShowBoard();
	    swap_ctm();
	} else {  return 1; }

	# send the move along to BLACK
	# ----------------------------
	snd( 0, "$cur_id[0] white $gn" );
	$state = 'genmove_black';

	if ($consecutive_passes == 2) {
	    $state = 'gameover';
	}

	return;
    }


    if ( $state eq 'gameover' ) {    
	print "Game Over\n";
	ttScore();

    }






}





sub snd
{
    my ($who, $str) = @_;

    if ($who == 0) {
	print $Aprg_in "$str\n";
    } else {
	print $Bprg_in "$str\n";
    }

    print STDERR "----> $str\n";

}


sub swap_ctm
{
    if ( $ctm eq 'B' ) { 
	$ctm = 'W'; 
    } else { 
	$ctm = 'B'; 
    }

}
