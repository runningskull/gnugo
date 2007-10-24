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

package ttgo;
require Exporter;

use strict;


our @ISA    = qw(Exporter);
our @EXPORT = qw(ttNewGame ttShowBoard ttPlaceStone ttGetBoard ttScore);

my @bd = ();
my @sbd = ();   # working board
my $white = 1;
my $black = 2;
my $bs;
my @letter = qw ( A B C D E F G H J K L M N O P Q R S T U V W X Y Z );
my %tocol = ( 'b' => 2, 'B' => 2, 'w' => 1, 'W' => 1  );
my %toix = ();
foreach my $ix (0 .. $#letter) {
    $toix{ $letter[$ix] } = $ix;
}

my %hashed_boards = ();  # for convenient rep testing
my @all_boards = ();     # for move takebacks


my %tovisual = ( 2 => 'W',  1 => 'B', 0 => '+' );

my @dir = ();



sub ttNewGame
{
    ($bs) = @_;

    my $s = ($bs+2) * ($bs+1);

    foreach my $i (0 .. $s-1) {
	$bd[$i] = 3;                            # w+b
    }

    foreach my $x (0 .. $bs - 1) {
	foreach my $y (0 .. $bs - 1) {
	    $bd[ ($y+1) * ($bs+1) + $x ] = 0;   # empty
	}
    }

    @dir = ();

    $dir[0] = -1;
    $dir[1] =  1;
    $dir[2] = $bs + 1;
    $dir[3] = -($bs + 1);

    push( @all_boards, join(',', @bd) );
}



sub ttPlaceStone
{
    my ($c, $loc) = @_;

    my @prev_board = @bd;        # to take back if needed
    $hashed_boards{join(',',@prev_board)} = 1;  # hash previous board

    if ($loc eq 'PASS') {
	return(0);
    }

    $loc =~ /^(.)(.*)/;
    my $y = $bs - $2;
    my $x = $toix{$1};

    my $sq = ($y+1) * ($bs+1) + $x;


    # occupied?
    # =========
    if ($bd[ ($y+1) * ($bs+1) + $x ] != 0) {
	print "Illegal move, square occupied\n";
	return(1);
    }

    # Make move 
    # =========
    $bd[$sq] = $tocol{$c};

    # did we capture anything?
    # ========================
    my $cc = $tocol{$c};   # current color
    my $cap = 0;
    foreach my $d (@dir) {
  	if ($bd[$sq+$d] == (3 ^ $cc)) {
  	    @sbd = @bd;
  	    my $lc = lib_count( 3 ^ $cc, $sq + $d );
  	    if ($lc == 0) {
		$cap = 1;
		print "Capture possible\n";
  		capture( 3 ^ $cc, $sq+$d );
  	    }
  	}
    }

    # if capture not possible, it might be suicide
    # ============================================

    if (!$cap) {   
	$bd[$sq] = 0;   # make it empty again
	@sbd = @bd;
	$sbd[$sq] = $tocol{$c};
	my $lc = lib_count($tocol{$c}, $sq );
	print "liberty count = $lc\n";
	if ($lc == 0) {
	    print "Illegal move,  suicide!\n";
	    return(2);
	}
	# Make move
	# =========
	$bd[$sq] = $tocol{$c};    
    }


    if ( defined( $hashed_boards{ join(',',@bd) } ) ) {
	print "Illegal move, repeated positions\n";
	# @bd = @prev_board;
	# return(0);        
    }

    push( @all_boards, join(',', @bd) );

    ttScore();
	
    return 0;
}




sub lib_count
{
    my ($c, $sq) = @_;
    my $count = 0;

    foreach my $d (@dir) {
	if ($sbd[ $sq + $d ] == 0) { 
	    $count++; 
	    $sbd[$sq + $d ] = 9;
	    next;
	}
	if ($sbd[ $sq + $d ] == 3)  { next; }
	if ($sbd[ $sq + $d ] == $c) { 
	    $sbd[$sq + $d ] = 9;
	    $count += lib_count( $c, $sq + $d );
	}
    }

    return $count;
}


sub capture
{
    my ($c, $sq) = @_;

    $bd[$sq] = 0;
    foreach my $d (@dir) {
	if ( $bd[ $sq + $d ] == $c ) {
	    capture( $c, $sq + $d );
	}
    }
}



sub ttShowBoard
{
    foreach my $y (0 .. $bs + 1) {
	foreach my $x (0 .. $bs) {
	    printf ( "%2d", $bd[ $y * ($bs+1) + $x ] );
	}
	print "\n";
    }

    print "\n";
}



sub ttGetBoard
{
    my @tbd = ();

    foreach my $y (0 .. $bs-1) {
	foreach my $x (0 .. $bs-1) {
	    push @tbd, $tovisual{ $bd[ ($y+1) * ($bs+1) + $x ] };
	}
    }
    return @tbd;
}



sub ttScore
{
    @sbd = @bd;

    my $who = 0;
    my @ter = (0, 0, 0);
    my @stc = (0, 0, 0);

    foreach my $sq (0 .. (($bs+2) * ($bs+1))-1 ) {
	if ( $bd[$sq]==1 || $bd[$sq]==2 ) { $stc[$bd[$sq]] ++; }
	if ($sbd[$sq] == 0) {
	    my ($cnt, $who) = count_space($sq);
	    if ($who == 1 || $who == 2) {
		$ter[$who] += $cnt;
	    }
	}  
    }

    print "white stones=$stc[$white]   territory=$ter[$white]\n";
    print "black stones=$stc[$black]   territory=$ter[$black]\n";

    return( ($stc[$black] + $ter[$black])-($stc[$white] + $ter[$white]) );
}




# return count
# ------------
sub count_space
{
    my ($sq) = @_;
    my $count = 0;
    my $who = 0;

    if ( $sbd[$sq] == 9 || $sbd[$sq] == 3) {
	return (0,0);
    } elsif ( $sbd[$sq] != 0 ) {
	$who |= $sbd[$sq];
	return( 0, $who);
    } else {  # must be zero	       
	$count++;
	$sbd[$sq] = 9;   # mark it
	foreach my $d (@dir) {
	    my ($c, $w) = count_space( $sq + $d );
	    $count += $c;
	    $who |= $w;
	}
    }
    return ( $count, $who );
}


1;





