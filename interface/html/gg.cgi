#!/usr/bin/tclsh

# a cgi for playing against GnuGo
# Written in 1999 by Doug Ridgway
# This code is public domain: no copyright, no restrictions, no warranty.

# INSTALLATION

#   o Install Tcl 8.0 or later (see http://www.scriptics.com/).
#   o Install cgi.tcl 0.8.0 (from http://expect.nist.gov/)
#      apply patch cgi.patch
#   o Put script in a place accessible to your webserver, and set it up
#     to be executed as a CGI.
#   o Put patched cgi.tcl in same directory.
#   o Set the GNUGO_PATH, DOMAIN, and PATH variables below.
#   o Pick directory of desired images.
#     The naming convention is consistent with Andrew Grant's GIFs,
#     and his GIFs can be obtained at
#     http://www.britgo.demon.co.uk/gopcres/gopcres1.html#ag-gifs
#     Create a new directory, called "images", and unpack the gifs there.
#   o Edit mumble file as desired.
#   o Uncomment the cgi_debug -on command during debugging.

# CONFIGURATION

# location of GnuGo. Use full path.
set GNUGO_PATH /home/myaccount/gostuff/gnugo
# name of webserver (for cookie)
set DOMAIN my.domain.here
# location of script in web namespace (for cookie)
set PATH   /~myaccount/go/gg.cgi

# change these if you want different images
# what's needed: w,b,wt,bt,p,h,LS,RS,TS,BS,ULC,URL,LLC,LRC
# naming consistent with ag-gifs in sizes 55, 39, 27
# sizes 19 and 15 require slight futzing
set idir images
set iext .gif

source cgi.tcl

# don't change this unless you've got other mumble files available
set LANG en

# uncomment when debugging
#     cgi_debug -on

# END OF CONFIGURATION -- code follows

source mumble.$LANG

set bug_list [list \
  "Allows GnuGo to consume arbitrary amounts of CPU time"\
  "Constant mumbling"\
]

# hoshi point offset / period
array set hoff {19 4 17 4 15 4 13 4 11 3 9 3 7 3 5 3}
array set hper {19 6 17 5 15 4 13 3 11 3 9 4 7 2 5 3}

proc im {img alt} {
    global idir iext
    put [img $idir/$img$iext "alt= $alt" border=0]
}

proc ib {img alt} {
    global illegal
    upvar i i j j
    global idir iext
    # shouldn't need extra quotes here: cgi.tcl bug?
    if {[info exists illegal($i,$j)]} {
	im $img $alt
    } else {
	image_button [ij2sgf $i $j]=$idir/$img$iext "alt=\" $alt\"" border=0
    }
}

proc W {i j} {
    global lm
    if {"[ij2sgf $i $j]"=="$lm"} {
	im wt O
    } else {
	im w O
    }
}

proc B {i j} {
    global lm
    if {[ij2sgf $i $j]==$lm} {
	im bt X
    } else {
	im b X
    }
}

proc TW {i j} {
    W $i $j
}

proc TB {i j} {
    B $i $j
}

proc E {i j} {
    global boardsize hoff hper
    if {$i == 1 && $j == 1} {
	ib ULC +
    } elseif {$i == 1 && $j == $boardsize} {
	ib URC +
    } elseif {$i == $boardsize && $j == 1} {
	ib LLC +
    } elseif {$i == $boardsize && $j == $boardsize} {
	ib LRC +
    } elseif {$i == 1} {
	ib TS -
    } elseif {$i == $boardsize} {
	ib BS -
    } elseif {$j == 1} {
	ib LS |
    } elseif {$j == $boardsize} {
	ib RS |
    } elseif {($i-$hoff($boardsize))%$hper($boardsize) == 0 && ($j-$hoff($boardsize))%$hper($boardsize) == 0} {
	ib h +
    } else {
	ib p .
    }
}

proc print_board {} {
    global boardsize
    global board
    center {
	for {set i 1} {$i <= $boardsize} {incr i} {
	    for {set j 1} {$j <= $boardsize} {incr j} {
		$board($i,$j) $i $j
	    }
	    br
	}
    }
}

proc sgf2ij {c} {
    set c [string tolower $c]
    binary scan $c cc j i
    binary scan "a" c a
    foreach v {i j} {
	set $v [expr $$v+1-$a]
    }
    return $i,$j
}

proc ij2sgf {i j} {
    binary scan "a" c a
    foreach v {i j} {
	set $v [expr $$v-1+$a]
    }
    binary format cc $j $i
}

proc new_board {{size 19}} {
    global boardsize board illegal
    set boardsize $size
    for {set i 1} {$i <= $boardsize} {incr i} {
	for {set j 1} {$j <= $boardsize} {incr j} {
	    set board($i,$j) E
	}  
    }
    catch {unset illegal}
}

proc scan_sgf {sgf} {
    global board boardsize Size Color Handicap Komi illegal
    cgi_debug {p "Scan SGF:<br>$sgf"}
    set ttsgf $sgf
    set sgf [gnugo simplify $sgf]
    regexp -nocase {SZ\[([0-9]+)\]} $sgf {} boardsize
    set Size $boardsize
    new_board $boardsize
    regexp -nocase {HA\[([0-9]+)\]} $sgf {} Handicap
    regexp -nocase {KM\[(([0-9]|\.)+)\]} $sgf {} Komi
    regexp -nocase {PL\[(W|B)\]} $sgf {} Color
    if {"$Color" == "W"} {
	set Color White
    } {
	set Color Black
    }
    while {[regexp {AW\[(..)\]} $sgf {} coords]} {
	regsub {AW\[(..)\]} $sgf {AW} sgf
	set board([sgf2ij $coords]) W
    }
    while {[regexp {AB\[(..)\]} $sgf {} coords]} {
	regsub {AB\[(..)\]} $sgf {AB} sgf
	set board([sgf2ij $coords]) B
    }
    while {[regexp {TW\[(..)\]} $ttsgf {} coords]} {
	regsub {TW\[(..)\]} $ttsgf {TW} ttsgf
	set board([sgf2ij $coords]) TW
    }
    while {[regexp {TB\[(..)\]} $ttsgf {} coords]} {
	regsub {TB\[(..)\]} $ttsgf {TB} ttsgf
	set board([sgf2ij $coords]) TB
    }
    while {[regexp {IL\[(..)\]} $sgf {} coords]} {
	regsub {IL\[(..)\]} $sgf {IL} sgf
	set illegal([sgf2ij $coords]) 1 
    }
    cgi_debug {p "board is [array get board]"}
}

proc gnugo {cmd sgf} {
    global GNUGO_PATH
    cgi_debug {p "GG $cmd:<br> $sgf"}
    switch $cmd {
	play {
	    set f [open "| $GNUGO_PATH -l - --quiet -o -" RDWR]
	}
	score {
	    set f [open "| $GNUGO_PATH -l - --quiet --score end" RDWR]
	}
	simplify {
	    set f [open "| $GNUGO_PATH -l - --quiet --printsgf -" RDWR]
	}
	showterritory {
	    set f [open "| $GNUGO_PATH -l - --quiet --score end --analyzerfile -" RDWR]
	}
	default {
	    cgi_debug {p "Unknown cmd $cmd"}
	}
    }
    # dunno why I used to have to do this
    if 0 {
    fconfigure $f -blocking 0
    puts $f $sgf
    flush $f
    set out ""
    while {![eof $f]} {
	append out [read $f]
    }
    cgi_debug {p "GG returns:<br> $out"}

    return $out
    }

    puts $f $sgf
    flush $f
    set out [read $f]
    cgi_debug {p "GG returns:<br> $out"}

    return $out
}

proc mumble {event} {
    global mumble
    lindex $mumble($event) [expr int(rand()*[llength $mumble($event)])]
}

proc other {c} {
  switch $c {
      White {
	  return Black
      }
      Black {
	  return White
      }
  }
}

cgi_eval {
    
    cgi_input
    
    set msg ""
    set lm ""
    # first, deal with button presses
    if {![catch {cgi_import zz}]} {
	switch $zz {
	    Pass {
		set mode pass
		cgi_import sgf
		regexp {PL\[(B|W)\]} [gnugo simplify $sgf] {} player
		regsub {\)} $sgf ";$player\[\])" sgf
		set sgf [gnugo play $sgf]
	    }
	    Score {
		set mode score
		cgi_import sgf
		if {[catch {
		    cgi_import_cookie won
		    cgi_import_cookie lost}]} {
			set won 0
			set lost 0
		}
		set score [gnugo score $sgf]
		set msg [format $message(score) $score]
		cgi_import Color
		if {[regexp -nocase "$Color (seems to )?win" $score]} {
		    append msg $message(youwin)
		    incr won
		} elseif {[regexp -nocase "[other $Color] (seems to )?win" $score]} {
		    append msg [format $message(iwin) [mumble advice]]
		    incr lost
		    if {$lost > $won + 3 && $won/($lost+$won) < 0.4} {
			switch $Color {
			    Black {
				append msg $message(upyours)
			    }
			    White {
				append msg $message(downmine)
			    }
			}
		    }
		    if {$won > $lost + 3 && $won/($lost+$won) > 0.75} {
			switch $Color {
			    Black {
				append msg $message(downyours)
			    }
			    White {
				append msg $message(upmine)
			    }
			}
		    }
		    
		} else {
		    append msg $message(jigo)
		}
		append msg [format $message(stats) $won $lost]
		append msg $message(another)
		set sgf [gnugo showterritory $sgf]

	    }
	    Save {
		cgi_import sgf
		http_head {
		    content_type application/x-go-sgf
		}
		puts $sgf
		exit
	    }
	    Load {
		set mode load
		set local [cgi_import_filename -local sgf_file]
		set f [open $local]
		set sgf [read $f]
		close $f
		file delete $local
		set msg $message(restart)
	    }
	    New {
		# get old values
		cgi_import sgf
		scan_sgf $sgf
		set changed 0
		foreach v {Size Handicap Color Komi} {
		    if {[set $v] != [cgi_import $v]} {
			set changed 1
		    }
		}
		if {"$Komi"==""} {set Komi 5.5}
		set sgf [format \
			{(;GM[1]FF[3]RU[Japanese]SZ[%d]HA[%d]KM[%.1f]} \
			$Size $Handicap $Komi]
		if {$Handicap} {
		    append sgf {PL[W])}
		} else {
		    append sgf {PL[B])}
		}
		if {"$Color" == "White" && $Handicap == 0 
		|| "$Color" == "Black" && $Handicap > 0} {
		    set sgf [gnugo play $sgf]
		}
		set mode new
		if $changed {
		    append msg $message(handichange)
		}
		append msg [format $message(new) [mumble new]]
	    }
	}
    } elseif {![catch {cgi_import sgf}]} { 
	# Next, clicking on an empty vertex means a move
	set mode play
	set l [cgi_import_list]
	set mn [lsearch $l {[a-z][a-z].x}]
	set move [string range [lindex $l $mn] 0 1]
	regexp {PL\[(B|W)\]} [gnugo simplify $sgf] {} player
	regsub {\)} $sgf ";$player\[$move\])" sgf
	set sgf [gnugo play $sgf]
    } elseif {![catch {cgi_import_cookie sgf} err]} {
	# old game stored in a cookie?
	set mode load
	set msg [format $message(resume) [exec $GNUGO_PATH --version |& cat]]
    } else {
	# A newbie! Give them an game to play...
	set mode play
	set msg [format $message(newbie) [exec $GNUGO_PATH --version |& cat]]
	set sgf {(;GM[1]FF[3]RU[Japanese]SZ[9]HA[4]KM[0.5])}
	set sgf [gnugo play $sgf]
    }

# only for old buggy cgi.tcl
#    regsub -all "\[ \r\n\]+" $sgf {} sgf
    cgi_http_head {
	cgi_content_type text/html
	cgi_export_cookie sgf domain=$DOMAIN path=$PATH expires=never
	if {"$mode" == "new" && $changed} {
	    cgi_cookie_set won=0 domain=$DOMAIN path=$PATH expires=never
	    cgi_cookie_set lost=0 domain=$DOMAIN path=$PATH expires=never
	} elseif {"$mode" == "score"} {
	    cgi_export_cookie won domain=$DOMAIN path=$PATH expires=never
	    cgi_export_cookie lost domain=$DOMAIN path=$PATH expires=never
	}
    }

    scan_sgf $sgf

    cgi_title {GnuGo CGI}
    cgi_body bgcolor=white {

	if {$mode=="play" || $mode=="pass"} {
	    unset lm
	    regexp {\[([^]]*)\][^]]*$} $sgf {} lm
	    if {![info exists lm]} {
		cgi_debug {p "No Move found in $sgf! WTF???}
	    } elseif {"$lm"=="" || ("$lm"=="tt" && $boardsize <=19)} {
		if {$mode == "pass"} {
		    append msg $message(pass2)
		} else {
		    append msg $message(pass1)
		}
	    } else {
		cgi_debug {p "Last move is $lm."}
		append msg [format $message(move) [mumble move]]
	    }
	}
	switch $mode {
	    default {
		p $msg
	    }
	}
	cgi_form gg {
	    print_board
	    center {
		cgi_submit_button zz=Pass
		cgi_submit_button zz=Score
		cgi_submit_button zz=Save
	    }
	    export sgf=$sgf
	    hr
	    center {
		put [bold "New game: "]
		put Size
		cgi_select Size {
		    for {set i 5} {$i <= 19} {incr i 2} {
			option $i selected_if_equal=$Size
		    }
		}
		put Handicap
		cgi_select Handicap {
		    foreach i {0 2 3 4 5 6 7 8 9} {
			option $i selected_if_equal=$Handicap
		    }
		}
		put "Your Color"
		cgi_select Color {
		    foreach c {White Black} {
			option $c selected_if_equal=$Color
		    }
		}
		put "Komi"
		cgi_text Komi=$Komi size=3
		submit_button zz=New
		br
	    }
	}

	cgi_form gg enctype=multipart/form-data {
	    center {
		put [bold "Restart saved game:"]
		cgi_file_button sgf_file
		submit_button zz=Load
	    }
	}
    }	
}








