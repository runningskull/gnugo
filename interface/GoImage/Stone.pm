# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program is distributed with GNU Go, a Go program.            #
#                                                                   #
# Write gnugo@gnu.org or see http://www.gnu.org/software/gnugo/     #
# for more information.                                             #
#                                                                   #
# Copyright 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006 and 2007 #
# by the Free Software Foundation.                                  #
#                                                                   #
# This program is free software; you can redistribute it and/or     #
# modify it under the terms of the GNU General Public License       #
# as published by the Free Software Foundation - version 3,         #
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
# Uses Perl GB package to create mini-PNG files used by the
# regression html views.
#

package GoImage::Stone;

if (0) {
  require GD;
}

use GD;
use strict;
use warnings;

BEGIN {
      use Exporter ();
      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);
      # set the version for version checking
      $VERSION     = 0.01;
      # if using RCS/CVS, this may be preferred (???-tm)
      $VERSION = do { my @r = (q$Revision: 1.1.1.1 $ =~ /\d+/g); sprintf "%d."."%02d" x $#r, @r }; # must be all one line, for MakeMaker
      @ISA         = qw(Exporter);
      @EXPORT      = qw(&createPngFile &parseFileName);
      %EXPORT_TAGS = ( );     # eg: TAG => [ qw!name1 name2! ],
      # your exported package globals go here,
      # as well as any optionally exported functions
      @EXPORT_OK   = (); #qw($Var1 %Hashit &func3);
}
our @EXPORT_OK;
  # exported package globals go here
  #our $Var1;
  #our %Hashit;
  # non-exported package globals go here
  #our @more;
  #our $stuff;
  # initialize package globals, first exported ones
  #$Var1   = '';
  #%Hashit = ();
  # then the others (which are still accessible as $Some::Module::stuff)
  #$stuff  = '';
  #@more   = ();
  # all file-scoped lexicals must be created before
  # the functions below that use them.
  # file-private lexicals go here
  #my $priv_var    = '';
  #my %secret_hash = ();
  # here's a file-private function as a closure,
  # callable as &$priv_func;  it cannot be prototyped.
  #my $priv_func = sub {
      # stuff goes here.
  #};
  # make all your functions, whether exported or not;
  # remember to put something interesting in the {} stubs
  #sub createPngFile      {}    # no prototype
  #sub func2()    {}    # proto'd void
  #sub func3($$)  {}    # proto'd to 2 scalars
  # this one isn't exported, but could be called!
  #sub func4(\%)  {}    # proto'd to 1 hash ref

my $overwrite = "";

my $image_dir = "html/images";

sub parseFileName {

  #FIXME: !!!!!!!!!!!!!!!!!!!!!!!
  #   Need to support text2 & text2_color attributes correctly.
  my $fn = shift;
  $fn =~ s/(.png)?\s*$//mg;
#  print "$fn\n";
  $fn =~ /([WBE])([1-9][0-9]*)([NSEWH]{0,2})((?:x[0-9a-fA-F]{2})*)_?([a-z]*)(?:-s([a-z]*))?/;
  my ($color, $pixels, $position, $text, $text_color, $square_color) = ($1,$2,$3,$4,$5,$6);
  my ($text2, $text2_color);
  #print "$1:$2:$3:$4:$5\n";
  if ($color eq "B") { $color = "black"; }
  elsif ($color eq "W") { $color = "white"; }
  elsif ($color =~ /^E/ || die "bad color in: $fn;$color;") { $color = ""; }
  if ($text) {
    my $new_text="";
    while ($text =~ s/(...)//) {
      $new_text .= chr(hex("0$1"));
    }
    $text = $new_text;
  }



  my $out = createPngFile($color, $pixels, $position, $text, $text_color, $text2, $text2_color, $square_color);
  if ("$fn.png" ne $out) {
    print "IN:$fn\tOUT:$out\n";
  }
}


#createStone:
#Does: Creates an appropriate PNG file if it doesn't exist, and...
#Returns: name of the image file to use
#Parameters:
# color - stone color: "black", "white", ""
# pixels - default 15.
# position - "H", "N", "S", "E", "W", "NE", "SW", "SE", "NW", ""  (H == hoshi)
#          : edge or star point location.
# text  - stone label: "3 char max recommended."
# text_color - "white", "black", "green","cyan","red","yellow","magenta","blue", ""
# text2 - appended t text...;
# text2_color - ...but in this color.
# square_color - same choices as text_color
#
#Details:
# creates file named like:
#  COLORLIST := white|black|green|cyan|red|blue|yellow|magenta|grey
#  [WBE]$pixels[NSEWH]{0,2}(${text}_(COLORLIST))?(__?${text}$(_COLORLIST))?(-s(COLORLIST))?
#  Note that $text is written with each character converted to it's ord value
#   in hex preceeded by an underscore to avoid bogus file names.  Also allows
#   upper & lower case easily on case-insensitive file systems, like Windows.
#  For example:
#    W25.png - large white stone;
#    B10.png - smaller black stone;
#    B14x61x6d_R - black stone w/ red 'am' text.


sub createPngFile {
  my ($color, $pixels, $position, $text, $text_color, $text2, $text2_color, $square_color)= @_;
  if (!$color) {$color = "";}
  elsif (!($color eq "black" || $color eq "white")) { die "invalid color: $color"; }
  if (!$text) {$text = "";}
  if (!$text_color) {$text_color = "blue";}
  if (!$text2) {$text2 = "";}
  if (!$text2_color) {$text2_color = "blue";}
  if (!$position) {$position = ""};
  if (!$pixels) {$pixels = 15};
  if (!$square_color) {$square_color = ""};
  
  my $image_name;
  if ($color eq "black") { $image_name = "B"; }
  elsif ($color eq "white") { $image_name = "W"; }
  else {$image_name = "E"}

  $image_name .= $pixels;
  $image_name .= $position;
  if ($text) {
    foreach (split(//,$text)) {
      $image_name .= "x" . (sprintf "%x", ord($_));
    }
    $image_name .= "_" . $text_color;
  }
  if ($text2) {
    $image_name .= '__';
    foreach (split(//,$text2)) {
      $image_name .= "x" . (sprintf "%x", ord($_));
    }
    $image_name .= "_" . $text2_color;
  }
  
  if ($square_color) {
    $image_name .= "-s" . $square_color;
  }
  

#gdGiantFont, gdLargeFont, gdMediumBoldFont, gdSmallFont and gdTinyFont  
  $image_name .= ".png";

  #Note: Create image name first; don't re-create if it already exists.
  #The caller now caches the images names, so they're regenerated every
  #time.  Maybe make this a package-level option?
  if ((!$overwrite) && -e "$image_dir/$image_name") {
    return $image_name;
  }

  my $im = new GD::Image($pixels,$pixels);
  my %colors = ("white", $im->colorAllocate(255,255,255),
                "black", $im->colorAllocate(0,0,0),
                "red",   $im->colorAllocate(255,0,0),
                "blue",  $im->colorAllocate(0,0,255),
                "green", $im->colorAllocate(0,255,0),
                "grey",  $im->colorAllocate(127,127,127),
                "dkgrey",  $im->colorAllocate(63,63,63),
                "ltgrey",  $im->colorAllocate(190,190,190),
                "brown", $im->colorAllocate(170,140,70),
                "cyan",  $im->colorAllocate(0,255,255),
                "yellow",$im->colorAllocate(255,255,0),
                "magenta",$im->colorAllocate(255,0,255),
                );
                
  $im->fill(1,1, $colors{"brown"});                
  if ($color) {
    $im->arc($pixels/2, $pixels/2, $pixels+1, $pixels+1, 0, 360, $colors{$color});
    $im->fill($pixels/2, $pixels/2, $colors{$color});
  } else {
    $im->line($pixels/2,0,$pixels/2,$pixels, $colors{"black"});
    $im->line(0,$pixels/2,$pixels,$pixels/2, $colors{"black"});
  }

  if ($text || $text2) {
    my $f = gdSmallFont;#gdLargeFont;#gdMediumBoldFont;#
    my $ftext = $text.$text2;
    my ($fw, $fh) = ($f->width,$f->height);
    my ($tw, $th) = ($fw * length($ftext), $fh);  #TODO: Allow multi-line text.
    my ($ulx, $uly) = ($pixels/2 - $tw/2 + 1, $pixels/2 - $th/2);
    my ($lrx, $lry) = ($ulx + $tw, $uly + $th);
    if (!$color or $text_color eq "blue" or $text2_color eq "blue"
        or ($color eq "white" and $text_color eq "yellow" and $text2_color eq "yellow")) {
      $im->filledRectangle($ulx-2, $uly, $lrx, $lry, $colors{"ltgrey"});
    }
    $im->string($f, $ulx, $uly, $text, $colors{$text_color});
    $im->string($f, $ulx+ length($text) * $fw, $uly, $text2, $colors{$text2_color});
  }
  
  if ($square_color) {
    $im->rectangle(1,1,$pixels-2, $pixels-2, $colors{$square_color});
    $im->rectangle(2,2,$pixels-3, $pixels-3, $colors{$square_color});
  }


  
  open(IMAGE, ">$image_dir/$image_name") || die "Couldn't create file: $image_dir/$image_name";
  binmode IMAGE;
  print IMAGE $im->png;
  close IMAGE;
  return $image_name;
}

sub createTestHtml {
  opendir(IMAGES, $image_dir);
  foreach (sort readdir(IMAGES)) {
    if (/\.png$/) {
      print "$_:&nbsp;<IMG SRC=\"$image_dir/$_\"><HR>\n";
    }
  }
}

1;

END { }

if (!-e "html") {  #Wher's perl's  mkdir -p ????
  mkdir "html";
}

if (!(-e $image_dir)) {
  mkdir ($image_dir)  || die "Couldn't create directory: $image_dir\n";
}

1;
