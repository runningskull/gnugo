<html><head>
<!-- ViewCVS       -- http://viewcvs.sourceforge.net/
     by Greg Stein -- mailto:gstein@lyra.org
  -->

<title>gnugo/gnugo/regression/trevord.tst - view - 1.1</title></head>
<body bgcolor="#eeeeee">
<table width="100&#37;" border=0 cellspacing=0 cellpadding=1 bgcolor="#9999ee">
<tr valign=bottom><td>
<a href="trevord.tst#rev1.1"><img src="/icons/back.gif" alt="[BACK_ICON]" border=0 width=20 height=22></a>
<b>Return to <a href="trevord.tst#rev1.1">trevord.tst</a> CVS log</b> <img src="/icons/text.gif" alt="[FILE_ICON]" border=0 width=20 height=22></td>
<td align=right><img src="/icons/dir.gif" alt="[DIR_ICON]" border=0 width=20 height=22> <b>Up to <a href="/cgi-bin/viewcvs/#dirlist">[Sources]</a> / <a href="/cgi-bin/viewcvs/gnugo/#dirlist">gnugo</a> / <a href="/cgi-bin/viewcvs/gnugo/gnugo/#dirlist">gnugo</a> / <a href="/cgi-bin/viewcvs/gnugo/gnugo/regression/#dirlist">regression</a></b></td>
</tr></table>
<hr noshade>
<table width="100&#37;"><tr><td bgcolor="#ffffff">
File: <a href="/cgi-bin/viewcvs/#dirlist">[Sources]</a> / <a href="/cgi-bin/viewcvs/gnugo/#dirlist">gnugo</a> / <a href="/cgi-bin/viewcvs/gnugo/gnugo/#dirlist">gnugo</a> / <a href="/cgi-bin/viewcvs/gnugo/gnugo/regression/#dirlist">regression</a> / <a href="/cgi-bin/viewcvs/gnugo/gnugo/regression/trevord.tst#dirlist">trevord.tst</a> </b>
(<a href="/cgi-bin/viewcvs/~checkout~/gnugo/gnugo/regression/trevord.tst?rev=1.1"
 target="cvs_checkout"
 onClick="window.open('/cgi-bin/viewcvs/~checkout~/gnugo/gnugo/regression/trevord.tst?rev=1.1','cvs_checkout','resizeable=1,scrollbars=1 ');"
><b>download</b></a>)
<br>
Revision <b>1.1</b>
, <i>Sun Dec 30 16:40:45 2001 UTC</i> (3 days, 6 hours ago) by <i>bump</i>
<br>Branch: <b>MAIN</b>
<pre>added trevor_1_19.[1234]
</pre>
</td></tr></table>
<hr noshade>
<pre>


# games/trevor/auto/d01.sgf problems:

#Extend along side bigger.  Probably C7
loadsgf games/trevor/auto/d01.sgf 22
100 gg_genmove white
#? [!E10]*


#Other moves possible, !P6
loadsgf games/trevor/auto/d01.sgf 28
110 gg_genmove white
#? [N16]*


loadsgf games/trevor/auto/d01.sgf 36
120 gg_genmove white
#? [B9]*


loadsgf games/trevor/auto/d01.sgf 46
130 gg_genmove white
#? [!O9]*


# around L7|L8 looks right.
loadsgf games/trevor/auto/d01.sgf 52
140 gg_genmove white
#? [!N15]*


loadsgf games/trevor/auto/d01.sgf 54
150 gg_genmove white
#? [!F9]*


loadsgf games/trevor/auto/d01.sgf 144
160 gg_genmove white
#? [N7]*


loadsgf games/trevor/auto/d01.sgf 148
170 gg_genmove white
#? [Q8]*


loadsgf games/trevor/auto/d01.sgf 154
180 gg_genmove white
#? [S9]*


loadsgf games/trevor/auto/d01.sgf 184
190 gg_genmove white
#? [T10]*




# games/trevor/auto/d02.sgf problems:

# Other moves possible.  Time to reduce black, or play at the boundary, something like O11 or E6 are better than P10
loadsgf games/trevor/auto/d02.sgf 52
200 gg_genmove white
#? [O11|E6]*


loadsgf games/trevor/auto/d02.sgf 58
210 gg_genmove white
#? [!C15]*


#Other moves possible.
loadsgf games/trevor/auto/d02.sgf 64
220 gg_genmove white
#? [M10|L10|K10|K9|J10|J9]*


#Other moves possible.  !F2
loadsgf games/trevor/auto/d02.sgf 68
230 gg_genmove white
#? [G6]*


#Must reduce the moyo.
loadsgf games/trevor/auto/d02.sgf 72
240 gg_genmove white
#? [!T17]*


#Other moves also possible.
#N10|M9 are good shape moves.
loadsgf games/trevor/auto/d02.sgf 78
250 gg_genmove white
#? [N10|M9]*


loadsgf games/trevor/auto/d02.sgf 142
260 gg_genmove white
#? [Q2]*


#Set up big ko.
loadsgf games/trevor/auto/d02.sgf 150
270 gg_genmove white
#? [G8]*


loadsgf games/trevor/auto/d02.sgf 162
280 gg_genmove white
#? [!E13]*




# games/trevor/auto/d03.sgf problems:

loadsgf games/trevor/auto/d03.sgf 36
290 gg_genmove white
#? [!Q8]*


loadsgf games/trevor/auto/d03.sgf 86
300 gg_genmove white
#? [Q15]*


loadsgf games/trevor/auto/d03.sgf 88
310 gg_genmove white
#? [S17]*


loadsgf games/trevor/auto/d03.sgf 96
320 gg_genmove white
#? [B6]*


loadsgf games/trevor/auto/d03.sgf 162
330 gg_genmove white
#? [D16]*


loadsgf games/trevor/auto/d03.sgf 176
340 gg_genmove white
#? [H10]*


loadsgf games/trevor/auto/d03.sgf 210
350 gg_genmove white
#? [F18]*




# games/trevor/auto/d08.sgf problems:

loadsgf games/trevor/auto/d08.sgf 48
360 gg_genmove white
#? [K3|K4]*


#K10 better than J9
loadsgf games/trevor/auto/d08.sgf 66
370 gg_genmove white
#? [!J9]*


# L9 or close is better.
loadsgf games/trevor/auto/d08.sgf 68
380 gg_genmove white
#? [!G15]*


# L10 or H12 required.
loadsgf games/trevor/auto/d08.sgf 70
390 gg_genmove white
#? [!N9]*


loadsgf games/trevor/auto/d08.sgf 72
400 gg_genmove white
#? [H12|J12|J11|H11]*


loadsgf games/trevor/auto/d08.sgf 114
410 gg_genmove white
#? [S7]*


loadsgf games/trevor/auto/d08.sgf 118
420 gg_genmove white
#? [S9]*


loadsgf games/trevor/auto/d08.sgf 150
430 gg_genmove white
#? [E19]*


loadsgf games/trevor/auto/d08.sgf 206
440 gg_genmove white
#? [F10]*


loadsgf games/trevor/auto/d08.sgf 214
450 gg_genmove white
#? [G12|F13]*


loadsgf games/trevor/auto/d08.sgf 232
460 gg_genmove white
#? [F19]*


loadsgf games/trevor/auto/d08.sgf 240
470 gg_genmove white
#? [J18]*




# games/trevor/auto/d12.sgf problems:

loadsgf games/trevor/auto/d12.sgf 16
480 gg_genmove white
#? [S3|S5]*


loadsgf games/trevor/auto/d12.sgf 40
490 gg_genmove white
#? [D5]*


loadsgf games/trevor/auto/d12.sgf 52
500 gg_genmove white
#? [E3]*


loadsgf games/trevor/auto/d12.sgf 66
510 gg_genmove white
#? [!C15]*




# games/trevor/auto/d24.sgf problems:

loadsgf games/trevor/auto/d24.sgf 24
520 gg_genmove white
#? [D15]*


loadsgf games/trevor/auto/d24.sgf 52
530 gg_genmove white
#? [!F7]*


loadsgf games/trevor/auto/d24.sgf 66
540 gg_genmove white
#? [S8]*


loadsgf games/trevor/auto/d24.sgf 80
550 gg_genmove white
#? [T8]*


loadsgf games/trevor/auto/d24.sgf 98
560 gg_genmove white
#? [E18]*


loadsgf games/trevor/auto/d24.sgf 174
570 gg_genmove white
#? [K9]*


loadsgf games/trevor/auto/d24.sgf 178
580 gg_genmove white
#? [R13]*


loadsgf games/trevor/auto/d24.sgf 180
590 gg_genmove white
#? [!N14]*


#L14 is ko threat for both.
loadsgf games/trevor/auto/d24.sgf 212
600 gg_genmove white
#? [L14]*


loadsgf games/trevor/auto/d24.sgf 216
610 gg_genmove white
#? [D2]*


#M11 loses points.
loadsgf games/trevor/auto/d24.sgf 218
620 gg_genmove white
#? [!M11]*


loadsgf games/trevor/auto/d24.sgf 224
630 gg_genmove white
#? [E2]*


loadsgf games/trevor/auto/d24.sgf 242
640 gg_genmove white
#? [H2]*




# games/trevor/auto/d25.sgf problems:

loadsgf games/trevor/auto/d25.sgf 114
650 gg_genmove white
#? [E9]*


loadsgf games/trevor/auto/d25.sgf 176
660 gg_genmove white
#? [O11]*


loadsgf games/trevor/auto/d25.sgf 178
670 gg_genmove white
#? [O11]*




# games/trevor/auto/d29.sgf problems:

loadsgf games/trevor/auto/d29.sgf 182
680 gg_genmove white
#? [S16]*


loadsgf games/trevor/auto/d29.sgf 187
690 gg_genmove black
#? [!PASS]*


loadsgf games/trevor/auto/d29.sgf 188
700 gg_genmove white
#? [!PASS]*




# games/trevor/auto/d32.sgf problems:

loadsgf games/trevor/auto/d32.sgf 90
710 gg_genmove white
#? [M2]*


loadsgf games/trevor/auto/d32.sgf 94
720 gg_genmove white
#? [!K4]*


loadsgf games/trevor/auto/d32.sgf 106
730 gg_genmove white
#? [H3]*


loadsgf games/trevor/auto/d32.sgf 130
740 gg_genmove white
#? [B15]*


loadsgf games/trevor/auto/d32.sgf 166
750 gg_genmove white
#? [Q10]*


loadsgf games/trevor/auto/d32.sgf 176
760 gg_genmove white
#? [H6]*


loadsgf games/trevor/auto/d32.sgf 184
770 gg_genmove white
#? [A12]*


loadsgf games/trevor/auto/d32.sgf 198
780 gg_genmove white
#? [B18]*


loadsgf games/trevor/auto/d32.sgf 240
790 gg_genmove white
#? [P14]*


loadsgf games/trevor/auto/d32.sgf 244
800 gg_genmove white
#? [Q9]*




# games/trevor/auto/d33.sgf problems:

#Other moves possible, but must defend F3 group.
loadsgf games/trevor/auto/d33.sgf 36
810 gg_genmove white
#? [F2|H3]*


loadsgf games/trevor/auto/d33.sgf 40
820 gg_genmove white
#? [T18]*


loadsgf games/trevor/auto/d33.sgf 62
830 gg_genmove white
#? [!G6|E6]*


loadsgf games/trevor/auto/d33.sgf 186
840 gg_genmove white
#? [A5]*


loadsgf games/trevor/auto/d33.sgf 210
850 gg_genmove white
#? [O6]*




# games/trevor/auto/d39.sgf problems:

loadsgf games/trevor/auto/d39.sgf 24
860 gg_genmove white
#? [Q5]*


loadsgf games/trevor/auto/d39.sgf 26
870 gg_genmove white
#? [R6]*


loadsgf games/trevor/auto/d39.sgf 30
880 gg_genmove white
#? [R7]*


#locally, E18 is better.
loadsgf games/trevor/auto/d39.sgf 38
890 gg_genmove white
#? [!F18]*


#R7 or R2 are much better.
loadsgf games/trevor/auto/d39.sgf 46
900 gg_genmove white
#? [!O2]*


loadsgf games/trevor/auto/d39.sgf 100
910 gg_genmove white
#? [!B1]*


loadsgf games/trevor/auto/d39.sgf 170
920 gg_genmove white
#? [G17]*


loadsgf games/trevor/auto/d39.sgf 196
930 gg_genmove white
#? [!P19]*


loadsgf games/trevor/auto/d39.sgf 198
940 gg_genmove white
#? [!R19]*


loadsgf games/trevor/auto/d39.sgf 204
950 gg_genmove white
#? [R9]*


loadsgf games/trevor/auto/d39.sgf 208
960 gg_genmove white
#? [J6]*


loadsgf games/trevor/auto/d39.sgf 210
970 gg_genmove white
#? [J9]*


loadsgf games/trevor/auto/d39.sgf 222
980 gg_genmove white
#? [M13]*




# games/trevor/auto/d40.sgf problems:

loadsgf games/trevor/auto/d40.sgf 36
990 gg_genmove white
#? [!G6]*


loadsgf games/trevor/auto/d40.sgf 38
1000 gg_genmove white
#? [C5]*


loadsgf games/trevor/auto/d40.sgf 166
1010 gg_genmove white
#? [C16]*


loadsgf games/trevor/auto/d40.sgf 168
1020 gg_genmove white
#? [L11]*


loadsgf games/trevor/auto/d40.sgf 172
1030 gg_genmove white
#? [C16]*




# games/trevor/auto/d45.sgf problems:

loadsgf games/trevor/auto/d45.sgf 66
1040 gg_genmove white
#? [!C4]*




# games/trevor/auto/d46.sgf problems:

loadsgf games/trevor/auto/d46.sgf 132
1050 gg_genmove white
#? [!M12]*


loadsgf games/trevor/auto/d46.sgf 156
1060 gg_genmove white
#? [!H5]*


loadsgf games/trevor/auto/d46.sgf 228
1070 gg_genmove white
#? [R13]*




# games/trevor/auto/d47.sgf problems:

loadsgf games/trevor/auto/d47.sgf 22
1080 gg_genmove white
#? [!O16]*


loadsgf games/trevor/auto/d47.sgf 100
1090 gg_genmove white
#? [L7]*




# games/trevor/auto/d48.sgf problems:

loadsgf games/trevor/auto/d48.sgf 80
1100 gg_genmove white
#? [N5]*


loadsgf games/trevor/auto/d48.sgf 150
1110 gg_genmove white
#? [O17]*


#Mostly, !Q7
loadsgf games/trevor/auto/d48.sgf 226
1120 gg_genmove white
#? [Q8]*


</pre>
<hr noshade><table width="100&#37;" border=0 cellpadding=0 cellspacing=0><tr>
<td align=left><address>Send suggestions and report problems to the GNU CVS Hackers <a href="mailto:cvs-hackers@gnu.org">&lt;cvs-hackers@gnu.org&gt;</a>;</address></td>
<td align=right>Powered by<br><a href="http://viewcvs.sourceforge.net/">ViewCVS 0.7</a>
</td></tr></table>
</body></html>
