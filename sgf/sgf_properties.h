/* # */
/* # This document was taken from the SGF Specfication. See: */
/* # http://www.red-bean.com/sgf/ */
/* # */
/* # [SGF FF[4] - Smart Game Format] */
/* # */
/* # FF[4] property index */
/* # */
/* # This is an alphabetical index to all properties defined in FF[4]. */
/* # New properties are marked with '*', changed properties are marked with '!'. */
/* # */
/* #ID   Description     property type    property value */
/* #---- --------------- ---------------  -------------------------------------- */
     /*  Add Black       setup            list of stone */
#define SGFAB   16961
     /*  Add Empty       setup            list of point */
#define SGFAE   17729
     /*  Annotation      game-info        simpletext */
#define SGFAN   20033
     /*  Application     root             composed simpletext ':' simpletext */
#define SGFAP   20545
     /*  Arrow           -                list of composed point ':' point */
#define SGFAR   21057
     /*  Who adds stones - (LOA)          simpletext */
#define SGFAS   21313
     /*  Add White       setup            list of stone */
#define SGFAW   22337
     /*  Black           move             move */
#define SGFB     8258
     /*  Black time left move             real */
#define SGFBL   19522
     /*  Bad move        move             double */
#define SGFBM   19778
     /*  Black rank      game-info        simpletext */
#define SGFBR   21058
     /*  Black team      game-info        simpletext */
#define SGFBT   21570
     /*  Comment         -                text */
#define SGFC     8259
     /*  Charset         root             simpletext */
#define SGFCA   16707
     /*  Copyright       game-info        simpletext */
#define SGFCP   20547
     /*  Circle          -                list of point */
#define SGFCR   21059
     /*  Dim points      - (inherit)      elist of point */
#define SGFDD   17476
     /*  Even position   -                double */
#define SGFDM   19780
     /*  Doubtful        move             none */
#define SGFDO   20292
     /*  Date            game-info        simpletext */
#define SGFDT   21572
     /*  Event           game-info        simpletext */
#define SGFEV   22085
     /*  Fileformat      root             number (range: 1-4) */
#define SGFFF   17990
     /*  Figure          -                none | composed number ":" simpletext */
#define SGFFG   18246
     /*  Good for Black  -                double */
#define SGFGB   16967
     /*  Game comment    game-info        text */
#define SGFGC   17223
     /*  Game            root             number (range: 1-5,7-16) */
#define SGFGM   19783
     /*  Game name       game-info        simpletext */
#define SGFGN   20039
     /*  Good for White  -                double */
#define SGFGW   22343
     /*  Handicap        game-info (Go)   number */
#define SGFHA   16712
     /*  Hotspot         -                double */
#define SGFHO   20296
     /*  Initial pos.    game-info (LOA)  simpletext */
#define SGFIP   20553
     /*  Interesting     move             none */
#define SGFIT   21577
     /*  Invert Y-axis   game-info (LOA)  simpletext */
#define SGFIY   22857
     /*  Komi            game-info (Go)   real */
#define SGFKM   19787
     /*  Ko              move             none */
#define SGFKO   20299
     /*  Label           -                list of composed point ':' simpletext */
#define SGFLB   16972
     /*  Line            -                list of composed point ':' point */
#define SGFLN   20044
     /*  Mark            -                list of point */
#define SGFMA   16717
     /*  set move number move             number */
#define SGFMN   20045
     /*  Nodename        -                simpletext */
#define SGFN     8270
     /*  OtStones Black  move             number */
#define SGFOB   16975
     /*  Opening         game-info        text */
#define SGFON   20047
     /*  Overtime        game-info        simpletext */
#define SGFOT   21583
     /*  OtStones White  move             number */
#define SGFOW   22351
     /*  Player Black    game-info        simpletext */
#define SGFPB   16976
     /*  Place           game-info        simpletext */
#define SGFPC   17232
     /*  Player to play  setup            color */
#define SGFPL   19536
     /*  Print move mode - (inherit)      number */
#define SGFPM   19792
     /*  Player White    game-info        simpletext */
#define SGFPW   22352
     /*  Result          game-info        simpletext */
#define SGFRE   17746
     /*  Round           game-info        simpletext */
#define SGFRO   20306
     /*  Rules           game-info        simpletext */
#define SGFRU   21842
     /*  Markup          - (LOA)          point */
#define SGFSE   17747
     /*  Selected        -                list of point */
#define SGFSL   19539
     /*  Source          game-info        simpletext */
#define SGFSO   20307
     /*  Square          -                list of point */
#define SGFSQ   20819
     /*  Style           root             number (range: 0-3) */
#define SGFST   21587
     /*  Setup type      game-info (LOA)  simpletext */
#define SGFSU   21843
     /*  Size            root             (number | composed number ':' number) */
#define SGFSZ   23123
     /*  Territory Black - (Go)           elist of point */
#define SGFTB   16980
     /*  Tesuji          move             double */
#define SGFTE   17748
     /*  Timelimit       game-info        real */
#define SGFTM   19796
     /*  Triangle        -                list of point */
#define SGFTR   21076
     /*  Territory White - (Go)           elist of point */
#define SGFTW   22356
     /*  Unclear pos     -                double */
#define SGFUC   17237
     /*  User            game-info        simpletext */
#define SGFUS   21333
     /*  Value           -                real */
#define SGFV     8278
     /*  View            - (inherit)      elist of point */
#define SGFVW   22358
     /*  White           move             move */
#define SGFW     8279
     /*  White time left move             real */
#define SGFWL   19543
     /*  White rank      game-info        simpletext */
#define SGFWR   21079
     /*  White team      game-info        simpletext */
#define SGFWT   21591
/* # */
/* # These are additions to the SGF spec- old commands and some others */
/* # */

/* # */
/* # outdated FF3 properties */
/* # */
#define SGFBS 21314
#define SGFWS 21335
#define SGFID 17481
#define SGFRG 18258
#define SGFSC 17235

/* # */
/* # some random ones used by CGoban */
/* # */
#define SGFSY 22867

/* # */
/* # nonstandard SGF property used by GNU Go to mark illegal moves */
/* # */
#define SGFIL 19529
