{
    Copyright (c) 1998 by Michael Van Canneyt
    member of the Free Pascal development team

    Unit to access the ncurses library

    See the file COPYING.FPC included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{
  Many thanks to Ken Wright for his patches !
}
unit ncurses;
interface

{$packrecords C}
{$ifdef OpenBSD}                // openbsd curses=ncurses. Openbsd ocurses=old curses.
{$linklib curses}
{$else}
{$linklib ncurses}
{$endif}
{$linklib c}

{ Manually Added types }
type
  Bool = byte;
  PINTEGER = ^Longint;
  PLongint = ^ longint;
  PFILE = pointer;

const
{$ifndef openbsd}
  libncurses = 'ncurses';
{$else openbsd}
  libncurses = 'curses';
{$endif openbsd}
  NCURSES_VERSION_MAJOR = 5;
  NCURSES_VERSION_MINOR = 0;
  NCURSES_VERSION_PATCH = 19991023;
  NCURSES_VERSION = '5.0';

type
  chtype  = longint;
  pchtype = pchar;

const
  CXX_BUILTIN_BOOL = 1;
type
  CXX_TYPE_OF_BOOL = char;

var
{$ifndef darwin}
    COLORS : longint; cvar; external;
    COLOR_PAIRS : longint; cvar; external;
{$else darwin}
    COLORS : longint; external libncurses name 'COLORS';
    COLOR_PAIRS : longint; external libncurses name 'COLOR_PAIRS';
{$endif darwin}

    const
       COLOR_BLACK = 0;
       COLOR_RED = 1;
       COLOR_GREEN = 2;
       COLOR_YELLOW = 3;
       COLOR_BLUE = 4;
       COLOR_MAGENTA = 5;
       COLOR_CYAN = 6;
       COLOR_WHITE = 7;

type
    tacs_map = array [char] of chtype;
    pacs_map = ^tacs_map;

var
{$ifndef darwin}
    acs_map : tacs_map; cvar; external;
{$else darwin}
    acs_map : tacs_map; external libncurses name 'acs_map';
{$endif darwin}

    function ACS_ULCORNER : chtype;
    function ACS_LLCORNER : chtype;
    function ACS_URCORNER : chtype;
    function ACS_LRCORNER : chtype;
    function ACS_LTEE     : chtype;
    function ACS_RTEE     : chtype;
    function ACS_BTEE     : chtype;
    function ACS_TTEE     : chtype;
    function ACS_HLINE    : chtype;
    function ACS_VLINE    : chtype;
    function ACS_PLUS     : chtype;
    function ACS_S1       : chtype;
    function ACS_S9       : chtype;
    function ACS_DIAMOND  : chtype;
    function ACS_CKBOARD  : chtype;
    function ACS_DEGREE   : chtype;
    function ACS_PLMINUS  : chtype;
    function ACS_BULLET   : chtype;
    function ACS_LARROW   : chtype;
    function ACS_RARROW   : chtype;
    function ACS_DARROW   : chtype;
    function ACS_UARROW   : chtype;
    function ACS_BOARD    : chtype;
    function ACS_LANTERN  : chtype;
    function ACS_BLOCK    : chtype;
    function ACS_S3       : chtype;
    function ACS_S7       : chtype;
    function ACS_LEQUAL   : chtype;
    function ACS_GEQUAL   : chtype;
    function ACS_PI       : chtype;
    function ACS_NEQUAL   : chtype;
    function ACS_STERLING : chtype;
    {
       Line drawing ACS names are of the form ACS_trbl, where t is the top, r
       is the right, b is the bottom, and l is the left.  t, r, b, and l might
       be B (blank), S (single), D (double), or T (thick).  The subset defined
       here only uses B and S.
     }
    {
    #define ACS_BSSB    ACS_ULCORNER
    #define ACS_SSBB    ACS_LLCORNER
    #define ACS_BBSS    ACS_URCORNER
    #define ACS_SBBS    ACS_LRCORNER
    #define ACS_SBSS    ACS_RTEE
    #define ACS_SSSB    ACS_LTEE
    #define ACS_SSBS    ACS_BTEE
    #define ACS_BSSS    ACS_TTEE
    #define ACS_BSBS    ACS_HLINE
    #define ACS_SBSB    ACS_VLINE
    #define ACS_SSSS    ACS_PLUS
    }

    const
       ERR = -(1);
       OK = 0;
       _SUBWIN = $01;
       _ENDLINE = $02;
       _FULLWIN = $04;
       _SCROLLWIN = $08;
       _ISPAD = $10;
       _HASMOVED = $20;
       _WRAPPED = $40;
    {
       this value is used in the firstchar and lastchar fields to mark
       unchanged lines
     }
       _NOCHANGE = -(1);
    {
       this value is used in the oldindex field to mark lines created by insertions
       and scrolls.
     }
       _NEWINDEX = -(1);
    {
    typedef struct screen  SCREEN;
    typedef struct _win_st WINDOW;
    }

    type

       attr_t = chtype;
       ldat = record
            text : ^chtype;
            firstchar : smallint;
            lastchar : smallint;
            oldindex : smallint;
         end;

       _win_st = record
            _cury : smallint;
            _curx : smallint;
            _maxy : smallint;
            _maxx : smallint;
            _begy : smallint;
            _begx : smallint;
            _flags : smallint;
            _attrs : attr_t;
            _bkgd : chtype;
            _notimeout : bool;
            _clear : bool;
            _leaveok : bool;
            _scroll : bool;
            _idlok : bool;
            _idcok : bool;
            _immed : bool;
            _sync : bool;
            _use_keypad : bool;
            _delay : longint;
            _line : ^ldat;
            _regtop : smallint;
            _regbottom : smallint;
            _parx : longint;
            _pary : longint;
            _parent : ^WINDOW;
            _pad : record
                 _pad_y : smallint;
                 _pad_x : smallint;
                 _pad_top : smallint;
                 _pad_left : smallint;
                 _pad_bottom : smallint;
                 _pad_right : smallint;
              end;
            _yoffset : smallint;
         end;
        WINDOW = _win_st;
        PWINDOW = ^WINDOW;
       SCREEN=WINDOW;
       PSCREEN = PWINDOW;

      var
{$ifndef darwin}
       stdscr  : PWINDOW; cvar; external;
       curscr  : PWINDOW; cvar; external;
       newscr  : PWINDOW; cvar; external;
       LINES   : longint; cvar; external;
       COLS    : longint; cvar; external;
       TABSIZE : longint; cvar; external;
       ESCDELAY: longint; cvar; external;
{$else darwin}
       stdscr  : PWINDOW; external libncurses name 'stdscr';
       curscr  : PWINDOW; external libncurses name 'curscr';
       newscr  : PWINDOW; external libncurses name 'newscr';
       LINES   : longint; external libncurses name 'LINES';
       COLS    : longint; external libncurses name 'COLS';
       TABSIZE : longint; external libncurses name 'TABSIZE';
       ESCDELAY: longint; external libncurses name 'ESCDELAY';
{$endif darwin}

    function define_key(_para1:pchar; _para2:longint):longint; cdecl;external libncurses;
    function keyok(_para1:longint; _para2:bool):longint; cdecl;external libncurses;
    function resizeterm(_para1:longint; _para2:longint):longint; cdecl;external libncurses;
    function use_default_colors:longint; cdecl;external libncurses;
    function wresize(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    {
    extern char ttytype[];
    }
    function baudrate:longint; cdecl;external libncurses;
    function beep:longint; cdecl;external libncurses;
    function can_change_color:bool; cdecl;external libncurses;
    function cbreak:longint; cdecl;external libncurses;
    function clearok(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function color_content(_para1:longint; _para2:plongint; _para3:plongint; _para4:plongint):longint; cdecl;external libncurses;

    function copywin(_para1:pWINDOW; _para2:pWINDOW; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint; _para8:longint; _para9:longint):longint;cdecl;external libncurses;
    function curs_set(_para1:longint):longint; cdecl;external libncurses;
    function def_prog_mode:longint; cdecl;external libncurses;
    function def_shell_mode:longint; cdecl;external libncurses;
    function delay_output(_para1:longint):longint; cdecl;external libncurses;
    procedure delscreen(_para1:pSCREEN);cdecl;external libncurses;
    function delwin(_para1:pWINDOW):longint; cdecl;external libncurses;

    function doupdate:longint; cdecl;external libncurses;

    function echo:longint; cdecl;external libncurses;
    function endwin:longint; cdecl;external libncurses;
    function erasechar:char; cdecl;external libncurses;
    procedure filter;cdecl;external libncurses;
    function flash:longint; cdecl;external libncurses;
    function flushinp:longint; cdecl;external libncurses;

    function halfdelay(_para1:longint):longint; cdecl;external libncurses;
    function has_colors:bool; cdecl;external libncurses;
    function has_ic:longint; cdecl;external libncurses;
    function has_il:longint; cdecl;external libncurses;
    procedure idcok(_para1:pWINDOW; _para2:bool);cdecl;external libncurses;
    function idlok(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    procedure immedok(_para1:pWINDOW; _para2:bool);cdecl;external libncurses;

    function init_color(_para1:longint; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external libncurses;
    function init_pair(_para1:longint; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    function intrflush(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function isendwin:longint; cdecl;external libncurses;
    function is_linetouched(_para1:pWINDOW; _para2:longint):longint; cdecl;external libncurses;
    function is_wintouched(_para1:pWINDOW):longint; cdecl;external libncurses;


    function keypad(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function killchar:char; cdecl;external libncurses;
    function leaveok(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;

    function meta(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function mvcur(_para1:longint; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external libncurses;
    function mvderwin(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    function mvprintw(_para1:longint;_para2:longint;_para3:pchar;_para4:array of const):longint; cdecl;external libncurses;
    {
    extern int mvscanw(int,int,const char  ,...)
                GCC_SCANFLIKE(3,4);
    }
    function mvwin(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    function mvwprintw(_para1:pWINDOW;_para2,_para3:longint;_para4:pchar;_para5:array of const):longint; cdecl;external libncurses;
    {
    extern int mvwprintw(WINDOW ,int,int,const char  ,...)
                GCC_PRINTFLIKE(4,5);
    extern int mvwscanw(WINDOW  ,int,int,const char  ,...)
                GCC_SCANFLIKE(4,5);
    }
    function napms(_para1:longint):longint; cdecl;external libncurses;

    function nl:longint; cdecl;external libncurses;
    function nocbreak:longint; cdecl;external libncurses;
    function nodelay(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function noecho:longint; cdecl;external libncurses;
    function nonl:longint; cdecl;external libncurses;
    function noqiflush:longint; cdecl;external libncurses;
    function noraw:longint; cdecl;external libncurses;
    function notimeout(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;

    function overlay(_para1:pWINDOW; _para2:pWINDOW):longint; cdecl;external libncurses;

    function overwrite(_para1:pWINDOW; _para2:pWINDOW):longint; cdecl;external libncurses;
    function pair_content(_para1:longint; _para2:plongint; _para3:plongint):longint; cdecl;external libncurses;

    function pechochar(_para1:pWINDOW; _para2:chtype):longint; cdecl;external libncurses;
    function pnoutrefresh(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint):longint;cdecl;external libncurses;
    function prefresh(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint):longint;cdecl;external libncurses;
    {
    extern int printw(const char  ,...)
                GCC_PRINTFLIKE(1,2);
    }
    function putp(_para1:pchar):longint; cdecl;external libncurses;
    function putwin(_para1:pWINDOW; _para2:pFILE):longint; cdecl;external libncurses;
    function qiflush:longint; cdecl;external libncurses;
    function raw:longint; cdecl;external libncurses;
    function resetty:longint; cdecl;external libncurses;
    function reset_prog_mode:longint; cdecl;external libncurses;
    function reset_shell_mode:longint; cdecl;external libncurses;
{
    function ripoffline(_para1:longint; init:function (_para1:pWINDOW; _para2:longint):longint):longint; cdecl;external libncurses;
}
    function savetty:longint; cdecl;external libncurses;
    {
    extern int scanw(const char  ,...)
                GCC_SCANFLIKE(1,2);
    }
    function scr_dump(_para1:pchar):longint; cdecl;external libncurses;

    function scr_init(_para1:pchar):longint; cdecl;external libncurses;
    function scrollok(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;

    function scr_restore(_para1:pchar):longint; cdecl;external libncurses;
    function scr_set(_para1:pchar):longint; cdecl;external libncurses;

    function slk_attroff(_para1:attr_t):longint; cdecl;external libncurses;
    function slk_attron(_para1:attr_t):longint; cdecl;external libncurses;
    function slk_attrset(_para1:attr_t):longint; cdecl;external libncurses;
    function slk_attr:attr_t; cdecl;external libncurses;
    function slk_clear:longint; cdecl;external libncurses;
    function slk_init(_para1:longint):longint; cdecl;external libncurses;

    function slk_noutrefresh:longint; cdecl;external libncurses;
    function slk_refresh:longint; cdecl;external libncurses;
    function slk_restore:longint; cdecl;external libncurses;

    function slk_set(_para1:longint; _para2:pchar; _para3:longint):longint; cdecl;external libncurses;
    function slk_touch:longint; cdecl;external libncurses;
    function start_color:longint; cdecl;external libncurses;

    function syncok(_para1:pWINDOW; _para2:bool):longint; cdecl;external libncurses;
    function termattrs:chtype; cdecl;external libncurses;

    function tigetflag(_para1:pchar):longint; cdecl;external libncurses;

    function tigetnum(_para1:pchar):longint; cdecl;external libncurses;

    function derwin (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external libncurses;
    function dupwin (_para1:pWINDOW):PWINDOW; cdecl;external libncurses;
    function getwin (_para1:pFILE):PWINDOW; cdecl;external libncurses;
    function initscr :PWINDOW; cdecl;external libncurses;
    function keyname  (_para1:longint):pchar; cdecl;external libncurses;
    function longname :pchar; cdecl;external libncurses;
    function newpad (_para1:longint; _para2:longint):PWINDOW; cdecl;external libncurses;
    function newterm (_para1:pchar; _para2:pFILE; _para3:pFILE):PSCREEN; cdecl;external libncurses;
    function newwin  (_para1:longint; _para2:longint; _para3:longint; _para4:longint):PWINDOW; cdecl;external libncurses;
    function set_term (_para1:pSCREEN):PSCREEN; cdecl;external libncurses;
    function slk_label (_para1:longint):pchar; cdecl;external libncurses;
    function subpad (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external libncurses;
    function subwin (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external libncurses;
    function termname :pchar; cdecl;external libncurses;
    function tigetstr (_para1:pchar):pchar; cdecl;external libncurses;
    function typeahead(_para1:longint):longint; cdecl;external libncurses;
    function ungetch(_para1:longint):longint; cdecl;external libncurses;
    procedure use_env(_para1:bool);cdecl;external libncurses;
    function vidattr(_para1:chtype):longint; cdecl;external libncurses;
{
    function vidputs(_para1:chtype; _para2:function (_para1:longint):longint):longint; cdecl;external libncurses;
}
{
    function vwprintw(_para1:pWINDOW; _para2:pchar; _para3:va_list):longint; cdecl;external libncurses;
    function vwscanw(_para1:pWINDOW; _para2:pchar; _para3:va_list):longint; cdecl;external libncurses;
}
    function waddch(_para1:pWINDOW; _para2:chtype):longint; cdecl;external libncurses;
    function waddchnstr(_para1:pWINDOW; _para2:pchtype; _para3:longint):longint; cdecl;external libncurses;
    function waddnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external libncurses;
    function wattr_on(_para1:pWINDOW; _para2:attr_t):longint; cdecl;external libncurses;
    function wattr_off(_para1:pWINDOW; _para2:attr_t):longint; cdecl;external libncurses;
    function wattr_set(win : pwindow; at : longint) : longint; cdecl;external libncurses;
    function wattron(win : pwindow;at : longint) : longint; cdecl;external libncurses;
    function wattroff(win : pwindow;at : longint) : longint; cdecl;external libncurses;
    function wattrset(win : pwindow;at : longint) : longint; cdecl;external libncurses;
    function wbkgd(_para1:pWINDOW; _para2:chtype):longint; cdecl;external libncurses;
    procedure wbkgdset(_para1:pWINDOW; _para2:chtype);cdecl;external libncurses;
    function wborder(_para1:pWINDOW; _para2:chtype; _para3:chtype; _para4:chtype; _para5:chtype;
               _para6:chtype; _para7:chtype; _para8:chtype; _para9:chtype):longint;cdecl;external libncurses;
    function wchgat(_para1:pWINDOW; _para2:longint; _para3:attr_t; _para4:longint; _para5:pointer):longint; cdecl;external libncurses;
    function wclear(_para1:pWINDOW):longint; cdecl;external libncurses;
    function wclrtobot(_para1:pWINDOW):longint; cdecl;external libncurses;
    function wclrtoeol(_para1:pWINDOW):longint; cdecl;external libncurses;
    procedure wcursyncup(_para1:pWINDOW);cdecl;external libncurses;
    function wdelch(_para1:pWINDOW):longint; cdecl;external libncurses;
    function wechochar(_para1:pWINDOW; _para2:chtype):longint; cdecl;external libncurses;
    function werase(_para1:pWINDOW):longint; cdecl;external libncurses;
    function wgetch(_para1:pWINDOW):longint; cdecl;external libncurses;
    function wgetnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external libncurses;
    function whline(_para1:pWINDOW; _para2:chtype; _para3:longint):longint; cdecl;external libncurses;
    function winch (win : PWindow) : longint; cdecl;external libncurses;
    function winchnstr(_para1:pWINDOW; _para2:pchtype; _para3:longint):longint; cdecl;external libncurses;
    function winnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external libncurses;
    function winsch(_para1:pWINDOW; _para2:chtype):longint; cdecl;external libncurses;
    function winsdelln(_para1:pWINDOW; _para2:longint):longint; cdecl;external libncurses;
    function winsnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external libncurses;
    function wmove(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    function wnoutrefresh(_para1:pWINDOW):longint; cdecl;external libncurses;
    {
    extern int wprintw(WINDOW  ,const char  ,...)
                GCC_PRINTFLIKE(2,3);
    }
    function wredrawln(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    function wrefresh(_para1:pWINDOW):longint; cdecl;external libncurses;
    {
    extern int wscanw(WINDOW  ,const char  ,...)
                GCC_SCANFLIKE(2,3);
    }
    function wscrl(_para1:pWINDOW; _para2:longint):longint; cdecl;external libncurses;
    function wsetscrreg(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external libncurses;
    procedure wsyncdown(_para1:pWINDOW);cdecl;external libncurses;
    procedure wsyncup(_para1:pWINDOW);cdecl;external libncurses;
    function wtimeout(_para1:pWINDOW; _para2:longint):longint; cdecl;external libncurses;
    function wtouchln(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external libncurses;
    function wvline(_para1:pWINDOW; _para2:chtype; _para3:longint):longint; cdecl;external libncurses;
    function mvwchgat(_para1:pWINDOW; _para2:longint; _para3:longint;
                      _para4:longint; _para5:longint; _para6:longint;
                      _para7:longint):longint;cdecl;external libncurses;
    function PAIR_NUMBER(_para1:longint):longint;cdecl;external libncurses;

    const
           A_NORMAL = 0;
           A_ATTRIBUTES = (not 0) shl 8;
           A_CHARTEXT=(1 shl (0 + 8)) - 1;
           A_COLOR=((1 shl 8) - 1) shl 8;
           A_STANDOUT = 1 shl (8 + 8);
           A_UNDERLINE = 1 shl (9 + 8);
           A_REVERSE = 1 shl (10 + 8);
           A_BLINK = 1 shl (11 + 8);
           A_DIM = 1 shl (12 + 8);
           A_BOLD = 1 shl (13 + 8);
           A_ALTCHARSET = 1 shl (14 + 8);
           A_INVIS = 1 shl (15 + 8);
           A_PROTECT = 1 shl (16 + 8);
           A_HORIZONTAL = 1 shl (17 + 8);
           A_LEFT = 1 shl (18 + 8);
           A_LOW = 1 shl (19 + 8);
           A_RIGHT = 1 shl (20 + 8);
           A_TOP = 1 shl (21 + 8);
           A_VERTICAL = 1 shl (22 + 8);
     function color_pair(n : longint): longint;
{
           PAIR_NUMBER = (a(@(A_COLOR))) shr 8;
}

    {
       pseudo functions
     }
    function wgetstr(w : pwindow;s : pchar) : longint;
    function getnstr(s : pchar;n : longint) : longint;
    function setterm(term : longint) : longint;
    function fixterm : longint;
    function resetterm : longint;
    function saveterm : longint;
    function crmode : longint;
    function nocrmode : longint;
    procedure getyx   (win : pwindow; var y,x : longint);
    procedure getbegyx(win : pwindow; var y,x : longint);
    procedure getmaxyx(win : pwindow; var y,x : longint);
    procedure getparyx(win : pwindow; var y,x : longint);
    procedure getsyx  (var y,x : longint);
    procedure setsyx (y,x : longint);
    function getattrs(win : pwindow) : longint;
    function getcurx(win : pwindow) : longint;
    function getcury(win : pwindow) : longint;
    function getbegx(win : pwindow) : longint;
    function getbegy(win : pwindow) : longint;
    function getmaxx(win : pwindow) : longint;
    function getmaxy(win : pwindow) : longint;
    function getparx(win : pwindow) : longint;
    function getpary(win : pwindow) : longint;
    function wstandout(win : pwindow) : longint;
    function wstandend(win : pwindow) : longint;
{kjw, 08/24/2000, changed to cdecl; external
    function wattr_set(win : pwindow; at : longint) : longint;
    function wattron(win : pwindow;at : longint) : longint;
    function wattroff(win : pwindow;at : longint) : longint;
    function wattrset(win : pwindow;at : longint) : longint;
}
    function scroll(win : pwindow) : longint;
    function touchwin(win : pwindow) : longint;
    function touchline(win : pwindow;s,c : longint) : longint;
    function untouchwin(win : pwindow) : longint;
    function box(win : pwindow;v,h : longint) : longint;
    function border(ls,rs,ts,bs,tl,tr,bl,br : longint) : longint;
    function hline(ch,n : longint) : longint;
    function vline(ch,n : longint) : longint;
    function winstr(w : pwindow;s : pchar) : longint;
    function winchstr(w : pwindow;s : pchar) : longint;
    function winsstr(w : pwindow;s : pchar) : longint;
    function redrawwin(w : pwindow) : longint;
    function waddstr(win : pwindow;st : pchar) : longint;
    function waddchstr(win : pwindow;st : pchar) : longint;
    {
       pseudo functions for standard screen
     }
    function addch(ch : longint) : longint;
    function addchnstr(st : pchar;n : longint) : longint;
    function addchstr(st : pchar) : longint;
    function addnstr(st : pchar;n : longint) : longint;
    function addstr(st : pchar) : longint;
    function attroff(at : longint) : longint;
    function attron(at : longint) : longint;
    function attrset(at : longint) : longint;
    function bkgd(ch : longint) : longint;
    procedure bkgdset(ch : longint);
    function clear : longint;
    function clrtobot : longint;
    function clrtoeol : longint;
    function delch : longint;
    function deleteln : longint;
    function echochar(c : longint) : longint;
    function erase : longint;
    function getch : longint;
    function getstr(st : pchar) : longint;
    function inch : longint;
    function inchnstr(s : pchar;n : longint) : longint;
    function inchstr(s : pchar) : longint;
    function innstr(s : pchar;n : longint) : longint;
    function insch(c : longint) : longint;
    function insdelln(n : longint) : longint;
    function insertln : longint;
    function insnstr(s : pchar;n : longint) : longint;
    function insstr(s : pchar) : longint;
    function instr(s : pchar) : longint;
    function move(y,x : longint) : longint;
    function refresh : longint;
    function scrl(n : longint) : longint;
    function setscrreg(t,b : longint) : longint;
    function standend : longint;
    function standout : longint;
    function timeout(delay : longint) : longint;
    function wdeleteln(win : pwindow) : longint;
    function winsertln(win : pwindow) : longint;
    {
       mv functions
     }
    function mvwaddch(win : pwindow;y,x : longint; ch : chtype) : longint;
    function mvwaddchnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
    function mvwaddchstr(win : pwindow;y,x : longint;st : pchar) : longint;
    function mvwaddnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
    function mvwaddstr(win : pwindow;y,x : longint;st : pchar) : longint;
    function mvwdelch(win : pwindow;y,x : longint) : longint;
    function mvwgetch(win : pwindow;y,x : longint) : longint;
    function mvwgetnstr(win : pwindow;y,x : longint;st : pchar;n: longint) : longint;
    function mvwgetstr(win : pwindow;y,x : longint;st: pchar) : longint;
    function mvwhline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
    function mvwinch(win : pwindow;y,x : longint) : longint;
    function mvwinchnstr(win : pwindow;y,x : longint;s : pchar; n : longint) : longint;
    function mvwinchstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function mvwinnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
    function mvwinsch(win : pwindow;y,x : longint;c : chtype) : longint;
    function mvwinsnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
    function mvwinsstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function mvwinstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function mvwvline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
    function mvaddch(y,x,ch : longint) : longint;
    function mvaddchnstr(y,x : longint; st: pchar;n : longint) : longint;
    function mvaddchstr(y,x : longint; st : pchar) : longint;
    function mvaddnstr(y,x : longint; st : pchar;n : longint) : longint;
    function mvaddstr(y,x : longint; st : pchar) : longint;
    function mvdelch(y,x : longint) : longint;
    function mvgetch(y,x : longint) : longint;
    function mvgetnstr(y,x : longint; st : pchar;n : longint) : longint;
    function mvgetstr(y,x : longint; st : pchar) : longint;
    function mvhline(y,x : longint;c : chtype;n : longint) : longint;
    function mvinch(y,x : longint) : longint;
    function mvinchnstr(y,x : longint; s : pchar;n : longint) : longint;
    function mvinchstr(y,x : longint; s : pchar) : longint;
    function mvinnstr(y,x : longint; s : pchar;n : longint) : longint;
    function mvinsch(y,x: longint;c : chtype) : longint;
    function mvinsnstr(y,x : longint; s : pchar;n : longint) : longint;
    function mvinsstr(y,x : longint; s : pchar) : longint;
    function mvinstr(y,x : longint; s : pchar) : longint;
    function mvvline(y,x,c,n : longint) : longint;
    function attr_get : longint;
    function attr_off(a : longint) : longint;
    function attr_on(a : longint) : longint;
    function attr_set(a : longint) : longint;
    function chgat(n,a,c,o : longint) : longint;
    function getbkgd(win : pwindow) : longint;
    function slk_attr_off(a : longint) : longint;
    function slk_attr_on(a : longint) : longint;
    function slk_attr_set(a : longint) : longint;
    function vid_attr(a : longint) : longint;
    function wattr_get(win : pwindow) : longint;
    {
       Pseudo-character tokens outside ASCII range.  The curses wgetch() function
       will return any given one of these only if the corresponding k- capability
       is defined in your terminal's terminfo entry.
     }
    const                       {octal}
       KEY_CODE_YES = 256;      {0400}
       KEY_MIN = 257;           {0401}
       KEY_BREAK = 257;         {0401}
       KEY_DOWN = 258;          {0402}
       KEY_UP = 259;            {0403}
       KEY_LEFT = 260;          {0404}
       KEY_RIGHT = 261;         {0405}
       KEY_HOME = 262;          {0406}
       KEY_BACKSPACE = 263;     {0407}
       KEY_F0 = 264;            {0410}

    function KEY_F(n : longint) : longint;

    const
       KEY_DL = 328;            {0510}
       KEY_IL = 329;            {0511}
       KEY_DC = 330;            {0512}
       KEY_IC = 331;            {0513}
       KEY_EIC = 332;           {0514}
       KEY_CLEAR = 333;         {0515}
       KEY_EOS = 334;           {0516}
       KEY_EOL = 335;           {0517}
       KEY_SF = 336;            {0520}
       KEY_SR = 337;            {0521}
       KEY_NPAGE = 338;         {0522}
       KEY_PPAGE = 339;         {0523}
       KEY_STAB = 340;          {0524}
       KEY_CTAB = 341;          {0525}
       KEY_CATAB = 342;         {0526}
       KEY_ENTER = 343;         {0527}
       KEY_SRESET = 344;        {0530}
       KEY_RESET = 345;         {0531}
       KEY_PRINT = 346;         {0532}
       KEY_LL = 347;            {0533}
       KEY_A1 = 348;            {0534}
       KEY_A3 = 349;            {0535}
       KEY_B2 = 350;            {0536}
       KEY_C1 = 351;            {0537}
       KEY_C3 = 352;            {0540}
       KEY_BTAB = 353;          {0541}
       KEY_BEG = 354;           {0542}
       KEY_CANCEL = 355;        {0543}
       KEY_CLOSE = 356;         {0544}
       KEY_COMMAND = 357;       {0545}
       KEY_COPY = 358;          {0546}
       KEY_CREATE = 359;        {0547}
       KEY_END = 360;           {0550}
       KEY_EXIT = 361;          {0551}
       KEY_FIND = 362;          {0552}
       KEY_HELP = 363;          {0553}
       KEY_MARK = 364;          {0554}
       KEY_MESSAGE = 365;       {0555}
       KEY_MOVE = 366;          {0556}
       KEY_NEXT = 367;          {0557}
       KEY_OPEN = 368;          {0560}
       KEY_OPTIONS = 369;       {0561}
       KEY_PREVIOUS = 370;      {0562}
       KEY_REDO = 371;          {0563}
       KEY_REFERENCE = 372;     {0564}
       KEY_REFRESH = 373;       {0565}
       KEY_REPLACE = 374;       {0566}
       KEY_RESTART = 375;       {0567}
       KEY_RESUME = 376;        {0570}
       KEY_SAVE = 377;          {0571}
       KEY_SBEG = 378;          {0572}
       KEY_SCANCEL = 379;       {0573}
       KEY_SCOMMAND = 380;      {0574}
       KEY_SCOPY = 381;         {0575}
       KEY_SCREATE = 382;       {0576}
       KEY_SDC = 383;           {0577}
       KEY_SDL = 384;           {0600}
       KEY_SELECT = 385;        {0601}
       KEY_SEND = 386;          {0602}
       KEY_SEOL = 387;          {0603}
       KEY_SEXIT = 388;         {0604}
       KEY_SFIND = 389;         {0605}
       KEY_SHELP = 390;         {0606}
       KEY_SHOME = 391;         {0607}
       KEY_SIC = 392;           {0610}
       KEY_SLEFT = 393;         {0611}
       KEY_SMESSAGE = 394;      {0612}
       KEY_SMOVE = 395;         {0613}
       KEY_SNEXT = 396;         {0614}
       KEY_SOPTIONS = 397;      {0615}
       KEY_SPREVIOUS = 398;     {0616}
       KEY_SPRINT = 399;        {0617}
       KEY_SREDO = 400;         {0620}
       KEY_SREPLACE = 401;      {0621}
       KEY_SRIGHT = 402;        {0622}
       KEY_SRSUME = 403;        {0623}
       KEY_SSAVE = 404;         {0624}
       KEY_SSUSPEND = 405;      {0625}
       KEY_SUNDO = 406;         {0626}
       KEY_SUSPEND = 407;       {0627}
       KEY_UNDO = 408;          {0630}
       KEY_MOUSE = 409;         {0631}
       KEY_RESIZE = 410;        {0632}
       KEY_MAX = 511;           {0777}

    function mcprint(_para1:pchar; _para2:longint):longint;cdecl;external libncurses;
    function has_key(_para1:longint):longint;cdecl;external libncurses;

implementation

function wgetstr(w : pwindow;s : pchar) : longint;
begin
  wgetstr:=wgetnstr(w,s,-(1));
end;

function getnstr(s : pchar;n : longint) : longint;
begin
  getnstr:=wgetnstr(stdscr,s,n);
end;

function setterm(term : longint) : longint;
begin
  {
  setterm:=setupterm(term,1,plongint(0));
  }
  setterm:=0;
end;

function fixterm : longint;
begin
  fixterm:=reset_prog_mode;
end;

function resetterm : longint;
begin
  resetterm:=reset_shell_mode;
end;

function saveterm : longint;
begin
  saveterm:=def_prog_mode;
end;

function crmode : longint;
begin
  crmode:=cbreak;
end;

function nocrmode : longint;
begin
  nocrmode:=nocbreak;
end;

procedure getsyx(var y,x : longint);
begin
  getyx(stdscr,y,x);
end;

function getattrs(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>nil then
    if_local1:=win^._attrs
  else
    if_local1:=A_NORMAL;
  getattrs:=if_local1;
end;

function getcurx(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>nil then
    if_local1:=win^._curx
  else
    if_local1:=ERR;
  getcurx:=if_local1;
end;

function getcury(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=win^._cury
  else
    if_local1:=ERR;
  getcury:=if_local1;
end;
function getbegx(win : pwindow) : longint;
var
  if_local1 : longint;

begin
  if win<>Nil then
    if_local1:=win^._begx
  else
    if_local1:=ERR;
  getbegx:=if_local1;
end;

function getbegy(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=win^._begy
  else
    if_local1:=ERR;
  getbegy:=if_local1;
end;

function getmaxx(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=(win^._maxx) + 1
  else
    if_local1:=ERR;
  getmaxx:=if_local1;
end;

function getmaxy(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=(win^._maxy) + 1
  else
    if_local1:=ERR;
  getmaxy:=if_local1;
end;

function getparx(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=win^._parx
  else
    if_local1:=ERR;
  getparx:=if_local1;
end;

function getpary(win : pwindow) : longint;
var
  if_local1 : longint;
begin
  if win<>Nil then
    if_local1:=win^._pary
  else
    if_local1:=ERR;
  getpary:=if_local1;
end;

function wstandout(win : pwindow) : longint;
begin
  wstandout:=wattr_set(win,A_STANDOUT);
end;

function wstandend(win : pwindow) : longint;
begin
  wstandend:=wattr_set(win,A_NORMAL);
end;

(*
function wattron(win : pwindow;at : longint) : longint;
begin
  wattron:=wattr_on(win,at);
end;

function wattroff(win : pwindow;at : longint) : longint;
begin
  wattroff:=wattr_off(win,at);
end;

function wattrset(win : pwindow;at : longint) : longint;
begin
  wattrset:=wattr_set(win,at);
end;
*)
function scroll(win : pwindow) : longint;
begin
  scroll:=wscrl(win,1);
end;

function touchwin(win : pwindow) : longint;
begin
  touchwin:=wtouchln(win,0,getmaxy(win),1);
end;

function touchline(win : pwindow;s,c : longint) : longint;
begin
  touchline:=wtouchln(win,s,c,1);
end;

function untouchwin(win : pwindow) : longint;
begin
  untouchwin:=wtouchln(win,0,getmaxy(win),0);
end;

function box(win : pwindow;v,h : longint) : longint;
begin
  box:=wborder(win,v,v,h,h,0,0,0,0);
end;

function border(ls,rs,ts,bs,tl,tr,bl,br : longint) : longint;
begin
  border:=wborder(stdscr,ls,rs,ts,bs,tl,tr,bl,br);
end;

function hline(ch,n : longint) : longint;
begin
  hline:=whline(stdscr,ch,n);
end;

function vline(ch,n : longint) : longint;
begin
  vline:=wvline(stdscr,ch,n);
end;

function winstr(w : pwindow;s : pchar) : longint;
begin
  winstr:=winnstr(w,s,-(1));
end;

function winchstr(w : pwindow;s : pchar) : longint;
begin
  winchstr:=winchnstr(w,s,-1);
end;

function winsstr(w : pwindow;s : pchar) : longint;
begin
  winsstr:=winsnstr(w,s,-(1));
end;

function redrawwin(w : pwindow) : longint;
begin
  redrawwin:=wredrawln(w,0,(w^._maxy) + 1);
end;

function waddstr(win : pwindow;st : pchar) : longint;
begin
  waddstr:=waddnstr(win,st,-1);
end;

function waddchstr(win : pwindow;st : pchar) : longint;
begin
  waddchstr:=waddchnstr(win,st,-1);
end;

function addch(ch : longint) : longint;
begin
  addch:=waddch(stdscr,ch);
end;

function addchnstr(st : pchar;n : longint) : longint;
begin
  addchnstr:=waddchnstr(stdscr,st,n);
end;

function addchstr(st : pchar) : longint;
begin
  addchstr:=waddchstr(stdscr,st);
end;

function addnstr(st : pchar;n : longint) : longint;
begin
  addnstr:=waddnstr(stdscr,st,n);
end;

function addstr(st : pchar) : longint;
begin
  addstr:=waddnstr(stdscr,st,-1);
end;

function attroff(at : longint) : longint;
begin
  attroff:=wattroff(stdscr,at);
end;

function attron(at : longint) : longint;
begin
  attron:=wattron(stdscr,at);
end;

function attrset(at : longint) : longint;
begin
  attrset:=wattrset(stdscr,at);
end;

function bkgd(ch : longint) : longint;
begin
  bkgd:=wbkgd(stdscr,ch);
end;

procedure bkgdset(ch : longint);
begin
  wbkgdset(stdscr,ch);
end;

function clear : longint;
begin
  clear:=wclear(stdscr);
end;

function clrtobot : longint;
begin
  clrtobot:=wclrtobot(stdscr);
end;

function clrtoeol : longint;
begin
  clrtoeol:=wclrtoeol(stdscr);
end;

function delch : longint;
begin
  delch:=wdelch(stdscr);
end;

function deleteln : longint;
begin
  deleteln:=winsdelln(stdscr,-1);
end;

function echochar(c : longint) : longint;
begin
  echochar:=wechochar(stdscr,c);
end;

function erase : longint;
begin
  erase:=werase(stdscr);
end;

function getch : longint;
begin
  getch:=wgetch(stdscr);
end;

function getstr(st : pchar) : longint;
begin
  getstr:=wgetstr(stdscr,st);
end;

function inch : longint;
begin
  inch:=winch(stdscr);
end;

function inchnstr(s : pchar;n : longint) : longint;
begin
  inchnstr:=winchnstr(stdscr,s,n);
end;

function inchstr(s : pchar) : longint;
begin
  inchstr:=winchstr(stdscr,s);
end;

function innstr(s : pchar;n : longint) : longint;
begin
  innstr:=winnstr(stdscr,s,n);
end;

function insch(c : longint) : longint;
begin
  insch:=winsch(stdscr,c);
end;

function insdelln(n : longint) : longint;
begin
  insdelln:=winsdelln(stdscr,n);
end;

function insertln : longint;
begin
  insertln:=winsdelln(stdscr,1);
end;

function insnstr(s : pchar;n : longint) : longint;
begin
  insnstr:=winsnstr(stdscr,s,n);
end;

function insstr(s : pchar) : longint;
begin
  insstr:=winsstr(stdscr,s);
end;

function instr(s : pchar) : longint;
begin
  instr:=winstr(stdscr,s);
end;

function move(y,x : longint) : longint;
begin
  move:=wmove(stdscr,y,x);
end;

function refresh : longint;
begin
  refresh:=wrefresh(stdscr);
end;

function scrl(n : longint) : longint;
begin
  scrl:=wscrl(stdscr,n);
end;

function setscrreg(t,b : longint) : longint;
begin
  setscrreg:=wsetscrreg(stdscr,t,b);
end;

function standend : longint;
begin
  standend:=wstandend(stdscr);
end;

function standout : longint;
begin
  standout:=wstandout(stdscr);
end;

function timeout(delay : longint) : longint;
begin
  timeout:=wtimeout(stdscr,delay);
end;

function wdeleteln(win : pwindow) : longint;
begin
  wdeleteln:=winsdelln(win,-1);
end;

function winsertln(win : pwindow) : longint;
begin
  winsertln:=winsdelln(win,1);
end;

function mvaddch(y,x,ch : longint) : longint;
begin
  mvaddch:=mvwaddch(stdscr,y,x,ch);
end;

function mvaddchnstr(y,x: longint;st : pchar;n : longint) : longint;
begin
  mvaddchnstr:=mvwaddchnstr(stdscr,y,x,st,n);
end;

function mvaddchstr(y,x : longint;st : pchar) : longint;
begin
  mvaddchstr:=mvwaddchstr(stdscr,y,x,st);
end;

function mvaddnstr(y,x: longint;st : pchar; n : longint) : longint;
begin
  mvaddnstr:=mvwaddnstr(stdscr,y,x,st,n);
end;

function mvaddstr(y,x : longint;st : pchar) : longint;
begin
  mvaddstr:=mvwaddstr(stdscr,y,x,st);
end;

function mvdelch(y,x : longint) : longint;
begin
  mvdelch:=mvwdelch(stdscr,y,x);
end;

function mvgetch(y,x : longint) : longint;
begin
  mvgetch:=mvwgetch(stdscr,y,x);
end;

function mvgetnstr(y,x : longint;st : pchar;n : longint) : longint;
begin
  mvgetnstr:=mvwgetnstr(stdscr,y,x,st,n);
end;

function mvgetstr(y,x: longint;st : pchar) : longint;
begin
  mvgetstr:=mvwgetstr(stdscr,y,x,st);
end;

function mvhline(y,x : longint;c : chtype;n : longint) : longint;
begin
  mvhline:=mvwhline(stdscr,y,x,c,n);
end;

function mvinch(y,x : longint) : longint;
begin
  mvinch:=mvwinch(stdscr,y,x);
end;

function mvinchnstr(y,x : longint;s : pchar;n : longint) : longint;
begin
  mvinchnstr:=mvwinchnstr(stdscr,y,x,s,n);
end;

function mvinchstr(y,x : longint;s : pchar) : longint;
begin
  mvinchstr:=mvwinchstr(stdscr,y,x,s);
end;

function mvinnstr(y,x : longint;s : pchar;n : longint) : longint;
begin
  mvinnstr:=mvwinnstr(stdscr,y,x,s,n);
end;

function mvinsch(y,x: longint;c : chtype) : longint;
begin
  mvinsch:=mvwinsch(stdscr,y,x,c);
end;

function mvinsnstr(y,x : longint;s : pchar;n : longint) : longint;
begin
  mvinsnstr:=mvwinsnstr(stdscr,y,x,s,n);
end;

function mvinsstr(y,x : longint;s : pchar) : longint;
begin
  mvinsstr:=mvwinsstr(stdscr,y,x,s);
end;

function mvinstr(y,x : longint;s : pchar) : longint;
begin
  mvinstr:=mvwinstr(stdscr,y,x,s);
end;

function mvvline(y,x,c,n : longint) : longint;
begin
  mvvline:=mvwvline(stdscr,y,x,c,n);
end;

function attr_get : longint;
begin
  attr_get:=wattr_get(stdscr);
end;

function attr_off(a : longint) : longint;
begin
  attr_off:=wattr_off(stdscr,a);
end;

function attr_on(a : longint) : longint;
begin
  attr_on:=wattr_on(stdscr,a);
end;

function attr_set(a : longint) : longint;
begin
  attr_set:=wattr_set(stdscr,a);
end;

function chgat(n,a,c,o : longint) : longint;
begin
 chgat:=wchgat(stdscr,n,a,c,pointer(ptrint(o)));
end;

function getbkgd(win : pwindow) : longint;
begin
  getbkgd:=win^._bkgd;
end;

function mvchgat(y,x,n,a,c,o : longint) : longint;
begin
  mvchgat:=mvwchgat(stdscr,y,x,n,a,c,o);
end;

function slk_attr_off(a : longint) : longint;
begin
  slk_attr_off:=slk_attroff(a);
end;

function slk_attr_on(a : longint) : longint;
begin
  slk_attr_on:=slk_attron(a);
end;

function slk_attr_set(a : longint) : longint;
begin
  slk_attr_set:=slk_attrset(a);
end;

function vid_attr(a : longint) : longint;
begin
  vid_attr:=vidattr(a);
end;

function wattr_get(win : pwindow) : longint;
begin
  wattr_get:=win^._attrs;
end;

function KEY_F(n : longint) : longint;
begin
  KEY_F:=KEY_F0 + n;
end;

procedure getyx(win : pwindow; var y,x : longint);
begin
  X:=ERR;
  Y:=ERR;
  if Win<>Nil then
   begin
     Y:=win^._cury;
     X:=Win^._curx;
   end;
end;

procedure getbegyx(win : pwindow; var y,x : longint);
begin
  X:=ERR;
  Y:=ERR;
  if Win<>Nil then
   begin
     Y:=win^._begy;
     X:=Win^._begx;
   end;
end;

procedure getmaxyx(win : pwindow; var y,x : longint);
begin
  X:=ERR;
  Y:=ERR;
  if Win<>Nil then
   begin
     Y:=win^._maxy+1;
     X:=Win^._maxx+1;
   end;
end;

procedure getparyx(win : pwindow; var y,x : longint);
begin
  X:=ERR;
  Y:=ERR;
  if Win<>Nil then
   begin
     Y:=win^._pary;
     X:=Win^._parx;
   end;
end;
(* kjw, 08/23/2000, external in v4.2
function winch (win : PWindow) : longint;
begin
  if win<>nil then
   winch:=win^._line[win^._cury].text[Win^ ._curx]
  else
   winch:=0;
end;

function wattr_set(win : pwindow; at : longint) : longint;
begin
  If win<>nil then
   begin
     win^._attrs := at;
     wattr_set:=at;
   end
  else
   wattr_set:=0;
end;
*)
procedure setsyx (y,x : longint);
begin
  stdscr^._cury := y;
  stdscr^._curx := x;
end;

function  mvwaddch(win : pwindow;y,x : longint; ch : chtype) : longint;
begin
  if wmove(win,y,x) = ERR then
   exit(ERR)
  else
   exit(waddch(win,ch))
end;

function  mvwaddchnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(waddchnstr(win,st,n))
end;

function  mvwaddchstr(win : pwindow;y,x : longint;st : pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(waddchnstr(win,st,-1))
end;

function  mvwaddnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(waddnstr(win,st,n))
end;

function  mvwaddstr(win : pwindow;y,x : longint;st : pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(waddnstr(win,st,-1))
end;

function  mvwdelch(win : pwindow;y,x : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(wdelch(win))
end;

function  mvwgetch(win : pwindow;y,x : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(wgetch(win))
end;

function  mvwgetnstr(win : pwindow;y,x : longint;st : pchar;n: longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(wgetnstr(win,st,n))
end;

function  mvwgetstr(win : pwindow;y,x : longint;st: pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(wgetstr(win,st))
end;

function  mvwhline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(whline(win,c,n))
end;

function  mvwinch(win : pwindow;y,x : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winch(win))
end;

function  mvwinchnstr(win : pwindow;y,x : longint;s : pchar; n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winchnstr(win,s,n))
end;

function  mvwinchstr(win : pwindow;y,x : longint;s : pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winchstr(win,s))
end;

function  mvwinnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winnstr(win,s,n))
end;

function  mvwinsch(win : pwindow;y,x : longint;c : chtype) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winsch(win,c))
end;

function  mvwinsnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winsnstr(win,s,n))
end;

function  mvwinsstr(win : pwindow;y,x : longint;s : pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winsstr(win,s))
end;

function  mvwinstr(win : pwindow;y,x : longint;s : pchar) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(winstr(win,s))
end;

function  mvwvline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
begin
  if wmove (win,y,x)=ERR then
   exit(ERR)
  else
   exit(wvline(win,c,n))
end;

function color_pair(n : longint): longint;
begin
  color_pair:=n shl 8;
end;

function ACS_ULCORNER : chtype;
begin
  ACS_ULCORNER:=acs_map['l'];
end;

function ACS_LLCORNER : chtype;
begin
  ACS_LLCORNER:=acs_map['m'];
end;

function ACS_URCORNER : chtype;
begin
  ACS_URCORNER:=acs_map['k'];
end;

function ACS_LRCORNER : chtype;
begin
  ACS_LRCORNER:=acs_map['j'];
end;

function ACS_LTEE : chtype;
begin
  ACS_LTEE:=acs_map['t'];
end;

function ACS_RTEE : chtype;
begin
  ACS_RTEE:=acs_map['u'];
end;

function ACS_BTEE : chtype;
begin
  ACS_BTEE:=acs_map['v'];
end;

function ACS_TTEE : chtype;
begin
  ACS_TTEE:=acs_map['w'];
end;

function ACS_HLINE : chtype;
begin
  ACS_HLINE:=acs_map['q'];
end;

function ACS_VLINE : chtype;
begin
  ACS_VLINE:=acs_map['x'];
end;

function ACS_PLUS : chtype;
begin
  ACS_PLUS:=acs_map['n'];
end;

function ACS_S1      : chtype;
begin
  ACS_S1    :=acs_map['o'];
end;

function ACS_S9      : chtype;
begin
  ACS_S9    :=acs_map['s'];
end;

function ACS_DIAMOND : chtype;
begin
  ACS_DIAMOND:=acs_map['`'];
end;

function ACS_CKBOARD : chtype;
begin
  ACS_CKBOARD:=acs_map['a'];
end;

function ACS_DEGREE : chtype;
begin
  ACS_DEGREE:=acs_map['f'];
end;

function ACS_PLMINUS : chtype;
begin
  ACS_PLMINUS:=acs_map['g'];
end;

function ACS_BULLET : chtype;
begin
  ACS_BULLET:=acs_map['~'];
end;

function ACS_LARROW : chtype;
begin
  ACS_LARROW:=acs_map[','];
end;

function ACS_RARROW : chtype;
begin
  ACS_RARROW:=acs_map['+'];
end;

function ACS_DARROW : chtype;
begin
  ACS_DARROW:=acs_map['.'];
end;

function ACS_UARROW : chtype;
begin
  ACS_UARROW:=acs_map['-'];
end;

function ACS_BOARD : chtype;
begin
  ACS_BOARD:=acs_map['h'];
end;

function ACS_LANTERN : chtype;
begin
  ACS_LANTERN:=acs_map['i'];
end;

function ACS_BLOCK : chtype;
begin
  ACS_BLOCK:=acs_map['0'];
end;

function ACS_S3      : chtype;
begin
  ACS_S3    :=acs_map['p'];
end;

function ACS_S7      : chtype;
begin
  ACS_S7    :=acs_map['r'];
end;

function ACS_LEQUAL : chtype;
begin
  ACS_LEQUAL:=acs_map['y'];
end;

function ACS_GEQUAL : chtype;
begin
  ACS_GEQUAL:=acs_map['z'];
end;

function ACS_PI      : chtype;
begin
  ACS_PI    :=acs_map['{'];
end;

function ACS_NEQUAL : chtype;
begin
  ACS_NEQUAL:=acs_map['|'];
end;

function ACS_STERLING : chtype;
begin
  ACS_STERLING:=acs_map['}'];
end;

end.
