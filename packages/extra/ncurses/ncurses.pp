{
    $Id$
    Copyright (c) 1998 by Michael Van Canneyt
    member of the Free Pascal development team

    Unit to access the ncurses library

    See the file COPYING.FPC included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Many thanks to Ken Wright for his patches !
}
unit ncurses;
interface

{$packrecords C}
{$ifdef OpenBSD}		// openbsd curses=ncurses. Openbsd ocurses=old curses.
{$linklib curses}
{$else}
{$linklib ncurses}
{$endif}
{$linklib c}

{ Manually Added types }
Type
  Bool = byte;
  PINTEGER = ^Longint;
  PLongint = ^ longint;
  PFILE = pointer;

const
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

Var
    COLORS : longint; cvar; external;
    COLOR_PAIRS : longint; cvar; external;

    const
       COLOR_BLACK = 0;
       COLOR_RED = 1;
       COLOR_GREEN = 2;
       COLOR_YELLOW = 3;
       COLOR_BLUE = 4;
       COLOR_MAGENTA = 5;
       COLOR_CYAN = 6;
       COLOR_WHITE = 7;

Type
    tacs_map = array [char] of chtype;
    pacs_map = ^tacs_map;

Var
    acs_map : tacs_map; cvar; external;

    function ACS_ULCORNER : chtype;
    function ACS_LLCORNER : chtype;
    function ACS_URCORNER : chtype;
    function ACS_LRCORNER : chtype;
    function ACS_LTEE : chtype;
    function ACS_RTEE : chtype;
    function ACS_BTEE : chtype;
    function ACS_TTEE : chtype;
    function ACS_HLINE : chtype;
    function ACS_VLINE : chtype;
    function ACS_PLUS : chtype;
    function ACS_S1      : chtype;
    function ACS_S9      : chtype;
    function ACS_DIAMOND : chtype;
    function ACS_CKBOARD : chtype;
    function ACS_DEGREE : chtype;
    function ACS_PLMINUS : chtype;
    function ACS_BULLET : chtype;
    function ACS_LARROW : chtype;
    function ACS_RARROW : chtype;
    function ACS_DARROW : chtype;
    function ACS_UARROW : chtype;
    function ACS_BOARD : chtype;
    function ACS_LANTERN : chtype;
    function ACS_BLOCK : chtype;
    function ACS_S3      : chtype;
    function ACS_S7      : chtype;
    function ACS_LEQUAL : chtype;
    function ACS_GEQUAL : chtype;
    function ACS_PI      : chtype;
    function ACS_NEQUAL : chtype;
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
    Const
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
            firstchar : integer;
            lastchar : integer;
            oldindex : integer;
         end;

       _win_st = record
            _cury : integer;
            _curx : integer;
            _maxy : integer;
            _maxx : integer;
            _begy : integer;
            _begx : integer;
            _flags : integer;
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
            _regtop : integer;
            _regbottom : integer;
            _parx : longint;
            _pary : longint;
            _parent : ^WINDOW;
            _pad : record
                 _pad_y : integer;
                 _pad_x : integer;
                 _pad_top : integer;
                 _pad_left : integer;
                 _pad_bottom : integer;
                 _pad_right : integer;
              end;
            _yoffset : integer;
         end;
        WINDOW = _win_st;
        PWINDOW = ^WINDOW;
       SCREEN=WINDOW;
       PSCREEN = PWINDOW;

      var
       stdscr  : PWINDOW; cvar; external;
       curscr  : PWINDOW; cvar; external;
       newscr  : PWINDOW; cvar; external;
       LINES   : longint; cvar; external; 
       COLS    : longint; cvar; external; 
       TABSIZE : longint; cvar; external;
       ESCDELAY: longint; cvar; external;

    Function define_key(_para1:pchar; _para2:longint):longint; cdecl;external;
    Function keyok(_para1:longint; _para2:bool):longint; cdecl;external;
    Function resizeterm(_para1:longint; _para2:longint):longint; cdecl;external;
    Function use_default_colors:longint; cdecl;external;
    Function wresize(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    {
    extern char ttytype[];
    }
    Function baudrate:longint; cdecl;external;
    Function beep:longint; cdecl;external;
    Function can_change_color:bool; cdecl;external;
    Function cbreak:longint; cdecl;external;
    Function clearok(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function color_content(_para1:longint; _para2:plongint; _para3:plongint; _para4:plongint):longint; cdecl;external;

    Function copywin(_para1:pWINDOW; _para2:pWINDOW; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint; _para8:longint; _para9:longint):longint;cdecl;external;
    Function curs_set(_para1:longint):longint; cdecl;external;
    Function def_prog_mode:longint; cdecl;external;
    Function def_shell_mode:longint; cdecl;external;
    Function delay_output(_para1:longint):longint; cdecl;external;
    procedure delscreen(_para1:pSCREEN);cdecl;external;
    Function delwin(_para1:pWINDOW):longint; cdecl;external;

    Function doupdate:longint; cdecl;external;

    Function echo:longint; cdecl;external;
    Function endwin:longint; cdecl;external;
    Function erasechar:char; cdecl;external;
    procedure filter;cdecl;external;
    Function flash:longint; cdecl;external;
    Function flushinp:longint; cdecl;external;

    Function halfdelay(_para1:longint):longint; cdecl;external;
    Function has_colors:bool; cdecl;external;
    Function has_ic:longint; cdecl;external;
    Function has_il:longint; cdecl;external;
    procedure idcok(_para1:pWINDOW; _para2:bool);cdecl;external;
    Function idlok(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    procedure immedok(_para1:pWINDOW; _para2:bool);cdecl;external;

    Function init_color(_para1:longint; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external;
    Function init_pair(_para1:longint; _para2:longint; _para3:longint):longint; cdecl;external;
    Function intrflush(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function isendwin:longint; cdecl;external;
    Function is_linetouched(_para1:pWINDOW; _para2:longint):longint; cdecl;external;
    Function is_wintouched(_para1:pWINDOW):longint; cdecl;external;


    Function keypad(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function killchar:char; cdecl;external;
    Function leaveok(_para1:pWINDOW; _para2:bool):longint; cdecl;external;

    Function meta(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function mvcur(_para1:longint; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external;
    Function mvderwin(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    {
    extern int mvprintw(int,int,const char  ,...)
                GCC_PRINTFLIKE(3,4);
    extern int mvscanw(int,int,const char  ,...)
                GCC_SCANFLIKE(3,4);
    }
    Function mvwin(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    {
    extern int mvwprintw(WINDOW ,int,int,const char  ,...)
                GCC_PRINTFLIKE(4,5);
    extern int mvwscanw(WINDOW  ,int,int,const char  ,...)
                GCC_SCANFLIKE(4,5);
    }
    Function napms(_para1:longint):longint; cdecl;external;

    Function nl:longint; cdecl;external;
    Function nocbreak:longint; cdecl;external;
    Function nodelay(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function noecho:longint; cdecl;external;
    Function nonl:longint; cdecl;external;
    Function noqiflush:longint; cdecl;external;
    Function noraw:longint; cdecl;external;
    Function notimeout(_para1:pWINDOW; _para2:bool):longint; cdecl;external;

    Function overlay(_para1:pWINDOW; _para2:pWINDOW):longint; cdecl;external;

    Function overwrite(_para1:pWINDOW; _para2:pWINDOW):longint; cdecl;external;
    Function pair_content(_para1:longint; _para2:plongint; _para3:plongint):longint; cdecl;external;

    Function pechochar(_para1:pWINDOW; _para2:chtype):longint; cdecl;external;
    Function pnoutrefresh(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint):longint;cdecl;external;
    Function prefresh(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
               _para6:longint; _para7:longint):longint;cdecl;external;
    {
    extern int printw(const char  ,...)
                GCC_PRINTFLIKE(1,2);
    }
    Function putp(_para1:pchar):longint; cdecl;external;
    Function putwin(_para1:pWINDOW; _para2:pFILE):longint; cdecl;external;
    Function qiflush:longint; cdecl;external;
    Function raw:longint; cdecl;external;
    Function resetty:longint; cdecl;external;
    Function reset_prog_mode:longint; cdecl;external;
    Function reset_shell_mode:longint; cdecl;external;
{
    Function ripoffline(_para1:longint; init:function (_para1:pWINDOW; _para2:longint):longint):longint; cdecl;external;
}
    Function savetty:longint; cdecl;external;
    {
    extern int scanw(const char  ,...)
                GCC_SCANFLIKE(1,2);
    }
    Function scr_dump(_para1:pchar):longint; cdecl;external;

    Function scr_init(_para1:pchar):longint; cdecl;external;
    Function scrollok(_para1:pWINDOW; _para2:bool):longint; cdecl;external;

    Function scr_restore(_para1:pchar):longint; cdecl;external;

    Function scr_set(_para1:pchar):longint; cdecl;external;


    Function slk_attroff(_para1:attr_t):longint; cdecl;external;

    Function slk_attron(_para1:attr_t):longint; cdecl;external;

    Function slk_attrset(_para1:attr_t):longint; cdecl;external;
    Function slk_attr:attr_t; cdecl;external;
    Function slk_clear:longint; cdecl;external;
    Function slk_init(_para1:longint):longint; cdecl;external;

    Function slk_noutrefresh:longint; cdecl;external;
    Function slk_refresh:longint; cdecl;external;
    Function slk_restore:longint; cdecl;external;

    Function slk_set(_para1:longint; _para2:pchar; _para3:longint):longint; cdecl;external;
    Function slk_touch:longint; cdecl;external;
    Function start_color:longint; cdecl;external;

    Function syncok(_para1:pWINDOW; _para2:bool):longint; cdecl;external;
    Function termattrs:chtype; cdecl;external;

    Function tigetflag(_para1:pchar):longint; cdecl;external;

    Function tigetnum(_para1:pchar):longint; cdecl;external;

    Function derwin (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external;
    Function dupwin (_para1:pWINDOW):PWINDOW; cdecl;external;
    Function getwin (_para1:pFILE):PWINDOW; cdecl;external;
    Function initscr :PWINDOW; cdecl;external;
    Function keyname  (_para1:longint):pchar; cdecl;external;
    Function longname :pchar; cdecl;external;
    Function newpad (_para1:longint; _para2:longint):PWINDOW; cdecl;external;
    Function newterm (_para1:pchar; _para2:pFILE; _para3:pFILE):PSCREEN; cdecl;external;
    Function newwin  (_para1:longint; _para2:longint; _para3:longint; _para4:longint):PWINDOW; cdecl;external;
    Function set_term (_para1:pSCREEN):PSCREEN; cdecl;external;
    Function slk_label (_para1:longint):pchar; cdecl;external;
    Function subpad (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external;
    Function subwin (_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint; _para5:longint):PWINDOW; cdecl;external;
    Function termname :pchar; cdecl;external;
    Function tigetstr (_para1:pchar):pchar; cdecl;external;
    Function typeahead(_para1:longint):longint; cdecl;external;
    Function ungetch(_para1:longint):longint; cdecl;external;
    procedure use_env(_para1:bool);cdecl;external;
    Function vidattr(_para1:chtype):longint; cdecl;external;
{
    Function vidputs(_para1:chtype; _para2:function (_para1:longint):longint):longint; cdecl;external;
}
{
    Function vwprintw(_para1:pWINDOW; _para2:pchar; _para3:va_list):longint; cdecl;external;
    Function vwscanw(_para1:pWINDOW; _para2:pchar; _para3:va_list):longint; cdecl;external;
}
    Function waddch(_para1:pWINDOW; _para2:chtype):longint; cdecl;external;
    Function waddchnstr(_para1:pWINDOW; _para2:pchtype; _para3:longint):longint; cdecl;external;
    Function waddnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external;
    Function wattr_on(_para1:pWINDOW; _para2:attr_t):longint; cdecl;external;
    Function wattr_off(_para1:pWINDOW; _para2:attr_t):longint; cdecl;external;
    Function wattr_set(win : pwindow; at : longint) : longint; cdecl;external;
    function wattron(win : pwindow;at : longint) : longint; cdecl;external;
    function wattroff(win : pwindow;at : longint) : longint; cdecl;external;
    function wattrset(win : pwindow;at : longint) : longint; cdecl;external;
    Function wbkgd(_para1:pWINDOW; _para2:chtype):longint; cdecl;external;
    procedure wbkgdset(_para1:pWINDOW; _para2:chtype);cdecl;external;
    Function wborder(_para1:pWINDOW; _para2:chtype; _para3:chtype; _para4:chtype; _para5:chtype;
               _para6:chtype; _para7:chtype; _para8:chtype; _para9:chtype):longint;cdecl;external;
    Function wchgat(_para1:pWINDOW; _para2:longint; _para3:attr_t; _para4:longint; _para5:pointer):longint; cdecl;external;
    Function wclear(_para1:pWINDOW):longint; cdecl;external;
    Function wclrtobot(_para1:pWINDOW):longint; cdecl;external;
    Function wclrtoeol(_para1:pWINDOW):longint; cdecl;external;
    procedure wcursyncup(_para1:pWINDOW);cdecl;external;
    Function wdelch(_para1:pWINDOW):longint; cdecl;external;
    Function wechochar(_para1:pWINDOW; _para2:chtype):longint; cdecl;external;
    Function werase(_para1:pWINDOW):longint; cdecl;external;
    Function wgetch(_para1:pWINDOW):longint; cdecl;external;
    Function wgetnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external;
    Function whline(_para1:pWINDOW; _para2:chtype; _para3:longint):longint; cdecl;external;
    Function winch (win : PWindow) : longint; cdecl;external;
    Function winchnstr(_para1:pWINDOW; _para2:pchtype; _para3:longint):longint; cdecl;external;
    Function winnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external;
    Function winsch(_para1:pWINDOW; _para2:chtype):longint; cdecl;external;
    Function winsdelln(_para1:pWINDOW; _para2:longint):longint; cdecl;external;
    Function winsnstr(_para1:pWINDOW; _para2:pchar; _para3:longint):longint; cdecl;external;
    Function wmove(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    Function wnoutrefresh(_para1:pWINDOW):longint; cdecl;external;
    {
    extern int wprintw(WINDOW  ,const char  ,...)
                GCC_PRINTFLIKE(2,3);
    }
    Function wredrawln(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    Function wrefresh(_para1:pWINDOW):longint; cdecl;external;
    {
    extern int wscanw(WINDOW  ,const char  ,...)
                GCC_SCANFLIKE(2,3);
    }
    Function wscrl(_para1:pWINDOW; _para2:longint):longint; cdecl;external;
    Function wsetscrreg(_para1:pWINDOW; _para2:longint; _para3:longint):longint; cdecl;external;
    procedure wsyncdown(_para1:pWINDOW);cdecl;external;
    procedure wsyncup(_para1:pWINDOW);cdecl;external;
    Function wtimeout(_para1:pWINDOW; _para2:longint):longint; cdecl;external;
    Function wtouchln(_para1:pWINDOW; _para2:longint; _para3:longint; _para4:longint):longint; cdecl;external;
    Function wvline(_para1:pWINDOW; _para2:chtype; _para3:longint):longint; cdecl;external;
    Function mvwchgat(_para1:pWINDOW; _para2:longint; _para3:longint;
                      _para4:longint; _para5:longint; _para6:longint;
                      _para7:longint):longint;cdecl;external;
    Function PAIR_NUMBER(_para1:longint):longint;cdecl;external;

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
     Function color_pair(n : longint): longint;
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
    Procedure getsyx  (var y,x : longint);
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
    function  mvwaddch(win : pwindow;y,x : longint; ch : chtype) : longint;
    function  mvwaddchnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
    function  mvwaddchstr(win : pwindow;y,x : longint;st : pchar) : longint;
    function  mvwaddnstr(win : pwindow;y,x : longint;st : pchar;n : longint) : longint;
    function  mvwaddstr(win : pwindow;y,x : longint;st : pchar) : longint;
    function  mvwdelch(win : pwindow;y,x : longint) : longint;
    function  mvwgetch(win : pwindow;y,x : longint) : longint;
    function  mvwgetnstr(win : pwindow;y,x : longint;st : pchar;n: longint) : longint;
    function  mvwgetstr(win : pwindow;y,x : longint;st: pchar) : longint;
    function  mvwhline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
    function  mvwinch(win : pwindow;y,x : longint) : longint;
    function  mvwinchnstr(win : pwindow;y,x : longint;s : pchar; n : longint) : longint;
    function  mvwinchstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function  mvwinnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
    function  mvwinsch(win : pwindow;y,x : longint;c : chtype) : longint;
    function  mvwinsnstr(win : pwindow;y,x : longint;s : pchar;n : longint) : longint;
    function  mvwinsstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function  mvwinstr(win : pwindow;y,x : longint;s : pchar) : longint;
    function  mvwvline(win : pwindow;y,x : longint;c : chtype;n : longint) : longint;
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

    function mcprint(_para1:pchar; _para2:longint):longint;cdecl;external;
    function has_key(_para1:longint):longint;cdecl;external;

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
 chgat:=wchgat(stdscr,n,a,c,pointer(o));
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
{
  $Log$
  Revision 1.5  2003-02-09 19:10:13  marco
   OpenBSD links curses, not ncurses

  Revision 1.4  2003/02/09 19:05:05  marco
   * And now with new refreshing "external;" added.

  Revision 1.3  2003/02/09 16:22:51  marco
   * xx : thetype; external name ='xx'; to xx :thetype; cvar;

  Revision 1.2  2002/09/07 15:43:01  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:17  peter
    * splitted to base and extra

}
