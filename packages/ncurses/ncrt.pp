Unit nCrt;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 1999
 ---------------------------------------------------------------------------
  Filename..: ncrt.pp
  Programmer: Ken J. Wright
  Date......: 03/01/99

  Purpose - Misc crt replacements & extras using ncurses.

  NOTE: Although most of the crt procedures & functions have been replaced,
  this is NOT intended as a total replacement for the crt unit. It simply
  makes the ncurses library a little easier to use in a Pascal program,
  including the most commonly used crt functions, with some familiar naming.
  This mostly eliminates the need for using crt, or ncurses directly. By
  utilizing ncurses, we get terminal independence, among other things.

  If you also need some of the functionality of crt, then just add crt to the
  uses clause of your program *before* ncrt.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  1.00 | 03/01/99 | kjw | Initial Release.
       | 03/22/99 | kjw | Added nDelWindow(), delwin() does not nil pointer.
  1.01 | 11/22/99 | kjw | Added the following: nEcho, ClrEol, ClrBot, InsLine,
                        | DelLine, Delay, nClrEol, nClrBot, nInsLine, nDelLine,
                        | nRefresh, nScroll, nDrawBox, nNewWindow, nWinColor,
                        | nWriteScr, nFrame & some functions for returning
                        | line drawing character values.
------------------------------------------------------------------------------
}
Interface

Uses linux,ncurses,strings;

Const

   { border styles for text boxes }
   btNone : integer = 0;
   btSingle : integer = 1;
   btDouble : integer = 2;

   Black        =  0;
   Blue         =  1;
   Green        =  2;
   Cyan         =  3;
   Red          =  4;
   Magenta      =  5;
   Brown        =  6;
   LightGray    =  7;
   DarkGray     =  8;
   LightBlue    =  9;
   LightGreen   = 10;
   LightCyan    = 11;
   LightRed     = 12;
   LightMagenta = 13;
   Yellow       = 14;
   White        = 15;

Type
   pwin = ^Window;

 Function StartCurses(var win : pWindow) : Boolean;
Procedure EndCurses;
Procedure ClrScr;
Procedure ClrEol;
Procedure ClrBot;
Procedure InsLine;
Procedure DelLine;
Procedure GotoXY(x,y : integer);
 Function WhereX : integer;
 Function WhereY : integer;
 Function Readkey : char;
 Function Keypressed : boolean;
Procedure Delay(DTime: Word);
Procedure nEcho(b : boolean);
Procedure nWindow(var win : pWindow; x,y,x1,y1 : integer);
Procedure nNewWindow(var win : pWindow; x,y,x1,y1 : integer);
Procedure nDelWindow(var win : pWindow);
Procedure nWinColor(win : pWindow; att : integer);
Procedure nClrScr(win : pWindow; att : integer);
Procedure nClrEol(win : pWindow);
Procedure nClrBot(win : pWindow);
Procedure nInsLine(win : pWindow);
Procedure nDelLine(win : pWindow);
Procedure nGotoXY(win : pWindow; x,y : integer);
 Function nWhereX(win : pWindow) : integer;
 Function nWhereY(win : pWindow) : integer;
 Function nReadkey(win : pWindow) : char;
 Function nReadln(win : pWindow) : string;
Procedure nWrite(win : pWindow; s : string);
Procedure nWriteln(win : pWindow; s : string);
Procedure nWriteScr(win : pWindow; x,y,att : integer; s : string);
Procedure nRefresh(win : pWindow);
Procedure nScroll(win : pWindow; lines,dir : integer);
Procedure nDrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
Procedure nFrame(win : pWindow);
 Function nHL : char; { horizontal line }
 Function nVL : char; { vertical line }
 Function nUL : char; { upper left corner }
 Function nLL : char; { lower loft corner }
 Function nUR : char; { upper right corner }
 Function nLR : char; { lower right corner }
 Function nLT : char; { left tee }
 Function nRT : char; { right tee }
 Function nTT : char; { top tee }
 Function nBT : char; { bottom tee }
 Function nPL : char; { plus, + }
 Function nLA : char; { left arrow }
 Function nRA : char; { right arrow }
 Function nUA : char; { up arror }
 Function nDA : char; { down arrow }
 Function IsBold(att : integer) : boolean;
 Function SetColorPair(att : integer) : integer;
Procedure FWrite(Col,Row,Attrib:byte;Clear:Integer;s:String);

Implementation

Var
   fg,bg : integer;
   cp : array [0..7,0..7] of integer; { color pair array }
   ps : array [0..255] of char;       { for use with pchars }

{--------------------------------------
  initialize ncurses screen & keyboard,
  return a pointer to stdscr
 --------------------------------------}
Function StartCurses(var win : pWindow) : Boolean;
Begin
   if initscr=Nil then Begin
      StartCurses := FALSE;
      halt;
   End Else Begin
      StartCurses := TRUE;
      start_color;
      cbreak; { don't buffer keyboard input }
      noecho; { don't echo kepresses }
      nonl;   { don't process cr in newline }
      intrflush(stdscr,bool(false));
      keypad(stdscr,bool(true));
      win := stdscr;
   End;

End;

{-------------------
  Shutdown ncurses
 -------------------}
Procedure EndCurses;
Begin
   echo;
   nocbreak;
   refresh;
   endwin;
End;

{ clear stdscr }
Procedure ClrScr;
Begin
   TouchWin(stdscr);
   erase;
   refresh;
End;

{ clear from the cursor to the end of line in stdscr }
Procedure ClrEol;
Begin
   clrtoeol;
   refresh;
End;

{ clear from the cursor to the bottom of stdscr }
Procedure ClrBot;
Begin
   clrtobot;
   refresh;
End;

{ insert a line at the cursor line in stdscr }
Procedure InsLine;
Begin
   insertln;
   refresh;
End;

{ delete line at the cursor in stdscr }
Procedure DelLine;
Begin
   deleteln;
   refresh;
End;

{ position cursor in stdscr }
Procedure GotoXY(x,y : integer);
Begin
   move(y-1,x-1);
   refresh;
End;

{ find cursor x position in stdscr }
Function WhereX : integer;
var x,y : longint;
Begin
   getyx(stdscr,y,x);
   WhereX := x+1;
End;

{ find cursor y position in stdscr }
Function WhereY : integer;
var x,y : longint;
Begin
   getyx(stdscr,y,x);
   WhereY := y+1;
End;

{ Wait for DTime milliseconds }
Procedure Delay(DTime: Word);
Begin
  Select(0,nil,nil,nil,DTime);
End;

{ set the echo flag }
Procedure nEcho(b : boolean);
Begin
   Case b of
      true : echo;
      false: noecho;
   End;
End;

{ create a new subwindow }
Procedure nWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := subwin(stdscr,y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
End;

{ create a new window }
Procedure nNewWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := newwin(y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
End;

{ delete a window, note this does not clear it }
Procedure nDelWindow(var win : pWindow);
Begin
   If win <> Nil Then delwin(win);
   win := Nil;
End;

{ set the color of the entire window, }
{ delayed until next refresh }
Procedure nWinColor(win : pWindow; att : integer);
Begin
   wbkgd(win,COLOR_PAIR(SetColorPair(att)));
   If IsBold(att) Then
      wattr_set(win,A_BOLD);
End;

{ clear the specified screen }
procedure nClrScr(win : pWindow; att : integer);
Begin
   wbkgd(win,COLOR_PAIR(SetColorPair(att)));
   If IsBold(att) Then
      wattr_set(win,A_BOLD);
   TouchWin(win);
   werase(win);
   wrefresh(win);
End;

{ clear from the cursor to the end of line in a window }
Procedure nClrEol(win : pWindow);
Begin
   wclrtoeol(win);
   wrefresh(win);
End;

{ clear from the cursor to the bottom in a window }
Procedure nClrBot(win : pWindow);
Begin
   wclrtobot(win);
   wrefresh(win);
End;

{ insert a line at the cursor line in a window }
Procedure nInsLine(win : pWindow);
Begin
   winsertln(win);
   wrefresh(win);
End;

{ delete line at the cursor in stdscr }
Procedure nDelLine(win : pWindow);
Begin
   wdeleteln(win);
   wrefresh(win);
End;

{ position cursor in a window }
Procedure nGotoXY(win : pWindow; x,y : integer);
Begin
   wmove(win,y-1,x-1);
   touchwin(win);
   wrefresh(win);
End;

{ find cursor x position in a window }
Function nWhereX(win : pWindow) : integer;
var x,y : longint;
Begin
   getyx(win,y,x);
   nWhereX := x+1;
End;

{ find cursor y position in a window }
Function nWhereY(win : pWindow) : integer;
var x,y : longint;
Begin
   getyx(win,y,x);
   nWhereY := y+1;
End;

{ repaint a window }
Procedure nRefresh(win : pWindow);
Begin
   touchwin(win);
   wrefresh(win);
End;

{
 Check if a key has been pressed.
 Note: this is best used along with select() on STDIN, as it can suck
 up lots of cpu time.
}
function  Keypressed : boolean;
var l : longint;
Begin
   Keypressed := FALSE;
   nodelay(stdscr,bool(TRUE));
   l := getch;
   If l <> ERR Then Begin { ERR = -(1) from unit ncurses }
      ungetch(l);
      Keypressed := TRUE;
   End;
   nodelay(stdscr,bool(FALSE));
End;

{ silently read a key from stdscr }
Function Readkey : char;
Begin
   Readkey := nReadkey(stdscr);
End;

{
 read a keystroke from a window, including function keys
 and extended keys (arrows, etc.)
 Note: Make sure that keypad(win,true) has been issued prior to use.
       ( nWindow does this )
}
Function nReadkey(win : pWindow) : char;
var
   c : char;
   l : longint;
   xtnded : boolean;
Begin
   l := wgetch(win);
   { if it's an extended key, then map to the IBM values }
   if l > 255 then begin
      xtnded := true;
      c := #27;
      Case l of
         KEY_BREAK : Begin xtnded := false; c := #3; End;
         KEY_BACKSPACE : Begin xtnded := false; c := #8; End;
         KEY_IC    : c := #82; { insert }
         KEY_DC    : c := #83; { delete }
         KEY_HOME  : c := #71; { home }
         KEY_END   : c := #79; { end }
         KEY_UP    : c := #72; { up arrow }
         KEY_DOWN  : c := #80; { down arrow }
         KEY_LEFT  : c := #75; { left arrow }
         KEY_RIGHT : c := #77; { right arrow }
         KEY_NPAGE : c := #81; { page down }
         KEY_PPAGE : c := #73; { page up }
      Else
         Begin
            If l = Key_f(1) Then c := #59 Else
            If l = Key_f(2) Then c := #60 Else
            If l = Key_f(3) Then c := #61 Else
            If l = Key_f(4) Then c := #62 Else
            If l = Key_f(5) Then c := #63 Else
            If l = Key_f(6) Then c := #64 Else
            If l = Key_f(7) Then c := #65 Else
            If l = Key_f(8) Then c := #66 Else
            If l = Key_f(9) Then c := #67 Else
            If l = Key_f(10) Then c := #68;
         End;
      End;
      If xtnded Then Begin
         nReadKey := #0;
         ungetch(ord(c));
         Exit;
      End Else
         nReadkey := c;
   End Else
      nReadkey := chr(ord(l));
End;

{ read input string from a window }
{ note: by default, echo is false }
Function nReadln(win : pWindow) : string;
Begin
   wgetstr(win,ps);
   nReadln := StrPas(ps);
End;

{ write a string to a window at the current cursor position }
Procedure nWrite(win : pWindow; s : string);
Begin
   waddstr(win,StrPCopy(ps,s));
   wrefresh(win);
End;

{ write a string to a window at the current cursor position }
{ followed by a newline }
Procedure nWriteln(win : pWindow; s : string);
Begin
   waddstr(win,StrPCopy(ps,s+#10));
   wrefresh(win);
End;

{ write a string to a window without refreshing screen }
Procedure nWriteScr(win : pWindow; x,y,att : integer; s : string);
Var
   xx,yy,
   cp : longint;
Begin
   cp := SetColorPair(att);
   { write string with current attributes }
   mvwaddstr(win,y-1,x-1,StrPCopy(ps,s));
   { save the new cursor position }
   getyx(win,yy,xx);
   { update with new attributes }
   If IsBold(att) Then
      mvwchgat(win,y-1,x-1,-1,A_BOLD,cp,0)
   Else
      mvwchgat(win,y-1,x-1,-1,A_NORMAL,cp,0);
   { return cursor to saved position }
   wmove(win,yy,xx);
End;

{ scroll a window, up or down, a specified number of lines }
Procedure nScroll(win : pwindow; lines,dir : integer);
var i : integer;
Begin
   ScrollOk(win,bool(True));
   For i := 1 to lines Do Begin
      wscrl(win,dir);
   End;
   wRefresh(win);
End;

{ draw a colored box, with or without a border }
Procedure nDrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
Var
   win : pWindow;
Begin
   win := SubWin(stdscr,y2-y1+1,x2-x1+1,y1-1,x1-1);
   If win = nil Then Begin
      write('drawbox: could not allocate window: ',
            (y2-y1+1),',',(x2-x1+1),',',(y1-1),',',(x1-1));
      exit;
   end;
   wbkgd(win,COLOR_PAIR(SetColorPair(att)));
   If IsBold(att) Then
      wattr_set(win,A_BOLD);
   werase(win);
   case LineStyle of
      1,2 : box(win, ACS_VLINE, ACS_HLINE);
   End;
   wrefresh(win);
   nDelWindow(win);
End;

{ add a border to a window }
{ waits for a refresh }
Procedure nFrame(win : pWindow);
Begin
   box(win, ACS_VLINE, ACS_HLINE);
End;

Function nHL : char;
Begin
   nHL := char(ACS_HLINE);
End;

Function nVL : char;
Begin
   nVL := char(ACS_VLINE);
End;

Function nUL : char;
Begin
   nUL := char(ACS_ULCORNER);
End;

Function nLL : char;
Begin
   nLL := char(ACS_LLCORNER);
End;

Function nUR : char;
Begin
   nUR := char(ACS_URCORNER);
End;

Function nLR : char;
Begin
   nLR := char(ACS_LRCORNER);
End;

Function nLT : char;
Begin
   nLT := char(ACS_LTEE);
End;

Function nRT : char;
Begin
   nRT := char(ACS_RTEE);
End;

Function nTT : char;
Begin
   nTT := char(ACS_TTEE);
End;

Function nBT : char;
Begin
   nBT := char(ACS_BTEE);
End;

Function nPL : char;
Begin
   nPL := char(ACS_PLUS);
End;

Function nLA : char;
Begin
   nLA := char(ACS_LARROW);
End;

Function nRA : char;
Begin
   nRA := char(ACS_RARROW);
End;

Function nUA : char;
Begin
   nUA := char(ACS_UARROW);
End;

Function nDA : char;
Begin
   nDA := char(ACS_DARROW);
End;

{ see if the specified attribute is high intensity, }
{ used by fwrite() }
Function IsBold(att : integer) : boolean;
Begin
   bg := att div 16;
   fg := att - ((att div 16) * 16);
   isbold := (fg > 7);
End;

{ initialize a color pair, used by fwrite() }
Function SetColorPair(att : integer) : integer;
var
   i : integer;
{ ncurses constants
   COLOR_BLACK   = 0;
   COLOR_RED     = 1;
   COLOR_GREEN   = 2;
   COLOR_YELLOW  = 3;
   COLOR_BLUE    = 4;
   COLOR_MAGENTA = 5;
   COLOR_CYAN    = 6;
   COLOR_WHITE   = 7;
}
Begin
   bg := att div 16;
   fg := att - ((att div 16) * 16);
   While bg > 7 Do dec(bg,8);
   While fg > 7 Do dec(fg,8);
   { map to ncurses color values }
   case bg of
      0 : bg := COLOR_BLACK;
      1 : bg := COLOR_BLUE;
      2 : bg := COLOR_GREEN;
      3 : bg := COLOR_CYAN;
      4 : bg := COLOR_RED;
      5 : bg := COLOR_MAGENTA;
      6 : bg := COLOR_YELLOW;
      7 : bg := COLOR_WHITE;
   end;
   case fg of
      0 : fg := COLOR_BLACK;
      1 : fg := COLOR_BLUE;
      2 : fg := COLOR_GREEN;
      3 : fg := COLOR_CYAN;
      4 : fg := COLOR_RED;
      5 : fg := COLOR_MAGENTA;
      6 : fg := COLOR_YELLOW;
      7 : fg := COLOR_WHITE;
   end;
   i := cp[bg,fg];
   init_pair(i,fg,bg);
   SetColorPair := i;
End;

{---------------------------------------------------------------
 write a string to stdscr with color, without moving the cursor

   Col    = x position
   Row    = y position
   Attrib = color (0..127)
   Clear  = clear line up to x position
   s      = string to write
 ---------------------------------------------------------------}
procedure FWrite(Col,Row,Attrib:byte;Clear:Integer;s:String);
Const
   ClearLine = { Following line is 80 Spaces }
'                                                                                ';

Var
   cs : string;
   win : pWindow;
Begin
   if Clear > 0 Then Begin
      If Clear > 80 Then Clear := 80;
      cs := Copy(ClearLine,1,(Clear-Col)-Length(s)+1);
   End Else
      cs := '';
   s := s+cs;
   If s = '' Then Exit;
   win := subwin(stdscr,1,Length(s),row-1,col-1);
   If win = nil Then Begin
      s := ' FWrite: failed to create sub-window for '+s;
      write(s,':',length(s));
      Exit;
   End;
   wbkgd(win,COLOR_PAIR(SetColorPair(Attrib)));
   If isbold(Attrib) then
      wattr_set(win,A_BOLD);
   mvwaddstr(win,0,0,StrPCopy(ps,s));
   wrefresh(win);
   delwin(win);
   refresh;
End;

Begin
   { load the color pairs array with color pair indices (0..63) }
   For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
End. { of Unit nCrt }
