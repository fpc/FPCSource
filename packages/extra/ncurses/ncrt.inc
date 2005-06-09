{---------------------------------------------------------------------------
                                 CncWare
                         (c) Copyright 1999-2000
                   Portions copyright the FreePascal Team
 ---------------------------------------------------------------------------
  Filename..: ncrt.inc
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 03/01/99

  Purpose - Code that is common to nCrt and oCrt.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  2.00 | 12/13/99 | kjw | Initial Release.
  2.02 | 12/15/99 | kjw | Removed use of endwin. Replaced with tcget/setattr.
  2.03 | 12/16/99 | kjw | 1) Added shifted f-keys to nReadkey.
                        | 2) Added raw & scrollok to StartCurses.
                        | 3) Added alt'd keyset support.
  2.04 | 01/04/00 | kjw | keypressed changed back to method of using getch
                        | rather than select.
  2.05 | 01/06/00 | kjw | 1) StartCurses now defaults to echo. Readkey sets to
                        | noecho. This allows nCrt to handle echoing in the
                        | default manor, but allows oCrt to control echoing
                        | in the app with nEcho. Note: Read(ln) will always
                        | echo as normal, regardless of any setting by nEcho.
                        | Also set DoRefresh to true.
                        | 2) nDelWindow now checks for stdscr or curscr and
                        | makes sure that ActiveWn is not nil.
                        | 3) Window() now moves to 1,1 and does not do a
                        | clrscr.
  2.06 | 01/11/00 | kjw | 1) Oops! 2.04 change went back to stdscr vs. ActiveWn.
                        | Keypressed works correctly with windows again.
                        | 2) ClrEol works correctly now with color.
  2.07 | 01/31/00 | kjw | 1) Added NCRT_VERSION constants.
                        | 2) Added prev_textattr to detect a change in
                        | TextAttr value so current color gets updated.
                        | 3) See ocrt.pp
  2.08 | 06/09/00 | kjw | See ocrt.pp

  2.08.01 | 06/11/2000 | kjw | See ocrt.pp
  2.09.00 | 06/16/2000 | kjw | See ocrt.pp
  2.10.00 | 06/16/2000 | kjw | See ocrt.pp
  2.11.00 | 06/27/2000 | kjw
          | 1) See ocrt.pp
          | 2) Now uses ncurses for CrtRead so console control characters
          | work correctly (i.e., <ctrl/h>, <backspace>, etc.).
  2.12.00 | 06/29/2000 | kjw | See ocrt.pp
  2.13.00 | 06/30/2000 | kjw
          | Added nStop and nStart procedures.
  2.14.00 | 07/05/2000 | kjw
          | 1) Added nCursor and nEscDelay functions.
          | 2) Added nInit and moved code from ncrt.pp & ocrt.pp to it.
          | 3) KEY_ALTMINUS & KEYALTEQUAL were reversed, but mapping ended
          | up correct.
  2.15.00 | 1) Added nMaxRows & nMaxCols constants.
          | 2) See ocrt.pp
  2.16.00 | 08/14/2000 | kjw | See ocrt.pp
          | 08/24/2000 | kjw |
          | 1) Added nTermName.
          | 2) Added CursesFailed.
          | 3) Moved all common initialization code to nInit.
          | 4) prev_textattr more reliable.
------------------------------------------------------------------------------
}

Procedure AssignCrt(var F: Text);
Procedure ClrEol;
Procedure ClrScr;
Procedure ClrBot;
Procedure Delay(DTime: Word);
Procedure DelLine;
Procedure GotoXY(x,y : integer);
Procedure HighVideo;
Procedure InsLine;
 Function Keypressed : boolean;
Procedure LowVideo;
Procedure NormVideo;
Procedure NoSound;
 Function Readkey : char;
Procedure Sound(hz : word);
Procedure TextBackground(att : byte);
Procedure TextColor(att : byte);
Procedure TextMode(mode : word);
 Function WhereX : integer;
 Function WhereY : integer;
Procedure Window(x,y,x1,y1 : integer);
Procedure nStop;
Procedure nStart;
 Function nCursor(c : integer) : integer;
 Function nEscDelay(d : longint) : longint;
 Function nTermName : string;

Const

   NCRT_VERSION_MAJOR = 2;
   NCRT_VERSION_MINOR = 16;
   NCRT_VERSION_PATCH = 0;
   NCRT_VERSION = '2.16.00';

 { CRT modes }
   BW40          = 0;            { 40x25 B/W on Color Adapter }
   CO40          = 1;            { 40x25 Color on Color Adapter }
   BW80          = 2;            { 80x25 B/W on Color Adapter }
   CO80          = 3;            { 80x25 Color on Color Adapter }
   Mono          = 7;            { 80x25 on Monochrome Adapter }
   Font8x8       = 256;          { Add-in for ROM font }

 { Mode constants for 3.0 compatibility }
   C40           = CO40;
   C80           = CO80;

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
   Blink        = 128;

   TextAttr : Byte = $07;
   LastMode : Word = 3;
   WindMin  : Word = $0;
   WindMax  : Word = $184f;

   { support for the alt'd characters }
   { these get initialized by StartCurses }
   KEY_ALTA = 465; { alt/a }
   KEY_ALTB = 466;
   KEY_ALTC = 467;
   KEY_ALTD = 468;
   KEY_ALTE = 469;
   KEY_ALTF = 470;
   KEY_ALTG = 471;
   KEY_ALTH = 472;
   KEY_ALTI = 473;
   KEY_ALTJ = 474;
   KEY_ALTK = 475;
   KEY_ALTL = 476;
   KEY_ALTM = 477;
   KEY_ALTN = 478;
   KEY_ALTO = 479;
   KEY_ALTP = 480;
   KEY_ALTQ = 481;
   KEY_ALTR = 482;
   KEY_ALTS = 483;
   KEY_ALTT = 484;
   KEY_ALTU = 485;
   KEY_ALTV = 486;
   KEY_ALTW = 487;
   KEY_ALTX = 488;
   KEY_ALTY = 489;
   KEY_ALTZ = 490; { alt/z }
   KEY_ALT1 = 491; { alt/1 }
   KEY_ALT2 = 492; { alt/2 }
   KEY_ALT3 = 493; { alt/3 }
   KEY_ALT4 = 494; { alt/4 }
   KEY_ALT5 = 495; { alt/5 }
   KEY_ALT6 = 496; { alt/6 }
   KEY_ALT7 = 497; { alt/7 }
   KEY_ALT8 = 498; { alt/8 }
   KEY_ALT9 = 499; { alt/9 }
   KEY_ALT0 = 500; { alt/0 }
   KEY_ALTMINUS = 501; { alt/- }
   KEY_ALTEQUAL = 502; { alt/= }
   KEY_ALTTAB   = 503; { alt/tab }

   { cursor type }
   cOFF = 0; { invisible cursor }
   cON  = 1; { normal cursor }
   cBIG = 2; { very visible cursor }

   { fullscreen size }
   nMaxRows : word = 25; { reset at startup to terminal setting }
   nMaxCols : word = 80; { for columns and rows }

 var
   CheckBreak,
   CheckEOF,
   CheckSnow,
   DirectVideo: Boolean;

Implementation

uses strings;

Const
   { standard file descriptors }
   STDIN  = 0;
   STDOUT = 1;
   STDERR = 2;

Var
   ExitSave : pointer;                { pointer to original exit proc }
   fg,bg : integer;                   { foreground & background }
   cp : array [0..7,0..7] of integer; { color pair array }
   ps : array [0..255] of char;       { for use with pchars }
   doRefresh : boolean;               { immediate refresh toggle }
   SubWn,                             { window created from window() }
   PrevWn,                            { previous window when active changes }
   ActiveWn : pwindow;                { current active window for stdout }
   tmp_b : boolean;
   isEcho : boolean;                  { keeps track of echo status }
   MaxRows,                           { set at startup to terminal values }
   MaxCols : longint;                 { for columns and rows }
   tios : TermIOS;                    { saves the term settings at startup }
   prev_textattr : integer;           { detect change in TextAttr }

{==========================================================================}

{ set the active window for write(ln), read(ln) }
Procedure SetActiveWn(win : pwindow);
Begin
   If win <> ActiveWn Then PrevWn := ActiveWn;
   { don't set to a nil window! }
   If win <> Nil Then
      ActiveWn := win
   Else
      ActiveWn := stdscr;
End;

{--------------------------------------------
  initialize ncurses screen & keyboard, and
  return a pointer to stdscr.
  NOTE: This is done at unit initialization.
 --------------------------------------------}
Function StartCurses(var win : pWindow) : Boolean;
Var
   i : integer;
   s : string[3];
Begin
   { save the current terminal settings }
   tcGetAttr(STDIN,tios);
   if initscr=Nil then Begin
      StartCurses := false;
      win := nil;
      Exit;
   End Else Begin
      StartCurses := true;
      start_color;
      cbreak; { disable keyboard buffering }
      raw;    { disable flow control, etc. }
      echo;   { echo keypresses }
      nonl;   { don't process cr in newline }
      intrflush(stdscr,bool(false));
      keypad(stdscr,bool(true));
      scrollok(stdscr,bool(true));
      win := stdscr;
      isEcho := true;
      doRefresh := true;
      getmaxyx(stdscr,MaxRows,MaxCols);
      { make these values visible to apps }
      nMaxRows := MaxRows;
      nMaxCols := MaxCols;
      { define the the alt'd keysets for ncurses }
      { alt/a .. atl/z }
      for i := ord('a') to ord('z') do Begin
         s := #27+chr(i)+#0;
         define_key(@s[1],(KEY_ALTA-97)+i);
      End;
      { alt/1 .. alt/9 }
      for i := 1 to 9 do Begin
         s := #27+chr(i)+#0;
         define_key(@s[1],(KEY_ALT1-1)+i);
      End;
      s := #27+'0'+#0; define_key(@s[1],KEY_ALT0);     { alt/0 }
      s := #27+'-'+#0; define_key(@s[1],KEY_ALTMINUS); { alt/- }
      s := #27+'='+#0; define_key(@s[1],KEY_ALTEQUAL); { alt/= }
      s := #27+#9+#0;  define_key(@s[1],KEY_ALTTAB);   { alt/tab }
   End;
End;

{----------------------------------
  Shutdown ncurses.
  NOTE: This is done via ExitProc.
 ----------------------------------}
Procedure EndCurses;
Begin
   { restore the original terminal settings }
   { and leave the screen how the app left it }
   tcSetAttr(STDIN,TCSANOW,tios);
End;

{--------------------------------------------------------
  This disables any curses activity until a refresh.
  Use this BEFORE any shelling (shell,exec,execv,etc)
  to put the terminal temporarily back into cooked mode.
 --------------------------------------------------------}
Procedure nStop;
Begin
   endwin;
End;

{---------------------------------------------
  Simply a refresh to re-establish the curses
  terminal settings following an nStop.
 ---------------------------------------------}
Procedure nStart;
Begin
   refresh;
End;

{ see if the specified attribute is high intensity }
Function nIsBold(att : integer) : boolean;
Begin
   bg := att div 16;
   fg := att - (bg * 16);
   nisbold := (fg > 7);
End;

{ map a curses color to an ibm color }
Function c2ibm(c : integer) : integer;
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
Var
   att : integer;
Begin
   Case c of
      COLOR_BLACK   : att := black;
      COLOR_RED     : att := red;
      COLOR_GREEN   : att := green;
      COLOR_YELLOW  : att := brown;
      COLOR_BLUE    : att := blue;
      COLOR_MAGENTA : att := magenta;
      COLOR_CYAN    : att := cyan;
      COLOR_WHITE   : att := lightgray;
      else att := c;
   End;
   c2ibm := att;
End;

{ map an ibm color to a curses color }
Function ibm2c(c : integer) : integer;
Var
   att : integer;
Begin
   Case c of
      black     : att := COLOR_BLACK;
      red       : att := COLOR_RED;
      green     : att := COLOR_GREEN;
      brown     : att := COLOR_YELLOW;
      blue      : att := COLOR_BLUE;
      magenta   : att := COLOR_MAGENTA;
      cyan      : att := COLOR_CYAN;
      lightgray : att := COLOR_WHITE;
      else att := c;
   End;
   ibm2c := att;
End;

{ initialize a color pair }
Function nSetColorPair(att : integer) : integer;
var
   i : integer;
Begin
   bg := att div 16;
   fg := att - (bg * 16);
   While bg > 7 Do dec(bg,8);
   While fg > 7 Do dec(fg,8);
   bg := ibm2c(bg);
   fg := ibm2c(fg);
   i := cp[bg,fg];
   init_pair(i,fg,bg);
   nSetColorPair := i;
End;

{ map a standard color attribute to an ncurses attribute }
Function CursesAtts(att : byte) : longint;
Var
   atts : longint;
Begin
   atts := COLOR_PAIR(nSetColorPair(att));
   If nIsBold(att) Then atts := atts or A_BOLD;
   If (att and $80) = $80 Then atts := atts or A_BLINK;
   CursesAtts := atts;
End;

{------------------------------------------------
  Delete a window.
  NOTE: This does not clear it from the display.
 ------------------------------------------------}
Procedure nDelWindow(var win : pWindow);
Begin
   If (win = stdscr) or (win = curscr) Then Exit;
   If win <> Nil Then delwin(win);
   win := Nil;
   If ActiveWn = Nil Then SetActiveWn(stdscr);
End;

{-----------------------------------------
  Set the current text color of a window,
  delayed until next refresh.
 -----------------------------------------}
Procedure nWinColor(win : pWindow; att : integer);
Begin
   wattrset(win,CursesAtts(att));
   prev_textattr := att;
End;

{ clear the specified window }
procedure nClrScr(win : pWindow; att : integer);
Begin
   wbkgd(win,CursesAtts(att));
   TouchWin(win);
   werase(win);
   If doRefresh Then wrefresh(win);
   prev_textattr := att;
End;

{ clear from the cursor to the end of line in a window }
Procedure nClrEol(win : pWindow);
Var
   tmp : pwindow;
   x,y,
   xb,yb,
   xm,ym : longint;
Begin
   {--------------------------------------------------------
     In order to have the correct color, we must define and
     clear a temporary window. ncurses wclrtoeol() uses the
     window background color rather that the current color
     attribute ;-(
    --------------------------------------------------------}
   getyx(win,y,x);
   getbegyx(win,yb,xb);
   getmaxyx(win,ym,xm);
   tmp := subwin(win,1,xm-x,yb+y,xb+x);
   If tmp = nil then Exit;
   wbkgd(tmp,CursesAtts(TextAttr));
   werase(tmp);
{   wclrtoeol(win);}
   If doRefresh Then wrefresh(tmp);
   delwin(tmp);
End;

{ clear from the cursor to the bottom in a window }
Procedure nClrBot(win : pWindow);
Begin
   wclrtobot(win);
   If doRefresh Then wrefresh(win);
End;

{ insert a line at the cursor line in a window }
Procedure nInsLine(win : pWindow);
Begin
   winsertln(win);
   If doRefresh Then wrefresh(win);
End;

{ delete line at the cursor in a window }
Procedure nDelLine(win : pWindow);
Begin
   wdeleteln(win);
   If doRefresh Then wrefresh(win);
End;

{ position cursor in a window }
Procedure nGotoXY(win : pWindow; x,y : integer);
Begin
   wmove(win,y-1,x-1);
   touchwin(win);
   If doRefresh Then wrefresh(win);
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

{---------------------------------------------------------------------
 read a keystroke from a window, including function keys and extended
 keys (arrows, etc.)
 Note: Make sure that keypad(win,true) has been issued prior to use.
       ( nWindow does this )
 ---------------------------------------------------------------------}
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
         KEY_ALTA  : c := #30; { alt/a }
         KEY_ALTB  : c := #48;
         KEY_ALTC  : c := #46;
         KEY_ALTD  : c := #32;
         KEY_ALTE  : c := #18;
         KEY_ALTF  : c := #33;
         KEY_ALTG  : c := #34;
         KEY_ALTH  : c := #35;
         KEY_ALTI  : c := #23;
         KEY_ALTJ  : c := #36;
         KEY_ALTK  : c := #37;
         KEY_ALTL  : c := #38;
         KEY_ALTM  : c := #50;
         KEY_ALTN  : c := #49;
         KEY_ALTO  : c := #24;
         KEY_ALTP  : c := #25;
         KEY_ALTQ  : c := #16;
         KEY_ALTR  : c := #19;
         KEY_ALTS  : c := #31;
         KEY_ALTT  : c := #20;
         KEY_ALTU  : c := #22;
         KEY_ALTV  : c := #47;
         KEY_ALTW  : c := #17;
         KEY_ALTX  : c := #45;
         KEY_ALTY  : c := #21;
         KEY_ALTZ  : c := #44;  { alt/z }
         KEY_ALT1  : c := #120; { alt/1 }
         KEY_ALT2  : c := #121; { alt/2 }
         KEY_ALT3  : c := #122; { alt/3 }
         KEY_ALT4  : c := #123; { alt/4 }
         KEY_ALT5  : c := #124; { alt/5 }
         KEY_ALT6  : c := #125; { alt/6 }
         KEY_ALT7  : c := #126; { alt/7 }
         KEY_ALT8  : c := #127; { alt/8 }
         KEY_ALT9  : c := #128; { alt/9 }
         KEY_ALT0  : c := #129; { alt/0 }
         KEY_ALTMINUS : c := #130; { alt/- }
         KEY_ALTEQUAL : c := #131; { alt/= }
         KEY_ALTTAB : c := #15; { alt/tab }
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
            If l = Key_f(10) Then c := #68 Else
            If l = Key_f(11) Then c := #84 Else
            If l = Key_f(12) Then c := #85 Else
            If l = Key_f(13) Then c := #86 Else
            If l = Key_f(14) Then c := #87 Else
            If l = Key_f(15) Then c := #88 Else
            If l = Key_f(16) Then c := #89 Else
            If l = Key_f(17) Then c := #90 Else
            If l = Key_f(18) Then c := #91 Else
            If l = Key_f(19) Then c := #92 Else
            If l = Key_f(20) Then c := #93;
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

{ write a string to a window at the current cursor position }
Procedure nWrite(win : pWindow; s : string);
Begin
   If TextAttr <> prev_textattr Then
      nWinColor(win,TextAttr);
   waddstr(win,StrPCopy(ps,s));
   If doRefresh Then wrefresh(win);
End;

{=========================================================================
  CrtWrite, CrtRead, CrtReturn, CrtClose, CrtOpen, AssignCrt.
  These functions come from the FPC distribution rtl/linux/crt unit.
  These are the hooks into the input/output stream needed for write(ln)
  and read(ln).
 =========================================================================}

{ used by CrtWrite }
Procedure DoWrite(temp : string);
Begin
   nWrite(ActiveWn,temp);
End;

Function CrtWrite(Var F: TextRec): Integer;
{
  Top level write function for CRT
}
Var
  Temp : String;
  idx,i : Longint;
{  oldflush : boolean;}
Begin
{  oldflush:=ttySetFlush(Flushing);}
  idx:=0;
  while (F.BufPos>0) do
   begin
     i:=F.BufPos;
     if i>255 then
      i:=255;
     system.Move(F.BufPTR^[idx],Temp[1],F.BufPos);
     Temp[0]:=Chr(i);
     DoWrite(Temp);
     dec(F.BufPos,i);
     inc(idx,i);
   end;
{  ttySetFlush(oldFLush);}
  CrtWrite:=0;
End;

Function CrtRead(Var F: TextRec): Integer;
{
  Read from CRT associated file.
}
Begin
  { let's use ncurses instead! }
  FillChar(F.BufPtr^, F.BufSize, #0);
  wgetnstr(ActiveWn,F.BufPtr^, F.BufSize-1);
  F.BufEnd := Length(StrPas(F.BufPtr^))+1;
  F.BufPtr^[F.BufEnd-1] := #10;
  F.BufPos:=0;
{  CrtWrite(F);}
  CrtRead:=0;
End;

Function CrtReturn(Var F:TextRec):Integer;
Begin
  F.BufEnd := 0;
  F.BufPos:= 0;
  CrtReturn:=0;
end;

Function CrtClose(Var F: TextRec): Integer;
{
  Close CRT associated file.
}
Begin
  F.Mode:=fmClosed;
  CrtClose:=0;
End;

Function CrtOpen(Var F: TextRec): Integer;
{
  Open CRT associated file.
}
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@CrtWrite;
     TextRec(F).FlushFunc:=@CrtWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:=@CrtRead;
     TextRec(F).FlushFunc:=@CrtReturn;
   end;
  TextRec(F).CloseFunc:=@CrtClose;
  CrtOpen:=0;
End;

procedure AssignCrt(var F: Text);
{
  Assign a file to the console. All output on file goes to console instead.
}
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;

{==========================================================================
                      Standard crt unit replacements
 ==========================================================================}
{ set the text background color }
Procedure TextBackground(att : byte);
Begin
   TextAttr:=
    ((att shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
   nWinColor(ActiveWn,TextAttr);
End;

{ set the text foreground color }
Procedure TextColor(att : byte);
Begin
   TextAttr := (att and $8f) or (TextAttr and $70);
   nWinColor(ActiveWn,TextAttr);
End;

{ set to high intensity }
Procedure HighVideo;
Begin
  TextColor(TextAttr Or $08);
End;

{ set to low intensity }
Procedure LowVideo;
Begin
  TextColor(TextAttr And $77);
End;

{ set to normal display colors }
Procedure NormVideo;
Begin
  TextColor(7);
  TextBackGround(0);
End;

{ clear stdscr }
Procedure ClrScr;
Begin
   nClrScr(ActiveWn,TextAttr);
End;

{ clear from the cursor to the end of line in stdscr }
Procedure ClrEol;
Begin
   nClrEol(ActiveWn);
End;

{ clear from the cursor to the bottom of stdscr }
Procedure ClrBot;
Begin
   nClrBot(ActiveWn);
End;

{ insert a line at the cursor line in stdscr }
Procedure InsLine;
Begin
   nInsLine(ActiveWn);
End;

{ delete line at the cursor in stdscr }
Procedure DelLine;
Begin
   nDelLine(ActiveWn);
End;

{ position cursor in stdscr }
Procedure GotoXY(x,y : integer);
Begin
   nGotoXY(ActiveWn,x,y);
End;

{ find cursor x position in stdscr }
Function WhereX : integer;
Begin
   WhereX := nWhereX(ActiveWn);
End;

{ find cursor y position in stdscr }
Function WhereY : integer;
Begin
   WhereY := nWhereY(ActiveWn);
End;

{ Wait for DTime milliseconds }
Procedure Delay(DTime: Word);
Begin
  fpselect(0,nil,nil,nil,DTime);
End;

{ create a new subwindow of stdscr }
Procedure Window(x,y,x1,y1 : integer);
Begin
   nDelWindow(SubWn);
   SubWn := subwin(stdscr,y1-y+1,x1-x+1,y-1,x-1);
   If SubWn = nil then Exit;
   intrflush(SubWn,bool(false));
   keypad(SubWn,bool(true));
   scrollok(SubWn,bool(true));
   SetActiveWn(SubWn);
   GotoXY(1,1);
End;

{------------------------------------------------------
 Check if a key has been pressed.
 Note: this is best used along with select() on STDIN,
 as it can suck up lots of cpu time.
 Better yet, use nKeypressed instead if you don't need
 to include file descriptors other than STDIN.
 ------------------------------------------------------}
function  Keypressed : boolean;
var
   l : longint;
{   fd : fdSet;}
Begin
   Keypressed := FALSE;
   nodelay(ActiveWn,bool(TRUE));
   l := wgetch(ActiveWn);
   If l <> ERR Then Begin { ERR = -(1) from unit ncurses }
      ungetch(l);
      Keypressed := TRUE;
   End;
   nodelay(ActiveWn,bool(FALSE));

{ Below is more efficient code, but does not work well with
  nReadkey & extended keys because nReadkey's ungetch does not
  force a change in STDIN. So, a "while keypressed" block does
  not produce the expected results when trapping for char(0)
  followed by a second scan code.

   FD_Zero(fd);
   fd_Set(STDIN,fd);
   Keypressed := (Select(STDIN+1,@fd,nil,nil,0) > 0);
}
End;

{ silently read a key from stdscr }
Function Readkey : char;
Begin
   tmp_b := IsEcho;
   noecho;
   Readkey := nReadkey(ActiveWn);
   If tmp_b Then echo;
End;

{ a cheap replacement! }
Procedure Sound(hz : word);
Begin
   Beep;
   wrefresh(ActiveWn);
End;

Procedure NoSound;
Begin
End;

Procedure TextMode(mode : word);
Begin
   nDelWindow(SubWn);
   SetActiveWn(stdscr);
   LastMode := mode;
   DirectVideo := true;
   CheckSnow := true;
   NormVideo;
   ClrScr;
End;

{ Set the cursor visibility. Returns the previous value }
{ or (-1) if value c is not supported by the terminal. }
Function nCursor(c : integer) : integer;
Begin
   nCursor := curs_set(c);
End;

{ Set the <esc> key delay time in milliseconds. }
{ Use d=(-1) to return current value without updating. }
Function nEscDelay(d : longint) : longint;
Begin
   nEscDelay := ESCDELAY;
   If d >= 0 Then ESCDELAY := d;
End;

{ return the current terminal name (same as $TERM env variable) }
Function nTermName : string;
Begin
   nTermName := StrPas(termname);
End;

{ could not initialize ncurses }
Procedure CursesFailed;
Begin
   { give 'em a clue! }
   Writeln('StartCurses() failed');
   Halt;
End;

{ exit procedure to ensure curses is closed up cleanly }
Procedure nExit;
Begin
   ExitProc := ExitSave;
   EndCurses;
End;

Procedure nInit;
Begin
   { set the unit exit procedure }
   ExitSave := ExitProc;
   ExitProc := @nExit;
   { load the color pairs array with color pair indices (0..63 }
   For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
   { initial window pointers }
   SubWn := nil;
   PrevWn := ActiveWn;
   { basic gray on black screen }
   TextMode(LastMode);
   { Redirect the standard output }
   assigncrt(Output);
   Rewrite(Output);
   TextRec(Output).Handle:=StdOutputHandle;
   { Redirect the standard input }
   assigncrt(Input);
   Reset(Input);
   TextRec(Input).Handle:=StdInputHandle;
   { some defaults }
   nEscDelay(500); { default is 1000 (1 second) }
   nCursor(cON);   { normal cursor }
End;
