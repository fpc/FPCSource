Unit oCrt;
{---------------------------------------------------------------------------
                                 CncWare
                           (c) Copyright 1999
 ---------------------------------------------------------------------------
  Filename..: ocrt.pp
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 03/01/99

  Purpose - crt unit replacement plus OOP windows using ncurses.

  NOTE: All of the crt procedures & functions have been replaced with ncurses
  driven versions. This makes the ncurses library a little easier to use in a
  Pascal program and benefits from terminal independence.

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
  1.02 | 11/26/99 | kjw | Added nKeypressed().
  1.03 | 12/01/99 | kjw | Added global boolean nIsActive.
  1.04 | 12/03/99 | kjw | 1) Added procedures nHline, nVLine, & nWriteAC.
                        | 2) Changed all the line draw character functions
                        | (i.e., nHL, nVL) to return the longint value from
                        | ncurses rather than the character value (which was
                        | not very useful!). Now these can be passed to
                        | nWriteAC() to correctly write the line drawing
                        | characters.
                        | 3) Added more of the ACS characters.
  1.05 | 12/08/99 | kjw | 1) StartCurses() is now done as part of the unit
                        | initialization block. EndCurses() is done via an
                        | exit procedure.
                        | 2) nIsActive is now a function (safer!).
                        | 3) Added panel unit for windowing.
                        | 4) Added tnWindow object.
  1.10 | 12/12/99 | kjw | Added nSEdit().
  1.11 | 12/12/99 | kjw | Added Special property to tEC object. Now any normal
                        | character can trigger sedit to exit.
------------------------------------------------------------------------------
  2.00 | 12/13/99 | kjw | nCrt renamed to oCrt. A new nCrt has been created
                        | which is a drop-in replacement for the FPC crt unit.
                        | oCrt contains all of nCrt plus the OOP extensions.
                        | All of the common code is in ncrt.inc.
  2.01 | 12/15/99 | kjw | 1) A tnWindow object now becomes the target for
                        | stdout following Init & Show. A Hide will put the
                        | target back to stdscr.
                        | 2) Added nSetActiveWin() to manually pick a target
                        | window for stdout.
  2.02 | 12/15/99 | kjw | 1) PutFrame applied keypad to stdscr instead of sub.
                        | 2) See ncrt.inc
  2.03 | 12/16/99 | kjw | 1) See ncrt.inc
                        | 2) Added shift/f-key constants.
  2.04 | 01/04/00 | kjw | See ncrt.inc
  2.05 | 01/06/00 | kjw | 1) See ncrt.inc.
                        | 2) Added boolean internal_fwrite. FWrite was failing
                        | when trying to write outside of the active window.
                        | 3) nSEdit was not handling tec.firsttime correctly
                        | when a tec.special was processed.
  2.06 | 01/11/00 | kjw | See ncrt.inc.
  2.07 | 01/31/00 | kjw | 1) See ncrt.inc.
                        | 2) Added getcolor, getframecolor, getheadercolor
                        | methods to tnWindow.
------------------------------------------------------------------------------
}
Interface

Uses linux,ncurses,panel;

Const

   { border styles for text boxes }
   btNone : integer = 0;
   btSingle : integer = 1;
   btDouble : integer = 2;

   nKeyEnter     = 13;      { Enter key }
   nKeyEsc       = 27;      { Home key }
   nKeyHome      = 71;      { Home key }
   nKeyUp        = 72;      { Up Arrow }
   nKeyPgUp      = 73;      { PgUp Key }
   nKeyLeft      = 75;      { Left Arrow }
   nKeyRight     = 77;      { Right Arrow }
   nKeyEnd       = 79;      { End Key }
   nKeyDown      = 80;      { Down Arrow }
   nKeyPgDn      = 81;      { PgDn Key }
   nKeyF1        = 59;      { f1 key }
   nKeyF2        = 60;      { f2 key }
   nKeyF3        = 61;      { f3 key }
   nKeyF4        = 62;      { f4 key }
   nKeyF5        = 63;      { f5 key }
   nKeyF6        = 64;      { f6 key }
   nKeyF7        = 65;      { f7 key }
   nKeyF8        = 66;      { f8 key }
   nKeyF9        = 67;      { f9 key }
   nKeyF10       = 68;      { f10 key }
   nKeyF11       = 84;      { shift/f1 key }
   nKeyF12       = 85;      { shift/f2 key }
   nKeyF13       = 86;      { shift/f3 key }
   nKeyF14       = 87;      { shift/f4 key }
   nKeyF15       = 88;      { shift/f5 key }
   nKeyF16       = 89;      { shift/f6 key }
   nKeyF17       = 90;      { shift/f7 key }
   nKeyF18       = 91;      { shift/f8 key }
   nKeyF19       = 92;      { shift/f9 key }
   nKeyF20       = 93;      { shift/f10 key }


Type
   { for scrolling a window }
   tnUpDown = (up,down);
   { for window & header positioning }
   tnJustify = (none,left,center,right,top,bottom);

   { used for nSEdit }
   {------------------------------------------------------------------
     FirstTime = true : passed string is initialized to ''.
      IsHidden = true : causes a string of '*' to display in place of
                        the actual characters typed.
       InsMode        : toggle for insert/overwrite mode.
      ExitMode = true : sedit exits after every keystroke.
               = false: sedit only exits when #27,#13, or any extended
                        key *except* for Home,End,RArrow,LArrow.
    ------------------------------------------------------------------}
   tEC = Object
      FirstTime,
      IsHidden,
      InsMode,
      ExitMode : boolean;
      special : string;
      Constructor Init(ft,ih,im,em : boolean; s : string);
      Destructor Done;
   End;

   pwin = ^Window;

   pnWindow = ^tnWindow;
   tnWindow = Object
      Private
          wn : pwindow;       { pointer to win or sub to read/write to }
         win : pwindow;       { pointer to main window record }
         sub : pwindow;       { sub window if a bordered window }
         pan : ppanel;        { pointer to panel record }
         subp : ppanel;       { sub panel if a bordered window }
         visible : boolean;   { is the window visible? }
         hasframe : boolean;
         wincolor,            { window color }
         framecolor,          { frame color }
         hdrcolor : integer;  { header color }
         header : string[80]; { header string }
      Public
         ec : tEC;            { edit control settings }
         Constructor Init(x,y,x1,y1,wcolor : integer;
                                    border : boolean;
                                    fcolor : integer);
         Destructor Done;
         Procedure Show;    { display the window }
         Procedure Hide;    { hide the window }
         Procedure ClrScr;
         Procedure ClrEol;
         Procedure ClrBot;
         Procedure InsLine;
         Procedure DelLine;
         Procedure GotoXY(x,y : integer);
          Function WhereX : integer;
          Function WhereY : integer;
          Function ReadKey : char;
          Function Readln : string;
         Procedure Write(s : string);
         Procedure Writeln(s : string);
         Procedure WriteAC(x,y,att,c : longint);
         Procedure FWrite(x,y,att,z : integer; s : string);
         Procedure DrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
          Function GetHeader : string;
         Procedure PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
         Procedure SetColor(att : integer);
          Function GetColor : integer;
          Function GetFrameColor : integer;
          Function GetHeaderColor : integer;
         Procedure PutFrame(att : integer);
         Procedure Move(x,y : integer);
         Procedure Scroll(ln : integer; dir : tnUpDown);
         Procedure Align(hpos,vpos : tnJustify);
          Function Rows : integer;
          Function Cols : integer;
          Function SEdit(x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
   End;

Var
   nscreen : pwin;
   nEC : tEC;

Procedure nSetActiveWin(win : pwindow);
Procedure nDoNow(donow : boolean);
 Function nKeypressed(timeout : word) : boolean;
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
Procedure nScroll(win : pWindow; lines : integer; dir : tnUpDown);
Procedure nDrawBox(win : pWindow; LineStyle,x1,y1,x2,y2,att : Integer);
Procedure nFrame(win : pWindow);
 Function nRows(win : pWindow) : integer;
 Function nCols(win : pWindow) : integer;
 Function nHL : longint; { horizontal line }
 Function nVL : longint; { vertical line }
 Function nUL : longint; { upper left corner }
 Function nLL : longint; { lower loft corner }
 Function nUR : longint; { upper right corner }
 Function nLR : longint; { lower right corner }
 Function nLT : longint; { left tee }
 Function nRT : longint; { right tee }
 Function nTT : longint; { top tee }
 Function nBT : longint; { bottom tee }
 Function nPL : longint; { plus, + }
 Function nLA : longint; { left arrow }
 Function nRA : longint; { right arrow }
 Function nUA : longint; { up arror }
 Function nDA : longint; { down arrow }
 Function nDI : longint; { diamond }
 Function nCB : longint; { checkerboard }
 Function nDG : longint; { degree }
 Function nPM : longint; { plus/minus }
 Function nBL : longint; { bullet }
Procedure nHLine(win : pwindow; col,row,attr,x : integer);
Procedure nVLine(win : pwindow; col,row,attr,y : integer);
Procedure nWriteAC(win : pwindow; x,y : integer; att,acs_char : longint);
 Function IsBold(att : integer) : boolean;
 Function SetColorPair(att : integer) : integer;
Procedure FWrite(col,row,attrib : integer; clear : integer; s : string);
 Function nSEdit(win : pwindow; x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;

{$i ncrt.inc}

Const
   internal_fwrite : Boolean = false;

{ internal wrapper }
Procedure intFWrite(win : pwindow; col,row,attrib,clear : integer; s : string);
Var
   tmp : pwindow;
Begin
   tmp := ActiveWn;
   ActiveWn := win;
   internal_fwrite := true;
   FWrite(col,row,attrib,clear,s);
   internal_fwrite := false;
   ActiveWn := tmp;
End;

{---------------------------------------------------------------------
  tnWindow.Init

  Create a new window.
       x = upper left corner x, screen relative
       y = upper left corner y, screen relative
      x1 = lower right corner x, screen relative
      y1 = lower right corner y, screen relative
  wcolor = window/text color
  border = include a frame?
  fcolor = frame color
 ---------------------------------------------------------------------}
Constructor tnWindow.Init(x,y,x1,y1,wcolor : integer;
                           border : boolean;
                           fcolor : integer);
Begin
   visible := false;
   hasframe := false;
   wincolor := wcolor;
   framecolor := fcolor;
   hdrcolor := wcolor;
   header := '';
   win := nil;
   sub := nil;
   pan := nil;
   subp := nil;
   visible := false;
   win := newwin(y1-y+1,x1-x+1,y-1,x-1);
   pan := new_panel(win);
   hide_panel(pan);
   If border Then
      PutFrame(fcolor)
   Else Begin
      wn := win;
      wbkgd(win,COLOR_PAIR(SetColorPair(wcolor)));
      If isbold(wcolor) then wattr_on(win,A_BOLD);
      scrollok(win,bool(true));
      intrflush(stdscr,bool(false));
      keypad(stdscr,bool(true));
   End;
   ec.Init(false,false,false,false,'');
   ActiveWn := wn;
End;

{ deallocate the window }
Destructor tnWindow.Done;
Begin
   If subp <> nil Then del_panel(subp);
   If pan <> nil Then del_panel(pan);
   If sub <> nil Then delwin(sub);
   If win <> nil Then delwin(win);
   ec.Done;
End;

{ display the window and move to the top }
Procedure tnWindow.Show;
Begin
   ActiveWn := wn;
   visible := true;
   show_panel(pan);
   If subp <> nil Then show_panel(subp);
   update_panels;
   doupdate;
End;

{ hide the window }
Procedure tnWindow.Hide;
Begin
   ActiveWn := stdscr;
   visible := false;
   If subp <> nil Then hide_panel(subp);
   hide_panel(pan);
   update_panels;
   doupdate;
End;

Procedure tnWindow.ClrScr;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrScr(wn,wincolor);
   dorefresh := tmp_b;
End;

Procedure tnWindow.ClrEol;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrEol(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.ClrBot;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nClrBot(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.InsLine;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nInsLine(wn);
   dorefresh := tmp_b;
End;

Procedure tnWindow.DelLine;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nDelLine(wn);
   dorefresh := tmp_b;
End;

{ return the window border header string }
Function tnWindow.GetHeader : string;
Begin
   GetHeader := header;
End;

{----------------------------------------------------------------------
  put/replace a header string at the top of a bordered window

     hdr = header string (top line of window, only if hasframe = true)
  hcolor = header line color
    hpos = justfication of header string, left, center, or right
 ----------------------------------------------------------------------}
Procedure tnWindow.PutHeader(hdr : string; hcolor : integer; hpos : tnJustify);
Var
   cp,
   hx,
   len : integer;
   att,
   mx,my : longint;
Begin
   If Hasframe Then Begin
      If hdr <> '' Then Begin
         header := hdr;
         hdrcolor := hcolor;
         getmaxyx(win,my,mx);
         nHline(win,2,1,framecolor,mx-1);
         len := mx-2;
         hdr := Copy(hdr,1,len);
         len := Length(hdr);
         Case hpos of
           left   : hx := 1;
           center : hx := (mx - len) div 2;
           right  : hx := (mx - len) - 1;
         End;
         mvwaddstr(win,0,hx,StrPCopy(ps,hdr));
         cp := SetColorPair(hcolor);
         If IsBold(hcolor) Then
            att := A_BOLD
         Else
            att := A_NORMAL;
         mvwchgat(win,0,hx,len,att,cp,0);
      End;
   End;
End;

{ set the the color of the writable window }
Procedure tnWindow.SetColor(att : integer);
Begin
   wbkgd(wn,COLOR_PAIR(SetColorPair(att)));
   If isbold(att) then wattr_set(wn,A_BOLD);
   wincolor := att;
   If visible Then wrefresh(wn);
End;

{ get the writeable window color }
Function tnWindow.GetColor : integer;
Begin
   GetColor := wincolor;
End;

{ get the frame color }
Function tnWindow.GetFrameColor : integer;
Begin
   GetFrameColor := framecolor;
End;

{ get the header color }
Function tnWindow.GetHeaderColor : integer;
Begin
   GetHeaderColor := hdrcolor;
End;

{ frame an un-framed window, or update the frame color of a framed window }
Procedure tnWindow.PutFrame(att : integer);
Var
   x,y,
   mx,my,
   atts : longint;
Begin
   wbkgd(win,COLOR_PAIR(SetColorPair(att)));
   atts := wattr_get(win);
   If isbold(att) then wattr_on(win,atts or A_BOLD);
   box(win,ACS_VLINE,ACS_HLINE);
   framecolor := att;
   If framecolor = -1 Then framecolor := wincolor;
   hasframe := true;
   If sub = nil Then Begin
      getbegyx(win,y,x);
      getmaxyx(win,my,mx);
      sub := newwin(my-2,mx-2,y+1,x+1);
      If sub <> nil Then Begin
         subp := new_panel(sub);
         hide_panel(subp);
         wbkgd(sub,COLOR_PAIR(SetColorPair(wincolor)));
         If isbold(wincolor) then wattr_on(sub,A_BOLD);
         scrollok(sub,bool(true));
         intrflush(sub,bool(false));
         keypad(sub,bool(true));
         wn := sub;
      End;
   End;
   touchwin(sub);
   If visible Then Begin
      wrefresh(win);
      wrefresh(sub);
   End;
End;

{ move the window }
Procedure tnWindow.Move(x,y : integer);
Begin
   move_panel(pan,y-1,x-1);
   If subp <> nil Then move_panel(subp,y,x);
   If visible Then Begin
      update_panels;
      doupdate;
   End;
End;

Procedure tnWindow.Align(hpos,vpos : tnJustify);
Var
   x,y,
   bx,by : longint;
Begin
   getmaxyx(win,y,x);
   getbegyx(win,by,bx);
   Case hpos of
      none   : x := bx+1;
      left   : x := 1;
      right  : x := MaxCols - x;
      center : x := (MaxCols - x) div 2;
   End;
   Case vpos of
      none   : y := by+1;
      top    : y := 1;
      bottom : y := MaxRows - y;
      center : y := (MaxRows - y) div 2;
   End;
   move(x,y);
End;

Procedure tnWindow.Scroll(ln : integer; dir : tnUpDown);
Begin
   nScroll(wn,ln,dir);
End;

Procedure tnWindow.GotoXY(x,y : integer);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nGotoXY(wn,x,y);
   dorefresh := tmp_b;
End;

Function tnWindow.WhereX : integer;
Begin
   WhereX := nWhereX(wn);
End;

Function tnWindow.WhereY : integer;
Begin
   WhereY := nWhereY(wn);
End;

Function tnWindow.ReadKey : char;
Begin
   ReadKey := nReadKey(wn);
End;

Function tnWindow.Readln : string;
Begin
   Readln := nReadln(wn);
End;


Procedure tnWindow.Write(s : string);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nWrite(wn,s);
   dorefresh := tmp_b;
End;

Procedure tnWindow.Writeln(s : string);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nWriteln(wn,s);
   dorefresh := tmp_b;
End;

Procedure tnWindow.WriteAC(x,y,att,c : longint);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nWriteAC(wn,x,y,att,c);
   dorefresh := tmp_b;
End;

Procedure tnWindow.FWrite(x,y,att,z : integer; s : string);
Var tmp : pwindow;
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   tmp := ActiveWn;
   ActiveWn := wn;
   intFWrite(wn,x,y,att,z,s);
   ActiveWn := tmp;
   dorefresh := tmp_b;
End;

Procedure tnWindow.DrawBox(LineStyle,x1,y1,x2,y2,att : Integer);
Begin
   tmp_b := dorefresh;
   dorefresh := visible;
   nDrawBox(wn,LineStyle,x1,y1,x2,y2,att);
   dorefresh := tmp_b;
End;

Function tnWindow.Rows : integer;
Begin
   Rows := nRows(wn);
End;

Function tnWindow.Cols : integer;
Begin
   Cols := nCols(wn);
End;

Function tnWindow.SEdit(x,y,att,z,CursPos:Integer;es:String;Var ch : Char) : String;
var
   tmp_ec : tec;
Begin
   tmp_ec.Init(nEC.FirstTime,nEC.IsHidden,nEC.InsMode,nEC.ExitMode,
               nEC.Special);
   nEC.Init(ec.FirstTime,ec.IsHidden,ec.InsMode,ec.ExitMode,
            ec.Special);
   SEdit := nSEdit(wn,x,y,att,z,CursPos,es,ch);
   ec.Init(nEC.FirstTime,nEC.IsHidden,nEC.InsMode,ec.ExitMode,
           ec.Special);
   nEC.Init(tmp_ec.FirstTime,tmp_ec.IsHidden,tmp_ec.InsMode,tmp_ec.ExitMode,
            tmp_ec.Special);
   tmp_ec.Done;
End;

{--------------------------- tEC -------------------------------}

Constructor tEC.Init(ft,ih,im,em : boolean; s : string);
Begin
   FirstTime := ft;
   IsHidden := ih;
   InsMode := im;
   ExitMode := em;
   Special := s;
End;

Destructor tEC.Done;
Begin
End;

{==========================================================================}

{ set the active window for write(ln), read(ln) }
Procedure nSetActiveWin(win : pwindow);
Begin
   ActiveWn := win;
End;

{----------------------------------------------------------------
  Set the refresh toggle.
  If true, then all changes to a window are immediate. If false,
  then changes appear following the next call to nRefresh.
 ----------------------------------------------------------------}
Procedure nDoNow(donow : boolean);
Begin
   dorefresh := donow;
End;

{-----------------------------------------------------
  Set the echo flag.
  This determines whether or not, characters are
  echoed to the display when entered via the keyboard.
 -----------------------------------------------------}
Procedure nEcho(b : boolean);
Begin
   Case b of
      true : echo;
      false: noecho;
   End;
   isEcho := b;
End;

{ create a new subwindow of stdscr }
Procedure nWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := subwin(stdscr,y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
   scrollok(win,bool(true));
   ActiveWn := win;
End;

{ create a new window }
Procedure nNewWindow(var win : pWindow; x,y,x1,y1 : integer);
Begin
   nDelWindow(win);
   win := newwin(y1-y+1,x1-x+1,y-1,x-1);
   If win = nil then Exit;
   intrflush(win,bool(false));
   keypad(win,bool(true));
   scrollok(win,bool(true));
   ActiveWn := win;
End;

{ repaint a window }
Procedure nRefresh(win : pWindow);
Begin
   touchwin(win);
   wrefresh(win);
End;

{----------------------------------------------
 Wait for a key to be pressed, with a timeout.
 If a key is pressed, then nKeypressed returns
 immediately as true, otherwise it return as
 false after the timeout period.
 ----------------------------------------------}
function nKeypressed(timeout : word) : boolean;
var
   fds : FDSet;
   maxFD : longint;
Begin
   FD_Zero(fds);
   maxFD := 1;
   { turn on stdin bit }
   If not FD_IsSet(STDIN,fds) Then FD_Set(STDIN,fds);
   { wait for some input }
   If Select(maxFD,@fds,nil,nil,timeout) > 0 Then
      nKeypressed := TRUE
   Else
      nKeypressed := FALSE;
End;

{---------------------------------
  read input string from a window
 ---------------------------------}
Function nReadln(win : pWindow) : string;
Begin
   wgetstr(win,ps);
   nReadln := StrPas(ps);
End;

{ write a string to a window without refreshing screen }
Procedure nWriteScr(win : pWindow; x,y,att : integer; s : string);
Var
   tmp : pwindow;
Begin
   tmp := ActiveWn;
   tmp_b := doRefresh;
   ActiveWn := win;
   doRefresh := false;
   intFWrite(win,x,y,att,0,s);
   ActiveWn := tmp;
   doRefresh := tmp_b;
End;

{----------------------------------------------------------
  Scroll a window, up or down, a specified number of lines.
  lines = number of lines to scroll.
  dir = direction, up or down.
 ----------------------------------------------------------}
Procedure nScroll(win : pWindow; lines : integer; dir : tnUpDown);
Begin
   ScrollOk(win,bool(True));
   Case dir of
        up : lines := abs(lines);
      down : lines := abs(lines) * (-1);
   End;
   wscrl(win,lines);
   If doRefresh Then wRefresh(win);
End;

{ draw a colored box, with or without a border }
Procedure nDrawBox(win : pWindow; LineStyle,x1,y1,x2,y2,att : Integer);
Var
   sub : pWindow;
   x,y : longint;
Begin
   getbegyx(win,y,x);
   sub := subwin(win,y2-y1+1,x2-x1+1,y+y1-1,x+x1-1);
   If sub = nil Then exit;
   wbkgd(sub,CursesAtts(att));
   werase(sub);
   case LineStyle of
      1,2 : box(sub, ACS_VLINE, ACS_HLINE);
   End;
   If doRefresh Then wrefresh(sub);
   nDelWindow(sub);
End;

{---------------------------
  add a border to a window,
  waits for a refresh
 ---------------------------}
Procedure nFrame(win : pWindow);
Begin
   box(win, ACS_VLINE, ACS_HLINE);
End;

{-----------------------------------------------------------
  write a string to a window at the current cursor position
  followed by a newline
 -----------------------------------------------------------}
Procedure nWriteln(win : pWindow; s : string);
Begin
   waddstr(win,StrPCopy(ps,s+#10));
   If doRefresh Then wrefresh(win);
End;

{ return then number of rows in a window }
Function nRows(win : pWindow) : integer;
Var
   x,y : longint;
Begin
   getmaxyx(win,y,x);
   nRows := y;
End;

{ return then number of columns in a window }
Function nCols(win : pWindow) : integer;
Var
   x,y : longint;
Begin
   getmaxyx(win,y,x);
   nCols := x;
End;

{-------------------------------------------------------
 Line drawing characters have to be handled specially.
 Use nWriteAC() to write these characters. They cannot
 be simply included as characters in a string.
 -------------------------------------------------------}

{ returns horizontal line character }
Function nHL : longint;
Begin
   nHL := ACS_HLINE;
End;

{ returns vertical line character }
Function nVL : longint;
Begin
   nVL := ACS_VLINE;
End;

{ returns upper left corner character }
Function nUL : longint;
Begin
   nUL := ACS_ULCORNER;
End;

{ returns lower left corner character }
Function nLL : longint;
Begin
   nLL := ACS_LLCORNER;
End;

{ returns upper right corner character }
Function nUR : longint;
Begin
   nUR := ACS_URCORNER;
End;

{ returns lower right corner character }
Function nLR : longint;
Begin
   nLR := ACS_LRCORNER;
End;

{ returns left tee character }
Function nLT : longint;
Begin
   nLT := ACS_LTEE;
End;

{ returns right tee character }
Function nRT : longint;
Begin
   nRT := ACS_RTEE;
End;

{ returns top tee character }
Function nTT : longint;
Begin
   nTT := ACS_TTEE;
End;

{ returns bottom tee character }
Function nBT : longint;
Begin
   nBT := ACS_BTEE;
End;

{ returns plus/cross character }
Function nPL : longint;
Begin
   nPL := ACS_PLUS;
End;

{ returns left arrow character }
Function nLA : longint;
Begin
   nLA := ACS_LARROW;
End;

{ returns right arrow character }
Function nRA : longint;
Begin
   nRA := ACS_RARROW;
End;

{ returns up arrow character }
Function nUA : longint;
Begin
   nUA := ACS_UARROW;
End;

{ returns down arrow character }
Function nDA : longint;
Begin
   nDA := ACS_DARROW;
End;

{ returns diamond character }
Function nDI : longint;
Begin
   nDI := ACS_DIAMOND;
End;

{ returns checkerboard character }
Function nCB : longint;
Begin
   nCB := ACS_CKBOARD;
End;

{ returns degree character }
Function nDG : longint;
Begin
   nDG := ACS_DEGREE;
End;

{ returns plus/minus character }
Function nPM : longint;
Begin
   nPM := ACS_PLMINUS;
End;

{ returns bullet character }
Function nBL : longint;
Begin
   nBL := ACS_BULLET;
End;

{ draw a horizontal line with color and a start & end position }
Procedure nHLine(win : pwindow; col,row,attr,x : integer);
var
   sub : pwindow;
   bx,by : longint;
Begin
   getbegyx(win,by,bx);
   sub := subwin(win,1,x-col+1,by+row-1,bx+col-1);
   If sub = nil Then Exit;
   x := getmaxx(sub);
   wbkgd(sub,CursesAtts(attr));
   mvwhline(sub,0,0,ACS_HLINE,x);
   If doRefresh Then wrefresh(sub);
   delwin(sub);
End;

{ draw a vertical line with color and a start & end position }
Procedure nVLine(win : pwindow; col,row,attr,y : integer);
var sub : pwindow;
Begin
   sub := subwin(win,y-row+1,1,row-1,col-1);
   If sub = nil Then Exit;
   wbkgd(sub,CursesAtts(attr));
   mvwvline(sub,0,0,ACS_VLINE,y);
   If doRefresh Then wrefresh(sub);
   delwin(sub);
End;

{----------------------------------------------------------------
  Write a character from the alternate character set. A normal
  value from the alternate character set is larger than $400000.
  If the value passed here is 128..255, then we assume it to be
  the ordinal value from the IBM extended character set, and try
  to map it to curses correctly. If it does not map, then we just
  make it an alternate character and hope the output is what the
  programmer expected. Note: this will work on the Linux console
  just fine, but for other terminals the passed value must match
  the termcap definition for the alternate character.
  Note: The cursor returns to it's original position.
 ----------------------------------------------------------------}
Procedure nWriteAC(win : pwindow; x,y : integer; att,acs_char : longint);
var
   xx,yy,
   cp : longint;
Begin
   If acs_char in [0..255] Then Begin
      Case acs_char of
         176 : acs_char := ACS_CKBOARD;
         179 : acs_char := ACS_VLINE;
         180 : acs_char := ACS_RTEE;
         191 : acs_char := ACS_URCORNER;
         192 : acs_char := ACS_LLCORNER;
         193 : acs_char := ACS_BTEE;
         194 : acs_char := ACS_TTEE;
         195 : acs_char := ACS_LTEE;
         196 : acs_char := ACS_HLINE;
         197 : acs_char := ACS_PLUS;
         218 : acs_char := ACS_ULCORNER;
         217 : acs_char := ACS_LRCORNER;
         241 : acs_char := ACS_PLMINUS;
         248 : acs_char := ACS_DEGREE;
         249 : acs_char := ACS_BULLET;
         else  acs_char := acs_char or A_ALTCHARSET;
      End;
   End;
   { save the current cursor position }
   getyx(win,yy,xx);
   cp := SetColorPair(att);
   { write character with current attributes }
   mvwaddch(win,y-1,x-1,acs_char);
   { update with new attributes }
   If IsBold(att) Then
      att := A_BOLD or A_ALTCHARSET
   Else
      att := A_NORMAL or A_ALTCHARSET;
   mvwchgat(win,y-1,x-1,1,att,cp,0);
   { return cursor to saved position }
   wmove(win,yy,xx);
   If doRefresh Then wrefresh(win);
End;

{-------------------------------------------------------------------
 write a string to stdscr with color, without moving the cursor

   Col    = x start position
   Row    = y start position
   Attrib = color (0..127), note color = (background*16)+foreground
   Clear  = clear line up to x position
   s      = string to write
 -------------------------------------------------------------------}
Procedure FWrite(col,row,attrib : integer; clear : integer; s : string);
Const
   ClearLine = { Following line is 80 Spaces }
'                                                                                ';

Var
   cs : string;
   tmp,
   sub : pWindow;
   x,y,
   xx,yy : longint;
Begin
   if Clear > 0 Then Begin
      If Clear > 80 Then Clear := 80;
      cs := Copy(ClearLine,1,(Clear-Col)-Length(s)+1);
   End Else
      cs := '';
   s := s+cs;
   If s = '' Then Exit;
   tmp := ActiveWn;
   getyx(ActiveWn,yy,xx);
   If Not internal_fwrite Then ActiveWn := stdscr;
   getbegyx(ActiveWn,y,x);
   sub := subwin(ActiveWn,1,Length(s),y+row-1,x+col-1);
   ActiveWn := tmp;
   If sub = nil Then Exit;
   wbkgd(sub,COLOR_PAIR(SetColorPair(Attrib)));
   If isbold(Attrib) then
      wattr_on(sub,A_BOLD);
   mvwaddstr(sub,0,0,StrPCopy(ps,s));
   If doRefresh Then wrefresh(sub);
   delwin(sub);
   wmove(ActiveWn,yy,xx);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
{                            String Editor                           }
Function nSEdit(win : pwindow; x,y,att,z,CursPos:integer;
                es:string;var ch : char) : string;
Var
   ZMode,
   SEditExit : boolean;
   Index : integer;
   hes : string;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure NewString;
BEGIN
   nSEdit := es;
   hes := es;
   FillChar(hes[1],Length(hes),'*');
END;

Procedure WriteString;
Var
   xx,yy : integer;
Begin
   xx := nWhereX(win);
   yy := nWhereY(win);
   If nEC.IsHidden Then
      intFWrite(win,x,y,att,z,hes)
   Else
      intFWrite(win,x,y,att,z,es);
   nGotoXY(win,xx,yy);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EInsMode;
Begin
   nEC.InsMode := (not nEC.InsMode)
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure WriteChar;
Begin
   If nWhereX(win) >= Length(es)+x Then Repeat
      es := es + ' ';
   Until Length(es)+X-1 = nWhereX(win);
   If Length(es)+X-1 = nWhereX(win) Then Index := Length(es);
   es[Index] := ch;
   If nEC.IsHidden Then Ch := '*';
   intFWrite(win,nWhereX(win),nWhereY(win),Att,0,Ch);
   If (Index < Z-X+1) or not ZMode Then Begin
      Index := Index+1;
      nGotoXY(win,X+Index-1,Y);
   End;
   Ch := #255;{ Set Ch to No Execute Character }
   NewString;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EInsert;            { Insert      }
Begin
   If Length(es) < Z-X+1 Then Begin
      Insert(' ',es,Index);
      NewString;
      WriteString;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EDelete;            { Delete      }
Begin
   Delete(es,Index,1);
   NewString;
   WriteString;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlEnd;           { <CTRL> End  }
Begin
   Delete(es,Index,Length(es));
   NewString;
   WriteString;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EHome;             { Home        }
Begin
   Index := 1;
   nGotoXY(win,x,y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ELeftArrow;         { Left Arrow  }
Begin
  Index := Index - 1;
  If Index < 1 Then
     Index := 1
  Else
     nGotoXY(win,nWhereX(win)-1,nWhereY(win));
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ERightArrow;       { Right Arrow }
Begin
   If Index < z-x+1 Then Begin
      nGotoXY(win,nWhereX(win)+1,nWhereY(win));
      Index := Index + 1;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EEnd;               { End         }
Begin
   Index := Length(es)+1;
   If Index >= z-x+1 Then Index := Length(es);
   If Index < 1 Then Index := 1;
   nGotoXY(win,x+(Index-1),y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure EBackSpace;          { Backspace  }
Begin
   Index := Index - 1;
   If Index < 1 Then Begin
      Index := 1;
      Exit;
   End Else
      If nWhereX(win) > x Then nGotoXY(win,nWhereX(win) - 1,nWhereY(win));
   Delete(es,Index,1);
   NewString;
   WriteString;
   nGotoXY(win,x+(Index-1),y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ETurboBackSpace; { Ctrl/Backspace  }
Begin
   If Index = 1 Then Exit;
   Delete(es,1,Index-1);
   NewString;
   Index := 1;
   If nWhereX(win) > x Then nGotoXY(win,1,nWhereY(win));
   WriteString;
   nGotoXY(win,x,y);
END;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlLeftArrow;{ Ctrl Left Arrow }
Begin
   If nEC.IsHidden Then Begin
      EHome;
      Exit;
   End;
   If es[Index-1] = ' ' Then Index := Index-1;
   If es[Index] <> ' ' Then Begin
      While (Index > 1) And (es[Index] <> ' ') Do
         Index := Index-1;
   End Else
   If es[Index] = ' ' Then Begin
      While (Index > 1) And (es[Index] = ' ') Do
         Index := Index-1;
      While (Index > 1) And (es[Index] <> ' ') Do
         Index := Index-1;
   End;
   If Index = 1 Then
      nGotoXY(win,x,y)
   Else Begin
      nGotoXY(win,x+Index,y);
      Index := Index+1;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ECtrlRightArrow;{ Ctrl Right Arrow  }
Begin
   If nEC.IsHidden Then Begin
      EEnd;
      Exit;
   End;
   While (Index < Length(es)) And (es[Index] <> ' ') Do
   Begin
        Index := Index+1;
   End;
   While (Index < Length(es)) And (es[Index] = ' ') Do
   Begin
        Index := Index+1;
   End;
   nGotoXY(win,x+Index-1,y);
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure CheckForWriteChar;
Begin
   If Not (Ch In [#8,#9,#27,#127,#255]) Then Begin
      If (ch in [#10,#13]) {and not ControlKey} Then exit;
      If nEC.FirstTime Then Begin
         es := '';
         WriteString;
         nGotoXY(win,X,Y);
         Index := 1;
         WriteChar;
         nEC.FirstTime := False;
      End Else Begin
         If nEC.InsMode Then Begin
            EInsert;
            WriteChar;
         End Else WriteChar;
      End;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ProcessSpecialKey;
begin
   Case ch of
   #16..#25,
   #30..#38,
   #44..#50,
   #59..#68,
   #84..#90,
  #92..#113,
       #118,
       #132,
        #72,
        #73,
        #80,
        #81 : Begin SEditExit:=True;Exit;End;
        #71 : EHome;
        #75 : ELeftArrow;
        #77 : ERightArrow;
        #79 : EEnd;
        #82 : EInsMode;
        #83 : EDelete;
        #15,
       #115 : ECtrlLeftArrow;
       #116 : ECtrlRightArrow;
       #117 : ECtrlEnd;
   End;
End;

{~~~~~~~~~~~~~~~~~~~~~~~~~~~~}
Procedure ProcessNormalKey;
Var
   i : integer;
begin
   For i := 1 to Length(nEC.Special) Do Begin
      If ch = nEC.Special[i] Then Begin
         SEditExit:=True;
         Exit;
      End;
   End;
   case ch of
        #8 : Begin nEC.FirstTime := False;EBackSpace;End;
        #9 : ECtrlRightArrow;
      #127 : Begin nEC.FirstTime := False;ETurboBackSpace;End;
   end;
   CheckForWriteChar;
end;

{============================================================================}
Begin
   SEditExit := nEC.ExitMode;
   ZMode := z <> 0;
   If CursPos > Length(es)+x Then
      Index := Length(es)+1                { End Of String    }
   Else Index := CursPos+1-x;              { Inside Of String }
   If Not ZMode then z := x+length(es);
   Newstring;
   WriteString;
   nGotoXY(win,CursPos,y);
   Repeat
      If Not ZMode then z := x+length(es);
      ch := ReadKey;
      If ch = #0 Then Begin
         ch := ReadKey;
         ProcessSpecialKey;
      End Else
         ProcessNormalKey;
   Until (ch In [#10,#13,#27]) or SEditExit;
   If ch = #10 Then ch := #13;
   nEC.FirstTime := False;
   NewString;
End;{ of nSEdit }

Begin
   nEC.Init(false,false,false,false,'');
   { load the color pairs array with color pair indices (0..63) }
   For bg := 0 to 7 Do For fg := 0 to 7 do cp[bg,fg] := (bg*8)+fg;
   { initialize ncurses }
   If StartCurses(ActiveWn) Then
      nscreen := ActiveWn
   Else
      Halt;

   SubWn := nil;
   TextMode(LastMode);

   { Redirect the standard output }
   assigncrt(Output);
   Rewrite(Output);
   TextRec(Output).Handle:=StdOutputHandle;
   { Redirect the standard input }
   assigncrt(Input);
   Reset(Input);
   TextRec(Input).Handle:=StdInputHandle;

   { set the unit exit procedure }
   ExitSave := ExitProc;
   ExitProc := @nExit;

End. { of Unit nCrt }
     
