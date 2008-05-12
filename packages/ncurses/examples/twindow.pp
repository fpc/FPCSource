program test_window;
{$mode objfpc}

uses
  ncurses, panel, sysutils;

procedure printw(win: PWINDOW; y,x: Smallint; fmt: AnsiString; args: Array of const);
var
  tstr: AnsiString;
begin
  FmtStr(tstr, fmt, args);
  mvwaddstr(win,y,x, PChar(tstr));
end;

procedure printinfo(win: PWINDOW);
begin
  with win^ do
  begin
    printw(win,1 ,1,'_cury=%-3d, _curx=%-3d : cursor position',[_cury,_curx]);
    printw(win,2 ,1,'_maxy=%-3d, _maxx=%-3d : maximums of x and y, NOT window size',[_maxy,_maxx]);
    printw(win,3 ,1,'_begy=%-3d, _begx=%-3d : screen coords of upper-left-hand corner',[_begy,_begx]);
    printw(win,4 ,1,'_flags=%-3d           : window state flags',[_flags]);
    printw(win,5 ,1,'_attrs=%-4d          : current attribute for non-space character',[_attrs]);
    printw(win,6 ,1,'_bkgd=%-3d            : current background char/attribute pair',[_bkgd]);
    printw(win,7 ,1,'_notimeout=%-1d         :  no time out on function-key entry?', [Byte(_notimeout)]);
    printw(win,8 ,1,'_clear=%-1d             : consider all data in the window invalid?',[Byte(_clear)]);
    printw(win,9 ,1,'_leaveok=%-1d           : OK to not reset cursor on exit?',[Byte(_leaveok)]);
    printw(win,10,1,'_scroll=%-1d            : OK to scroll this window?',[Byte(_scroll)]);
    printw(win,11,1,'_idlok=%-1d             : OK to use insert/delete line?',[Byte(_idlok)]);
    printw(win,12,1,'_idcok=%-1d             : OK to use insert/delete char?',[Byte(_idcok)]);
    printw(win,13,1,'_immed=%-1d             : window in immed mode? (not yet used)',[Byte(_immed)]);
    printw(win,14,1,'_sync=%-1d              : window in sync mode?',[Byte(_sync)]);
    printw(win,15,1,'_use_keypad=%-1d        : process function keys into KEY_ symbols?',[Byte(_use_keypad)]);
    printw(win,16,1,'_delay=%-3d           : 0 = nodelay, <0 = blocking, >0 = delay',[_delay]);
    printw(win,17,1,'_parx=%-3d            : x coordinate of this window in parent',[_parx]);
    printw(win,18,1,'_pary=%-3d            : y coordinate of this window in parent',[_pary]);
    printw(win,19,1,'_yoffset=%-3d         : real begy is _begy + _yoffset',[_yoffset]);
    printw(win,20,1,'_bkgrnd.attr=%-4d    : current background char/attribute pair',[_bkgrnd.attr]);
  end;
end;

var
  win : pWINDOW;
  cy, cx, by, bx, my, mx: Longint;
begin

  try
    initscr();
    start_color;
    noecho;
    init_pair(1,COLOR_WHITE,COLOR_BLUE);
    init_pair(2,COLOR_RED,COLOR_BLUE);

    win:= newwin( LINES - 2, COLS - 6, 1, 3);

    wbkgd(win, COLOR_PAIR(1));
    erase;
    refresh;

    box(win, ACS_VLINE, ACS_HLINE);

    wmove(win,12,24);
    printinfo(win);

    wrefresh(win);
    getch;

    getyx(win,cy,cx);
    getbegyx(win,by,bx);
    getmaxyx(win,my,mx);
    delwin(win);
    clear();

    printw(stdscr,1 ,1,'getyx(win,%d,%d)',[cy,cx]);
    printw(stdscr,2 ,1,'getbegyx(win,%d,%d);',[by,bx]);
    printw(stdscr,3 ,1,'getmaxyx(win,%d,%d);',[my,mx]);
    getch;

  finally
    endwin();
  end;
end.