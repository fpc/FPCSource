program mouse_test;
{$MODE OBJFPC}
{$COPERATORS ON}


uses
  ncurses, panel, sysutils;

procedure draw;

function randomchar: chtype;
var
  ch: Char = #0;
begin
  while not (ch in ['0'..'9','A'..'Z','a'..'z']) do
    ch := Char(Random(123));
  randomchar := chtype(ch);
end;

function randompair: longint;
var
  pair: longint = 0;
begin
  while not (pair in [1..5]) do
    pair := Random(6);
  randompair := pair;
end;

var
  y, x:  Smallint;
begin
  for y := 0 to 2 do
    for x := 0 to COLS - 7 do
      mvaddch(y, x, randomchar OR COLOR_PAIR(randompair));
  attron(A_BOLD OR COLOR_PAIR(7));
  mvaddstr(0, COLS - 6, '      ');
  mvaddstr(1, COLS - 6, ' QUIT ');
  mvaddstr(2, COLS - 6, '      ');
  attroff(A_BOLD OR COLOR_PAIR(7));
  for y := 3 to LINES - 1 do
    for x := 0 to COLS - 1 do
      mvaddch(y, x, randomchar OR COLOR_PAIR(randompair));
end;


var
  win: PWINDOW;
  pan: PPANEL;
  str: AnsiString;
function doevent: chtype;
var
  event: MEVENT;
begin
  getmouse(@event);
  if (event.y > 2) OR (event.x < COLS - 6) then
  begin
    mvwaddstr(win, 1, 1, '                  ');
    str := Format('y := %D, x := %D', [event.y, event.x]);
    mvwaddstr(win, 1, 2, PChar(str));
    wattron(win,A_BOLD);
    mvwaddch(win, 3, 9, mvinch(event.y,event.x ));
    wattroff(win,A_BOLD);
    halfdelay(12);

    show_panel(pan);

    if event.bstate AND  BUTTON1_RELEASED<> 0 then
      mvwaddstr(win, 5, 2,'BUTTON1_RELEASED')
    else if event.bstate AND BUTTON2_RELEASED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON2_RELEASED')
    else if event.bstate AND BUTTON3_RELEASED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON3_RELEASED')
    else if event.bstate AND BUTTON1_PRESSED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON1_PRESSED ')
    else if event.bstate AND BUTTON2_PRESSED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON2_PRESSED ')
    else if event.bstate AND BUTTON3_PRESSED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON3_PRESSED ')
    else if event.bstate AND BUTTON1_CLICKED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON1_CLICKED ')
    else if event.bstate AND BUTTON2_CLICKED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON2_CLICKED ')
    else if event.bstate AND BUTTON3_CLICKED <> 0 then
      mvwaddstr(win, 5, 2,'BUTTON3_CLICKED ');

    doevent := wgetch(win);
    cbreak();
    hide_panel(pan);
  end
  else
    doevent := chtype('q')
end;

var
  ch: chtype;
  my_bg: NC_FPC_COLOR = COLOR_BLACK;
begin
  try
    initscr();
    noecho();
    clear();
    cbreak();
    keypad(stdscr, TRUE);
    curs_set(0);
    mousemask(ALL_MOUSE_EVENTS, nil);

    if (has_colors()) then
    begin
      start_color();
      if (use_default_colors() = OK) then
        my_bg := -1
      else
        my_bg := COLOR_BLACK;

      init_pair(1, COLOR_YELLOW, my_bg);
      init_pair(2, COLOR_RED, my_bg);
      init_pair(3, COLOR_MAGENTA, my_bg);
      init_pair(4, COLOR_CYAN, my_bg);
      init_pair(5, COLOR_GREEN, my_bg);
      init_pair(6, COLOR_WHITE, COLOR_BLUE);
      init_pair(7, COLOR_WHITE, COLOR_RED);
    end;

    win:= newwin(7, 20, (LINES - 7) DIV 2 , (COLS - 20) DIV 2);
    pan := new_panel(win);
    box(win, ACS_VLINE, ACS_HLINE);
    wbkgd(win, COLOR_PAIR(6));

    draw;
    repeat
      if ch = KEY_MOUSE then
        ch := doevent
      else
        ch := getch();
    until  (ch = chtype('q')) OR (ch = KEY_F(10));

  finally
    del_panel(pan);
    delwin(win);
    curs_set(1);
    endwin();
  end;
end.

