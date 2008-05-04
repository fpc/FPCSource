program test_panel;

{$MODE OBJFPC}

uses
  ncurses, panel, sysutils;


Type
  PPWINDOW = ^PWINDOW;
  PPPANEL  = ^PPANEL;

const
  NLINES = 8;
  NCOLS  = 32;

procedure print_in_middle(win: PWINDOW; starty, startx, width: Longint; str: AnsiString; color: chtype);
var
  slength: Longint;
  x, y: Longint;
  temp: Double;
begin

  if win = nil then
    win := stdscr;
  getyx(win, y, x);
  if startx <> 0 then
    x := startx;
  if starty <> 0 then
    y := starty;
  if width = 0 then
    width := 80;

  slength := Length(str);
  temp := (width - slength)/ 2;
  x := startx + round(temp);
  wattron(win, color);
  mvwaddstr(win, y, x, PChar(str));
  wattroff(win, color);
  refresh();

end;


procedure win_show(win: PWINDOW; lab: AnsiString; label_color: Longint);
var
  startx, starty, height, width: Longint;
begin
        getbegyx(win, starty, startx);
        getmaxyx(win, height, width);

        box(win, 0, 0);
        mvwaddch(win, 2, 0, ACS_LTEE); 
        mvwhline(win, 2, 1, ACS_HLINE, width - 2);
        mvwaddch(win, 2, width - 1, ACS_RTEE);

        print_in_middle(win, 1, 0, width, lab, COLOR_PAIR(label_color));
end;

procedure init_panels(pans: PPPANEL; n: Longint);
var
  x, y, i: Longint;
  lab: AnsiString;
  win: PWINDOW;
begin
  y := 2;
  x := 3;
  for i := 0 to n - 1 do
  begin
    win := newwin(NLINES, NCOLS, y, x);
    FmtStr(lab, 'Window Number %d', [i + 1]);
    win_show(win, lab, i + 1);
    pans[i] := new_panel(win);
    y += 2;
    x += 4;
  end
end;

procedure select(var oldp: PPANEL; newp: PPANEL);
var
  win: PWINDOW;
begin
  win := panel_window(oldp);
  wattroff(win,A_BOLD);
  box(win,0,0);

  win := panel_window(newp);
  wattron(win,A_BOLD);
  box(win,0,0);

  oldp := newp;
end;

var
  my_panels: array[0..4] of PPANEL;
  selected:  PPANEL;
  ch:  chtype;
begin
  try

(* Initialize curses *)
  initscr();
  start_color();
  cbreak();
  noecho();
  keypad(stdscr, TRUE);

(* Initialize all the colors *)
  init_pair(1, COLOR_RED, COLOR_BLACK);
  init_pair(2, COLOR_GREEN, COLOR_BLACK);
  init_pair(3, COLOR_BLUE, COLOR_BLACK);
  init_pair(4, COLOR_CYAN, COLOR_BLACK);
  init_pair(5, COLOR_YELLOW, COLOR_BLACK);

  init_panels(my_panels, 5);

  set_panel_userptr(my_panels[0], my_panels[4]);
  set_panel_userptr(my_panels[1], my_panels[3]);
  set_panel_userptr(my_panels[2], my_panels[1]);
  set_panel_userptr(my_panels[3], my_panels[0]);
  set_panel_userptr(my_panels[4], my_panels[2]);

  select(selected,my_panels[4]);

(* Update the stacking order. 2nd panel will be on top *)
  update_panels();

(* Show it on the screen *)
  attron(COLOR_PAIR(4));
  mvprintw(LINES - 5, 1, 't : top');
  mvprintw(LINES - 4, 1, 'h : show or hide toggle');
  mvprintw(LINES - 3, 1, '1..5, home, end, up, down, tab : navigate ');
  mvprintw(LINES - 2, 1, 'F1 : to Exit');
  attroff(COLOR_PAIR(4));
  doupdate();

  ch := getch;
  while ch <> KEY_F(1) do
  begin
    case ch of
      chtype('1'): select(selected,my_panels[0]);
      chtype('2'): select(selected,my_panels[1]);
      chtype('3'): select(selected,my_panels[2]);
      chtype('4'): select(selected,my_panels[3]);
      chtype('5'): select(selected,my_panels[4]);
      KEY_HOME: select(selected,panel_above(nil));
      KEY_END: select(selected,panel_below(nil));
      KEY_UP: select(selected,panel_above(selected));
      KEY_DOWN: select(selected,panel_below(selected));
      9: select(selected,panel_userptr(selected));
      chtype('t'): top_panel(selected);
      chtype('h'):
      begin
        if panel_hidden(selected) = OK then
          hide_panel(selected)
        else
          show_panel(selected);
      end;
      else
      end;
    update_panels();
    doupdate();
    ch := getch;
  end;

  finally
    endwin();
  end;
end.