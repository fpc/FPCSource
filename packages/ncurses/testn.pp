{
  $Id$

  Simple ncurses test
}
program testn;
uses
  ncurses;

var
  win : pWINDOW;
begin
  if initscr=Nil then halt(1);
  start_color;
  win:= newwin (10,60,10,10);
  if win=nil then
   begin
     endwin;
     halt(1);
   end;
  init_pair(1,COLOR_WHITE,COLOR_BLUE);
  wbkgd(win, COLOR_PAIR(1));
  erase;
  refresh;
  box(win, ACS_VLINE, ACS_HLINE);
  wrefresh(win);
  mvwaddstr(win,1,1,'Press any key to continue !');
  wrefresh(win);
  raw;
  wgetch(win);
  endwin;
end.
{
  $Log$
  Revision 1.1  2000-07-13 06:34:14  michael
  + Initial import

  Revision 1.2  1999/11/24 23:42:00  peter
    * fpcmake updates

}
