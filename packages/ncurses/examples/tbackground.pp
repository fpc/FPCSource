
uses
   ncurses, sysutils;

var
    f, b: Smallint;
begin

 initscr();
 cbreak();
 noecho();

 if (has_colors()) then
  begin
	start_color();

	pair_content(0, @f, @b);
	printw(PChar('pair 0 contains (%d,%d)'#10), f, b);
	getch();

	printw('Initializing pair 1 to red/black'#10);
	init_pair(1, COLOR_RED, COLOR_BLACK);
	bkgdset(chtype(' ') OR COLOR_PAIR(1));
	printw('RED/BLACK'#10);
	getch();

	printw('Initializing pair 2 to white/blue'#10);
	init_pair(2, COLOR_WHITE, COLOR_BLUE);
	bkgdset(chtype(' ') OR COLOR_PAIR(2));
	printw('WHITE/BLUE'#10);
	getch();

	printw('Resetting colors to pair 0'#10);
	bkgdset(chtype(' ') OR COLOR_PAIR(0));
	printw('Default Colors'#10);
	getch();

	printw('Resetting colors to pair 1'#10);
	bkgdset(chtype(' ') OR COLOR_PAIR(1));
	printw('RED/BLACK'#10);
	getch();

	printw('Setting screen to pair 0'#10);
	bkgd(chtype(' ') OR COLOR_PAIR(0));
	getch();

	printw('Setting screen to pair 1'#10);
	bkgd(chtype(' ') OR COLOR_PAIR(1));
	getch();

	printw('Setting screen to pair 2'#10);
	bkgd(chtype(' ') OR COLOR_PAIR(2));
	getch();

	printw('Setting screen to pair 0'#10);
	bkgd(chtype(' ') OR COLOR_PAIR(0));
	getch();

 end
 else
 begin
	printw('This demo requires a color terminal'#10);

	getch();
    end;
    endwin();
end.
