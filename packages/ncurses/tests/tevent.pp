program test_event;

{$MODE OBJFPC}

uses
  ncurses, sysutils;


var
  ch: chtype;
begin
  try
    initscr();
    noecho();
    clear();
    cbreak();
    keypad(stdscr, TRUE);
    mousemask(1, nil);

    mvaddstr(1, 1,'press F10 or q to exit');
    mvaddstr(2, 1,'press 1 to cbreak mode');
    mvaddstr(3, 1,'press 2 to raw mode');
    mvaddstr(4, 1,'press 3 to halfdelay(10) mode');
    repeat
      ch := getch;
      mvaddstr(LINES - 1, 1,'                                ');
      case ch of
        ERR: mvaddstr(LINES - 1, 1,'timeout: 1 sec');
        chtype('1'): cbreak();
        chtype('2'): raw();
        chtype('3'): halfdelay(10);
      else
        mvaddstr(LINES - 1, 1,PChar(Format('name:%-14s code:%d', [ keyname(ch), ch ] )));
      end;
    until (ch = chtype('q')) OR (ch = KEY_F(10));
  finally
    endwin();
  end;
end.