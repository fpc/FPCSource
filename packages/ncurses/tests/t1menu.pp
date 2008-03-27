program Menu_Basics;
{
  Example 18. Menu Basics
  from ncurses howto
}
{$MODE OBJFPC}

uses
  ncurses, menu;

const
  choices: array[0..4] of PChar  =
                      (
                        'Choice 1',
                        'Choice 2',
                        'Choice 3',
                        'Choice 4',
                        'Exit'
                      );


var
  my_items: ppITEM;
  my_menu: pMENU;
  c, n_choices, i: Longint;
  cur_item: pITEM;
begin
  try
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);

    n_choices := 5;
    GetMem(my_items, (n_choices+1)*sizeof(pITEM));

    for i := 0 to n_choices - 1 do
      my_items[i] := new_item(choices[i], choices[i]);
    my_items[n_choices] := nil;

    my_menu := new_menu(my_items);
    mvprintw(LINES - 2, 0, 'F1 to Exit');
    post_menu(my_menu);
    refresh();

    c := getch();
    while c <> KEY_F(1) do
    begin
      case c of
        KEY_DOWN: menu_driver(my_menu, REQ_DOWN_ITEM);
        KEY_UP: menu_driver(my_menu, REQ_UP_ITEM);
      else
      end;
      c := getch();
    end

  finally
    free_item(my_items[0]);
    free_item(my_items[1]);
    free_menu(my_menu);
    FreeMem(my_items, (n_choices+1)*sizeof(pITEM));
    endwin();
  end;
end.