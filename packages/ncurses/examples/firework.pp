{
}
program firework;
uses
  ncurses;

CONST
  my_bg : LONGINT = COLOR_BLACK;

Procedure showit;
begin
  refresh;
  napms(120);
end;

Function get_colour(Var bold : chtype) : longint;
Var
  attr : longint;
begin
  attr:=random(16) + 1;
  bold:=A_NORMAL;
  if (attr > 8) then
   begin
     bold:=A_BOLD;
     attr:=attr and 7;
   end;
  get_colour:=attr;
end;


Procedure explode(Row,Col : longint);
var
  Bold : chtype;
begin
  ncurses.erase;
  mvaddstr(row,col,'-');
  showit;

  init_pair(1,get_colour(bold),my_bg);
  attrset(COLOR_PAIR(1) or bold);
  mvaddstr(row-1,col-1,' - ');
  mvaddstr(row,col-1,'-+-');
  mvaddstr(row+1,col-1,' - ');
  showit;

  init_pair(1,get_colour(bold),my_bg);
  attrset(COLOR_PAIR(1) or bold);
  mvaddstr(row-2,col-2,' --- ');
  mvaddstr(row-1,col-2,'-+++-');
  mvaddstr(row,  col-2,'-+#+-');
  mvaddstr(row+1,col-2,'-+++-');
  mvaddstr(row+2,col-2,' --- ');
  showit;


  init_pair(1,get_colour(bold),my_bg);
  attrset(COLOR_PAIR(1) or bold);
  mvaddstr(row-2,col-2,' +++ ');
  mvaddstr(row-1,col-2,'++#++');
  mvaddstr(row,  col-2,'+# #+');
  mvaddstr(row+1,col-2,'++#++');
  mvaddstr(row+2,col-2,' +++ ');
  showit;

  init_pair(1,get_colour(bold),my_bg);
  attrset(COLOR_PAIR(1) or bold);
  mvaddstr(row-2,col-2,'  #  ');
  mvaddstr(row-1,col-2,'## ##');
  mvaddstr(row,  col-2,'#   #');
  mvaddstr(row+1,col-2,'## ##');
  mvaddstr(row+2,col-2,'  #  ');
  showit;

  init_pair(1,get_colour(bold),my_bg);
  attrset(COLOR_PAIR(1) or bold);
  mvaddstr(row-2,col-2,' # # ');
  mvaddstr(row-1,col-2,'#   #');
  mvaddstr(row,  col-2,'     ');
  mvaddstr(row+1,col-2,'#   #');
  mvaddstr(row+2,col-2,' # # ');
  showit;
end;

Var
  startp,endp,row,diff,flag : longint;
  direction : boolean;
begin
  flag:=0;
  initscr;
  if (has_colors<>0) then
   start_color;
  curs_set(0);
  randomize;
  cbreak;
  While true do
   begin
     repeat
       startp:=random (COLS -3);
       endp:=random (COLS - 3);
       If startp < 2 then
        startp:=2;
       If endp <2  then
        endp:=2;
       direction:=startp > endp ;
       diff:=abs(startp-endp);
     until (diff>2) and (diff<(LINES-2));
     attrset(A_NORMAL);
     for row:=0 to diff do
      begin;
        If direction then
         mvaddstr(LINES - row,startp + row ,'/')
        else
         mvaddstr(LINES - row,startp - row ,'\');
        inc(flag);
        if flag<>0 then
         begin
           showit;
           erase;
           flag:=0;
         end;
      end;
     inc(flag);
     if (flag<>0) then
      begin
        showit;
        flag:=0;
      end;
     randomize;
     If Direction then
      explode(LINES-row,startp+diff)
     Else
      explode(LINES-row,startp-diff);
     erase;
     showit;
   end;
end.
