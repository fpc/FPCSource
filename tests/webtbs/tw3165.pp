{ Source provided for Free Pascal Bug Report 3165 }
{ Submitted by "" on  2004-06-13 }
{ e-mail: plugwash@p10link.net }
program testwith;

{$ifdef fpc}{$mode objfpc}{$endif}

uses
  SysUtils;

type
  tconnect4game=class
    board : array[1..7,0..5] of byte;
  end;

var
  row ,col: byte;
  g : tconnect4game;

begin
  {g := tconnect4game.create;}
  with {g} tconnect4game.create do begin

    writeln(board[1,5]);
    row :=1;
    col :=5;
    writeln(board[col,row]);
    for row := 5 downto 0 do begin
      for col := 1 to 7 do begin
        writeln(' :in inner loop row='+inttostr(row)+' col='+inttostr(col)+' board[col,row]='+inttostr(board[col,row]));
      end;
    end;
  end;
end.
