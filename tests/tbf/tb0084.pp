{ %FAIL }
{ Old file: tbf0361.pp }
{  }

type

  ExecProc = Procedure;

type
  MenuItem = record
               Caption: String[32];
               Exec: ExecProc;
             end;

Procedure AddItem(ACaption: String; AExec: ExecProc; var Item: MenuItem);
begin
  Item.Caption:=ACaption;
  Item.Exec:=AExec;
end;

Procedure ExecFirstItem;
begin
  Writeln('Result of "Item 1"');
end;

var M1,M2,M3: MenuItem;
    Ep: ExecProc;

begin
  AddItem('Item 1',Nil,M1);
  Ep:=ExecFirstItem;        // should give error in fpc mode
  AddItem('Item 2',Ep,M2);
  AddItem('Item 3',@ExecFirstItem,M3);
end.
