Program Example42;

{ This program demonstrates the FileSetAttr function }

Uses sysutils;

Begin
  If FileSetAttr ('ex40.pp',faReadOnly or faHidden)=0 then
    Writeln ('Successfully made file hidden and read-only.')
  else
    Writeln ('Coulnd''t make file hidden and read-only.');
End.