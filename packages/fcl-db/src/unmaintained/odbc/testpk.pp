program testpl;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  PrimaryKeyFields : TStringList;
  I : Integer;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='BUGS';
    Conn.Active:=True;
    PrimaryKeyFields:=TStringList.Create;
    PrimaryKeyFields.Sorted:=True;
    Try
      Conn.GetPrimaryKeyFields('BUGS',PrimaryKeyFields);
      Writeln('Found ',PrimaryKeyFields.Count,' primary key fields in table BUGS : ');
      For I:=0 to PrimaryKeyFields.Count-1 do
        Writeln(PrimaryKeyFields[i]);
    finally
      PrimaryKeyFields.Free;
    end;
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end.
