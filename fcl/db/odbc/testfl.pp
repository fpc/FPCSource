program testfl;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  FieldNames : TStringList;
  I : Integer;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.Active:=True;
    FieldNames:=TStringList.Create;
    FieldNames.Sorted:=True;
    Try
      Conn.GetFieldNames('FPDev',FieldNames);
      Writeln('Found ',FieldNames.Count,' Fields in table FPDev : ');
      For I:=0 to FieldNames.Count-1 do
        Writeln(FieldNames[i]);
    finally
      FieldNames.Free;
    end;
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end.
