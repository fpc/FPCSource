program testcon;

{$mode objfpc}

uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  TableNames : TStringList;
  I : Integer;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.Active:=True;
    TableNames:=TStringList.Create;
    TableNames.Sorted:=True;
    Try
      Conn.GetTableNames(TableNames,True);
      Writeln('Found ',TableNames.Count,' tables : ');
      For I:=0 to TableNames.Count-1 do
        Writeln(TableNames[i]);
    finally
      TableNames.Free;
    end;
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end.
