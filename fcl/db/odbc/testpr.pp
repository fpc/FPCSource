program testpr;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  ProcedureNames : TStringList;
  I : Integer;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='BUGS';
    Conn.Active:=True;
    ProcedureNames:=TStringList.Create;
    ProcedureNames.Sorted:=True;
    Try
      Conn.GetProcedureNames(ProcedureNames);
      Writeln('Found ',ProcedureNames.Count,' procedures:');
      For I:=0 to ProcedureNames.Count-1 do
        Writeln(ProcedureNames[i]);
    finally
      ProcedureNames.Free;
    end;
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end.
