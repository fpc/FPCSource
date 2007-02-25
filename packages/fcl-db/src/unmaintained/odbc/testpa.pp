program testpa;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;
  ProcedureParams : TStringList;
  I : Integer;
  PT : TODBCParamType;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='BUGS';
    Conn.Active:=True;
    ProcedureParams:=TStringList.Create;
    ProcedureParams.Sorted:=True;
    Try
      For PT:=ptUnknown to ptRetval do
        begin
        Conn.GetProcedureParams('GET_BUGID',[Pt],ProcedureParams);
        Writeln('Found ',ProcedureParams.Count,' parameters of type ',ODBCParamTypeNames[Pt],' :');
         For I:=0 to ProcedureParams.Count-1 do
          Writeln(ProcedureParams[i]);
        end;
    finally
      ProcedureParams.Free;
    end;
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end.
