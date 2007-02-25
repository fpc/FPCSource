program testbcon;
{
  Test browsingconnection
  - I don't have a driver which supports it though :/
}
{$mode objfpc}
uses fpodbc,Classes;

Type
  TApp = Class (TObject)
    Conn : TODBCConnection;
    Procedure GetParams (Sender : TObject; ListIn,ListOut : TStrings);
    Procedure Run;
  end;


{ TApp }

procedure TApp.GetParams(Sender: TObject; ListIn, ListOut: TStrings);

Var
  S : String;
  i : integer;

begin
  Writeln('Input parameters were :');
  With ListIN do
    For I:=0 to Count-1 do
      Writeln(Strings[i]);
  Writeln('Output parameters were :');
  With Listout do
    For I:=0 to Count-1 do
      Writeln(Strings[i]);
  Repeat
    Writeln('Parameter to add to input (empty to quit):');
    Readln(S);
    If S<>'' then
      ListIn.Add(S)
  Until S='';
end;

procedure TApp.Run;
begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.OnBrowseConnection:=@Self.GetParams;
    Conn.Active:=True;
    Writeln('Connected !!');
    Conn.Active:=False;
  Finally
    Conn.free;
  end;
end;

begin
  With Tapp.Create do
    Try
      Run;
    Finally
      Free;
    end;
end.
