program testcon;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.DSN:='FPC';
    Conn.Active:=True;
    Writeln('Connected !!');
    Conn.Active:=False;
    Writeln('Disconnected again');
  Finally
    Conn.free;
  end;
end.
