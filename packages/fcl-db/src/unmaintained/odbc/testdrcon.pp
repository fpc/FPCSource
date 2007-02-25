program testdrcon;
{$mode objfpc}
uses fpodbc,Classes;

var
  Conn : TODBCConnection;

begin
  Conn:=TODBCConnection.Create(Nil);
  Try
    Conn.drivername:='Microsoft Access Driver (*.mdb)';
    Conn.DriverParams.Add('DBQ=d:\temp\odbc\testodbc.mdb');
    Conn.Active:=True;
    Writeln('Connected !!');
    Conn.Active:=False;
    Writeln('Disconnected again.');
  Finally
    Conn.free;
  end;
end.
