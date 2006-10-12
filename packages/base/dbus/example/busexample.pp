program busexample;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  ctypes,
  dbus;

procedure BusSend;
begin

end;

procedure BusReceive;
begin

end;

procedure BusListen;
begin

end;

procedure BusQuery;
begin

end;

const
  SINTAX_TEXT = 'Syntax: dbus-example [send|receive|listen|query] [<param>]';
var
  err: DBusError;
  conn: PDBusConnection;
  ret: cint;
begin
  { Initializes the errors }
  dbus_error_init(@err);
  
  { Connection }
  conn := dbus_bus_get(DBUS_BUS_SESSION, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Connection Error: ' + err.message);
    dbus_error_free(@err);
  end;
  
  if conn = nil then Exit;
  
  { Request the name of the bus }
  ret := dbus_bus_request_name(conn, 'test.method.server', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;
  
  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

  { Parses parameters }
  
  if (ParamCount <> 1) and (ParamCount <> 2) then WriteLn(SINTAX_TEXT)
  else
  begin
    if ParamStr(1) = 'send' then BusSend
    else if ParamStr(1) = 'receive' then BusReceive
    else if ParamStr(1) = 'listen' then BusListen
    else if ParamStr(1) = 'query' then BusQuery
    else WriteLn(SINTAX_TEXT);
  end;

  { Finalization }
  dbus_connection_close(conn);
end.

