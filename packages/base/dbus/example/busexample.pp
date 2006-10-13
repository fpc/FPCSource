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

const
  SINTAX_TEXT = 'Syntax: dbus-example [send|receive|listen|query] [<param>]';

var
  err: DBusError;
  conn: PDBusConnection;
  ret: cint;

{
 * Send a broadcast signal
 }
procedure BusSend(sigvalue: PChar);
var
  msg: PDBusMessage;
  args: DBusMessageIter;
  serial: dbus_uint32_t = 0;
begin
  WriteLn('Sending signal with value ', string(sigvalue));

  { Request the name of the bus }
  ret := dbus_bus_request_name(conn, 'test.signal.source', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

  // create a signal & check for errors
  msg := dbus_message_new_signal('/test/signal/Object', // object name of the signal
                                 'test.signal.Type', // interface name of the signal
                                 'Test'); // name of the signal
  if (msg = nil) then
  begin
    WriteLn('Message Null');
    Exit;
  end;

  // append arguments onto signal
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @sigvalue) = 0) then
  begin
    WriteLn('Out Of Memory!');
    Exit;
  end;

  // send the message and flush the connection
  if (dbus_connection_send(conn, msg, @serial) = 0) then
  begin
    WriteLn('Out Of Memory!');
    Exit;
  end;
  
  dbus_connection_flush(conn);

  WriteLn('Signal Sent');

  // free the message and close the connection
  dbus_message_unref(msg);
end;

{
 * Listens for signals on the bus
 }
procedure BusReceive;
var
  msg: PDBusMessage;
  args: DBusMessageIter;
  sigvalue: PChar;
begin
  WriteLn('Listening for signals');

  { Request the name of the bus }
  ret := dbus_bus_request_name(conn, 'test.signal.sink', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  // add a rule for which messages we want to see
  dbus_bus_add_match(conn, 'type=''signal'',interface=''test.signal.Type''', @err); // see signals from the given interface
  dbus_connection_flush(conn);
  if (dbus_error_is_set(@err) <> 0) then
  begin
    WriteLn('Match Error (', err.message, ')');
    Exit;
  end;
  WriteLn('Match rule sent');

  // loop listening for signals being emmitted
  while (true) do
  begin

    // non blocking read of the next available message
    dbus_connection_read_write(conn, 0);
    msg := dbus_connection_pop_message(conn);

    // loop again if we haven't read a message
    if (msg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    // check if the message is a signal from the correct interface and with the correct name
    if (dbus_message_is_signal(msg, 'test.signal.Type', 'Test') <> 0) then
    begin
      // read the parameters
      if (dbus_message_iter_init(msg, @args) = 0) then
         WriteLn('Message Has No Parameters')
      else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
         WriteLn('Argument is not string!')
      else
         dbus_message_iter_get_basic(@args, @sigvalue);

      WriteLn('Got Signal with value ', sigvalue);
    end;

    // free the message
    dbus_message_unref(msg);
  end;
end;

procedure reply_to_method_call(msg: PDBusMessage; conn: PDBusConnection);
var
  reply: PDBusMessage;
  args: DBusMessageIter;
  stat: Boolean = true;
  level: dbus_uint32_t = 21614;
  serial: dbus_uint32_t = 0;
  param: PChar = '';
begin
   // read the arguments
   if (dbus_message_iter_init(msg, @args) = 0) then
      WriteLn('Message has no arguments!')
   else if (DBUS_TYPE_STRING <> dbus_message_iter_get_arg_type(@args)) then
      WriteLn('Argument is not string!')
   else
      dbus_message_iter_get_basic(@args, @param);

   WriteLn('Method called with ', param);

   // create a reply from the message
   reply := dbus_message_new_method_return(msg);

   // add the arguments to the reply
   dbus_message_iter_init_append(reply, @args);
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_BOOLEAN, @stat) = 0) then
   begin
     WriteLn('Out Of Memory!');
     Exit;
   end;
   if (dbus_message_iter_append_basic(@args, DBUS_TYPE_UINT32, @level) = 0) then
   begin
     WriteLn('Out Of Memory!');
     Exit;
   end;

   // send the reply && flush the connection
   if (dbus_connection_send(conn, reply, @serial) = 0) then
   begin
     WriteLn('Out Of Memory!');
     Exit;
   end;
   dbus_connection_flush(conn);

   // free the reply
   dbus_message_unref(reply);
end;

{
 * Server that exposes a method call and waits for it to be called
 }
procedure BusListen;
var
  msg, reply: PDBusMessage;
  args: DBusMessageIter;
  param: PChar;
begin
  WriteLn('Listening for method calls');

  { Request the name of the bus }
  ret := dbus_bus_request_name(conn, 'test.method.server', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

  // loop, testing for new messages
  while (true) do
  begin
    // non blocking read of the next available message
    dbus_connection_read_write(conn, 0);
    msg := dbus_connection_pop_message(conn);

    // loop again if we haven't got a message
    if (msg = nil) then
    begin
      sleep(1);
      Continue;
    end;

    // check this is a method call for the right interface & method
    if (dbus_message_is_method_call(msg, 'test.method.Type', 'Method') <> 0) then
       reply_to_method_call(msg, conn);

    // free the message
    dbus_message_unref(msg);
  end;
end;

{
 * Call a method on a remote object
 }
procedure BusCall(param: PChar);
var
  msg: PDBusMessage;
  args: DBusMessageIter;
  pending: PDBusPendingCall;
  stat: Boolean;
  level: dbus_uint32_t;
begin
  WriteLn('Calling remote method with ', param);

  { Request the name of the bus }
  ret := dbus_bus_request_name(conn, 'test.method.caller', DBUS_NAME_FLAG_REPLACE_EXISTING, @err);

  if dbus_error_is_set(@err) <> 0 then
  begin
    WriteLn('Name Error: ' + err.message);
    dbus_error_free(@err);
  end;

  if ret <> DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER then Exit;

  // create a new method call and check for errors
  msg := dbus_message_new_method_call('test.method.server', // target for the method call
                                      '/test/method/Object', // object to call on
                                      'test.method.Type', // interface to call on
                                      'Method'); // method name
  if (msg = nil) then
  begin
    WriteLn('Message Null');
    Exit;
  end;

  // append arguments
  dbus_message_iter_init_append(msg, @args);
  if (dbus_message_iter_append_basic(@args, DBUS_TYPE_STRING, @param) = 0) then
  begin
    WriteLn('Out Of Memory!');
    Exit;
  end;

  // send message and get a handle for a reply
  if (dbus_connection_send_with_reply(conn, msg, @pending, -1) = 0) then // -1 is default timeout
  begin
    WriteLn('Out Of Memory!');
    Exit;
  end;
  if (pending = nil) then
  begin
    WriteLn('Pending Call Null');
    Exit;
  end;
  dbus_connection_flush(conn);

  WriteLn('Request Sent');

  // free message
  dbus_message_unref(msg);

  // block until we recieve a reply
  dbus_pending_call_block(pending);

  // get the reply message
  msg := dbus_pending_call_steal_reply(pending);
  if (msg = nil) then
  begin
    WriteLn('Reply Null');
    Exit;
  end;
  // free the pending message handle
  dbus_pending_call_unref(pending);

  // read the parameters
  if (dbus_message_iter_init(msg, @args) = 0) then
     WriteLn('Message has no arguments!')
  else if (DBUS_TYPE_BOOLEAN <> dbus_message_iter_get_arg_type(@args)) then
     WriteLn('Argument is not boolean!')
  else
     dbus_message_iter_get_basic(@args, @stat);

  if (dbus_message_iter_next(@args) = 0) then
     WriteLn('Message has too few arguments!')
  else if (DBUS_TYPE_UINT32 <> dbus_message_iter_get_arg_type(@args)) then
     WriteLn('Argument is not int!')
  else
     dbus_message_iter_get_basic(@args, @level);

  WriteLn('Got Reply: ', stat, ', ', level);

  // free reply
  dbus_message_unref(msg);
end;

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
  
  { Parses parameters }
  
  if (ParamCount <> 1) and (ParamCount <> 2) then WriteLn(SINTAX_TEXT)
  else
  begin
    if ParamStr(1) = 'send' then BusSend(PChar(ParamStr(2)))
    else if ParamStr(1) = 'receive' then BusReceive()
    else if ParamStr(1) = 'listen' then BusListen()
    else if ParamStr(1) = 'call' then BusCall(PChar(ParamStr(2)))
    else WriteLn(SINTAX_TEXT);
  end;

  { Finalization }
  dbus_connection_close(conn);
end.

