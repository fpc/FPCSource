Program Client;

{
  Program to test Sockets unit by Michael van Canneyt and Peter Vreman
  Client Version, First Run sock_svr to let it create a socket and then
  sock_cli to connect to that socket
}

uses Sockets,BaseUnix;

procedure PError(const S : string);
begin
  writeln(S,SocketError);
  halt(100);
end;


Var
  Saddr    : String[25];
  Buffer   : string [255];
  S        : Longint;
  Sin,Sout : Text;
  i        : integer;
begin
  S:=Socket (AF_UNIX,SOCK_STREAM,0);
  if SocketError<>0 then
   Perror('Client : Socket : ');
  Saddr:='ServerSoc';
  if not Connect (S,SAddr,Sin,Sout) then
   PError('Client : Connect : ');
  Reset(Sin);
  ReWrite(Sout);
  Buffer:='This is a textstring sent by the Client.';
  for i:=1 to 10 do
   Writeln(Sout,Buffer);
  Flush(Sout);
  Readln(SIn,Buffer);
  WriteLn(Buffer);
  Close(sout);
end.
