Program server;

{
  Program to test Sockets unit by Michael van Canneyt and Peter Vreman
  Server Version, First Run sock_svr to let it create a socket and then
  sock_cli to connect to that socket
}

uses BaseUnix,Sockets;
const
  SPath='ServerSoc';

Var
  FromName : string;
  Buffer   : string[255];
  S        : Longint;
  Sin,Sout : Text;

procedure perror (const S:string);
begin
  writeln (S,SocketError);
  halt(100);
end;



begin
  S:=Socket (AF_UNIX,SOCK_STREAM,0);
  if SocketError<>0 then
   Perror ('Server : Socket : ');
  fpUnLink(SPath);
  if not Bind(S,SPath) then
   PError ('Server : Bind : ');
  if not Listen (S,1) then
   PError ('Server : Listen : ');
  Writeln('Waiting for Connect from Client, run now sock_cli in an other tty');
  if not Accept (S,FromName,Sin,Sout) then
   PError ('Server : Accept : '+fromname);
  Reset(Sin);
  ReWrite(Sout);
  Writeln(Sout,'Message From Server');
  Flush(SOut);
  while not eof(sin) do
   begin
     Readln(Sin,Buffer);
     Writeln('Server : read : ',buffer);
   end;
  FPUnlink(SPath);
end.
