program pfinger;

uses sockets,errors,inet;

Var Addr : TInetSockAddr;
    S : Longint;
    Sin,Sout : Text;
    Line : string;
    hostname,username : string;
    host : THost;

begin
  if paramcount<>1 then
    begin
    writeln ('Usage : pfinger username@hostname');
    end;
  HostName:=Paramstr(1);
  If pos('@',HostName)<>0 then
    begin
    username:=copy(HostName,1,pos('@',hostname)-1);
    hostname:=copy(HostName,pos('@',HostName)+1,255);
    end
  else
    username:='';
  Host.NameLookup(HostName);
  If Host.LastError<>0 then
    begin
    writeln ('Unknown host : ',host.name);
    halt(1);
    end;
  Addr.family:=AF_INET;
  { port 78 in network order }
  Addr.port:=ShortHostToNet(79);
  { localhost : 127.0.0.1 in network order }
  Addr.addr:=HostTonet(Longint(Host.IPAddress));
  S:=Socket(AF_INET,SOCK_STREAM,0);
  If Not Connect (S,ADDR,SIN,SOUT) Then
    begin
    Writeln ('Couldn''t connect to localhost');
    Writeln ('Socket error : ',strerror(SocketError));
    halt(1);
    end;
  rewrite (sout);
  reset(sin);
  writeln (sout,username);
  flush(sout);
  while not eof(sin) do
    begin
    readln (Sin,line);
    writeln (line);
    end;
  close (sin);
  close (sout);
end.
  $Log$
  Revision 1.2  2002-09-07 15:42:52  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:53  peter
    * splitted to base and extra

}
