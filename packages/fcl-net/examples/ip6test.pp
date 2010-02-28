program ip6test;

uses
  sockets,
  unix,
  errors,
  netdb,
  baseunix;

var
  dest: TInetSockAddr6;
  sock: LongInt;
  s: shortstring;
  i: integer;
  t1,t2:text;
  x: array of thostaddr6;
  hname : string;

Const
  ip6hosttest = 'whatismyv6.com';

begin
  hname:=ip6hosttest;
  if paramcount>0 then
    hname:=paramstr(1);
  setlength(x, 100);
  i:=resolvename6(hname, x);
  if i=-1 then
    begin
      writeln('Domain not found, ',hname);
      halt;
    end;
  setlength(x,i);
  if length(x) = 0 then halt(2);
  with dest do begin
    sin6_family := PF_INET6;
    sin6_port   := shorthosttonet(80);
    sin6_addr.u6_addr16 := x[0].u6_addr16;
  end;
  sock := fpsocket(PF_INET6, SOCK_STREAM, 6 {TCP});

  if fpConnect(sock, @dest, sizeof(dest))=0 then begin
    sock2text(sock,t1,t2);
    writeln(t2, 'GET / HTTP/1.0');
    writeln(t2);
    while not eof(t1) do begin
      readln(t1, s);
      writeln(s);
    end;
  end else begin
    writeln('not connected: ',socketerror, ': ', StrError(socketerror));
  end;
  closesocket(sock);
end.
