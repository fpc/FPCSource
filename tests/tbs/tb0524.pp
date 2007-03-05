{%TARGET=linux,freebsd,darwin}
program tb0524;

uses sockets,baseunix,sysutils;

const port=6667;

procedure do_server;

var s,t:string;
    lsock,usock:longint;
    saddr:Tinetsockaddr;
    len:longInt;
    sin,sout:text;
    i:byte;

begin
   lsock:=socket(af_inet,sock_stream,0);
   if lsock=-1 then
     begin
       writeln(socketerror);
       halt(1);
     end;

  with saddr do
    begin
      family:=af_inet;
      port:=ntobe(word(6667));
      addr:=0;
   end;

  if not bind(lsock,saddr,sizeof(saddr)) then
    begin
      writeln(socketerror);
      halt(1);
    end;

  if not listen(lsock,1) then
    begin
      writeln(socketerror);
      halt(1);
    end;

  len:=sizeof(saddr);
  usock:=accept(lsock,saddr,len);
  if usock=-1 then
    begin
      writeln(SocketError);
      halt(1);
    end;
  sock2text(usock,sin,sout);

  reset(sin);
  rewrite(sout);
  repeat
    readln(sin,s);
    t:='';
    for i:=length(s) downto 1 do
      t:=t+s[i];
    writeln(sout,t);
  until eof(sin);
  close(sin);
  close(sout);
  shutdown(usock,2);
end;

procedure do_client;

var s:sizeint;
    saddr:Tinetsockaddr;
    sin,sout:text;
    str:ansistring;

begin
   s:=socket(af_inet,sock_stream,0);
   saddr.sin_family:=af_inet;
   saddr.sin_port:=htons(port);
   saddr.sin_addr.s_addr:=hosttonet($7f000001); {127.0.0.1}
   if connect(s,saddr,sin,sout)=false then
     begin
       writeln(socketerror);
       halt(1);
     end;
   writeln(sout,'abcd');
   readln(sin,str);
   if str<>'dcba' then
     halt(1);
   writeln(sout,'1234');
   readln(sin,str);
   if str<>'4321' then
     halt(1);
   close(sin);
   close(sout);
   shutdown(s,2);
end;

begin
  if fpfork=0 then
    do_server
  else
    begin
      {Give server some time to start.}
      sleep(2000);
      do_client;
    end;
end.
