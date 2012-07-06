{%TARGET=linux,freebsd,darwin,aix,openbsd,netbsd}
program tb0524;

uses sockets,baseunix,sysutils;


const port=6667;
   textfile = 'tb0524.txt';

procedure reset_textfile;
var
  f : text;
begin
  assign(f,textfile);
  rewrite(f);
  writeln(f,'Normal server start');
  close(f);
end;

procedure stop(error : longint);
var
  f : text;
begin
  assign(f,textfile);
  rewrite(f);
  writeln(f,'Server startup failed');
  close(f);
  halt(error);
end;

function server_failed : boolean;
var
  f : text;
  st : string;
begin
  server_failed:=false;
  assign(f,textfile);
  reset(f);
  readln(f,st);
  if pos('Server startup failed',st)=1 then
    server_failed:=true;
  close(f);
end;

procedure do_server;

var s,t:string;
    lsock,usock:longint;
    saddr:Tinetsockaddr;
    len:longInt;
    sin,sout:text;
    i:byte;

begin
   reset_textfile;
   lsock:=fpsocket(af_inet,sock_stream,0);
   if lsock=-1 then
     begin
       writeln('socket call error:',socketerror);
       stop(1);
     end;

  with saddr do
    begin
      family:=af_inet;
      port:=ntobe(word(6667));
      addr:=0;
   end;

  if  fpbind(lsock,@saddr,sizeof(saddr))<>0 then
    begin
      writeln('bind call error:',socketerror);
      stop(1);
    end;

  if  fplisten(lsock,1)<>0 then
    begin
      writeln('listen call error:',socketerror);
      stop(1);
    end;

  len:=sizeof(saddr);
  usock:=fpaccept(lsock,@saddr,@len);
  if usock=-1 then
    begin
      writeln('accept call error:',SocketError);
      stop(1);
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
  fpshutdown(usock,2);
end;

procedure do_client;

var s:sizeint;
    saddr:Tinetsockaddr;
    sin,sout:text;
    str:ansistring;

begin
   s:=fpsocket(af_inet,sock_stream,0);
   saddr.sin_family:=af_inet;
   saddr.sin_port:=htons(port);
   saddr.sin_addr.s_addr:=hosttonet($7f000001); {127.0.0.1}
   if not connect(s,saddr,sin,sout) then
     begin
       writeln('connect:',socketerror);
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
   fpshutdown(s,2);
end;

begin
  if fpfork=0 then
    do_server
  else
    begin
      {Give server some time to start.}
      sleep(2000);
      if server_failed then
        begin
          writeln('Server startup failed, test can not be completed');
          halt(2);
        end
      else
        do_client;
    end;
end.
