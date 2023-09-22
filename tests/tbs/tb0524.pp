{%TARGET=linux,freebsd,darwin,aix,openbsd,netbsd,haiku}
program tb0524;

uses sockets,baseunix,sysutils;


const default_port=6667;
   textfile = 'tb0524.txt';
{$ifdef debug}
   verbose = true;
{$else}
   verbose = false;
{$endif}
var
   used_port : word;

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
  retry : boolean;
begin
  server_failed:=false;
  assign(f,textfile);
  retry:=true;
  while retry do
    begin
      reset(f);
      readln(f,st);
      if pos('Server startup failed',st)=1 then
        begin
          server_failed:=true;
          exit;
        end;
      if pos('port=',st)=1 then
        begin
          val(copy(st,length('port=')+1,length(st)),used_port);
          writeln('Server started at port ',used_port);
          retry:=false;
        end
      else
        begin
          sleep(1000);
          retry:=true;
        end;
      close(f);
    end;
end;

procedure write_server_port(used_port : word);
var
  f : text;
begin
  assign(f,textfile);
  rewrite(f);
  writeln(f,'port=',used_port);
  writeln('Using port ',used_port);
  close(f);
end;

procedure do_server;

var s,t:string;
    lsock,usock:longint;
    saddr:Tinetsockaddr;
    len:longInt;
    sin,sout:text;
    i:byte;
    port : word;
    server_started : boolean;
    attempt_count : longint;
const
    max_attempt_count = 50;
begin
   reset_textfile;
   lsock:=fpsocket(af_inet,sock_stream,0);
   if lsock=-1 then
     begin
       writeln('socket call error:',socketerror);
       stop(1);
     end;

  port:=default_port-1;
  attempt_count:=0;
  server_started:=false;
  while (attempt_count<max_attempt_count) and not server_started do
    begin
      inc(port);
      if verbose then
        writeln('Trying to use port ',port,' to start the server');
      inc(attempt_count);
      with saddr do
        begin
          sin_family:=af_inet;
          sin_port:=ntobe(port);
          sin_addr:=NoAddress;
       end;

      if fpbind(lsock,@saddr,sizeof(saddr))<>0 then
        if attempt_count<max_attempt_count then
          begin
            writeln('bind call error:',socketerror);
            continue;
          end
        else
          begin
            writeln('bind call error:',socketerror);
            stop(1);
          end;
      if verbose then
        writeln('fpbind OK for port ',port);

      if fplisten(lsock,1)<>0 then
        if attempt_count<max_attempt_count then
          begin
            writeln('listen call error:',socketerror);
            continue;
          end
        else
          begin
            writeln('listen call error:',socketerror);
            stop(1);
         end;
      if verbose then
        writeln('fplisten OK for port ',port);
      write_server_port(port);
      server_started:=true;

      len:=sizeof(saddr);
      usock:=fpaccept(lsock,@saddr,@len);
      if usock=-1 then
        if attempt_count<max_attempt_count then
          begin
            writeln('accept call error:',SocketError);
            continue;
          end
        else
          begin
            writeln('accept call error:',SocketError);
            stop(1);
          end;
      if verbose then
        writeln('fpaccept OK for port ',port);
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
   if verbose then
     writeln('Server at port ',port,' ending without error');
end;

procedure do_client;

var s:sizeint;
    saddr:Tinetsockaddr;
    sin,sout:text;
    str:ansistring;

begin
   s:=fpsocket(af_inet,sock_stream,0);
   saddr.sin_family:=af_inet;
   saddr.sin_port:=htons(used_port);
   saddr.sin_addr.s_addr:=hosttonet($7f000001); {127.0.0.1}
   if not connect(s,saddr,sin,sout) then
     begin
       writeln('connect:',socketerror);
       halt(1);
     end;
   writeln(sout,'abcd');
   readln(sin,str);
   if str<>'dcba' then
     begin
       writeln('Expecting dcba, but got ',str);
       halt(1);
     end;
   writeln(sout,'1234');
   readln(sin,str);
   if str<>'4321' then
     begin
       writeln('Expecting 4321, but got ',str);
       halt(1);
     end;
   close(sin);
   close(sout);
   fpshutdown(s,2);
   if verbose then
     writeln('Client at port ',used_port,' ending without error');
end;

begin
  used_port:=default_port;
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
