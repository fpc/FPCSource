{ %target=linux }
{ the test uses its own pipe: stdin cannot be used as it might be e.g. /dev/null
  or a regular file which do not support epoll (epoll_ctl fails with EPERM) }
uses
  baseunix,linux,ctypes;

var
  e : tepoll_event;
  es : array[0..10] of tepoll_event;
  fd : cint;
  fds : tfildes;
  i : Longint;
  b : byte;
begin
  fillchar(es,sizeof(es),$de);
  if fppipe(fds)<>0 then
    begin
      writeln('Error in fppipe');
      halt(2);
    end;
  fd:=epoll_create(1);
  if fd<0 then
    begin
      writeln('Error in epoll_create');
      halt(3);
    end;

  e.Events:=EPOLLIN;
  e.Data.u32:=$1234568;
  if (epoll_ctl(fd,EPOLL_CTL_ADD,fds[0],@e)<>0) then
    begin
      writeln('Error in epoll_ctl');
      fpclose(fd);
      halt(1);
    end;

  { nothing written yet, so no event }
  i:=epoll_wait(fd,@es,length(es),0);
  if i<>0 then
    begin
      writeln('Error: epoll_wait returned ',i,' for an empty pipe');
      halt(4);
    end;

  b:=42;
  if fpwrite(fds[1],b,1)<>1 then
    begin
      writeln('Error in fpwrite');
      halt(5);
    end;

  { exactly one event with the registered data, this checks also the layout of tepoll_event }
  i:=epoll_wait(fd,@es,length(es),1000);
  if (i<>1) or ((es[0].Events and EPOLLIN)=0) or (es[0].Data.u32<>$1234568) then
    begin
      writeln('Error: epoll_wait returned ',i,', events ',es[0].Events,', data ',es[0].Data.u32);
      halt(6);
    end;
  { the second element must not be touched }
  if es[1].Events<>$dededede then
    begin
      writeln('Error: epoll_wait wrote more than one event');
      halt(7);
    end;

  fpclose(fd);
  fpclose(fds[0]);
  fpclose(fds[1]);
  writeln('ok');
end.
