{%TARGET=darwin,haiku,linux,freebsd,netbsd,openbsd,solaris,aix,android }

{$mode fpc}

uses
  baseunix;

const
  testbyte = '!';

var
  pollfds: pollfd;
  fds: array[0..1] of cint;
  ret: cint;
  x: char;
begin
  fppipe(fds);
  pollfds.fd := fds[0];
  pollfds.events := POLLIN;
  pollfds.revents := 0;
  ret := fppoll(@pollfds, 1, 10);
  if ret <> 0 then
  begin
    writeln('poll returned ', ret, ' (expected 0 = timeout), errno=', errno);
    halt(1);
  end;
  x := testbyte;
  ret := fpwrite(fds[1], x, 1);
  if ret <> 1 then
  begin
    writeln('write returned ', ret, ' (expected 1 byte), errno=', errno);
    halt(2);
  end;
  ret := fppoll(@pollfds, 1, 10);
  if ret <> 1 then
  begin
    writeln('poll returned ', ret, ' (expected 1 = data ready), errno=', errno);
    halt(3);
  end;
  if (pollfds.revents and POLLIN) = 0 then
  begin
    writeln('poll did not set POLLIN');
    halt(4);
  end;
  x := #0;
  ret := fpread(fds[0], @x, 1);
  if ret <> 1 then
  begin
    writeln('read returned ', ret, ' (expected 1 byte), errno=', errno);
    halt(5);
  end;
  if x <> testbyte then
  begin
    writeln('test byte is ''', x, ''' (expected ''', testbyte, ''')');
    halt(6);
  end;
  ret := fppoll(@pollfds, 1, 10);
  if ret <> 0 then
  begin
    writeln('poll returned ', ret, ' (expected 0 = timeout), errno=', errno);
    halt(7);
  end;
  fpclose(fds[0]);
  fpclose(fds[1]);
end.
