Program Example53;

{ Program to demonstrate the S_ISLNK function. }

Uses oldlinux;

Var Info : Stat;

begin
  if LStat (paramstr(1),info) then
    begin
    if S_ISLNK(info.mode) then
      Writeln ('File is a link');
    if S_ISREG(info.mode) then
      Writeln ('File is a regular file');
    if S_ISDIR(info.mode) then
      Writeln ('File is a directory');
    if S_ISCHR(info.mode) then
      Writeln ('File is a character device file');
    if S_ISBLK(info.mode) then
      Writeln ('File is a block device file');
    if S_ISFIFO(info.mode) then
      Writeln ('File is a named pipe (FIFO)');
    if S_ISSOCK(info.mode) then
      Writeln ('File is a socket');
    end;
end.
