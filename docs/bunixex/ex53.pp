Program Example53;

{ Program to demonstrate the S_ISLNK function. }

Uses BaseUnix,Unix;

Var Info : Stat;

begin
  if fpLStat (paramstr(1),@info)=0 then
    begin
    if fpS_ISLNK(info.st_mode) then
      Writeln ('File is a link');
    if fpS_ISREG(info.st_mode) then
      Writeln ('File is a regular file');
    if fpS_ISDIR(info.st_mode) then
      Writeln ('File is a directory');
    if fpS_ISCHR(info.st_mode) then
      Writeln ('File is a character device file');
    if fpS_ISBLK(info.st_mode) then
      Writeln ('File is a block device file');
    if fpS_ISFIFO(info.st_mode) then
      Writeln ('File is a named pipe (FIFO)');
    if fpS_ISSOCK(info.st_mode) then
      Writeln ('File is a socket');
    end;
end.
