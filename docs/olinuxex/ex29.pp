program example29;

{ Program to demonstrate the LStat function. }

uses oldlinux;

var f : text;
    i : byte;
    info : stat;

begin
  { Make a file }
  assign (f,'test.fil');
  rewrite (f);
  for i:=1 to 10 do writeln (f,'Testline # ',i);
  close (f);
  { Do the call on made file. }
  if not fstat ('test.fil',info) then
     begin
     writeln('Fstat failed. Errno : ',linuxerror);
     halt (1);
     end;
  writeln;
  writeln ('Result of fstat on file ''test.fil''.');
  writeln ('Inode   : ',info.ino);
  writeln ('Mode    : ',info.mode);
  writeln ('nlink   : ',info.nlink);
  writeln ('uid     : ',info.uid);
  writeln ('gid     : ',info.gid);
  writeln ('rdev    : ',info.rdev);
  writeln ('Size    : ',info.size);
  writeln ('Blksize : ',info.blksze);
  writeln ('Blocks  : ',info.blocks);
  writeln ('atime   : ',info.atime);
  writeln ('mtime   : ',info.mtime);
  writeln ('ctime   : ',info.ctime);

  If not SymLink ('test.fil','test.lnk') then
    writeln ('Link failed ! Errno :',linuxerror);

  if not lstat ('test.lnk',info) then
     begin
     writeln('LStat failed. Errno : ',linuxerror);
     halt (1);
     end;
  writeln;
  writeln ('Result of fstat on file ''test.lnk''.');
  writeln ('Inode   : ',info.ino);
  writeln ('Mode    : ',info.mode);
  writeln ('nlink   : ',info.nlink);
  writeln ('uid     : ',info.uid);
  writeln ('gid     : ',info.gid);
  writeln ('rdev    : ',info.rdev);
  writeln ('Size    : ',info.size);
  writeln ('Blksize : ',info.blksze);
  writeln ('Blocks  : ',info.blocks);
  writeln ('atime   : ',info.atime);
  writeln ('mtime   : ',info.mtime);
  writeln ('ctime   : ',info.ctime);
  { Remove file and link }
  erase (f);
  unlink ('test.lnk');
end.