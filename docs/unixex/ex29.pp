program example29;

{ Program to demonstrate the LStat function. }

uses BaseUnix,Unix;

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
  if fpstat ('test.fil',info)<>0 then
     begin
     writeln('Fstat failed. Errno : ',fpgeterrno);
     halt (1);
     end;
  writeln;
  writeln ('Result of stat on file ''test.fil''.');
  writeln ('Inode   : ',info.st_ino);
  writeln ('Mode    : ',info.st_mode);
  writeln ('nlink   : ',info.st_nlink);
  writeln ('uid     : ',info.st_uid);
  writeln ('gid     : ',info.st_gid);
  writeln ('rdev    : ',info.st_rdev);
  writeln ('Size    : ',info.st_size);
  writeln ('Blksize : ',info.st_blksize);
  writeln ('Blocks  : ',info.st_blocks);
  writeln ('atime   : ',info.st_atime);
  writeln ('mtime   : ',info.st_mtime);
  writeln ('ctime   : ',info.st_ctime);

  If  fpSymLink ('test.fil','test.lnk')<>0 then
    writeln ('Link failed ! Errno :',fpgeterrno);

  if  fplstat ('test.lnk',@info)<>0 then
     begin
     writeln('LStat failed. Errno : ',fpgeterrno);
     halt (1);
     end;
  writeln;
  writeln ('Result of fstat on file ''test.lnk''.');
  writeln ('Inode   : ',info.st_ino);
  writeln ('Mode    : ',info.st_mode);
  writeln ('nlink   : ',info.st_nlink);
  writeln ('uid     : ',info.st_uid);
  writeln ('gid     : ',info.st_gid);
  writeln ('rdev    : ',info.st_rdev);
  writeln ('Size    : ',info.st_size);
  writeln ('Blksize : ',info.st_blksize);
  writeln ('Blocks  : ',info.st_blocks);
  writeln ('atime   : ',info.st_atime);
  writeln ('mtime   : ',info.st_mtime);
  writeln ('ctime   : ',info.st_ctime);
  { Remove file and link }
  erase (f);
  fpunlink ('test.lnk');
end.