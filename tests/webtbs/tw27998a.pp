program a;
{$mode delphi}
uses
  SysUtils, Classes;
const
  LockFile = 'lock.txt';
var
  H, H2 : TStream;
begin
  try
    H := TFileStream.Create(lockFile, fmCreate);
    { should raise exception because of exclusive lock above }
    H2 := TFileStream.create(LockFile, fmOpenRead or fmShareDenyNone);
    H2.free;
    H.free;
    DeleteFile(LockFile);
    halt(1);
  except
    H.free;
    DeleteFile(LockFile);
  end
end.

