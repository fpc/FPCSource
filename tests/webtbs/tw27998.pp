program a;
{$mode delphi}
uses
  SysUtils, Classes;
const
  LockFile = 'lock.txt';
var
  H : TStream;
begin
  try
    H := TFileStream.Create(lockFile, fmCreate);
    h.Write(H, 4);
    { should fail with an exception due to exclusion }
    H := TFileStream.Create(lockFile, fmCreate);
    Halt(1);
  except
    { check the size of the file, to ensure that the second
      create didn't overwrite the file }
    H.free;
    H := TFileStream.create(LockFile, fmOpenRead or fmShareDenyNone);
    if H.Size<>4 then
      halt(2);
  end
end.

