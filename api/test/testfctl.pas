uses
  FileCtrl;

var
  fd: TFileHandle;
  Tmp: String;
  I: Integer;

begin
  fd := CreateFile('Valami.txt');
  for I := 1 to 255 do Tmp[I] := Chr(I);
  Tmp[0] := #255;
  WriteFile(fd, Tmp, 256);
  SeekFile(fd, 0, skBeg);
  WriteLn('Filesize = ', FileSize(fd));
  ReadFile(fd, Tmp, 256);
  WriteLn(Tmp);
  WriteFile(fd, Tmp, 256);
  SeekFile(fd, 256, skBeg);
  WriteLn('FilePos = ', FilePos(fd));
  CloseFile(fd);
end.
