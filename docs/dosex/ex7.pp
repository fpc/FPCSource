Program Example7;
uses Dos;

{ Program to demonstrate the FindFirst and FindNext function. }

var
  Dir : SearchRec;
begin
  FindFirst('*.*',archive,Dir);
  WriteLn('FileName'+Space(32),'FileSize':9);
  while (DosError=0) do
   begin
     Writeln(Dir.Name+Space(40-Length(Dir.Name)),Dir.Size:9);
     FindNext(Dir);
   end;
  FindClose(Dir);
end.

