{$mode objfpc}
uses
  SysUtils;

var
  err : boolean;

procedure expect(const v,s:string);
var
  s1,s2 : string;
begin
  s1:=SetDirSeparators(v);
  s2:=SetDirSeparators(s);
  writeln('relative path: "',s1,'"');
  if s1<>s2 then
    begin
      writeln('Error, expected "',s2,'"');
      err:=true;
    end;
end;

begin
  expect(ExtractRelativePath('c:\one\two\three\test.pp','c:\one\two\three\four\five\test2.pp'),'four\five\test2.pp');
  expect(ExtractRelativePath('c:\one\two\three\four\five\test.pp','c:\one\two\three\test2.pp'),'..\..\test2.pp');
  expect(ExtractRelativePath('c:\one\two\three\','c:\one\two\three\four\five\test.pp'),'four\five\test.pp');
  expect(ExtractRelativePath('c:\one\two\three\four\five\','c:\one\two\three\test.pp'),'..\..\test.pp');
  expect(ExtractRelativePath('c:\one\two\three\','c:\one\two\three\four\five\'),'four\five\');
  expect(ExtractRelativePath('c:\one\two\three\four\five\','c:\one\two\three\'),'..\..\');
  if err then
    halt(1);
end.

