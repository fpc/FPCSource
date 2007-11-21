{$mode objfpc}
uses
  SysUtils;

var
  err : boolean;

procedure expect(const v,s:string);
var
  s1 : string;
begin
  s1:=SetDirSeparators(s);
  writeln('relative path: "',v,'"');
  if v<>s then
    begin
      writeln('Error, expected "',s,'"');
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

