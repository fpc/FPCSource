{ %opt=-gt -Sc }

{$mode objfpc}

program Project1;

type TLLVMTest = class
  str: ansistring;
  pos: pchar;
  procedure expect(c: char);
  procedure test();
end;
var
  l: TLLVMTest;

procedure TLLVMTest.expect(c: char);
  procedure error;
  begin
    while (pos^ <> c) and (pos^ <> #0) do pos += 1;
  end;

begin
  if pos^ = c then
    pos += 1
  else
    halt(1);
end;

procedure TLLVMTest.test();
begin
  str := 'abc';
  pos:=@str[1];
  expect('a');
end;


begin
  l := TLLVMTest.create;
  l.test();
end.

