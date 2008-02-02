{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

const buf: array[0..5] of char = 'abcdef';

function foo(const a: string): string;
begin
 SetLength(result, 6);
 Move(buf, result[1], sizeof(buf));
 if a <> '1234567890' then
 begin
   writeln('Failed: ', a);
   Halt(1);
 end
 else
   writeln('ok');
end;

procedure test_proc(var a: string);
var
 s: string;
begin
{ Don't call UniqueString(s) here because it makes the compiler assume
 that address of s is taken, and assignment s := foo(s) is not optimized }
 s := a;            // refcount=2
 a := 'whatever';   // modify source -> s.refcount becomes 1
 writeln('before: ', s);
 s := foo(s);
 writeln(s);
end;

var
 s: string;
begin
 s := '1234567890';
 UniqueString(s);
 test_proc(s);
end.
