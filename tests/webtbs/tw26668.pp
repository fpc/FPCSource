{ %CPU=i386 }
{ %OPT=-Cg- }

{$mode delphi}
const __dd = 1;
function f1 (var p : longword) : byte;
asm
  lea eax, [ eax + 2 ]
  mov al,  [eax - __dd + 1].byte
end;

function f2 (var p : longword) : byte;
asm
  lea eax, [ eax + 2 ]
  mov al,  [eax - __dd].byte [1]
end;

function f3 (var p : longword) : byte;
asm
  lea eax, [ eax + 2 ]
  mov al,  [eax - 1 + 1].byte
end;

var v : longword = $01020304;

begin
  { all three functions must produce the same code }
  if f1(v)<>2 then
    halt(1);
  if f2(v)<>2 then
    halt(2);
  if f3(v)<>2 then
    halt(3);
end.
