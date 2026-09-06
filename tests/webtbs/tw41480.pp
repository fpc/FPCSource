{ %OPT=-O2 }

program tw41480;

{$mode delphi}

function Convert(I: UInt32): Word;
var
  W: Word;
begin
  W := 0;
  if I < $10000 then
    W := Word(I)
  else if I < $20000 then
    W := Word(I - $10000)
  else if I < $30000 then
    W := Word(I - $20000);
  Result := W;
end;

begin
  if Convert($0000ffff) <> $ffff then
    Halt(1);
  if Convert($00012345) <> $2345 then
    Halt(2);
  if Convert($0002ffff) <> $ffff then
    Halt(3);
  if Convert($00030000) <> 0 then
    Halt(4);
  writeln('ok');
end.
