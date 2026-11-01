{ a conversion to or from a record with a generic method leaves the helpers nested
  in that record active }
program trhlp46;

{$mode delphi}

uses
  urhlp46;

var
  r: TRec;
  b: TBox;
  r2: TRec2;
  b2: TBox2;
  i: Integer;
begin
  r.v := 0;
  if r.Which <> 1 then
    Halt(1);
  b := 5;
  if b.v <> 5 then
    Halt(2);
  if r.Which <> 1 then
    Halt(3);
  { the same with the record as the source }
  r2.v := 0;
  b2.v := 7;
  if r2.Which <> 2 then
    Halt(4);
  i := b2;
  if i <> 7 then
    Halt(5);
  if r2.Which <> 2 then
    Halt(6);
  Writeln('ok');
end.
