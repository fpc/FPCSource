type
 TRecord = record
  P1,
  P2: pointer;
 end;

var
 var1: longint;
 varabs: longint absolute 1234;
const
 info: TRecord = (P1: @var1; // Works
                  P2: @varabs); // Won't work

begin
  if ptrint(info.p2)<>1234 then
    halt(1);
  writeln('ok');
end.

