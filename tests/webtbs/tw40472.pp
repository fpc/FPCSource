{ %CPU=ARM }
program tw40472;

function AddrCheck(): LongInt; assembler; nostackframe;
asm
  ADR R0, .L528Ahead
  LDR R0, [R0]
  BX  LR
.LPadding:
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  .long 0, 0, 0
.L528Ahead:
  .long 0x5555AAAA
end;

var
  Output: LongInt;
begin
  Output := AddrCheck();
  if Output <> $5555AAAA then
    begin
      WriteLn('ERROR: Expected $5555AAAA but got $', HexStr(Output, 8));
      Halt(1);
    end;

  WriteLn('ok');
end.
