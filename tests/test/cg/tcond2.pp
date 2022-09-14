{ %CPU=i386,x86_64 }
{ %OPT=-a -O2 -CpCOREI }

{ This test evaluates IsRefSafe returning false but still permitting CMOV
  because the condition reads it }

program tcond2;

uses
  CPU;

type
  PLongInt = ^LongInt;

const
  InputVal: array[0..3] of LongInt = (-1, 0, 2147483647, -2147483648);
  Expected: array[0..3] of LongInt = (0, 0, 2147483647, 0);

function ZeroClamp(const Reference: PLongInt): LongInt; noinline;
  begin
    ZeroClamp := 0;
    if Reference^ > 0 then
      ZeroClamp := Reference^;
  end;

var
  X, Output: LongInt;

begin
  if not CMOVSupport then
    begin
      WriteLn('unsupported');
      Halt(0);
    end;      

  for X := 0 to 3 do
    begin
      Output := ZeroClamp(@InputVal[X]);
      if Output <> Expected[X] then
        begin
          WriteLn('FAIL: ZeroClamp(', InputVal[X], ') returned ', Output, '; expected ', Expected[X]);
          Halt(1);
        end;
    end;

  WriteLn('ok');
end.
