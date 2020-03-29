program tbSwapEndian;

{$mode objfpc}

uses
    sysutils;

function InlineSwapEndian(const AValue: QWord): QWord;inline;
  begin
    result := (qword(SwapEndian(lo(avalue))) shl 32) or SwapEndian(hi(avalue));
  end;

var
    q : QWord;

procedure Check;
begin
    q := $0102030405060708;
    q := InlineSwapEndian(q);
end;

begin
    q:=0;
    Check;
    if q <> $0807060504030201 then begin
        writeln(format('failed Swap - expected $0807060504030201, got %x',[q]));
        halt(1);
    end;
end.
