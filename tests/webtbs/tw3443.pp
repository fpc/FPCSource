{ Source provided for Free Pascal Bug Report 3443 }
{ Submitted by "Alexey Barkovoy" on  2004-12-08 }
{ e-mail: clootie@ixbt.com }

{$mode delphi}

uses SysUtils;

procedure Test1(c: PChar; w: PWideChar);
begin
  if c <> Pointer(w) then Exit; // Do something
end;

begin
  Test1(nil, nil);
  Test1('SS', 'WW');
  Test1(PChar('a'+'b'), PWideChar('a' + 'b')); //  Delphi can't compile this
  Test1(@((AnsiString('xxx ' + IntToStr(1) + #10))[1]),
        @((WideString('xxx ' + IntToStr(1) + #10))[1])); // FPC: "Error: Variable identifier expected"
// Delphi CAN compile line above
end.
