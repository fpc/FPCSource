{ %OPT=-O3 }
{$mode objfpc}
program tw40401;

function OptPass1_V_MOVAP_Test: Single; noinline;
  var
    a, b, zero: single;
  begin
    repeat
      zero := 0 + random(0);
      a := 2 - zero;
      b := 5 - zero;
      Result := a * b; // must be 10
    until true;
  end;

var
  m: Single;
begin
  m := OptPass1_V_MOVAP_Test();
  if m <> 10 then
    begin
      WriteLn('FAIL - returned' , m);
      Halt(1);    
    end;

  WriteLn('ok');
end.