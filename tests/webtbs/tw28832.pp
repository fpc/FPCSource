{ %NORUN }

program tw28832;

{$mode objfpc}{$H+}

generic procedure Test<T>(AValue : T);
begin
  WriteLn(Low(AValue));
  WriteLn(Low(T));
  WriteLn(High(AValue));
  WriteLn(High(T));
end;

begin
  specialize Test<Integer>(0);
end.

