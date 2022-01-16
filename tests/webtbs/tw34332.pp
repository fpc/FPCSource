{ %NORUN }

program tw34332;

{$mode objfpc}{$h+}

var
  SS: ShortString;
  S: String;
begin
  SS := Copy(SS, 1); // << project1.lpr(9,14) Error: Type mismatch
  S := Copy(S, 1); // << OK
  SS := Copy(SS, 1, 1); // << OK
end.
