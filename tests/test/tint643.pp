{$q+}
{$mode objfpc}
uses
  sysutils;


type
   tqwordrec = packed record
{$ifndef ENDIAN_BIG}
      low,high : dword;
{$else}
      high, low : dword;
{$endif}
   end;

procedure assignqword(h,l : dword;var q : qword);

  begin
     tqwordrec(q).high:=h;
     tqwordrec(q).low:=l;
  end;

procedure testmulqword;
var
  q1, q2, q3, q4: qword;
  c: cardinal;
  loops: longint;
begin
  assignqword(0,$1000,q1);
  assignqword(0,$7fff,q2);
  c := $1000 * $7fff;
  q4 := c;
  loops := 0;
  try
    repeat
      q3 := q1 * q2;
      if q3 <> q4 then
        begin
          writeln('qword multiplication of shift error');
          halt(1);
        end;
      inc(loops);
      if (loops >= 39) then
        begin
          writeln('qword multiplication overflow detection failed');
          halt(1);
        end;
      q1 := q1 shl 1;
      q4 := q4 shl 1;
      writeln(loops,': ',q3);
    until false;
  except
    on eintoverflow do
      begin
        if loops < 38 then
          begin
            writeln('false qword multiplication overflow detected');
            halt(1);
          end;
      end;
  end;
end;

begin
  testmulqword;
end.
