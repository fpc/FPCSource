uses strings;

procedure test_strcopy;
var
  p: pchar;
  s: array[0..256] of char;
  buf: array[0..512] of char;
  i, j, k, l: longint;
begin
  for i := 0 to 256 do
    begin
      fillchar(s,sizeof(s),'b');
      s[i] := #0;
      for j := 0 to 3 do
        begin
          fillchar(buf,sizeof(buf),'a');
          p := strcopy(@buf[j+32],@s[0]);
          if (p <> @buf[j+32]) then
            begin
              writeln('error 0 (i=',i,')');
              halt(1);
            end;
          for l := 0 to j+31 do
            if buf[l] <> 'a' then
              begin
                writeln('error 1 (i=',i,')');
                halt(1);
              end;
          for l := j+32 to j+32+i-1 do
            if buf[l] <> 'b' then
              begin
                writeln('error 2 (i=',i,')');
                halt(1);
              end;
          if buf[j+i+32] <> #0 then
            begin
              writeln('error 3 (i=',i,')');
              halt(1);
            end;
          for l := j+i+32+1 to 512 do
            if buf[l] <> 'a' then
              begin
                writeln('error 4 (i=',i,')');
                halt(1);
              end;
        end;
    end;
end;

begin
  test_strcopy;
end.
