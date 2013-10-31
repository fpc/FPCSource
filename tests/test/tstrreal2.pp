const
  s: array[1..21] of string =
    ('10.00000000000000000',
     '1.00000000000000000',
{$ifdef FPC_HAS_TYPE_EXTENDED}
     '0.10000000000000000',
{$else FPC_HAS_TYPE_EXTENDED}
     '0.10000000000000001',
{$endif FPC_HAS_TYPE_EXTENDED}
     '0.01000000000000000',
     '0.00100000000000000',
     '0.00010000000000000',
     '0.00001000000000000',
     '0.00000100000000000',
     '0.00000010000000000',
     '0.00000001000000000',
     '0.00000000100000000',
     '0.00000000010000000',
     '0.00000000001000000',
     '0.00000000000100000',
     '0.00000000000010000',
     '0.00000000000001000',
     '0.00000000000000100',
     '0.00000000000000010',
     '0.00000000000000001',
     '0.00000000000000000',
     '0.00000000000000000');

var
  e: extended;
  c: longint;
  s2: string;
  lenadjust: longint;
begin
  if sizeof(extended) = 8 then
    lenadjust := 0
  else
    lenadjust := 0;
  e := 10.0;
  for c := 1 to 21 do
    begin
      str(e:0:17,s2);
      writeln(s2);
      if s2 <> copy(s[c],1,length(s[c])-lenadjust) then
        begin
          writeln('  Error, should be ',copy(s[c],1,length(s[c])-lenadjust));
        end;
      e := e / 10.0;
    end;
end.
