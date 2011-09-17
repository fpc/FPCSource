uses
{$ifdef unix}
  cwstring,
{$endif unix}
  sysutils;
  
type  
  ts850 = type string<850>;

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  x : ts850;
  i : Integer;
  sa : ansistring;
  ua : array[0..7] of UnicodeChar;
  uc : UnicodeChar;
  us : UnicodeString;
begin
  sa := 'abc'#$00A9#$00AE'123';
  ua := sa;
  for i := 1 to Length(sa) do
    begin
      uc := sa[i];
      if (uc <> ua[i-1]) then begin
        writeln(i);
        doerror(1);
      end;
    end;
  x := 'abc'#$00A9#$00AE'123';
  ua := x;
  us := x;
  for i := 1 to Length(sa) do
    begin
      uc := us[i];
      if (uc <> ua[i-1]) then begin
        writeln(i);
        doerror(2);
      end;
    end;

  WriteLn('Ok');
end.
