uses
{$ifdef unix}
  cwstring,
{$endif unix}
  sysutils;
  
type  
  ts850 = type AnsiString(850);

  procedure doerror(ANumber : Integer);
  begin
    WriteLn('error ',ANumber);
    Halt(ANumber);
  end;

var
  x : ts850;
  i : Integer;
  ua : array[0..7] of UnicodeChar;
  uc : UnicodeChar;
  us : UnicodeString;
begin
  x := 'abc'#$00A9#$00AE'123';
  ua := x;
  us := x;
  for i := 1 to Length(us) do
    begin
      uc := us[i];
      if (uc <> ua[i-1]) then begin
        writeln(i);
        doerror(2);
      end;
    end;

  WriteLn('Ok');
end.
