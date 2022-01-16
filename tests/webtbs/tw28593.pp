program tucs4toutf16FFFF;
{$H+}
uses
  sysutils;

procedure halt_error(const AErrorCode : Integer; const AMsg : string);
begin
  Write(AMsg);
  Halt(AErrorCode);
end;

var
  s4 : UCS4String;
  us : UnicodeString;
begin
  SetLength(s4,2);
  s4[0] := $FFFF;
  s4[1] := 0;
  us := UCS4StringToUnicodeString(s4);
  if (Length(us) <> 1) then
    halt_error(1, 'A single code point UTF6 string expected.');
  if (Ord(us[1]) <> $FFFF) then
    halt_error(2, 'code point U+FFFF expected, got U+'+IntToHex(Ord(us[1]),4));
  WriteLn('OK');
end.


