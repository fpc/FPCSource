{ A {$codepage} directive after a UTF-8 signature makes the module's code page the directive's, not the
  signature's, so a mode that turns on the system code page replaces it with the system one, as it does in a
  module without a signature. The directive's code page here, cp866, a DOS code page, is not the system code
  page of a usual host, so the literal below must not have been read in it (on a host whose system code page
  is cp866 the test cannot tell). }
{$codepage cp866}
{$mode delphiunicode}
program tbommode4;

var
  lLiteral: UnicodeString;
begin
  lLiteral := 'é';
  { the literal's two bytes, $C3 $A9, read in cp866 }
  if (Length(lLiteral) = 2) and (Ord(lLiteral[1]) = $251C) and (Ord(lLiteral[2]) = $0439) then
    Halt(1);
  WriteLn('ok');
end.
