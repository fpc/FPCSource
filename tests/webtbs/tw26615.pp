{ %NORUN }

program tw26615;

{$MODE OBJFPC}{$H+}
{$MODESWITCH TYPEHELPERS}

uses
  sysutils;

type
  TStringHelper = type helper for UnicodeString
    class function Cr(AStr: UnicodeString): UnicodeString; static; overload;
  end;

class function TStringHelper.Cr(AStr: UnicodeString): UnicodeString;
begin
  Result := '#'+AStr;
end;

var
  us: UnicodeString;

begin

  us := UnicodeString.Cr('a');
  writeln(us);

end.
