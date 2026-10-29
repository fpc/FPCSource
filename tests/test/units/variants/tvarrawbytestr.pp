{ Variant to RawByteString assignment, issue #21710 }
program tvarrawbytestr;

{$mode objfpc}{$h+}
{$codepage utf8}

uses
{$ifdef unix}
  cwstring,
{$endif}
  SysUtils, Variants;


// Halts with aCode when aCondition does not hold
procedure Check(aCode: Integer; aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
    writeln('Failed: ',aMsg);
    halt(aCode);
    end;
end;


// Returns aValue unchanged
function PassRaw(const aValue: RawByteString): RawByteString;

begin
  Result:=aValue;
end;


var
  lValue: Variant;
  lRaw: RawByteString;
  lAnsi: AnsiString;
  lText: UnicodeString;

begin
  DefaultSystemCodePage:=CP_UTF8;
  lText:='aé€';
  lValue:=lText;

  lRaw:=lValue;
  Check(1,StringCodePage(lRaw)=DefaultSystemCodePage,'RawByteString has the default system code page');
  Check(2,UnicodeString(lRaw)=lText,'RawByteString has the text of the variant');

  lRaw:=PassRaw(lValue);
  Check(3,UnicodeString(lRaw)=lText,'variant passed as RawByteString parameter');

  lRaw:=AnsiToUtf8(lValue);
  Check(4,StringCodePage(lRaw)=CP_UTF8,'AnsiToUtf8 returns UTF-8');
  Check(5,UnicodeString(lRaw)=lText,'AnsiToUtf8 keeps the text of the variant');

  lAnsi:='xy';
  Insert(lValue,lAnsi,2);
  Check(6,UnicodeString(lAnsi)='x'+lText+'y','Insert of a variant into an AnsiString');

  lValue:=42;
  lRaw:=lValue;
  Check(7,lRaw='42','integer variant to RawByteString');

  lValue:=Null;
  try
    lRaw:=lValue;
    Check(8,False,'Null variant to RawByteString raises');
  except
    on E: EVariantError do
      ;
  end;
  writeln('ok');
end.
