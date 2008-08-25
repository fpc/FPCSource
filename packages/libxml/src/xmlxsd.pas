unit xmlxsd;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils;

function xsdFormatBoolean(Value: Boolean): String;
function xsdFormatDate(Year, Month, Day: Longword): String;
function xsdFormatDateTime(Year, Month, Day, Daytime: Longword): String;
function xsdFormatDecimal(Value: Extended; Precision: Integer = 4; Digits: Integer = 1): String;
function xsdFormatDouble(Value: Double): String;
function xsdFormatFloat(Value: Single): String;
function xsdFormatByte(Value: Shortint): String;
function xsdFormatShort(Value: Smallint): String;
function xsdFormatInt(Value: Longint): String;
function xsdFormatLong(Value: Int64): String;
function xsdFormatUnsignedByte(Value: Byte): String;
function xsdFormatUnsignedShort(Value: Word): String;
function xsdFormatUnsignedInt(Value: Longword): String;
function xsdFormatUnsignedLong(Value: QWord): String;

implementation

function xsdFormatBoolean(Value: Boolean): String;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

function xsdFormatDate(Year, Month, Day: Longword): String;
begin
  Result := Format('%4.4d-%2.2d-%2.2dZ', [Year, Month, Day]);
end;

function xsdFormatDateTime(Year, Month, Day, Daytime: Longword): String;
var
  Hour, Minute, Second: Longword;
begin
  Daytime := Daytime div 1000;  // ms to sec
  Second  := Daytime mod 60;    // extract sec
  Daytime := Daytime div 60;    // sec to min
  Minute  := Daytime mod 60;    // extract min
  Daytime := Daytime div 60;    // min to hour
  Hour    := Daytime mod 60;    // extract hour

  Result := Format('%4.4d-%2.2d-%2.2dT%2.2d:%2.2d:%2.2dZ', [Year, Month, Day, Hour, Minute, Second]);
end;

function xsdFormatDecimal(Value: Extended; Precision: Integer; Digits: Integer): String;
begin
  Result := FloatToStrF(Value, ffFixed, Precision, Digits);
end;

function xsdFormatDouble(Value: Double): String;
begin
  Result := FloatToStr(Value);
end;

function xsdFormatFloat(Value: Single): String;
begin
  Result := FloatToStr(Value);
end;

function xsdFormatByte(Value: Shortint): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatShort(Value: Smallint): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatInt(Value: Integer): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatLong(Value: Int64): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedByte(Value: Byte): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedShort(Value: Word): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedInt(Value: Longword): String;
begin
  Result := IntToStr(Value);
end;

function xsdFormatUnsignedLong(Value: QWord): String;
begin
  Result := IntToStr(Value);
end;

end.