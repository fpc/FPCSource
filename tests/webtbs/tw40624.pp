program tw40624;

uses
  SysUtils
  { you can add units after this };

var
  C: Currency;
  V: Variant;
  S: string;
begin
  C := 822337203685477.5807;
  V := C;
  DefaultFormatSettings.DecimalSeparator := '.';

  if CurrToStr(C) <> '822337203685477.5807' then
    Halt(1);
  S := V;
  if S <> '822337203685477.5807' then
    Halt(2);
  WriteLn('OK');
end.
