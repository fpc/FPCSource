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
    begin
      Writeln('Currentcy variable C as string is ',CurrToStr(C),' expecting 822337203685477.5807');
      Halt(1);
    end;
  S := V;
  if S <> '822337203685477.5807' then
    begin
      Writeln('Currency variable C passed to Variant variable V is ',S,' expecting 822337203685477.5807');
      Halt(2);
    end;
  WriteLn('OK');
end.
