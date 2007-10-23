{$mode objfpc}
uses
  sysutils,typinfo;
{$M+}

type
  TMyClass = class(TObject)
  private
    FValue: Currency;
  published
    property Value:Currency read FValue write FValue;
  end;

var
  MyClass :TMyClass;
begin
  MyClass := TMyClass.Create;
  SetFloatProp(MyClass, 'Value', 1);
  if MyClass.Value<>1 then
    halt(1);
  WriteLn(CurrToStr(MyClass.Value));
  MyClass.Free;
  writeln('ok');
end.
