{ %NORUN }

program tw29546;

{$mode objfpc}

type
  TUtils = class sealed(TObject)
  public
    generic class function Iif<T>(ACondition: Boolean;
      const ATrueValue, AFalseValue: T): T; static;
  end;

  generic class function TUtils.Iif<T>(ACondition: Boolean;
    const ATrueValue, AFalseValue: T): T;
  begin
    if ACondition then
      Result := ATrueValue
    else
      Result := AFalseValue;
  end;

var
  S: string;
begin
  S := TUtils.specialize Iif<string>(False, 'YES', 'NO');
  S := TUtils.specialize Iif<string>(True, 'YES', 'NO');
end.

