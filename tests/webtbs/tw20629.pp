{ %NORUN }

{$MODE delphi}

type
  TWrapper<TValue> = class end;
  TObjectWrapper = TWrapper<TObject>;

begin
  with TObjectWrapper.Create do Free;     { OK }
  with TWrapper<TObject>.Create do Free;  { Error }
end.
