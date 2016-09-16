{ %NORUN }

{$MODE DELPHI}

type
  TSmartPtr<T: class> = record
    class operator Implicit(aValue: T): TSmartPtr<T>;
  end;

class operator TSmartPtr<T>.Implicit(aValue: T): TSmartPtr<T>;
begin
end;

var
  sp: TSmartPtr<TObject>;
begin
  sp := nil;
end.
