unit ugenfunc7;

{$mode objfpc}

interface

type
  TTest = class
    constructor Create;
    generic function Add<T>(aLeft, aRight: T): T;
    generic class function AddClass<T>(aLeft, aRight: T): T;
    generic function GetPrivate<T>: T;
    generic function GetProtected<T>: T;
    generic function GetStrictPrivate<T>: T;
    generic function GetStrictProtected<T>: T;
  strict private
    fStrictPrivate: LongInt;
  private
    fPrivate: LongInt;
  strict protected
    fStrictProtected: LongInt;
  protected
    fProtected: LongInt;
  end;

generic function Add<T>(aLeft, aRight: T): T;

implementation

constructor TTest.Create;
begin
  fStrictPrivate := 1;
  fPrivate := 2;
  fStrictProtected := 3;
  fProtected := 4;
end;

generic function TTest.Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

generic class function TTest.AddClass<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

generic function TTest.GetStrictPrivate<T>: T;
begin
  Result := fStrictPrivate;
end;

generic function TTest.GetPrivate<T>: T;
begin
  Result := fPrivate;
end;

generic function TTest.GetStrictProtected<T>: T;
begin
  Result := fStrictProtected;
end;

generic function TTest.GetProtected<T>: T;
begin
  Result := fProtected;
end;

generic function Add<T>(aLeft, aRight: T): T;
begin
  Result := aLeft + aRight;
end;

end.

