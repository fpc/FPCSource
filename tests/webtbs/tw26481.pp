{ %NORUN }

program tw26481;

{$MODE DELPHI}

type
  IComparer<T> = interface
    function Compare(constref Left, Right: T): Integer; overload;
  end;

  TOrdinalComparer<T, THashFactory> = class(TInterfacedObject, IComparer<T>)
  protected class var
    FComparer: IComparer<T>;
    FTest: TClass;
  public
    function Compare(constref Left, Right: T): Integer; virtual; abstract;
  end;

  TGOrdinalStringComparer<T, THashFactory> = class(TOrdinalComparer<T, THashFactory>)
  public
    function Compare(constref ALeft, ARight: T): Integer; override;
  end;

function TGOrdinalStringComparer<THashFactory, T>.Compare(constref ALeft,
  ARight: T): Integer;
begin
  Result := FComparer.Compare(ALeft, ARight);
end;

begin
end.

