unit uw40844c;

{$ifdef FPC}{$mode DELPHI}{$endif}

interface

uses
  uw40844b
  ;

type
  TArrayEnumerable<T> = class(TInterfacedObject, IEnumerable<T>)
  private
    function GetEnumerator: IEnumerable<T>;
  public
    constructor Create(const Arr: TArray<T>);
  end;


  TStableOrderByEnumerable = record
  strict private type
    TImpl<T> = class(TInterfacedObject, IEnumerable<T>)
    public
      function GetEnumerator: IEnumerable<T>;
    end;
  public
    class function Create<T>(const AItems: IEnumerable<T>; const AComparer: IComparer<T>): IEnumerable<T>; overload; static;
  end;

  TEmptyEnumerable<T> = class(TInterfacedObject, IEnumerable<T>)
  strict private
    class var FInstance: IEnumerable<T>;
    function GetEnumerator: IEnumerable<T>;
  end;

implementation

{ TArrayEnumerable<T> }

constructor TArrayEnumerable<T>.Create(const Arr: TArray<T>);
var IntfEnum: IEnumerable<IUnknown>;
begin

end;

function TArrayEnumerable<T>.GetEnumerator: IEnumerable<T>;
begin
  Result := nil;
end;

{ TStableOrderByEnumerable<T> }

function TStableOrderByEnumerable.TImpl<T>.GetEnumerator: IEnumerable<T>;
var ResultList: TFastListRec<T>;
begin

end;

class function TStableOrderByEnumerable.Create<T>(const AItems: IEnumerable<T>;
  const AComparer: IComparer<T>): IEnumerable<T>;
begin
  TImpl<TObject>.Create;
end;

{ TEmptyEnumerable<T> }

function TEmptyEnumerable<T>.GetEnumerator: IEnumerable<T>;
begin
  Result := nil;
end;

end.
