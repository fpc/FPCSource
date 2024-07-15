unit uw40844b;

{$ifdef FPC}{$mode DELPHI}{$endif}

interface

type
  IComparer<T> = interface
    function Compare(const Left, Right: T): Integer; overload;
  end;

  IEnumerable<T> = interface
    function GetEnumerator: IEnumerable<T>;
  end;


type
  Enumerable<T> = record
  public
    class function Create(const AItems: TArray<T>): Enumerable<T>; overload; static;
    class operator Implicit(const AItems: IEnumerable<T>): Enumerable<T>;
    class function Empty: Enumerable<T>; static;
    function OrderBy(const AComparer: IComparer<T>): Enumerable<T>; overload;
  end;

  TFastListRec<T> = record
  public
    procedure Sort; overload; inline;
    procedure Sort(const AComparer: IComparer<T>); overload; inline;
  end;

  { TFastList }

  TFastList<T> = class(TObject)
  strict private
    FList: TFastListRec<T>;
  public
    function AsEnumerable: Enumerable<T>;
  end;

  TFastArray = record
  strict private
    class procedure SortImpl<T>(L, R: Pointer; const AComparer: IComparer<T>); overload; static;
    class procedure SortImpl<T>(L, R: Pointer); overload; static;
  public

    class procedure Sort<T>(AValues: Pointer; ACount: Integer); overload; static; inline;
    class procedure Sort<T>(AValues: Pointer; ACount: Integer; const AComparer: IComparer<T>); overload; static; inline;
  end;

implementation

uses
  uw40844c;

{ TFastArray }

class procedure TFastArray.SortImpl<T>(L, R: Pointer);
begin

end;

class procedure TFastArray.Sort<T>(AValues: Pointer; ACount: Integer);
begin
  SortImpl<T>(AValues, nil);
end;

class procedure TFastArray.Sort<T>(AValues: Pointer; ACount: Integer; const AComparer: IComparer<T>);
begin
  SortImpl<TObject> (AValues, nil, nil);
  SortImpl<T>(AValues, nil, nil);
end;

class procedure TFastArray.SortImpl<T>(L, R: Pointer; const AComparer: IComparer<T>);
begin

end;

{ Enumerable<T> }

class function Enumerable<T>.Create(const AItems: TArray<T>): Enumerable<T>;
begin
  TArrayEnumerable<T>.Create(AItems);
end;

class operator Enumerable<T>.Implicit(const AItems: IEnumerable<T>): Enumerable<T>;
begin

end;

function Enumerable<T>.OrderBy(const AComparer: IComparer<T>): Enumerable<T>;
begin
  Result := TStableOrderByEnumerable.Create<T>(nil, AComparer);
end;

class function Enumerable<T>.Empty: Enumerable<T>;
begin
  Result := TEmptyEnumerable<T>.Create;
end;

{ TFastListRec<T> }

procedure TFastListRec<T>.Sort(const AComparer: IComparer<T>);
begin
  TFastArray.Sort<T>(nil, 0, AComparer);
end;

procedure TFastListRec<T>.Sort;
begin
  TFastArray.Sort<T>(nil, 0);
end;

{ TFastList<T> }

function TFastList<T>.AsEnumerable: Enumerable<T>;
begin

end;

end.
