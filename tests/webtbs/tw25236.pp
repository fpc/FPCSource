{ %NORUN }

program tw25236;

{$mode delphi}

type
  TCompare<T> = function(const A, B: T): Integer;

  TList<T> = class
  public
    type TListCompare = TCompare<T>;
  private
    {$define bug}
    {$ifdef bug}
    {Error: Generics without specialization cannot be used as a type for a variable}
    FComparer: TListCompare;
    {$else}
    FComparer: TCompare<T>;
    {$endif}
  public
    property Comparer: TListCompare read FComparer write FComparer;
  end;

begin
end.
