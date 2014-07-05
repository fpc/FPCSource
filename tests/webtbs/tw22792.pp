{ %NORUN }

program tw22792;

{$MODE DELPHI}

type
  TStaticArray<T> = array [0..MaxInt div SizeOf(T) - 1] of T;

  TWrapper<TValue> = class
  strict private
    type
      PValue = ^TValue;
      PList = ^TList;
      TList = TStaticArray<PValue>;
        { Expand this manually to get rid of the error }
  strict private
    FList: PList;
  public
    procedure Z(const value: TValue);
  end;

procedure TWrapper<TValue>.Z(const value: TValue);
begin
  FList := GetMem(SizeOf(PValue));
  FList^[0] := GetMem(SizeOf(TValue));
  FList^[0]^ := value;
    { Error: Illegal qualifier; use a manual cast to get rid of the error }
  FreeMem(FList^[0]);
  FreeMem(FList, SizeOf(PValue));
end;

begin
end.
