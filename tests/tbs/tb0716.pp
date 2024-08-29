program tbs9001;

{$mode Delphi}

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults,
  Math;

type
  TStackParam = record
    Base, Limit: QWord;
  end;

var
  List: TList<TStackParam>;
  S: TStackParam;
  Index: SizeInt;
begin
  List := TList<TStackParam>.Create(TComparer<TStackParam>.Construct(
    function (const A, B: TStackParam): Integer
    begin
      Result := IfThen(A.Limit < B.Limit, -1,
        IfThen(A.Limit = B.Limit, 0, 1));
    end)
  );
  try
    S.Limit := 100;
    S.Base := 200;
    List.Add(S);

    S.Limit := 210;
    S.Base := 300;
    List.Add(S);

    S.Limit := 350;
    S.Base := 400;
    List.Add(S);

    S.Limit := 50;
    if List.BinarySearch(S, Index) or (Index <> 0) then
      Halt(1);

    S.Limit := 100;
    if not List.BinarySearch(S, Index) or (Index <> 0) then
      Halt(2);

    S.Limit := 150;
    if List.BinarySearch(S, Index) or (Index <> 1) then
      Halt(3);

    S.Limit := 210;
    if not List.BinarySearch(S, Index) or (Index <> 1) then
      Halt(4);

    S.Limit := 290;
    if List.BinarySearch(S, Index) or (Index <> 2) then
      Halt(5);

    S.Limit := 340;
    if List.BinarySearch(S, Index) or (Index <> 2) then
      Halt(6);

    S.Limit := 350;
    if not List.BinarySearch(S, Index) or (Index <> 2) then
      Halt(7);

    S.Limit := 360;
    if List.BinarySearch(S, Index) or (Index <> 3) then
      Halt(8);

    S.Limit := 450;
    if List.BinarySearch(S, Index) or (Index <> 3) then
      Halt(9);

  finally
    List.Free;
  end;
end.

