program tw34497b;

{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
type
  TGenRec<T1, T2> = record
    A: T1;
    B: T2;
    class operator Implicit(const Rec: TGenRec<T1, T2>): T1;
    class operator Implicit(const Rec: TGenRec<T1, T2>): T2;
  end;

  class operator TGenRec<T1, T2>.Implicit(const Rec: TGenRec<T1, T2>): T1;
  begin
    Result := Rec.A;
  end;

  class operator TGenRec<T1, T2>.Implicit(const Rec: TGenRec<T1, T2>): T2;
  begin
    Result := Rec.B;
  end;

begin
end.

