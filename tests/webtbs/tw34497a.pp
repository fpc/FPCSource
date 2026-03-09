{ %NORUN }

program tw34497a;

{$mode ObjFPC}
{$modeswitch AdvancedRecords}

type
  generic TGenRec<T1, T2> = record
    A: T1;
    B: T2;
    function FuncA(const T: T1): T1;
    function FuncA(const T: T2): T2;
    class operator :=(constref Rec: TGenRec): T1;
    class operator :=(constref Rec: TGenRec): T2;
  end;

  function TGenRec.FuncA(const T: T1): T1;
  begin
    Result := T;
  end;

  function TGenRec.FuncA(const T: T2): T2;
  begin
    Result := T;
  end;

  class operator TGenRec.:=(constref Rec: TGenRec): T1;
  begin
    Result := Rec.A;
  end;

  class operator TGenRec.:=(constref Rec: TGenRec): T2;
  begin
    Result := Rec.B;
  end;

begin
end.
