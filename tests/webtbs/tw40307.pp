{ %OPT=-MObjFPC -O2 -a }

program tw40307;

type
  TOutStruct = record
    V1: SmallInt;
    V2: LongInt;
  end;

procedure SetParams(InputVal: Boolean; out V: TOutStruct); noinline;
  var
    V1: SmallInt;
    V2: LongInt;
  begin
    if not InputVal then
      begin
        V1 := 32;
        V2 := 8;
      end
    else
      begin
        V1 := 16;
        V2 := 16;
      end;

    V.V1 := V1;
    V.V2 := V2;
  end;

const
  Expected: array[Boolean] of TOutStruct = ((V1: 32; V2: 8), (V1: 16; V2: 16));
var
  Param: Boolean; OV: TOutStruct;
begin
  for Param := False to True do
    begin
      SetParams(Param, OV);
      if (OV.V1 <> Expected[Param].V1) then
        begin
          WriteLn('FAIL: OV.V1 = ', OV.V1, ' but expected ', Expected[Param].V1);
          Halt(1);
        end;

      if (OV.V2 <> Expected[Param].V2) then
        begin
          WriteLn('FAIL: OV.V2 = ', OV.V2, ' but expected ', Expected[Param].V2);
          Halt(2);
        end;
    end;

  WriteLn('ok');
end.