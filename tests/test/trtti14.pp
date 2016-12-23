program trtti14;

uses
  TypInfo;

var
  error: LongInt = 0;

procedure TestBooleanInfo(aTI: PTypeInfo; aOrdType: TOrdType; aWinBool: Boolean);
var
  td: PTypeData;
begin
  Inc(error);
  if aTI^.Kind <> tkBool then
    Halt(error);

  td := GetTypeData(aTI);

  Inc(error);
  if td^.OrdType <> aOrdType then
    Halt(error);

  if aWinBool then begin
    case td^.OrdType of
      otSQWord: begin
        Inc(error);
        if td^.MaxInt64Value <> High(Int64) then
          Halt(error);
        Inc(error);
        if td^.MinInt64Value <> Low(Int64) then
          Halt(error);
      end;
      otUByte,
      otUWord,
      otULong,
      otUQWord: begin
        Inc(error);
        Halt(error);
      end;
      else
        Inc(error);
        if td^.MaxValue <> High(LongInt) then
          Halt(error);
        Inc(error);
        if td^.MinValue <> Low(LongInt) then
          Halt(error);
    end;
  end else begin
    case td^.OrdType of
      otUQWord: begin
        Inc(error);
        if td^.MaxQWordValue <> 1 then
          Halt(error);
        Inc(error);
        if td^.MinQWordValue <> 0 then
          Halt(error);
      end;
      otSByte,
      otSWord,
      otSLong,
      otSQWord: begin
        Inc(error);
        Halt(error);
      end;
      else
        Inc(error);
        if td^.MaxValue <> 1 then
          Halt(error);
        Inc(error);
        if td^.MinValue <> 0 then
          Halt(error);
    end;
  end;
end;

begin
  TestBooleanInfo(PTypeInfo(TypeInfo(Boolean)), otUByte, False);
  TestBooleanInfo(PTypeInfo(TypeInfo(Boolean16)), otUWord, False);
  TestBooleanInfo(PTypeInfo(TypeInfo(Boolean32)), otULong, False);
  TestBooleanInfo(PTypeInfo(TypeInfo(Boolean64)), otUQWord, False);

  TestBooleanInfo(PTypeInfo(TypeInfo(ByteBool)), otSByte, True);
  TestBooleanInfo(PTypeInfo(TypeInfo(WordBool)), otSWord, True);
  TestBooleanInfo(PTypeInfo(TypeInfo(LongBool)), otSLong, True);
  TestBooleanInfo(PTypeInfo(TypeInfo(QWordBool)), otSQWord, True);
end.
