{ test for the various DynArray* functions }

program tarray13;

{$mode objfpc}

function TestDynArraySize: LongInt;
type
  TTest = record
    value1: QWord;
    value2: LongInt;
  end;

var
  arr1: array of LongInt;
  arr2: array of QWord;
  arr3: array of TTest;
begin
  SetLength(arr1, 5);
  SetLength(arr2, 8);
  SetLength(arr3, 6);

  if DynArraySize(Pointer(arr1)) <> 5 then
    Halt(1);
  if DynArraySize(@arr1[0]) <> 5 then
    Halt(2);
  if DynArraySize(Pointer(arr2)) <> 8 then
    Halt(3);
  if DynArraySize(@arr2[0]) <> 8 then
    Halt(4);
  if DynArraySize(Pointer(arr3)) <> 6 then
    Halt(5);
  if DynArraySize(@arr3[0]) <> 6 then
    Halt(6);

  Result := 7;
end;

function TestDynArrayIndexSingle(aBaseCode: LongInt): LongInt;
type
  TTest1 = record
    value1: QWord;
    value2: LongInt;
  end;

  TTest2 = packed record
    value1: LongInt;
    value2: QWord;
  end;

var
  arr1: array of Byte;
  arr2: array of LongInt;
  arr3: array of QWord;
  arr4: array of TTest1;
  arr5: array of TTest2;
  i, basecode: LongInt;
begin
  SetLength(arr1, 5);
  SetLength(arr2, 3);
  SetLength(arr3, 8);
  SetLength(arr4, 6);
  SetLength(arr5, 7);

  basecode := aBaseCode;
  for i := Low(arr1) to High(arr1) do begin
    if @arr1[i] <> DynArrayIndex(Pointer(arr1), [i], TypeInfo(arr1)) then
      Halt(basecode + i * 2);
    if @arr1[i] <> DynArrayIndex(@arr1[0], [i], TypeInfo(arr1)) then
      Halt(basecode + i * 2 + 1);
  end;

  basecode := basecode + 2 * Length(arr1);
  for i := Low(arr2) to High(arr2) do begin
    if @arr2[i] <> DynArrayIndex(Pointer(arr2), [i], TypeInfo(arr2)) then
      Halt(basecode + i * 2);
    if @arr2[i] <> DynArrayIndex(@arr2[0], [i], TypeInfo(arr2)) then
      Halt(basecode + i * 2 + 1);
  end;

  basecode := basecode + 2 * Length(arr2);
  for i := Low(arr3) to High(arr3) do begin
    if @arr3[i] <> DynArrayIndex(Pointer(arr3), [i], TypeInfo(arr3)) then
      Halt(basecode + i * 2);
    if @arr3[i] <> DynArrayIndex(@arr3[0], [i], TypeInfo(arr3)) then
      Halt(basecode + i * 2 + 1);
  end;

  basecode := basecode + 2 * Length(arr3);
  for i := Low(arr4) to High(arr4) do begin
    if @arr4[i] <> DynArrayIndex(Pointer(arr4), [i], TypeInfo(arr4)) then
      Halt(basecode + i * 2);
    if @arr4[i] <> DynArrayIndex(@arr4[0], [i], TypeInfo(arr4)) then
      Halt(basecode + i * 2 + 1);
  end;

  basecode := basecode + 2 * Length(arr4);
  for i := Low(arr5) to High(arr5) do begin
    if @arr5[i] <> DynArrayIndex(Pointer(arr5), [i], TypeInfo(arr5)) then
      Halt(basecode + i * 2);
    if @arr5[i] <> DynArrayIndex(@arr5[0], [i], TypeInfo(arr5)) then
      Halt(basecode + i * 2 + 1);
  end;

  Result := basecode + 2 * Length(arr5);
end;

function TestDynArrayIndexMulti(aBaseCode: LongInt): LongInt;
var
  arr1: array of array of LongInt;
  arr2: array of array of array of QWord;
  i, j, k, basecode: LongInt;
begin
  SetLength(arr1, 4, 8);
  SetLength(arr2, 3, 5, 9);

  basecode := aBaseCode;
  for i := Low(arr1) to High(arr1) do begin
    for j := Low(arr1[i]) to High(arr1[i]) do begin
      if @arr1[i, j] <> DynArrayIndex(Pointer(arr1), [i, j], TypeInfo(arr1)) then
        Halt(basecode + j * 2);
      if @arr1[i, j] <> DynArrayIndex(@arr1[0], [i, j], TypeInfo(arr1)) then
        Halt(basecode + j * 2 + 1);
      { Note: @arr1[0, 0] would be different from @arr1[0] or arr1! }
    end;
    basecode := basecode + Length(arr1[i]) * 3;
  end;

  for i := Low(arr2) to High(arr2) do begin
    for j := Low(arr2[i]) to High(arr2[i]) do begin
      for k := Low(arr2[i, j]) to High(arr2[i, j]) do begin
        if @arr2[i, j, k] <> DynArrayIndex(Pointer(arr2), [i, j, k], TypeInfo(arr2)) then
          Halt(basecode + k * 2);
        if @arr2[i, j, k] <> DynArrayIndex(@arr2[0], [i, j, k], TypeInfo(arr2)) then
          Halt(basecode + k * 2 + 1);
        { Note: @arr2[0, 0] and @arr2[0, 0, 0] would be different from @arr2[0] or arr2! }
      end;
      basecode := basecode + Length(arr2[i, j]) * 2;
    end;
  end;

  Result := basecode;
end;

var
  basecode: LongInt;
begin
  Writeln('TestDynArraySize errors starting at 1');
  basecode := TestDynArraySize;
  Writeln('TestDynArrayIndexSingle errors starting at ', basecode);
  basecode := TestDynArrayIndexSingle(basecode);
  Writeln('TestDynArrayIndexMulti errors starting at ', basecode);
  basecode := TestDynArrayIndexMulti(basecode);
  Writeln('ok');
end.
