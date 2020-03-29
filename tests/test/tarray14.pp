program tarray14;

{procedure Dump(arr: array of LongInt);
var
  i: LongInt;
begin
  Writeln('Length: ', Length(arr));
  Write('Data:');
  for i in arr do
    Write(' ', i);
  Writeln;
end;}

type
  TLongIntArray = array of LongInt;

procedure Check(darr: array of LongInt; sarr: array of TLongIntArray; var code: LongInt);
var
  i, j, k: LongInt;
begin
  j := 0;
  k := 0;
  for i:=0 to High(darr) do begin
    if j>High(sarr) then
      Halt(code);
    while Length(sarr[j]) = 0 do begin
      Inc(j);
      if j>High(sarr) then
        Halt(code + 1);
      k:=0;
    end;
    //writeln('comparing element ', i, ' against element ', k, ' of array ', j);
    if darr[i] <> sarr[j][k] then
      Halt(code + 2);
    Inc(k);
    if k=Length(sarr[j]) then begin
      Inc(j);
      k:=0;
    end;
  end;
  if (j < High(sarr)) or ((j = High(sarr)) and (k < High(sarr[j]))) then
    Halt(code + 3);
  code := code + 4;
end;

var
  ai, ai1, ai2, ai3, ai4: array of LongInt;
  code: LongInt = 0;
begin
  ai1 := [1, 2, 3];
  ai2 := [6, 8, 10];
  ai3 := [15, 17, 19];
  ai4 := [23, 24, 25];

  Writeln('Testing variables');
  ai := Concat(ai1);
  Check(ai, [ai1], code);
  ai := Concat(ai1, ai2);
  Check(ai, [ai1, ai2], code);
  ai := Concat(ai2, ai1);
  Check(ai, [ai2, ai1], code);
  ai := Concat(ai1, ai2, ai3, ai4);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat(Concat(ai1, ai2), Concat(ai3, ai4));
  Check(ai, [ai1, ai2, ai3, ai4], code);

  Writeln('Testing array constructors');
  ai := Concat([1, 2, 3]);
  Check(ai, [ai1], code);
  ai := Concat([1, 2, 3], [6, 8, 10]);
  Check(ai, [ai1, ai2], code);
  ai := Concat([6, 8, 10], [1, 2, 3]);
  Check(ai, [ai2, ai1], code);
  ai := Concat([1, 2, 3], [6, 8, 10], [15, 17, 19], [23, 24, 25]);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat(Concat([1, 2, 3], [6, 8, 10]), Concat([15, 17, 19], [23, 24, 25]));
  Check(ai, [ai1, ai2, ai3, ai4], code);

  Writeln('Testing mix of variables and array constructors');
  ai := Concat(ai1, [6, 8, 10]);
  Check(ai, [ai1, ai2], code);
  ai := Concat([1, 2, 3], ai2);
  Check(ai, [ai1, ai2], code);
  ai := Concat([6, 8, 10], ai1);
  Check(ai, [ai2, ai1], code);
  ai := Concat(ai2, [1, 2, 3]);
  Check(ai, [ai2, ai1], code);
  ai := Concat([1, 2, 3], ai2, [15, 17, 19], ai4);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat(ai1, [6, 8, 10], [15, 17, 19], ai4);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat([1, 2, 3], [6, 8, 10], [15, 17, 19], ai4);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat(ai1, [6, 8, 10], [15, 17, 19], [23, 24, 25]);
  Check(ai, [ai1, ai2, ai3, ai4], code);
  ai := Concat(Concat([1, 2, 3], [6, 8, 10]), Concat([15, 17, 19], [23, 24, 25]));
  Check(ai, [ai1, ai2, ai3, ai4], code);

  Writeln('ok');
end.
