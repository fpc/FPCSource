program tmoddiv3;

const
  TestValues: array[0..9] of LongWord = (500, 1, 0, 995, $7FFFFFFF, $80000000, $80000001, $80000002, $FFFFFFFF, 1000000);

const
  ExpectedResults: array[0..9,1..16] of LongWord = (
    (0,500,500,0,166,2,0,500,0,500,0,500,0,500,0,500),
    (0,1,1,0,0,1,0,1,0,1,0,1,0,1,0,1),
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,995,995,0,331,2,0,995,0,995,0,995,0,995,0,995),
    (2147483,647,2147483647,0,715827882,1,524287,4095,1,0,0,2147483647,0,2147483647,0,2147483647),
    (2147483,648,2147483648,0,715827882,2,524288,0,1,1,1,0,0,2147483648,0,2147483648),
    (2147483,649,2147483649,0,715827883,0,524288,1,1,2,1,1,1,0,0,2147483649),
    (2147483,650,2147483650,0,715827883,1,524288,2,1,3,1,2,1,1,0,2147483650),
    (4294967,295,4294967295,0,1431655765,0,1048575,4095,2,1,1,2147483647,1,2147483646,1,0),
    (1000,0,1000000,0,333333,1,244,576,0,1000000,0,1000000,0,1000000,0,1000000));

var
  X, Y, C, Col: LongWord;

procedure DoCheck;
  begin
    if Y<>ExpectedResults[C,Col] then
      begin
        writeln('Error at ',C,' ',Col);
        halt(1);
      end;
    Inc(Col);
  end;

begin
  for C := Low(TestValues) to High(TestValues) do
  begin
    X := TestValues[C];
    Col := 1;

    Y := X div 1000;
    Write(Y,',');
    DoCheck;

    Y := X mod 1000;
    Write(Y,',');
    DoCheck;

    Y := X div 1;
    Write(Y,',');
    DoCheck;

    Y := X mod 1;
    Write(Y,',');
    DoCheck;

    Y := X div 3;
    Write(Y,',');
    DoCheck;

    Y := X mod 3;
    Write(Y,',');
    DoCheck;

    Y := X div $1000;
    Write(Y,',');
    DoCheck;

    Y := X mod $1000;
    Write(Y,',');
    DoCheck;

    Y := X div $7FFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := X mod $7FFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := X div $80000000;
    Write(Y,',');
    DoCheck;

    Y := X mod $80000000;
    Write(Y,',');
    DoCheck;

    Y := X div $80000001;
    Write(Y,',');
    DoCheck;

    Y := X mod $80000001;
    Write(Y,',');
    DoCheck;

    Y := X div $FFFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := X mod $FFFFFFFF;
    Writeln(Y);
    DoCheck;
  end;
  writeln('ok');
end.
