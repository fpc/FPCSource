program tmoddiv4;

const
  TestValues: array[0..10] of QWord = (500, 1, 0, 995, $100000000, $100000001, $7FFFFFFFFFFFFFFF, QWord($8000000000000000), QWord($8000000000000001), QWord($8000000000000002), 1000000);

const
  ExpectedResults: array[0..10,1..18] of QWord = (
    (0,500,500,0,166,2,0,500,0,500,0,500,0,500,0,500,0,500),
    (0,1,1,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1),
    (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
    (0,995,995,0,331,2,0,995,0,995,0,995,0,995,0,995,0,995),
    (4294967,296,4294967296,0,1431655765,1,1048576,0,2,2,2,0,0,4294967296,0,4294967296,0,4294967296),
    (4294967,297,4294967297,0,1431655765,2,1048576,1,2,3,2,1,0,4294967297,0,4294967297,0,4294967297),
    (9223372036854775,807,9223372036854775807,0,3074457345618258602,1,2251799813685247,4095,4294967298,1,4294967295,2147483647,1,0,0,9223372036854775807,18446744073709551615,0),
    (9223372036854775,808,9223372036854775808,0,3074457345618258602,2,2251799813685248,0,4294967298,2,4294967296,0,1,1,1,0,1,18446744073709551615),
    (9223372036854775,809,9223372036854775809,0,3074457345618258603,0,2251799813685248,1,4294967298,3,4294967296,1,1,2,0,9223372036854775809,1,0),
    (9223372036854775,810,9223372036854775810,0,3074457345618258603,1,2251799813685248,2,4294967298,4,4294967296,2,1,3,0,9223372036854775810,0,9223372036854775810),
    (1000,0,1000000,0,333333,1,244,576,0,1000000,0,1000000,0,1000000,0,1000000,0,1000000));

var
  X, Y: QWord;
  C, Col: LongWord;

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
    Y := QWord(X) div 1000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod 1000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div 1;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod 1;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div 3;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod 3;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $1000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $1000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $7FFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $7FFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $80000000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $80000000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $7FFFFFFFFFFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $7FFFFFFFFFFFFFFF;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $8000000000000000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $8000000000000000;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) div $8000000000000001;
    Write(Y,',');
    DoCheck;

    Y := QWord(X) mod $8000000000000001;
    Writeln(Y);
    DoCheck;
  end;
  writeln('ok');
end.
