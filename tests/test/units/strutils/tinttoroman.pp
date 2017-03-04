{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  StrUtils;

var
  exitCode: integer = 0;

procedure IntToRomanTest(const testinteger: integer;
                         const expectation: string);
  var
    teststring: string;
  begin
    teststring := IntToRoman(testinteger);
    if teststring <> expectation then
    begin
      writeln('Testing strUtils/IntToRoman: Test failed with number ', testinteger);
      writeln('Returned String: ', teststring);
      writeln('Expected String: ', expectation);
      exitCode := 1;
    end;
  end;
  
var
  i, value, digit, safedValue: integer;
  testinteger: integer;
  teststring: string;

begin

  for testinteger := 1 to 2000 do
  begin
    value := testinteger;
    digit := value mod 10;
    case digit of
      0: teststring := '';
      1: teststring := 'I';
      2: teststring := 'II';
      3: teststring := 'III';
      4: teststring := 'IV';
      5: teststring := 'V';
      6: teststring := 'VI';
      7: teststring := 'VII';
      8: teststring := 'VIII';
      9: teststring := 'IX';
    end;
    value := value div 10;
    digit := value mod 10;
    case digit of
      1: teststring := 'X' + teststring;
      2: teststring := 'XX' + teststring;
      3: teststring := 'XXX' + teststring;
      4: teststring := 'XL' + teststring;
      5: teststring := 'L' + teststring;
      6: teststring := 'LX' + teststring;
      7: teststring := 'LXX' + teststring;
      8: teststring := 'LXXX' + teststring;
      9: teststring := 'XC' + teststring;
    end;
    value := value div 10;
    digit := value mod 10;
    case digit of
      1: teststring := 'C' + teststring;
      2: teststring := 'CC' + teststring;
      3: teststring := 'CCC' + teststring;
      4: teststring := 'CD' + teststring;
      5: teststring := 'D' + teststring;
      6: teststring := 'DC' + teststring;
      7: teststring := 'DCC' + teststring;
      8: teststring := 'DCCC' + teststring;
      9: teststring := 'CM' + teststring;
    end;
    value := value div 10;
    for i := 1 to value do
      teststring := 'M' + teststring;
    
    IntToRomanTest(testinteger, teststring);
  end;

  randomize;
  for testinteger := 1 to 1000 do
  begin
    value := random(100000);
    safedValue := value;
    digit := value mod 10;
    case digit of
      0: teststring := '';
      1: teststring := 'I';
      2: teststring := 'II';
      3: teststring := 'III';
      4: teststring := 'IV';
      5: teststring := 'V';
      6: teststring := 'VI';
      7: teststring := 'VII';
      8: teststring := 'VIII';
      9: teststring := 'IX';
    end;
    value := value div 10;
    digit := value mod 10;
    case digit of
      1: teststring := 'X' + teststring;
      2: teststring := 'XX' + teststring;
      3: teststring := 'XXX' + teststring;
      4: teststring := 'XL' + teststring;
      5: teststring := 'L' + teststring;
      6: teststring := 'LX' + teststring;
      7: teststring := 'LXX' + teststring;
      8: teststring := 'LXXX' + teststring;
      9: teststring := 'XC' + teststring;
    end;
    value := value div 10;
    digit := value mod 10;
    case digit of
      1: teststring := 'C' + teststring;
      2: teststring := 'CC' + teststring;
      3: teststring := 'CCC' + teststring;
      4: teststring := 'CD' + teststring;
      5: teststring := 'D' + teststring;
      6: teststring := 'DC' + teststring;
      7: teststring := 'DCC' + teststring;
      8: teststring := 'DCCC' + teststring;
      9: teststring := 'CM' + teststring;
    end;
    value := value div 10;
    for i := 1 to value do
      teststring := 'M' + teststring;
    
    IntToRomanTest(safedValue, teststring);
  end;

  halt(exitCode);
end.
