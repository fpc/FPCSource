{$mode objfpc}
{$h+}
{$hints on}
{$warnings on}

uses
  SysUtils,
  StrUtils;

var
  exitCode: integer = 0;

procedure Numb2USATest(const teststring: string;
                       const expectation: string);
  var
    usastring: string;
  begin
    usastring := Numb2USA(teststring);
    if usastring <> expectation then
    begin
      writeln('Testing strUtils/Numb2USA: Test with ', teststring, ' failed.');
      writeln('Returned String: ', usastring);
      writeln('Expected String: ', expectation);
      exitCode := 1;
    end;
  end; 

var
  i, j, len, value, pos, posusa, numberOfCommas, preDigits: integer;
  teststring: string;
  usastring: string;

begin
  randomize;
  for i := 0 to 1000 do
  begin
    value := trunc(exp(random(trunc(ln(MaxInt)))));
    teststring := intToStr(value);
    len := length(teststring);
    if len <= 3 then
      usastring := teststring
    else
    begin
      numberOfCommas := (len - 1) div 3;
      setlength(usastring, len + numberOfCommas);
      preDigits := (len - 1) mod 3 + 1; { gives 1, 2 or 3 }
      for j := 1 to preDigits do
        usastring[j] := teststring[j];
      pos := preDigits + 1;
      posusa := preDigits + 1;
      usastring[posusa] := ',';
      inc(posusa);
      if numberOfCommas > 1 then
        for j := 1 to numberOfCommas - 1 do
        begin
          usastring[posusa] := teststring[pos];
          inc(pos);
          inc(posusa);
          usastring[posusa] := teststring[pos];
          inc(pos);
          inc(posusa);
          usastring[posusa] := teststring[pos];
          inc(posusa);
          usastring[posusa] := ',';
          inc(pos);
          inc(posusa);
        end;
      usastring[posusa] := teststring[pos];
      inc(pos);
      inc(posusa);
      usastring[posusa] := teststring[pos];
      inc(pos);
      inc(posusa);
      usastring[posusa] := teststring[pos];
    end;

    Numb2USATest(teststring, usastring);
  end;
 
  halt(exitCode);
end.
