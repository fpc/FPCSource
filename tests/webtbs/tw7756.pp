program tw7756;

{$mode objfpc}

uses Variants, SysUtils;

var
//  s : string;
  cp, cd, ci, ce, cg : integer; //iterators
  fr : TFloatRec;
  v : variant;
  precs : array [1..3] of integer = (0, 15, 50);
  decs : array [1..6] of integer =
      (0, 5, 15, 25, 50, 60);
  i : array [1..7] of integer = (-9057, -9194, -9059, 0, 9057, 9194, 9059);
  e : array [1..11] of extended = (
        -1.1E256, -5.5E256, -1.1E-256, -5.5E-256, -pi, 0.0,  pi, 1.1E-256, 5.5E-256, 1.1E256, 5.5E256);

const results: array[1..324] of string =
('257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-',
'258-1',
'-255-',
'-255-',
'1-',
'0+',
'1+',
'-255+',
'-255+',
'257+',
'258+1',
'257-11',
'257-55',
'-255-',
'-255-',
'1-3',
'0+',
'1+3',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-55',
'-255-',
'-255-',
'1-314159',
'0+',
'1+314159',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-55',
'-255-',
'-255-',
'1-314159265358979',
'0+',
'1+314159265358979',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-55',
'-255-',
'-255-',
'1-314159265358979',
'0+',
'1+314159265358979',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-55',
'-255-',
'-255-',
'1-314159265358979',
'0+',
'1+314159265358979',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-55',
'-255-',
'-255-',
'1-314159265358979',
'0+',
'1+314159265358979',
'-255+',
'-255+',
'257+11',
'257+55',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-3',
'0+',
'1+3',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-314159',
'0+',
'1+314159',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-3141592653589793',
'0+',
'1+3141592653589793',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-31415926535897931',
'0+',
'1+31415926535897931',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-31415926535897931',
'0+',
'1+31415926535897931',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'257-11',
'257-54999999999999998',
'-255-',
'-255-',
'1-31415926535897931',
'0+',
'1+31415926535897931',
'-255+',
'-255+',
'257+11',
'257+54999999999999998',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'5-1',
'5-1',
'5-1',
'0+',
'5+1',
'5+1',
'5+1',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059',
'4-9057',
'4-9194',
'4-9059',
'0+',
'4+9057',
'4+9194',
'4+9059');

{$ifdef FPC_HAS_TYPE_EXTENDED}
  { FloatToDecimal of an Extended with a precision above 17 gives 19 significant digits }
  extended_results: array[1..6,1..2] of string = (
    ('257-11','257-1099999999999999966'),
    ('257+11','257+1099999999999999966'),
    ('257-54999999999999998','257-5499999999999999832'),
    ('257+54999999999999998','257+5499999999999999832'),
    ('1-31415926535897931','1-3141592653589793116'),
    ('1+31415926535897931','1+3141592653589793116'));
{$endif FPC_HAS_TYPE_EXTENDED}

const
  error_count : longint = 0;

procedure add_error(cg : longint);
  begin
    inc(error_count);
  end;

function DecimalToStr(fr: TFloatRec): string;
var
  s : string;
begin
        s := IntToStr(fr.Exponent);
        if fr.Negative
          then s := s+ '-'
          else s := s+ '+';
        s := s + StrPas(@fr.Digits[0]);
        Result := s;
end;

function expected_result(index, precision : longint) : string;
{$ifdef FPC_HAS_TYPE_EXTENDED}
var
  k : longint;
{$endif FPC_HAS_TYPE_EXTENDED}
begin
  expected_result:=results[index];
{$ifdef FPC_HAS_TYPE_EXTENDED}
  if precision>17 then
    for k:=low(extended_results) to high(extended_results) do
      if expected_result=extended_results[k,1] then
        exit(extended_results[k,2]);
{$endif FPC_HAS_TYPE_EXTENDED}
end;

var
  s: ansistring;
BEGIN
  cg := 1; // grid row index
  for cp := Low(Precs) to High(Precs) do  //iterate through precisions
    for cd := Low(decs) to High(decs) do  //iterate through decimals
      for ce := Low(e) to High(e) do  //iterate through extended values
        begin
//        write(IntToStr(precs[cp]):2,';',IntToStr(decs[cd]):2,';');
//        str(e[ce]:250, s); s := Trim(s);
        v := e[ce];
//        write(s:25, ';');
        FloatToDecimal(fr, v, precs[cp], decs[cd]);
//        write(DecimalToStr(fr):25, ';');
//        writeln(DecimalToStr(fr));
        if DecimalToStr(fr) <> expected_result(cg,precs[cp]) then
          begin
            writeln('Index: ',cg);
            writeln(' -- expected: "',expected_result(cg,precs[cp]),'"');
            writeln(' -- got:      "',DecimalToStr(fr),'"');
            add_error(cg);
          end;
        inc(cg);
        end;
  // integers
  for cp := Low(Precs) to High(Precs) do  //iterate through precisions
    for cd := Low(decs) to High(decs) do  //iterate through decimals
      for ci := Low(i) to High(i) do  //iterate through integers
        begin
//        write(IntToStr(precs[cp]):2, ';', IntToStr(decs[cd]):2, ';');
//        s := IntToStr(i[ci]);
        v := i[ci];
//        write(s:25, ';');
        FloatToDecimal(fr, v, precs[cp], decs[cd]);
//        write(DecimalToStr(fr):25, ';');
//        writeln(DecimalToStr(fr));
        if DecimalToStr(fr) <> results[cg] then
          begin
            writeln('Index: ',cg);
            writeln(' -- expected: "',results[cg],'"');
            writeln(' -- got:      "',DecimalToStr(fr),'"');
            add_error(cg);
          end;
        inc(cg);
        end;
  if error_count>0 then
    begin
      writeln('Test finished with ',error_count,' errors');
      halt(1);
    end;
END.


