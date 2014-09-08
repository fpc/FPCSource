

{ Converting 64-bit integers with more than 53 significant bits to double-precision 
  floating point format is subject to rounding. Hence result depends on rounding mode.
  The same goes for 32-bit integers with more than 23 significant bits converted to
  single. }
uses math;

type
  TExpected=array[TFPURoundingMode] of qword;

const
  res1_single: TExpected = (
    $4E800000,
    $4E800000,
    $4E800001,
    $4E800000
  );

  res2_single: TExpected = (
    $4EFFFFFF,
    $4EFFFFFF,
    $4F000000,
    $4EFFFFFF
  );

  res3_single: TExpected = (
    $CEFFFFFF,
    $CF000000,
    $CEFFFFFF,
    $CEFFFFFF
  );


  res1: TExpected = (
    $43D0000000000000,
    $43D0000000000000,
    $43D0000000000001,
    $43D0000000000000
  );

  res2: TExpected = (
    $43E0000000000000,
    $43DFFFFFFFFFFFFF,
    $43E0000000000000,
    $43DFFFFFFFFFFFFF
  );
  
  res3: TExpected = (
    qword($C3E0000000000000),
    qword($C3E0000000000000),
    qword($C3DFFFFFFFFFFFFF),
    qword($C3DFFFFFFFFFFFFF)
  );

var
  has_errors: boolean=false;

procedure fail;
begin
  writeln('Wrong!');
  has_errors:=true;
end;


procedure test32(x: longint; const res: texpected);
var
  y: single;
  yd: longword absolute y;
begin
  writeln('integer value=',hexstr(x,8));
  y:=x;
  writeln('rmNearest  ',y, ' ',hexstr(yd,8));
  if yd<>res[rmNearest] then fail;

  setroundmode(rmUp);
  y:=x;
  writeln('rmUp       ',y, ' ',hexstr(yd,8));
  if yd<>res[rmUp] then fail;

  setroundmode(rmDown);
  y:=x;
  writeln('rmDown     ',y, ' ',hexstr(yd,8));
  if yd<>res[rmDown] then fail;

  setroundmode(rmTruncate);
  y:=x;
  writeln('rmTruncate ',y, ' ',hexstr(yd,8));
  if yd<>res[rmTruncate] then fail;
end;


procedure testint64(x: int64; const res: TExpected);
var
  y: double;
  yq: qword absolute y;
begin
  writeln('integer value=',hexstr(x,16));
  setroundmode(rmNearest);
  y:=x;
  writeln('rmNearest  ',y, ' ',hexstr(yq,16));
  if yq<>res[rmNearest] then fail;
  
  setroundmode(rmUp);
  y:=x; 
  writeln('rmUp       ',y, ' ',hexstr(yq,16));
  if yq<>res[rmUp] then fail;
  
  setroundmode(rmDown);
  y:=x;
  writeln('rmDown     ',y, ' ',hexstr(yq,16));
  if yq<>res[rmDown] then fail;
  
  setroundmode(rmTruncate);
  y:=x;
  writeln('rmTruncate ',y, ' ',hexstr(yq,16));
  if yq<>res[rmTruncate] then fail;
end;


procedure testqword(x: qword; const res: TExpected);
var
  y: double;
  yq: qword absolute y;
begin
  writeln('integer value=',hexstr(x,16));
  setroundmode(rmNearest);
  y:=x;
  writeln('rmNearest  ',y, ' ',hexstr(yq,16));
  if yq<>res[rmNearest] then fail;

  setroundmode(rmUp);
  y:=x;
  writeln('rmUp       ',y, ' ',hexstr(yq,16));
  if yq<>res[rmUp] then fail;

  setroundmode(rmDown);
  y:=x;
  writeln('rmDown     ',y, ' ',hexstr(yq,16));
  if yq<>res[rmDown] then fail;

  setroundmode(rmTruncate);
  y:=x;
  writeln('rmTruncate ',y, ' ',hexstr(yq,16));
  if yq<>res[rmTruncate] then fail;
end;


begin
  writeln('Testing longint->single conversion');
  test32($40000001,res1_single);
  writeln;
  test32($7fffffff,res2_single);
  writeln;
  test32(longint($80000001),res3_single);
  writeln;

  writeln('Testing int64->double conversion');
  testint64($4000000000000001,res1);
  writeln;
  testint64($7fffffffffffffff,res2);
  writeln;
  testint64(int64($8000000000000001),res3);
  writeln;

  writeln('Testing qword->double conversion');
  testqword($4000000000000001,res1);
  writeln;
  testqword($7fffffffffffffff,res2);
  writeln;

  if has_errors then
    halt(1);
end.
