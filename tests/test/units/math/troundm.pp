

{ Converting 64-bit integers with more than 53 significant bits to double-precision 
  floating point format is subject to rounding. Hence result depends on rounding mode. }
uses math;

type
  TExpected=array[TFPURoundingMode] of qword;

const
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

procedure test(x: int64; const res: TExpected);
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
  test($4000000000000001,res1);
  writeln;
  test($7fffffffffffffff,res2); 
  writeln;
  test(int64($8000000000000001),res3);
  if has_errors then
    halt(1);
end.
