program testcstr;

{$mode objfpc}

resourcestring
  RsFDivFlawed = 'Res1';
  RsFDivOK     = 'Res2';

const
  c1 = 'A';
  c2 = 'B';
  s1 = 'String1';
  s2 = 'String2';

  FDIVResStringS  : array [0..1] of shortstring = (RsFDivFlawed, RsFDivOK);
  FDIVResStringsA : array [0..1] of ansistring  = (RsFDivFlawed, RsFDivOK);
  FDivChars    : array [0..1] of shortstring = (c1,c2);
  FDivCharsA   : array [0..1] of ansistring  = (c1,c2);
  FDIVStringS  : array [0..1] of shortstring = (s1,s2);
  FDIVStringsA : array [0..1] of ansistring  = (s1,s2);

var
  error : integer;
begin
  error:=0;
  if Fdivresstrings[0]<>'Res1' then
   inc(error);
  if FdivresstringsA[1]<>'Res2' then
   inc(error);
  if FdivChars[0]<>'A' then
   inc(error);
  if FdivCharsA[1]<>'B' then
   inc(error);
  if Fdivstrings[0]<>'String1' then
   inc(error);
  if FdivstringsA[1]<>'String2' then
   inc(error);
  if error>0 then
   begin
     writeln(error,' errors with constant strings');
     halt(1);
   end;
end.
