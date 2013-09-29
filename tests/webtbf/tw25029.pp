{%FAIL}
program tw25029;
type
  TMyEnum = (me1, me2, me3);
var
  e: TMyEnum;
begin
  e := me1.me2;
end.