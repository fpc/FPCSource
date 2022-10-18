{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (missing set)
}
program tdirrtti5;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES()}

begin
end.