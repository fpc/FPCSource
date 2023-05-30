{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (missing set)
}
program texrtti5;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES()}

begin
end.
