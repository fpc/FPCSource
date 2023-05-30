{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (leading comma)
}
program texrtti3;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES([,vcPrivate])}

begin
end.
