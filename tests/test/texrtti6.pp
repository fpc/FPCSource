{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (missing parentheses)
}
program texrtti6;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES}

begin
end.
