{%FAIL}
{$mode objfpc}

{
  Test RTTI directive (missing identifier)
}
program texrtti2;
uses
  typinfo;

{$RTTI EXPLICIT PROPERTIES([MissingIdentifier])}

begin
end.
