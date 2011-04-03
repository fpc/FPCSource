{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  private }
program trhlp18;

uses
  urhlp17;

var
  t: TTest2;
begin
  t.Test;
end.

