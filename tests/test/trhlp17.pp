{ %FAIL }

{ usage of nested helpers adheres to visibility rules as well - here:
  strict private }
program trhlp17;

uses
  urhlp17;

var
  t: TTest1;
begin
  t.Test;
end.
