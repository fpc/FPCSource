unit udots.dot;

interface

var
  test: char;

procedure t;

implementation

uses
  udots, udots.dot.next;

// test that type is resolved
var
  test1: udots.dot.next.ttest;

procedure t;
begin
  // test that we resolved the next identifier to the local variable test
  udots.dot.test := 'c';
end;

end.

