unit tudots.dot;

interface

var
  test: char;

procedure t;

implementation

uses
  tudots, tudots.dot.next;

// test that type is resolved
var
  test1: tudots.dot.next.ttest;

procedure t;
begin
  // test that we resolved the next identifier to the local variable test
  tudots.dot.test := 'c';
end;

end.

