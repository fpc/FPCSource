{ %norun }
{ %opt=-CO -Seh }

{$MODE OBJFPC}
type
   TNode = class end;

procedure Test(const Arg: array of TNode);
var
   Item: TNode;
begin
   for Item in Arg do ; // Warning here
end;

begin
  test([]);
end.

