{ %norun }
{$mode objfpc}
program project1;
function treeElementAsString(node: TObject): string; inline;
begin
  result:=node.ClassName
end;
begin
  writeln(treeElementAsString(TObject(2)));
end.
