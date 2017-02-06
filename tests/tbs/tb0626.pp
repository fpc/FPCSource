{ %norun }

program tb0626;

{$MODE OBJFPC}

type
  TNonManagedObj = object
    d: double;
  end;

var
  p: Pointer;
begin
  p := TypeInfo(TNonManagedObj);
end.

