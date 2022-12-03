{ %FAIL }

program tw40010;

type
  PA_Node = ^TA_Node;
  TA_Node = array[0..3] of PA_Node;

  var n1,n2:PA_Node;
begin
  n2 := nil;
  n1[3] := n2; { Should fail with a syntax error, not not cause a stack overflow }
end.
