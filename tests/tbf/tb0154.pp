{ %fail }

{$mode objfpc}
type
  tc = class
    left,right: tc;
    function test(var c: tc): boolean;
  end;

  testfunc = function(var c: tc):boolean;

  function foreach(var c: tc; p: testfunc): boolean;
    begin
      if not assigned(c) then
        exit;
    end;


  function tc.test(var c: tc): boolean;
  begin
    { The @tc.test is still a pointer to a method and not valid
      with a normal procvar }
    result := foreach(c.left,@tc.test);
    result := foreach(c.right,@tc.test) or result;
  end;


begin
end.
