{$mode objfpc}
type
  tc = class
    function test(var c: tc): boolean;
    left,right: tc;
  end;

  testfunc = function(var c: tc):boolean of object;

  function foreach(var c: tc; p: testfunc): boolean;
    begin
      if not assigned(c) then
        exit;
    end;


  function tc.test(var c: tc): boolean;
  begin
    result := foreach(c.left,@test);
    result := foreach(c.right,@test) or result;
  end;


begin
end.
