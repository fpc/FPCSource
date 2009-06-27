{$mode objfpc}
type
  tc = class
    left,right: tc;
    function test(var c: tc): boolean;
  end;

  testfunc = function(var c: tc):boolean of object;

  function foreach(var c: tc; p: testfunc): boolean;
    begin
      if not assigned(c) then
        exit;
    end;


  function tc.test(var c: tc): boolean;
  begin
    { if you use @test, the compiler tries to get the address of the }
    { function result instead of the address of the method (JM)       }
    result := foreach(c.left,@self.test);
    result := foreach(c.right,@self.test) or result;
  end;


begin
end.
