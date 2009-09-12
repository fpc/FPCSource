{ test for simple case-var with H+ dir }

{$H+}

unit tcase48;

interface
  procedure test_proc(var res : integer); inline;
    
implementation

var
  some_str: string;

procedure test_proc(var res : integer);
begin
  some_str := 'b';
  case some_str of
    ''..'ba' : res := 1;
    'bab'..'bbb' : res := 2;
    'bbc'..'bf' : res := 3;
    else res := 4;
  end;
end;

end.
