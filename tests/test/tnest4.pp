{$mode objfpc}

function test: longint;

  function func(aa: integer): integer;

    function func_nested(b: integer): integer;
    begin
      if b < 10 then
        Result:=func_nested(b+1)
      else
        Result:=b;
      Inc(Result, aa);
    end;

  begin
    Result:=func_nested(0);
  end;

begin
  result:=func(10);
end;

function test2: longint;
var
  i: integer;

  function func(aa: integer): integer;

    function func_nested(b: integer): integer;
    begin
      if b < 10 then
        Result:=func(b+1)
      else
        Result:=b;
    end;

  begin
    Result:=func_nested(aa);
    Inc(Result, i);
  end;

begin
  i:=100;
  result:=func(0);
end;

begin
  if test <> 120 then
    halt(1);
  if test2 <> 1110 then
    halt(2);
  writeln('OK');
end.
