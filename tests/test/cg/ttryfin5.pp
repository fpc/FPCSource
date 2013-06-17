{$mode objfpc}
uses sysutils;

var
  counter: integer;

{ flow control statements in try..except must not bypass finally code of outer try..finally }
{ test 1: 'exit' in 'try' block }
procedure test;
 begin
   try
     try
       inc(counter);
       exit;
     except
     end;
   finally
     inc(counter);
   end;
 end;

{ test 2: 'exit' in 'except' block }
procedure test2;
begin
  try
    try
      raise exception.create('catch me');
    except
      inc(counter);
      exit;
    end;
  finally
    inc(counter);
  end;
end;

{ test 3: 'exit' in 'on' statement }
procedure test3;
begin
  try
    try
      raise exception.create('catch me');
    except
      on E: Exception do
      begin
        inc(counter);
        exit;
      end;
    end;
  finally
    inc(counter);
  end;
end;


begin
  counter:=0;
  test;
  if counter<>2 then
    Halt(1);

  counter:=0;
  test2;
  if counter<>2 then
    Halt(2);

  counter:=0;
  test3;
  if counter<>2 then
    Halt(3);
end.
