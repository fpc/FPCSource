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

{ test 4: 'continue' in try..except nested in loop nested in try..finally
           in this case the control stays within protected region
           (Mantis #28584)  }
procedure test4;
var
  i: integer;
begin
  try
    for i:=0 to 2 do
    begin
      try
        inc(counter);
        raise exception.create('catch me');
      except
        continue;
      end;
    end;  
  finally
    inc(counter);
  end;
end;

{ test 5: same as above but with 'break' statement instead }
procedure test5;
var
  i: integer;
begin
  try
    for i:=0 to 15 do
    begin
      try
        inc(counter);
        raise exception.create('catch me');
      except
        if i=2 then
          break;
      end;
    end; 
    inc(counter);
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
    
  counter:=0;
  test4;
  if counter<>4 then
    Halt(4);
    
  counter:=0;
  test5;
  if counter<>5 then
    Halt(5);
end.
