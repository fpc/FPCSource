
{
  this file checks that calling done from init is
  done correctly
}


program test_init_done;


type
   pobject = ^tobject;
   tobject = object
     val : longint;
     constructor init (call_done : boolean);
     destructor done; virtual;
     procedure check;
   end;


constructor tobject.init (call_done : boolean);
  begin
    val:=7;
    if call_done then
      begin
        done;
        fail;
      end;
  end;

destructor tobject.done;
  begin
    check;
  end;

procedure tobject.check;
  begin
    if val<>7 then
      begin
        writeln('Error in codegeneration');
        halt(1);
      end;
  end;

var
  obj1 : tobject;
  obj2 : pobject;

begin
  obj1.init(false);
  obj1.done;

  new(obj2,init(true));
  if assigned(obj2) then
      begin
        writeln('Error in codegeneration of fail');
        halt(1);
      end;
end.
