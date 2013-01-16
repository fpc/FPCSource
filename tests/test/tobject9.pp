{ %OPT=-gh }
// Validate that objects with parent are finalized when statically allocated
type
  pobj = ^tobj;
  tobj = object
  public
    foo: ansistring;
    constructor init(const s: ansistring);
    destructor done;
  end;

  pobj1 = ^tobj1;
  tobj1 = object(tobj)
    constructor init;
    destructor done;
  end;

constructor tobj.init(const s: ansistring);
begin
  foo:=s;
end;

destructor tobj.done;
begin
end;

constructor tobj1.init;
var
  s: ansistring;
begin
  s:='abc';
  uniquestring(s);
  inherited init(s);
end;

destructor tobj1.done;
begin
  inherited done;
end;

var
  obj: tobj1;
  
procedure local;
var
  instance: tobj1;
begin
  instance.init;
end;

begin
  HaltOnNotReleased:=true;
  local;
  obj.init;
end.
