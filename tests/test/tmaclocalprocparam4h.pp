{$mode macpas}
type    
myObject = object
    procedure procA (x: integer);
    procedure procC (procedure procD (var y: myObject));
  end;

  procedure  myObject. procC (procedure procD (var y: myObject));
    var
      x: myobject;
    begin
      procD (x);
      {more code here ...}
    end;
  
var
  ok: boolean;

procedure myObject.ProcA (x: integer);

    procedure ProcB (var y: myObject);
      begin
        ok:=true;
      end;

  begin
    procC(ProcB);
  end;

var
  o: myobject;
begin
  ok:=false;
  new(o);
  o.proca(1);
  dispose(o);
  if not ok then
    halt(1);
end.
