{$mode objfpc}{$h+}
// Differs from tctr1.pp in the following directive:
{$implicitexceptions off}

type
  tobj=class(TObject)
    ffield:boolean;
    constructor Create;
    procedure AfterConstruction;override;
  end;

{ Exit statement in constructor must not jump over AfterConstruction! }
constructor tobj.Create;
begin
  exit;
end;
 
procedure tobj.AfterConstruction;
begin
  ffield:=true;
end;
 
 
var
  o: tobj;
begin
  o:=tobj.create;
  if not o.ffield then
    Halt(1);
end.
