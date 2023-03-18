{ %wpoparas=devirtcalls }
{ %wpopasses=1 }

{$mode objfpc}

type
  tderived = class;
 
  tbase = class
    procedure test; virtual;
  end;
  tbaseclass = class of tbase;

  tbasetop = class(tbase)
    function alloc(c: tbaseclass): tbase;
    function getderived: tderived;
  end;

  tderived = class(tbase)
    procedure test; override;
  end;

procedure tbase.test;
  begin
    writeln('error');
    halt(1);
  end;

function tbasetop.alloc(c: tbaseclass): tbase;
  begin
    result:=tbase(c.newinstance);
  end;

function tbasetop.getderived: tderived;
  begin
    result:=tderived(alloc(tderived));
    result.create;
  end;

procedure tderived.test;
  begin
    writeln('ok');
  end;

var
  t: tbasetop;
  b: tbase;
begin
  t:=tbasetop.create;
  b:=tbase(t.getderived);
  b.test;
  b.free;
  t.free;
end.
