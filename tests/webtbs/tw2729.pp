{ Source provided for Free Pascal Bug Report 2729 }
{ Submitted by "marco (the gory bugs department)" on  2003-10-09 }
{ e-mail:  }
{$mode delphi}

type
  tbla= class(tobject)
    l : longint;
    class function bla:tbla;
    function get : longint;virtual;
    procedure doset;
 end;

procedure tbla.doset;
  begin
     l:=$12345678;
  end;

function tbla.get : longint;
  begin
    result:=l;
  end;

class function tbla.bla:tbla;

  begin
    result:=Create;
  end;

var
  bla : tbla;

begin
  bla:=tbla.bla;
  bla.doset;
  if bla.get<>$12345678 then
    begin
      writeln('Problem');
      halt(1);
    end;
  bla.free;
end.
