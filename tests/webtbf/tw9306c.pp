{ %fail }

type
  tobj = object
    function f: integer; virtual;
  end;

  tobj2 = object(tobj)
    function f: string; virtual;
  end;

  function tobj.f: integer;
    begin
    end;

  function tobj2.f:string;
    begin
    end;

begin
end.
