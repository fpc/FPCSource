program tprop4;

{$mode delphi}

type
  tprop4c = class
   protected
    function Gettest2: longint; virtual; abstract;
   public
    property test: longint read Gettest2;
  end;

  tprop4c2 = class(tprop4c)
    protected
     function Gettest2: longint; override;
  end;

  function tprop4c2.Gettest2: longint;
    begin
      result:=1;
    end;

var
  c: tprop4c;
begin
  c:=tprop4c2.create;
  if c.test<>1 then
    halt(1);
end.
