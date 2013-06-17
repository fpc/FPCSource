program tprop3;

{$mode delphi}

type
  tprop3c = class
   protected
    function Gettest: longint; virtual; abstract;
   public
    property test: longint read Gettest;
  end;

  tprop3c2 = class(tprop3c)
   protected
    function Gettest: longint; override;
  end;

  tprop3c3 = class(tprop3c2)
   protected
    function Gettest: longint; override;
  end;


function tprop3c2.Gettest: longint;
begin
  result:=1;
end;


function tprop3c3.Gettest: longint;
begin
  result:=2;
end;

begin
end.
