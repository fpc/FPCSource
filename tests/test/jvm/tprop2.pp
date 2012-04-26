program tprop2;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
 tpropclass1 = class
 strict private
   fx : integer;
 public
   procedure Reset; virtual;

 end;

 tpropclass2 = class(tpropclass1)
 strict private
   fx : integer;
 public
   procedure Reset; override;
   property x : integer read fx write fx;
 end;

procedure tpropclass1.Reset;
begin
 fx := 777;
end;

procedure tpropclass2.Reset;
begin
 fx := 888;
end;

var
 t : tpropclass2;
begin
  t := tpropclass2.create;
  t.reset;
  if t.x<>888 then
    raise jlexception.create('error 1');
  t.x:=555;
  if t.x<>555 then
    raise jlexception.create('error 1');
end.
  
