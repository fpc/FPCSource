program Project1;
{$Mode objfpc}

type
  TFoo = class
    constructor create; virtual;
  end;

  TBar = type TFoo;

  TBaz = class(TBar)
    constructor create; override;
  end;

constructor TFoo.create;
begin end;

constructor TBaz.create;
begin end;

begin
  if not tbar.inheritsfrom(tfoo) then
    halt(1);
  if not tbaz.inheritsfrom(tbar) then
    halt(2);
  if tbar.classname<>'TBar' then
    halt(3);
  if tfoo.classname<>'TFoo' then
    halt(4);
end.
