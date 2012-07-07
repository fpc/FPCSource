program tenum2;

{$mode delphi}

type
  tenum2enum = (e_zero, e_one, e_two);

  tenum2base = class abstract
    constructor create;
    procedure init; virtual; abstract;
  end;

  tenum2child = class(tenum2base)
    fenum: tenum2enum;
    procedure init; override;
  end;

constructor tenum2base.create;
  begin
    init;
  end;

procedure tenum2child.init;
  begin
    fenum:=e_one;
  end;

var
  c: tenum2child;
begin
  c:=tenum2child.create;
  if c.fenum<>e_one then
    halt(1);
end.
