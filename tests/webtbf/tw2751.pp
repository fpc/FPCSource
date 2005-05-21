{ %fail }

{ Source provided for Free Pascal Bug Report 2751 }
{ Submitted by "Micha" on  2003-10-26 }
{ e-mail:  }
program Construct;

{$mode delphi}

type

  ta = class
  public
    constructor Create(a: tclass);
  end;

  tb = class(ta)
  public
    constructor Create(a: tobject);
  end;

constructor ta.create(a: tclass);
begin
end;

constructor tb.create(a: tobject);
begin
end;

var
  b: tb;
  c: tclass;

begin
  c := tb;
  // This should fail
  b := tb.create(c);
  b.free;
end.
