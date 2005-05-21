{ %fail }
{ %opt=-Sew }

{ Source provided for Free Pascal Bug Report 2752 }
{ Submitted by "Micha" on  2003-10-26 }
{ e-mail:  }
program Construct;

{$mode delphi}

type

  tb = class
  public
    constructor Create(a: tobject);
  end;

constructor tb.create(a: tobject);
begin
end;

var
  b,c: tb;

begin
  b := tb.create(b);
end.
