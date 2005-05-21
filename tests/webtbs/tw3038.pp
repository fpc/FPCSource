{ Source provided for Free Pascal Bug Report 3038 }
{ Submitted by "Marco (Gory Bugs Department)" on  2004-04-03 }
{ e-mail:  }
{$mode delphi}

type dasso     = class
                 procedure bla; virtual; abstract;
                end;
    dmyasso    = class(dasso)
                  procedure bla; override;
                end;
    dnextasso = class(dmyasso)
                   procedure bla; override;
                end;
    ClassFamily= class of dasso;

procedure dmyasso.bla;
begin
end;

procedure dnextasso.bla;
begin
end;

const cmyclass : array[0..1] of classfamily =(dmyasso,dnextasso);

var vmyclass : array[0..1] of classfamily =(dmyasso,dnextasso);

begin
end.
