{ Source provided for Free Pascal Bug Report 3179 }
{ Submitted by "Martin Schreiber" on  2004-06-21 }
{ e-mail:  }
unit uw3179b;

{$mode objfpc}{$H+}

interface
uses
 uw3179a;

type
 tclass2 = class(tclass1)
  public
   procedure proc2;
 end;

implementation

procedure tclass2.proc2;
begin
 proc1;
end;

end.
