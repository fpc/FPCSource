{ Source provided for Free Pascal Bug Report 3179 }
{ Submitted by "Martin Schreiber" on  2004-06-21 }
{ e-mail:  }
unit uw3179a;

{$mode objfpc}{$H+}

interface

type
 tclass0 = class
  private
   procedure proc; virtual; abstract;
 end;

 tclass1 = class(tclass0)
  private
   procedure proc; override;
  public
   procedure proc1;
 end;

implementation

procedure tclass1.proc;
begin
end;

procedure tclass1.proc1;
begin
 proc;
end;

end.
