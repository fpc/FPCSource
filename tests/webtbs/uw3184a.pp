{ Source provided for Free Pascal Bug Report 3184 }
{ Submitted by "Martin Schreiber" on  2004-06-25 }
{ e-mail:  }
unit uw3184a;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface


type
 tclass0 = class
  private
   function proc:longint; virtual;
 end;

 tclass1 = class(tclass0)
  protected   //same behaviour if private
   function proc:longint override;
  public
   function proc1:longint;
 end;

implementation

function tclass0.proc:longint;
begin
 writeln('tclass0.proc');
 result:=0;
end;

function tclass1.proc:longint;
begin
 writeln('tclass1.proc');
 result:=10;
end;

function tclass1.proc1:longint;
begin
 result:=proc;
end;

end.
