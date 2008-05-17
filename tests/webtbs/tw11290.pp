program optimiav;
//compile with -OG2p3

{$ifdef FPC}{$mode objfpc}{$h+}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;
type
 stringarty = array of string;
 ttestclass = class
  private
   fignoreexceptionclasses: stringarty;
  public
   procedure setignoreexceptionclasses(const avalue: stringarty);
 end;

procedure ttestclass.setignoreexceptionclasses(const avalue: stringarty);
var
 int1: integer;
begin
 setlength(fignoreexceptionclasses,length(avalue));
 for int1:= 0 to high(avalue) do begin
  fignoreexceptionclasses[int1]:= uppercase(avalue[int1]);
 end;
end;

var
 testclass1: ttestclass;
 ar1: stringarty;
begin
 testclass1:= ttestclass.create;
 setlength(ar1,2);
 testclass1.setignoreexceptionclasses(ar1);
 testclass1.free;
end.

