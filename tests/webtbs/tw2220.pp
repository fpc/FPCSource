{ Source provided for Free Pascal Bug Report 2220 }
{ Submitted by "marco" on  2002-11-06 }
{ e-mail: marcov@freepascal.org }
{$H+}
{$ifdef fpc}{$MODE DELPHI}{$endif}
// NO OUTPUT, GDB SHOWS SIGSEGV IN DECR_REF

type bla=class
        freceivebuffer : array[0..4095] of char;
        flastresponse  : String;
        procedure themethod;
        end;

procedure bla.themethod;
        var i : longint;
        begin
         i:=12;
         flastresponse:=copy(freceivebuffer,1,I-1);
         writeln('point 1: ',flastresponse);
        end;

var x : bla;

begin
  x:=bla.create;
  x.freceivebuffer:='test string is wrong!';
  x.themethod;
  writeln('point 2');
end.
