{ Source provided for Free Pascal Bug Report 3292 }
{ Submitted by "Vincent Snijdes" on  2004-09-03 }
{ e-mail: vslist@zonnet.nl }
program bug3292;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes, uw3292a;

type
  TDerived = class(TMiddle)
  private
    procedure a(var m); message 1;
    procedure b; override;
  end;

{ TDerived }

procedure TDerived.a(var m);
begin
  writeln('A; In TDerived');
  inc(acnt);
  inherited a(m);
end;

procedure TDerived.b;
begin
  writeln('B: In TDerived');
  inc(bcnt);
  inherited b;
end;

var
  o: TDerived;
  m: longint;

begin
  o := TDerived.Create;
  acnt:=0;
  bcnt:=0;
  o.a(m);
  o.b;
  if acnt<>2 then
    halt(1);
  if bcnt<>2 then
    halt(1);
end.
