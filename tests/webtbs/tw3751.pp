{ Source provided for Free Pascal Bug Report 3751 }
{ Submitted by "Nicola Lugato" on  2005-03-05 }
{ e-mail: msx_80@hotmail.com }
{$mode delphi}
program turn;

uses
{$ifdef unix}
  cthreads,
{$endif}
  classes, sysutils;

type

Tmy = class(TThread)
constructor Create(suspended:boolean);
Procedure Execute;override;
end;

constructor TMy.Create(suspended:boolean);
begin
FreeOnTerminate:=true;
inherited Create(suspended);
end;

Procedure TMy.Execute;
begin
writeln('Just print and exit!');
end;

var m:TMy;
begin
m:=Tmy.create(true);
m.resume;
sleep(1000);
end.
