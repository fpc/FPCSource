{$mode objfpc}
{$h+}
program testl;

uses
     fgl,
     classes,
     sysutils;

Const
  Max = 20000;


procedure do_normal;
Var
  L : TFPList;
  I : Ptrint;
  J,K : Integer;
  T1,T2 : TDateTime;
begin
  Writeln('Using old classes pointer list');
  {$i blists1.inc}
end;

{ overwrite with generic one }
Type
   TFPList = specialize TFPGList<Pointer>;
procedure do_generic;
Var
  L : TFPList;
  I : Ptrint;
  J,K : Integer;
  T1,T2 : TDateTime;
begin
  Writeln('Using generics based pointer list');
  {$i blists1.inc}
end;


begin
  do_generic;
  do_normal;
end.
