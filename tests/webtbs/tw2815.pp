{ %opt=-Sew }

{ Source provided for Free Pascal Bug Report 2815 }
{ Submitted by "Mattias Gaertner" on  2003-11-30 }
{ e-mail: mattias@freepascal.org }
program VirtualCreate;

{$mode objfpc}{$H+}

type
  TAbstractClass = class
  public
    constructor Create;
    constructor VirtualCreate; virtual; abstract;
  end;

constructor TAbstractClass.Create;
begin
  VirtualCreate;
end;

begin
end.
