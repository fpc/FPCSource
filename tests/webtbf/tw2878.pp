{ %fail }
{ Source provided for Free Pascal Bug Report 2878 }
{ Submitted by "Mattias Gaertner" on  2004-01-04 }
{ e-mail: mattias@freepascal.org }
program NoClassCheck;

{$mode objfpc}{$H+}

uses
  Classes;

var
  APersistent: TPersistent;
  AnObject: TObject;
begin
  AnObject:=nil;
  APersistent:=AnObject; // Should need a typecast
end.
