{ Source provided for Free Pascal Bug Report 3827 }
{ Submitted by "Thomas Schatzl" on  2005-03-25 }
{ e-mail:  }
{$MODE DELPHI}

uses
  sysutils;
  
var
  guid : tguid;  

begin
  CreateGUID(guid);
end.
