{ Source provided for Free Pascal Bug Report 4089 }
{ Submitted by "Martin Schreiber" on  2005-06-14 }
{ e-mail:  }
program project1;
{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
{$apptype console}
{$endif}

uses
  Classes, SysUtils, typinfo;

type

 itest1 = interface
  procedure test1;
 end;
 
 itest2 = interface(itest1)['{1A50A4E4-5B46-4C7C-A992-51EFEA1202B8}']
  procedure test2;
 end;

var
 po1: ptypeinfo;
 po2: ptypedata;
 
begin
 po1:= typeinfo(itest2);
 writeln('Kind: ',getenumname(typeinfo(ttypekind),ord(po1^.kind)));
 writeln('Name: "',po1^.name,'"');
 po2:= gettypedata(po1);
 writeln('IntfParent: ',integer(po2^.intfparent));
 writeln('Guid: ',po2^.guid.d1);
 writeln('IntfUnit: "',po2^.IntfUnit,'"');
end.