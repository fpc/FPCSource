{ Source provided for Free Pascal Bug Report 3578 }
{ Submitted by "Martin Schreiber" on  2005-01-21 }
{ e-mail:  }
program project1;

{$mode objfpc}{$H+}
uses
  Classes;

type
 widestringarty = array of widestring;

procedure proc(const ar1,ar2: array of widestring);
begin
end;

function getarray: widestringarty;
begin
 result:= nil;
end;

var
 ar1,ar2: widestringarty;
begin
 proc(ar1,ar2);            //ok
 proc(ar1,getarray);       //ok
 proc(getarray,ar2);       //ok
 proc(getarray,getarray);  //Internal error 9999
end.
