unit test1;

{$mode objfpc}

{ dummy unit for test of dbx stabs info PM }

interface

function TestOne : longint;
function TestOne(val : longint) : longint; overload;
function TestOne(val : string) : longint; overload;


implementation

function TestOne : longint;
begin
  result:=1;
end;

function TestOne(val : longint) : longint; overload;
begin
  result:=val;
end;

function TestOne(val : string) : longint; overload;
var
  value, error : longint;
begin
  system.val(val,value,error);
  if error=0 then
    result:=value
  else
    result:=-1;
end;
end.
