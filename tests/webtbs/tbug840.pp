{$mode TP}

program tbug840;

uses tbug840a;

begin
tbug840b.i:=1;
end.

----------------------------- cut here ----------------------------------------
unit ua;

interface
uses ub;
implementation
end.
----------------------------- cut here ----------------------------------------
unit ub;

interface
var i:longint;
implementation
end.