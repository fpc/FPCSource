{ Old file: tbs0348.pp }
{  }

{$mode delphi}

type fluparr=array[0..1000] of longint;
     flupptr=^fluparr;

var flup : Flupptr;
    Flupresult : longint;
    flupa : fluparr;
begin
  flup:=@flupa;
  flupresult:=flup[5];
end.
