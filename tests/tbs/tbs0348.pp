{$mode delphi}

type fluparr=array[0..1000] of longint;
     flupptr=^fluparr;

var flup : Flupptr;
    Flupresult : longint;

begin
 flupresult:=flup[5];
end.
