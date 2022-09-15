{$mode objfpc}
{$modeswitch functionreferences}

unit tw39903;
interface

type
 TCallback = record
   proc: reference to procedure;
 end;

 TObj = class
   proc: reference to procedure;
 end;

implementation
end.

