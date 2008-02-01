{%norun}

{$IFDEF FPC}
 {$MODE Delphi}
{$ENDIF}

var
  AStr : string;
  AComp : Comp;
  AInt : Integer;

begin
  AStr := '1.2345';
  Val(AStr, AComp, AInt);
end.
