{ %OPT=-gs }

unit tb0635;

{$mode objfpc}

INTERFACE
uses ub0635;

type
  TAnalyser = Class (TNode)
    end;

  TStat  = Class (TAnalyser) //eine Eingangsgruppe, kein Ausgang
    end;

//**************************************************************************************
IMPLEMENTATION


begin
end.

