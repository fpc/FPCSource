{$mode delphi}

type
(* {$IFDEF FPK}
  y = class; { shouldn't be necessary }
{$ENDIF} *)
  x = class of y;
  y = class
   z:Boolean;
  end;

begin
end.
