program tmacpas1;

{Tests of mac pascal constructs, concerning two units}

{$MODE MACPAS}

uses
  umacpas1;

{** Test exportable macros **}

{$IFC UNDEFINED UMACPAS_COMP_VAR }
{$NOTE In using unit: UMACPAS_COMP_VAR is undefined}
{$ERRORC UMACPAS_COMP_VAR should be defined}
{$ELSEC}
{$IFC UMACPAS_COMP_VAR }
{$NOTE In using unit: UMACPAS_COMP_VAR is true}
{$ELSEC}
{$NOTE In using unit: UMACPAS_COMP_VAR is false}
{$ERRORC UMACPAS_COMP_VAR should be true}
{$ENDC}
{$ENDC}

{$IFC UNDEFINED UMACPAS_PRE_IMPL_COMP_VAR }
{$ERRORC UMACPAS_PRE_IMPL_COMP_VAR is not defined}
{$ENDC}

{$IFC UNDEFINED UMACPAS_PRE_IMPL_COMP_VAR }
{$ERRORC UMACPAS_PRE_IMPL_COMP_VAR is defined, while it shoud not}
{$ENDC}


{** Test J directive for var and external for proc **}

{$J+}
var
  nisse: Integer; {Is available in Umacpas}
{$J-}

function Get84: Integer;
external;

begin
  Writeln(nisse);
  Writeln(Get84);
  if nisse <> 42 then
    halt(1);
  if Get84 <> 84 then
    halt(1);
end.
