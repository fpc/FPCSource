unit umacpas1;

{$DEFINE UMACPAS_PRE_INTERFACE_VAR}
{The above will _not_ be exported, unless it is redefined
 after the mode switch (which it is). Note the non-macpas
 style, because $mode macpas has not yet been parsed.}

{$MODE MACPAS}

interface
{$SETC UMACPAS_COMP_VAR = TRUE}
{The above macro is deliberatelly immediatelly after INTERFACE
 to check that this works.}

{$SETC UMACPAS_PRE_INTERFACE_VAR = TRUE}
{Is redefined here and thus exported..}

{$IFC UMACPAS_COMP_VAR }
{$NOTE UMACPAS_COMP_VAR is true}
{$ELSEC}
{$NOTE UMACPAS_COMP_VAR is false}
{$ENDC}

var
  a: Integer;

{The below macro should be exported}
{$SETC UMACPAS_PRE_IMPL_COMP_VAR = TRUE}
implementation
{$SETC UMACPAS_COMP_VAR = FALSE}
{The above macro is deliberatelly immediatelly after IMPLEMENTATION
 to check that this works. Note that here UMACPAS_COMP_VAR is set to false,
 but the exported value of UMACPAS_COMP_VAR should nevertheless be TRUE. }

{$Z+}
var
  nisse: Integer;
    {To be available in tmacpas via the mode macpas $J directive}

function Get84: Integer;
    {To be available in tmacpas via EXTERNAL}

begin
  Get84:= 84;
end;

{$Z-}

{$IFC UMACPAS_PRE_INTERFACE_VAR }
{$NOTE UMACPAS_PRE_INTERFACE_VAR is true}
{$ELSEC}
{$ERRORC UMACPAS_PRE_INTERFACE_VAR is false}
{Erroneous since it was set to true in the interface section.}
{$ENDC}


begin
  nisse:= 42;
end.
