{%NORUN}
{$MODE MACPAS}

{Tests of mac pascal constructs, concerning compile time constructs}

{** Test Compiler variables **}

{$SETC ADAM := TRUE}
{$IFC ADAM <> TRUE}
{$ERRORC Test failed}
{$ENDC}

{$SETC BERTIL := FALSE}
{$IFC BERTIL = FALSE}
{  OK  }
{$ELSEC}
{$ERRORC Test failed}
{$ENDC}

{** Test Defined/Undefined **}

{$IFC UNDEFINED FPC_MACPAS}
{$ERRORC Test failed}
{$ENDC}

{$IFC NOT DEFINED FPC_MACPAS}
{$ERRORC Test failed}
{$ENDC}

{** Test Push/Pop **}

{$J-}

{$PUSH}
{$PUSH}

{$J+}

{$POP}
{$POP}

{$IFC OPTION(J)}
{$ERRORC $PUSH/$POP doesnt work properly}
{$ENDC}

program tmacpas4;

begin
end.
