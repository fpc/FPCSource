{$IFNDEF FPC_DOTTEDUNITS}
unit cpbuildu;
{$ENDIF FPC_DOTTEDUNITS}
// buildunit for cp*
interface
{$IFDEF FPC_DOTTEDUNITS}
uses System.Unicode.Cp895,System.Unicode.Cp932,System.Unicode.Cp936,System.Unicode.Cp949,System.Unicode.Cp950;
{$ELSE FPC_DOTTEDUNITS}
uses cp895,cp932,cp936,cp949,cp950;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
