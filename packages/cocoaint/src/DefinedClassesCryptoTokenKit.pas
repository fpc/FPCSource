{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesCryptoTokenKit;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  TKSmartCard = objcclass external;
  TKSmartCardATR = objcclass external;
  TKSmartCardATRInterfaceGroup = objcclass external;
  TKSmartCardSlot = objcclass external;
  TKSmartCardSlotManager = objcclass external;

implementation
end.
