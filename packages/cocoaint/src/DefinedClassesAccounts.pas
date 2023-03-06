{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesAccounts;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  ACAccount = objcclass external;
  ACAccountCredential = objcclass external;
  ACAccountStore = objcclass external;
  ACAccountType = objcclass external;

implementation
end.
