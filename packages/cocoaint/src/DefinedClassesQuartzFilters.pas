{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesQuartzFilters;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  QuartzFilter = objcclass external;
  QuartzFilterManager = objcclass external;
  QuartzFilterView = objcclass external;

implementation
end.
