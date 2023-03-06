{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesCoreAudioKit;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  AUGenericView = objcclass external;
  AUPannerView = objcclass external;
  AUCustomViewPersistentDataProtocol = objcprotocol external name 'AUCustomViewPersistentData';

type
  NSDictionary = objcclass external;

implementation
end.
