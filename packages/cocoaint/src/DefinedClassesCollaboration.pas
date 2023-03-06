{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesCollaboration;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  CBGroupIdentity = objcclass external;
  CBIdentity = objcclass external;
  CBIdentityAuthority = objcclass external;
  CBIdentityPicker = objcclass external;
  CBUserIdentity = objcclass external;

type
  NSArray = objcclass external;
  NSData = objcclass external;
  NSImage = objcclass external;
  NSString = objcclass external;
  NSWindow = objcclass external;

implementation
end.
