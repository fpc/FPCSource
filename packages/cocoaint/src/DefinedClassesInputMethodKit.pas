{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesInputMethodKit;
interface

type
  IMKCandidates = objcclass external;
  IMKInputController = objcclass external;
  IMKServer = objcclass external;
  IMKMouseHandlingProtocol = objcprotocol external name 'IMKMouseHandling';
  IMKStateSettingProtocol = objcprotocol external name 'IMKStateSetting';

type
  IMKCandidatesPrivate = objcclass external;
  IMKInputControllerPrivate = objcclass external;
  IMKServerPrivate = objcclass external;
  IMKServerProxyProtocol = objcprotocol external name 'IMKServerProxy';

implementation
end.
