{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesAddressBook;
interface

type
  ABAddressBook = objcclass external;
  ABGroup = objcclass external;
  ABMultiValue = objcclass external;
  ABMutableMultiValue = objcclass external;
  ABPerson = objcclass external;
  ABRecord = objcclass external;
  ABSearchElement = objcclass external;
  ABImageClientProtocol = objcprotocol external name 'ABImageClient';

implementation
end.
