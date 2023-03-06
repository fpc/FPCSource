{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesNotificationCenter;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  NCWidgetController = objcclass external;
  NCWidgetListViewController = objcclass external;
  NCWidgetSearchViewController = objcclass external;
  NCWidgetListViewDelegateProtocol = objcprotocol external name 'NCWidgetListViewDelegate';
  NCWidgetProvidingProtocol = objcprotocol external name 'NCWidgetProviding';
  NCWidgetSearchViewDelegateProtocol = objcprotocol external name 'NCWidgetSearchViewDelegate';

implementation
end.
