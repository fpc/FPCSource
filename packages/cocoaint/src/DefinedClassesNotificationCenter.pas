{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesNotificationCenter;
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
