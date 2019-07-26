{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}

unit DefinedClassesQuickLookUI;
interface

type
  QLPreviewPanel = objcclass external;
  QLPreviewView = objcclass external;
  QLPreviewItemProtocol = objcprotocol external name 'QLPreviewItem';
  QLPreviewPanelDataSourceProtocol = objcprotocol external name 'QLPreviewPanelDataSource';
  QLPreviewPanelDelegateProtocol = objcprotocol external name 'QLPreviewPanelDelegate';

type
  QLPreviewPanelReserved = objcclass external;
  QLPreviewViewReserved = objcclass external;

implementation
end.
