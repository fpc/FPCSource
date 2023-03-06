{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesQuartzComposer;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  QCComposition = objcclass external;
  QCCompositionLayer = objcclass external;
  QCCompositionParameterView = objcclass external;
  QCCompositionPickerPanel = objcclass external;
  QCCompositionPickerView = objcclass external;
  QCCompositionRepository = objcclass external;
  QCPatchController = objcclass external;
  QCPlugIn = objcclass external;
  QCPlugInViewController = objcclass external;
  QCRenderer = objcclass external;
  QCView = objcclass external;
  QCCompositionRendererProtocol = objcprotocol external name 'QCCompositionRenderer';
  QCPlugInContextProtocol = objcprotocol external name 'QCPlugInContext';
  QCPlugInInputImageSourceProtocol = objcprotocol external name 'QCPlugInInputImageSource';
  QCPlugInOutputImageProviderProtocol = objcprotocol external name 'QCPlugInOutputImageProvider';

type
  NSImage = objcclass external;
  NSOpenGLContext = objcclass external;
  NSOpenGLPixelFormat = objcclass external;

implementation
end.
