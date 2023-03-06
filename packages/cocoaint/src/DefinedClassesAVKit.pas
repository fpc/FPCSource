{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesAVKit;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  AVCaptureView = objcclass external;
  AVPlayerView = objcclass external;
  AVCaptureViewDelegateProtocol = objcprotocol external name 'AVCaptureViewDelegate';

type
  AVCaptureFileOutput = objcclass external;
  AVCaptureSession = objcclass external;
  AVPlayer = objcclass external;

implementation
end.
