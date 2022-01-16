{ Parsed from QuartzCore.framework CARemoteLayerServer.h }


{$ifdef TYPES}
type
  CARemoteLayerServerPtr = ^CARemoteLayerServer;
{$endif}

{$ifdef CLASSES}

type
  CARemoteLayerServer = objcclass external (NSObject)
  private
    _impl: id;
  public
    class function sharedServer: CARemoteLayerServer; message 'sharedServer';
    function serverPort: mach_port_t; message 'serverPort';
  end;


type
  CALayer_CARemoteLayerServer = objccategory external name 'CARemoteLayerServer' (CALayer)
    class function layerWithRemoteClientId (client_id: cuint32): CALayer; message 'layerWithRemoteClientId:';
  end;
{$endif}

