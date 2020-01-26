{ Parsed from AudioVideoBridging.framework AVBEthernetInterface.h }


{$ifdef TYPES}
type
  AVBEthernetInterfacePtr = ^AVBEthernetInterface;
{$endif}

{$ifdef CLASSES}

type
  AVBEthernetInterface = objcclass external (AVBInterface)
  private
    _notification: io_object_t;
  end;
{$endif}

