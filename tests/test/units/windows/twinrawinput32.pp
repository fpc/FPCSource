{ %TARGET=win32 }

uses windows;

var
  errors_found: Boolean = false;

procedure DoCheckSize(const StructName: string; ActualSize, ExpectedSize: SizeUInt);
begin
  if ActualSize <> ExpectedSize then
  begin
    Writeln('SizeOf(', StructName, ') is wrong - got ', ActualSize, ', expected ', ExpectedSize);
    errors_found := true;
  end;
end;

procedure DoCheckOffset(const StructAndFieldName: string; ActualOffset, ExpectedOffset: SizeUInt);
begin
  if ActualOffset <> ExpectedOffset then
  begin
    Writeln('Offset of ', StructAndFieldName, ' is wrong - got ', ActualOffset, ', expected ', ExpectedOffset);
    errors_found := true;
  end;
end;




begin
  DoCheckSize('RAWINPUTHEADER', SizeOf( RAWINPUTHEADER ),   16 ) ;
    DoCheckOffset('RAWINPUTHEADER'+'.'+'dwType', SizeUInt(@(PRAWINPUTHEADER (nil)^.  dwType )),   0 ) ;
    DoCheckOffset('RAWINPUTHEADER'+'.'+'dwSize', SizeUInt(@(PRAWINPUTHEADER (nil)^.  dwSize )),   4 ) ;
    DoCheckOffset('RAWINPUTHEADER'+'.'+'hDevice', SizeUInt(@(PRAWINPUTHEADER (nil)^.  hDevice )),   8 ) ;
    DoCheckOffset('RAWINPUTHEADER'+'.'+'wParam', SizeUInt(@(PRAWINPUTHEADER (nil)^.  wParam )),   12 ) ;
  DoCheckSize('RAWMOUSE', SizeOf( RAWMOUSE ),   24 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'usFlags', SizeUInt(@(PRAWMOUSE (nil)^.  usFlags )),   0 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'ulButtons', SizeUInt(@(PRAWMOUSE (nil)^.  ulButtons )),   4 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'ulRawButtons', SizeUInt(@(PRAWMOUSE (nil)^.  ulRawButtons )),   8 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'lLastX', SizeUInt(@(PRAWMOUSE (nil)^.  lLastX )),   12 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'lLastY', SizeUInt(@(PRAWMOUSE (nil)^.  lLastY )),   16 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'ulExtraInformation', SizeUInt(@(PRAWMOUSE (nil)^.  ulExtraInformation )),   20 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'usButtonFlags', SizeUInt(@(PRAWMOUSE (nil)^.  usButtonFlags )),   4 ) ;
    DoCheckOffset('RAWMOUSE'+'.'+'usButtonData', SizeUInt(@(PRAWMOUSE (nil)^.  usButtonData )),   6 ) ;
  DoCheckSize('RAWKEYBOARD', SizeOf( RAWKEYBOARD ),   16 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'MakeCode', SizeUInt(@(PRAWKEYBOARD (nil)^.  MakeCode )),   0 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'Flags', SizeUInt(@(PRAWKEYBOARD (nil)^.  Flags )),   2 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'Reserved', SizeUInt(@(PRAWKEYBOARD (nil)^.  Reserved )),   4 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'VKey', SizeUInt(@(PRAWKEYBOARD (nil)^.  VKey )),   6 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'Message', SizeUInt(@(PRAWKEYBOARD (nil)^.  Message )),   8 ) ;
    DoCheckOffset('RAWKEYBOARD'+'.'+'ExtraInformation', SizeUInt(@(PRAWKEYBOARD (nil)^.  ExtraInformation )),   12 ) ;
  DoCheckSize('RAWHID', SizeOf( RAWHID ),   12 ) ;
    DoCheckOffset('RAWHID'+'.'+'dwSizeHid', SizeUInt(@(PRAWHID (nil)^.  dwSizeHid )),   0 ) ;
    DoCheckOffset('RAWHID'+'.'+'dwCount', SizeUInt(@(PRAWHID (nil)^.  dwCount )),   4 ) ;
    DoCheckOffset('RAWHID'+'.'+'bRawData', SizeUInt(@(PRAWHID (nil)^.  bRawData )),   8 ) ;
  DoCheckSize('RAWINPUT', SizeOf( RAWINPUT ),   40 ) ;
    DoCheckOffset('RAWINPUT'+'.'+'header', SizeUInt(@(PRAWINPUT (nil)^.  header )),   0 ) ;
    DoCheckOffset('RAWINPUT'+'.'+'data', SizeUInt(@(PRAWINPUT (nil)^.  data )),   16 ) ;
    DoCheckOffset('RAWINPUT'+'.'+'data.mouse', SizeUInt(@(PRAWINPUT (nil)^.  data.mouse )),   16 ) ;
    DoCheckOffset('RAWINPUT'+'.'+'data.keyboard', SizeUInt(@(PRAWINPUT (nil)^.  data.keyboard )),   16 ) ;
    DoCheckOffset('RAWINPUT'+'.'+'data.hid', SizeUInt(@(PRAWINPUT (nil)^.  data.hid )),   16 ) ;
  DoCheckSize('RID_DEVICE_INFO_MOUSE', SizeOf( RID_DEVICE_INFO_MOUSE ),   16 ) ;
    DoCheckOffset('RID_DEVICE_INFO_MOUSE'+'.'+'dwId', SizeUInt(@(PRID_DEVICE_INFO_MOUSE (nil)^.  dwId )),   0 ) ;
    DoCheckOffset('RID_DEVICE_INFO_MOUSE'+'.'+'dwNumberOfButtons', SizeUInt(@(PRID_DEVICE_INFO_MOUSE (nil)^.  dwNumberOfButtons )),   4 ) ;
    DoCheckOffset('RID_DEVICE_INFO_MOUSE'+'.'+'dwSampleRate', SizeUInt(@(PRID_DEVICE_INFO_MOUSE (nil)^.  dwSampleRate )),   8 ) ;
    DoCheckOffset('RID_DEVICE_INFO_MOUSE'+'.'+'fHasHorizontalWheel', SizeUInt(@(PRID_DEVICE_INFO_MOUSE (nil)^.  fHasHorizontalWheel )),   12 ) ;
  DoCheckSize('RID_DEVICE_INFO_KEYBOARD', SizeOf( RID_DEVICE_INFO_KEYBOARD ),   24 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwType', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwType )),   0 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwSubType', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwSubType )),   4 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwKeyboardMode', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwKeyboardMode )),   8 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwNumberOfFunctionKeys', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwNumberOfFunctionKeys )),   12 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwNumberOfIndicators', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwNumberOfIndicators )),   16 ) ;
    DoCheckOffset('RID_DEVICE_INFO_KEYBOARD'+'.'+'dwNumberOfKeysTotal', SizeUInt(@(PRID_DEVICE_INFO_KEYBOARD (nil)^.  dwNumberOfKeysTotal )),   20 ) ;
  DoCheckSize('RID_DEVICE_INFO_HID', SizeOf( RID_DEVICE_INFO_HID ),   16 ) ;
    DoCheckOffset('RID_DEVICE_INFO_HID'+'.'+'dwVendorId', SizeUInt(@(PRID_DEVICE_INFO_HID (nil)^.  dwVendorId )),   0 ) ;
    DoCheckOffset('RID_DEVICE_INFO_HID'+'.'+'dwProductId', SizeUInt(@(PRID_DEVICE_INFO_HID (nil)^.  dwProductId )),   4 ) ;
    DoCheckOffset('RID_DEVICE_INFO_HID'+'.'+'dwVersionNumber', SizeUInt(@(PRID_DEVICE_INFO_HID (nil)^.  dwVersionNumber )),   8 ) ;
    DoCheckOffset('RID_DEVICE_INFO_HID'+'.'+'usUsagePage', SizeUInt(@(PRID_DEVICE_INFO_HID (nil)^.  usUsagePage )),   12 ) ;
    DoCheckOffset('RID_DEVICE_INFO_HID'+'.'+'usUsage', SizeUInt(@(PRID_DEVICE_INFO_HID (nil)^.  usUsage )),   14 ) ;
  DoCheckSize('RID_DEVICE_INFO', SizeOf( RID_DEVICE_INFO ),   32 ) ;
    DoCheckOffset('RID_DEVICE_INFO'+'.'+'cbSize', SizeUInt(@(PRID_DEVICE_INFO (nil)^.  cbSize )),   0 ) ;
    DoCheckOffset('RID_DEVICE_INFO'+'.'+'dwType', SizeUInt(@(PRID_DEVICE_INFO (nil)^.  dwType )),   4 ) ;
    DoCheckOffset('RID_DEVICE_INFO'+'.'+'mouse', SizeUInt(@(PRID_DEVICE_INFO (nil)^.  mouse )),   8 ) ;
    DoCheckOffset('RID_DEVICE_INFO'+'.'+'keyboard', SizeUInt(@(PRID_DEVICE_INFO (nil)^.  keyboard )),   8 ) ;
    DoCheckOffset('RID_DEVICE_INFO'+'.'+'hid', SizeUInt(@(PRID_DEVICE_INFO (nil)^.  hid )),   8 ) ;
  DoCheckSize('RAWINPUTDEVICE', SizeOf( RAWINPUTDEVICE ),   12 ) ;
    DoCheckOffset('RAWINPUTDEVICE'+'.'+'usUsagePage', SizeUInt(@(PRAWINPUTDEVICE (nil)^.  usUsagePage )),   0 ) ;
    DoCheckOffset('RAWINPUTDEVICE'+'.'+'usUsage', SizeUInt(@(PRAWINPUTDEVICE (nil)^.  usUsage )),   2 ) ;
    DoCheckOffset('RAWINPUTDEVICE'+'.'+'dwFlags', SizeUInt(@(PRAWINPUTDEVICE (nil)^.  dwFlags )),   4 ) ;
    DoCheckOffset('RAWINPUTDEVICE'+'.'+'hwndTarget', SizeUInt(@(PRAWINPUTDEVICE (nil)^.  hwndTarget )),   8 ) ;
  DoCheckSize('RAWINPUTDEVICELIST', SizeOf( RAWINPUTDEVICELIST ),   8 ) ;
    DoCheckOffset('RAWINPUTDEVICELIST'+'.'+'hDevice', SizeUInt(@(PRAWINPUTDEVICELIST (nil)^.  hDevice )),   0 ) ;
    DoCheckOffset('RAWINPUTDEVICELIST'+'.'+'dwType', SizeUInt(@(PRAWINPUTDEVICELIST (nil)^.  dwType )),   4 ) ;
  if errors_found then
  begin
    Writeln('Errors found!');
    Halt(1);
  end
  else
    Writeln('Ok!');

end.
