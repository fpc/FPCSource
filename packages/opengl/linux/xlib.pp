{
  $Id$
}
unit xlib;

{$MODE objfpc}

{$LINKLIB c}
{$LINKLIB X11}

{$PACKRECORDS C}


interface

type

  XID = LongWord;
  TVisualID = LongWord;
  PDisplay = Pointer;
  PVisual = Pointer;

  PXVisualInfo = ^TXVisualInfo;
  TXVisualInfo = record
    visual: PVisual;
    visualid: TVisualID;
    screen, depth, c_class: LongInt;
    red_mask, green_mask, blue_mask: LongWord;
    colormap_size, bits_per_rgb: LongInt;
  end;

const
  VisualNoMask                  = 0;
  VisualIDMask                  = 1;
  VisualScreenMask              = 2;
  VisualDepthMask               = 4;
  VisualClassMask               = 8;
  VisualRedMaskMask             = $10;
  VisualGreenMaskMask           = $20;
  VisualBlueMaskMask            = $40;
  VisualColormapSizeMask        = $80;
  VisualBitsPerRGBMask          = $100;
  VisualAllMask                 = $1FF;



  function DefaultScreen(dpy: PDisplay): LongInt;


  function XFree(data: Pointer): LongInt; cdecl;
  function XVisualIDFromVisual(visual: PVisual): TVisualID; cdecl;
  function XGetVisualInfo(display: PDisplay; vinfo_mask: LongWord; vinfo_template: PXVisualInfo; var nitems_return: LongInt): PXVisualInfo; cdecl;


implementation

type
  PXExtData = Pointer;
  PXPrivate = Pointer;
  XPointer = PChar;
  PXrmHashBucketRec = Pointer;
  PScreenFormat = Pointer;
  PScreen = Pointer;

  PXPrivDisplay = ^TXPrivDisplay;
  TXPrivDisplay = record
    ext_data: PXExtData;                // hook for extension to hang data
    private1: PXPrivate;
    fd: LongInt;                        // Network socket.
    private2: LongInt;
    proto_major_version: LongInt;       // major version of server's X protocol
    proto_minor_version: LongInt;       // minor version of server's X protocol
    vendor: PChar;                      // vendor of the server hardware
    private3, private4, private5: XID;
    private6: LongInt;
    resource_alloc: Pointer;            // allocator function
    byte_order: LongInt;                // screen byte order, LSBFirst, MSBFirst
    bitmap_unit: LongInt;               // padding and data requirements
    bitmap_pad: LongInt;                // padding requirements on bitmaps
    bitmap_bit_order: LongInt;          // LeastSignificant or MostSignificant
    nformats: LongInt;                  // number of pixmap formats in list
    pixmap_format: PScreenFormat;       // pixmap format list
    private8: LongInt;
    release: LongInt;                   // release of the server
    private9, private10: PXPrivate;
    qlen: LongInt;                      // Length of input event queue
    last_request_read: LongWord;        // seq number of last event read
    request: LongWord;                  // sequence number of last request.
    private11, private12, private13,
      private14: XPointer;
    max_request_size: LongWord;         // maximum number 32 bit words in request
    db: PXrmHashBucketRec;
    private15: Pointer;
    display_name: PChar;                // "host:display" string used on this connect
    default_screen: LongInt;            // default screen for operations
    nscreens: LongInt;                  // number of screens on this server
    screens: PScreen;                   // pointer to list of screens
    motion_buffer: LongWord;            // size of motion buffer
    private16: LongWord;
    min_keycode: LongInt;               // minimum defined keycode
    max_keycode: LongInt;               // maximum defined keycode
    private17, private18: XPointer;
    private19: LongInt;
    xdefaults: PChar;                   // contents of defaults from server
    // there is more to this structure, but it is private to Xlib
  end;



function DefaultScreen(dpy: PDisplay): LongInt;
begin
  Result := PXPrivDisplay(dpy)^.default_screen;
end;

const
  libX = 'X11';

function XFree(data: Pointer): LongInt; cdecl; external libX;
function XVisualIDFromVisual(visual: PVisual): TVisualID; cdecl; external libX;
function XGetVisualInfo(display: PDisplay; vinfo_mask: LongWord; vinfo_template: PXVisualInfo; var nitems_return: LongInt): PXVisualInfo; cdecl; external libX;

end.

{
  $Log$
  Revision 1.1  1999-12-23 13:51:50  peter
    * reorganized, it now doesn't depend on fcl anymore by default

  Revision 1.1  1999/11/28 17:55:22  sg
  * Added new unit generation tools and auto-generated GL units for Linux

  Revision 1.1  1999/11/10 14:15:33  sg
  * Added to CVS

}
