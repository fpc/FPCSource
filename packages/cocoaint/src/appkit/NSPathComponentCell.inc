{ Parsed from AppKit.framework NSPathComponentCell.h }


{$ifdef TYPES}
type
  NSPathComponentCellPtr = ^NSPathComponentCell;
{$endif}

{$ifdef CLASSES}

type
  NSPathComponentCell = objcclass external (NSTextFieldCell)
  private
    _image: NSImage;
    _fullWidth: CGFloat;
    _resizedWidth: CGFloat;
    _currentWidth: CGFloat;
    _url: NSURL;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          shouldDrawArrow: 0..1;
          drawsAsNavigationBar: 0..1;
          isFirstItem: 0..1;
          isLastItem: 0..1;
          isDropTarget: 0..1;
          reserved: 0..((1 shl 27)-1);
        );
      end;
    _aux: id;
  public
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
  end;
{$endif}

