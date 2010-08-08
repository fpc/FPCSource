{ Parsed from Appkit.framework NSPathComponentCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:08 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPATHCOMPONENTCELL_PAS_T}
{$define NSPATHCOMPONENTCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPATHCOMPONENTCELL_PAS_R}
{$define NSPATHCOMPONENTCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPATHCOMPONENTCELL_PAS_F}
{$define NSPATHCOMPONENTCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPATHCOMPONENTCELL_PAS_S}
{$define NSPATHCOMPONENTCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPathComponentCell = objcclass;
  NSPathComponentCellPointer = ^NSPathComponentCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPATHCOMPONENTCELL_PAS_C}
{$define NSPATHCOMPONENTCELL_PAS_C}

{ NSPathComponentCell }
  NSPathComponentCell = objcclass(NSTextFieldCell)
  private
    __image: NSImage;
    __fullWidth: CGFloat;
    __resizedWidth: CGFloat;
    __currentWidth: CGFloat;
    __url: NSURL;
    __flags: bitpacked record
        shouldDrawArrow: 0..1;
        drawsAsNavigationBar: 0..1;
        isFirstItem: 0..1;
        isLastItem: 0..1;
        reserved: 0..((1 shl 28)-1);
      end;
    __aux: id;
    
  public
    class function alloc: NSPathComponentCell; message 'alloc';

    function image: NSImage; message 'image';
    procedure setImage(value: NSImage); message 'setImage:';
    function URL: NSURL; message 'URL';
    procedure setURL(URL_: NSURL); message 'setURL:';
  end; external;

{$endif}
{$endif}
