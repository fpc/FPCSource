{ Parsed from ImageKit.framework IKFilterUIView.h }


{$ifdef TYPES}
type
  IKFilterUIViewPtr = ^IKFilterUIView;
{$endif}

{$ifdef CLASSES}

type
  IKFilterUIView = objcclass external (NSView)
  private
    _priv:array[0..7] of pointer;
  public
    class function viewWithFrame_filter (frameRect: NSRect; inFilter: CIFilter): id; message 'viewWithFrame:filter:';
    function initWithFrame_filter (frameRect: NSRect; inFilter: CIFilter): id; message 'initWithFrame:filter:';
    function filter: CIFilter; message 'filter';
    function objectController: NSObjectController; message 'objectController';
  end;
{$endif}

