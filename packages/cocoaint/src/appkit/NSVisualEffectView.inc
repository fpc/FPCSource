{ Parsed from AppKit.framework NSVisualEffectView.h }


{$ifdef TYPES}
type
  NSVisualEffectViewPtr = ^NSVisualEffectView;
{$endif}

{$ifdef TYPES}
type
  NSVisualEffectMaterial = NSInteger;
  NSVisualEffectMaterialPtr = ^NSVisualEffectMaterial;

const
  NSVisualEffectMaterialAppearanceBased = 0;
  NSVisualEffectMaterialLight = 1;
  NSVisualEffectMaterialDark = 2;
  NSVisualEffectMaterialTitlebar = 3;

type
  NSVisualEffectBlendingMode = NSInteger;
  NSVisualEffectBlendingModePtr = ^NSVisualEffectBlendingMode;

const
  NSVisualEffectBlendingModeBehindWindow = 0;
  NSVisualEffectBlendingModeWithinWindow = 1;

type
  NSVisualEffectState = NSInteger;
  NSVisualEffectStatePtr = ^NSVisualEffectState;

const
  NSVisualEffectStateFollowsWindowActiveState = 0;
  NSVisualEffectStateActive = 1;
  NSVisualEffectStateInactive = 2;
{$endif}

{$ifdef CLASSES}

type
  NSVisualEffectView = objcclass external (NSView)
  private
    _NSVisualEffectViewInternal: NSVisualEffectViewInternalPtr;
    {$if  not defined(__LP64__)}
    _reserved:array[0..59] of cuint8;
    {$endif}
    _anonStruct_NSVisualEffectView0: record
      case byte of
        0: (anonBitField_NSVisualEffectView0: cuint);
        1: (data: bitpacked record
          _dirty: 0..1;
          _hasMask: 0..1;
          _disableBlurFilter: 0..1;
          _titlebarMaterialDrawsSeparator: 0..1;
          _maskSet: 0..1;
          _clear: 0..1;
          _updateInDidMoveToWindow: 0..1;
          _hasAcceleration: 0..1;
          _emphasized: 0..1;
          _hasAccelerationCached: 0..1;
          _reservedFlags: 0..((1 shl 22)-1);
        end;
      );
          end;
  public
    procedure setMaterial(newValue: NSVisualEffectMaterial); message 'setMaterial:';
    function material: NSVisualEffectMaterial; message 'material';
    function interiorBackgroundStyle: NSBackgroundStyle; message 'interiorBackgroundStyle';
    procedure setBlendingMode(newValue: NSVisualEffectBlendingMode); message 'setBlendingMode:';
    function blendingMode: NSVisualEffectBlendingMode; message 'blendingMode';
    procedure setState(newValue: NSVisualEffectState); message 'setState:';
    function state: NSVisualEffectState; message 'state';
    procedure setMaskImage(newValue: NSImage); message 'setMaskImage:';
    function maskImage: NSImage; message 'maskImage';
    procedure viewDidMoveToWindow; message 'viewDidMoveToWindow'; { NS_REQUIRES_SUPER }
    procedure viewWillMoveToWindow (newWindow: NSWindow); message 'viewWillMoveToWindow:'; { NS_REQUIRES_SUPER }
  end;
{$endif}

