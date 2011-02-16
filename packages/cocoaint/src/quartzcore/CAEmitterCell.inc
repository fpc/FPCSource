{ Parsed from Quartzcore.framework CAEmitterCell.h }

{$ifdef TYPES}
{$ifndef CAEMITTERCELL_PAS_T}
{$define CAEMITTERCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CAEMITTERCELL_PAS_R}
{$define CAEMITTERCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CAEMITTERCELL_PAS_F}
{$define CAEMITTERCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CAEMITTERCELL_PAS_S}
{$define CAEMITTERCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CAEmitterCell = objcclass;
  CAEmitterCellPointer = ^CAEmitterCell;
  CAEmitterCellPtr = CAEmitterCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CAEMITTERCELL_PAS_C}
{$define CAEMITTERCELL_PAS_C}

{ CAEmitterCell }
  CAEmitterCell = objcclass external (NSObject, NSCodingProtocol, CAMediaTimingProtocol)
  private
    __attr: Pointer;
    _state: Pointer;
    _flags: cuint32;
    
  public
    class function emitterCell: id; message 'emitterCell';
    class function defaultValueForKey(key: NSString): id; message 'defaultValueForKey:';
    function shouldArchiveValueForKey(key: NSString): Boolean; message 'shouldArchiveValueForKey:';
    procedure setName (newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setEnabled (newValue: Boolean); message 'setEnabled:';
    function isEnabled: Boolean; message 'isEnabled';
    procedure setBirthRate (newValue: single); message 'setBirthRate:';
    function birthRate: single; message 'birthRate';
    procedure setLifetime (newValue: single); message 'setLifetime:';
    function lifetime: single; message 'lifetime';
    procedure setLifetimeRange (newValue: single); message 'setLifetimeRange:';
    function lifetimeRange: single; message 'lifetimeRange';
    procedure setEmissionLatitude (newValue: CGFloat); message 'setEmissionLatitude:';
    function emissionLatitude: CGFloat; message 'emissionLatitude';
    procedure setEmissionLongitude (newValue: CGFloat); message 'setEmissionLongitude:';
    function emissionLongitude: CGFloat; message 'emissionLongitude';
    procedure setEmissionRange (newValue: CGFloat); message 'setEmissionRange:';
    function emissionRange: CGFloat; message 'emissionRange';
    procedure setVelocity (newValue: CGFloat); message 'setVelocity:';
    function velocity: CGFloat; message 'velocity';
    procedure setVelocityRange (newValue: CGFloat); message 'setVelocityRange:';
    function velocityRange: CGFloat; message 'velocityRange';
    procedure setXAcceleration (newValue: CGFloat); message 'setXAcceleration:';
    function xAcceleration: CGFloat; message 'xAcceleration';
    procedure setYAcceleration (newValue: CGFloat); message 'setYAcceleration:';
    function yAcceleration: CGFloat; message 'yAcceleration';
    procedure setZAcceleration (newValue: CGFloat); message 'setZAcceleration:';
    function zAcceleration: CGFloat; message 'zAcceleration';
    procedure setScale (newValue: CGFloat); message 'setScale:';
    function scale: CGFloat; message 'scale';
    procedure setScaleRange (newValue: CGFloat); message 'setScaleRange:';
    function scaleRange: CGFloat; message 'scaleRange';
    procedure setScaleSpeed (newValue: CGFloat); message 'setScaleSpeed:';
    function scaleSpeed: CGFloat; message 'scaleSpeed';
    procedure setSpin (newValue: CGFloat); message 'setSpin:';
    function spin: CGFloat; message 'spin';
    procedure setSpinRange (newValue: CGFloat); message 'setSpinRange:';
    function spinRange: CGFloat; message 'spinRange';
    procedure setColor (newValue: CGColorRef); message 'setColor:';
    function color: CGColorRef; message 'color';
    procedure setRedRange (newValue: single); message 'setRedRange:';
    function redRange: single; message 'redRange';
    procedure setGreenRange (newValue: single); message 'setGreenRange:';
    function greenRange: single; message 'greenRange';
    procedure setBlueRange (newValue: single); message 'setBlueRange:';
    function blueRange: single; message 'blueRange';
    procedure setAlphaRange (newValue: single); message 'setAlphaRange:';
    function alphaRange: single; message 'alphaRange';
    procedure setRedSpeed (newValue: single); message 'setRedSpeed:';
    function redSpeed: single; message 'redSpeed';
    procedure setGreenSpeed (newValue: single); message 'setGreenSpeed:';
    function greenSpeed: single; message 'greenSpeed';
    procedure setBlueSpeed (newValue: single); message 'setBlueSpeed:';
    function blueSpeed: single; message 'blueSpeed';
    procedure setAlphaSpeed (newValue: single); message 'setAlphaSpeed:';
    function alphaSpeed: single; message 'alphaSpeed';
    procedure setContents (newValue: id); message 'setContents:';
    function contents: id; message 'contents';
    procedure setContentsRect (newValue: CGRect); message 'setContentsRect:';
    function contentsRect: CGRect; message 'contentsRect';
    procedure setMinificationFilter (newValue: NSString); message 'setMinificationFilter:';
    function minificationFilter: NSString; message 'minificationFilter';
    procedure setMagnificationFilter (newValue: NSString); message 'setMagnificationFilter:';
    function magnificationFilter: NSString; message 'magnificationFilter';
    procedure setMinificationFilterBias (newValue: single); message 'setMinificationFilterBias:';
    function minificationFilterBias: single; message 'minificationFilterBias';
    procedure setEmitterCells (newValue: NSArray); message 'setEmitterCells:';
    function emitterCells: NSArray; message 'emitterCells';
    procedure setStyle (newValue: NSDictionary); message 'setStyle:';
    function style: NSDictionary; message 'style';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    procedure setBeginTime (newValue: CFTimeInterval);
    function beginTime: CFTimeInterval;
    procedure setDuration (newValue: CFTimeInterval);
    function duration: CFTimeInterval;
    procedure setSpeed (newValue: single);
    function speed: single;
    procedure setTimeOffset (newValue: CFTimeInterval);
    function timeOffset: CFTimeInterval;
    procedure setRepeatCount (newValue: single);
    function repeatCount: single;
    procedure setRepeatDuration (newValue: CFTimeInterval);
    function repeatDuration: CFTimeInterval;
    procedure setAutoreverses (newValue: Boolean);
    function autoreverses: Boolean;
    procedure setFillMode (newValue: NSString);
    function fillMode: NSString;
  end;

{$endif}
{$endif}
