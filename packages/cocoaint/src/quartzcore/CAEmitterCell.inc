{ Parsed from QuartzCore.framework CAEmitterCell.h }


{$ifdef TYPES}
type
  CAEmitterCellPtr = ^CAEmitterCell;
{$endif}

{$ifdef CLASSES}

type
  CAEmitterCell = objcclass external (NSObject, NSCodingProtocol, CAMediaTimingProtocol)
  private
    _attr:array[0..1] of pointer;
    _state: pointer;
    _flags: cuint32;
  public
    class function emitterCell: instancetype; message 'emitterCell';
    class function defaultValueForKey (key: NSString): id; message 'defaultValueForKey:';
    function shouldArchiveValueForKey (key: NSString): ObjCBOOL; message 'shouldArchiveValueForKey:';
    procedure setName(newValue: NSString); message 'setName:';
    function name: NSString; message 'name';
    procedure setEnabled(newValue: ObjCBOOL); message 'setEnabled:';
    function isEnabled: ObjCBOOL; message 'isEnabled';
    procedure setBirthRate(newValue: single); message 'setBirthRate:';
    function birthRate: single; message 'birthRate';
    procedure setLifetime(newValue: single); message 'setLifetime:';
    function lifetime: single; message 'lifetime';
    procedure setLifetimeRange(newValue: single); message 'setLifetimeRange:';
    function lifetimeRange: single; message 'lifetimeRange';
    procedure setEmissionLatitude(newValue: CGFloat); message 'setEmissionLatitude:';
    function emissionLatitude: CGFloat; message 'emissionLatitude';
    procedure setEmissionLongitude(newValue: CGFloat); message 'setEmissionLongitude:';
    function emissionLongitude: CGFloat; message 'emissionLongitude';
    procedure setEmissionRange(newValue: CGFloat); message 'setEmissionRange:';
    function emissionRange: CGFloat; message 'emissionRange';
    procedure setVelocity(newValue: CGFloat); message 'setVelocity:';
    function velocity: CGFloat; message 'velocity';
    procedure setVelocityRange(newValue: CGFloat); message 'setVelocityRange:';
    function velocityRange: CGFloat; message 'velocityRange';
    procedure setXAcceleration(newValue: CGFloat); message 'setXAcceleration:';
    function xAcceleration: CGFloat; message 'xAcceleration';
    procedure setYAcceleration(newValue: CGFloat); message 'setYAcceleration:';
    function yAcceleration: CGFloat; message 'yAcceleration';

    procedure setZAcceleration(newValue: CGFloat); message 'setZAcceleration:';
    function zAcceleration: CGFloat; message 'zAcceleration';
    procedure setScale(newValue: CGFloat); message 'setScale:';
    function scale: CGFloat; message 'scale';
    procedure setScaleRange(newValue: CGFloat); message 'setScaleRange:';
    function scaleRange: CGFloat; message 'scaleRange';

    procedure setScaleSpeed(newValue: CGFloat); message 'setScaleSpeed:';
    function scaleSpeed: CGFloat; message 'scaleSpeed';
    procedure setSpin(newValue: CGFloat); message 'setSpin:';
    function spin: CGFloat; message 'spin';
    procedure setSpinRange(newValue: CGFloat); message 'setSpinRange:';
    function spinRange: CGFloat; message 'spinRange';
    procedure setColor(newValue: CGColorRef); message 'setColor:';
    function color: CGColorRef; message 'color';
    procedure setRedRange(newValue: single); message 'setRedRange:';
    function redRange: single; message 'redRange';
    procedure setGreenRange(newValue: single); message 'setGreenRange:';
    function greenRange: single; message 'greenRange';

    procedure setBlueRange(newValue: single); message 'setBlueRange:';
    function blueRange: single; message 'blueRange';

    procedure setAlphaRange(newValue: single); message 'setAlphaRange:';
    function alphaRange: single; message 'alphaRange';
    procedure setRedSpeed(newValue: single); message 'setRedSpeed:';
    function redSpeed: single; message 'redSpeed';
    procedure setGreenSpeed(newValue: single); message 'setGreenSpeed:';
    function greenSpeed: single; message 'greenSpeed';

    procedure setBlueSpeed(newValue: single); message 'setBlueSpeed:';
    function blueSpeed: single; message 'blueSpeed';

    procedure setAlphaSpeed(newValue: single); message 'setAlphaSpeed:';
    function alphaSpeed: single; message 'alphaSpeed';
    procedure setContents(newValue: id); message 'setContents:';
    function contents: id; message 'contents';
    procedure setContentsRect(newValue: CGRect); message 'setContentsRect:';
    function contentsRect: CGRect; message 'contentsRect';
    procedure setMinificationFilter(newValue: NSString); message 'setMinificationFilter:';
    function minificationFilter: NSString; message 'minificationFilter';
    procedure setMagnificationFilter(newValue: NSString); message 'setMagnificationFilter:';
    function magnificationFilter: NSString; message 'magnificationFilter';
    procedure setMinificationFilterBias(newValue: single); message 'setMinificationFilterBias:';
    function minificationFilterBias: single; message 'minificationFilterBias';
    procedure setEmitterCells(newValue: NSArray); message 'setEmitterCells:';
    function emitterCells: NSArray; message 'emitterCells';
    procedure setStyle(newValue: NSDictionary); message 'setStyle:';
    function style: NSDictionary; message 'style';

    { Adopted protocols }
    procedure setAutoreverses(newValue: ObjCBOOL); message 'setAutoreverses:';
    function autoreverses: ObjCBOOL; message 'autoreverses';
    procedure setBeginTime(newValue: CFTimeInterval); message 'setBeginTime:';
    function beginTime: CFTimeInterval; message 'beginTime';
    procedure setDuration(newValue: CFTimeInterval); message 'setDuration:';
    function duration: CFTimeInterval; message 'duration';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    procedure setFillMode(newValue: NSString); message 'setFillMode:';
    function fillMode: NSString; message 'fillMode';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    procedure setRepeatCount(newValue: single); message 'setRepeatCount:';
    function repeatCount: single; message 'repeatCount';
    procedure setRepeatDuration(newValue: CFTimeInterval); message 'setRepeatDuration:';
    function repeatDuration: CFTimeInterval; message 'repeatDuration';
    procedure setSpeed(newValue: single); message 'setSpeed:';
    function speed: single; message 'speed';
    procedure setTimeOffset(newValue: CFTimeInterval); message 'setTimeOffset:';
    function timeOffset: CFTimeInterval; message 'timeOffset';
  end;
{$endif}

