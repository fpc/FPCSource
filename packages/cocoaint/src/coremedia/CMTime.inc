{ Parsed from CoreMedia.framework CMTime.h }

{$ifndef CMTIME_H}

{$ifdef TYPES}
type
  CMTimeValue = cint64;
  CMTimeValuePtr = ^CMTimeValue;
  CMTimeScale = cint32;
  CMTimeScalePtr = ^CMTimeScale;

const
  kCMTimeMaxTimescale = $7fffffff;

type
  CMTimeEpoch = cint64;
  CMTimeEpochPtr = ^CMTimeEpoch;
  CMTimeFlags = cuint32;
  CMTimeFlagsPtr = ^CMTimeFlags;

const
  kCMTimeFlags_Valid = 1 shl 0;
  kCMTimeFlags_HasBeenRounded = 1 shl 1;
  kCMTimeFlags_PositiveInfinity = 1 shl 2;
  kCMTimeFlags_NegativeInfinity = 1 shl 3;
  kCMTimeFlags_Indefinite = 1 shl 4;
  kCMTimeFlags_ImpliedValueFlagsMask = kCMTimeFlags_PositiveInfinity + kCMTimeFlags_NegativeInfinity + kCMTimeFlags_Indefinite;

type
  CMTime = record
    value: CMTimeValue;
    timescale: CMTimeScale;
    flags: CMTimeFlags;
    epoch: CMTimeEpoch;
  end;
type
  CMTimePtr = ^CMTime;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimeInvalid: CMTime; cvar; external;
  kCMTimeIndefinite: CMTime; cvar; external;
  kCMTimePositiveInfinity: CMTime; cvar; external;
  kCMTimeNegativeInfinity: CMTime; cvar; external;
  kCMTimeZero: CMTime; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeMake(value: cint64; timescale: cint32): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMakeWithEpoch(value: cint64; timescale: cint32; epoch: cint64): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMakeWithSeconds(seconds: Float64; preferredTimeScale: cint32): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeGetSeconds(time: CMTime): Float64; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef TYPES}
type
  CMTimeRoundingMethod = cuint32;
  CMTimeRoundingMethodPtr = ^CMTimeRoundingMethod;

const
  kCMTimeRoundingMethod_RoundHalfAwayFromZero = 1;
  kCMTimeRoundingMethod_RoundTowardZero = 2;
  kCMTimeRoundingMethod_RoundAwayFromZero = 3;
  kCMTimeRoundingMethod_QuickTime = 4;
  kCMTimeRoundingMethod_RoundTowardPositiveInfinity = 5;
  kCMTimeRoundingMethod_RoundTowardNegativeInfinity = 6;
  kCMTimeRoundingMethod_Default = kCMTimeRoundingMethod_RoundHalfAwayFromZero;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeConvertScale(time: CMTime; newTimescale: cint32; method: CMTimeRoundingMethod): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeAdd(addend1: CMTime; addend2: CMTime): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeSubtract(minuend: CMTime; subtrahend: CMTime): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMultiply(time: CMTime; multiplier: cint32): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMultiplyByFloat64(time: CMTime; multiplier: Float64): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMultiplyByRatio(time: CMTime; multiplier: cint32; divisor: cint32): CMTime; cdecl; external; { available starting in __MAC_10_10,__IPHONE_7_1 }
function CMTimeCompare(time1: CMTime; time2: CMTime): cint32; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMinimum(time1: CMTime; time2: CMTime): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMaximum(time1: CMTime; time2: CMTime): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeAbsoluteValue(time: CMTime): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeCopyAsDictionary(time: CMTime; allocator: CFAllocatorRef): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMakeFromDictionary(dict: CFDictionaryRef): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimeValueKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMTimeScaleKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMTimeEpochKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMTimeFlagsKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeCopyDescription(allocator: CFAllocatorRef; time: CMTime): CFStringRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
procedure CMTimeShow(time: CMTime); cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$endif}
