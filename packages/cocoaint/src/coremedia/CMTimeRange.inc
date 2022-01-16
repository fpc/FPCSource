{ Parsed from CoreMedia.framework CMTimeRange.h }

{$ifndef CMTIMERANGE_H}

{$ifdef TYPES}
type
  CMTimeRange = record
    start: CMTime;
    duration: CMTime;
  end;
type
  CMTimeRangePtr = ^CMTimeRange;
type
  CMTimeMapping = record
    source: CMTimeRange;
    target: CMTimeRange;
  end;
type
  CMTimeMappingPtr = ^CMTimeMapping;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimeRangeZero: CMTimeRange; cvar; external;
  kCMTimeRangeInvalid: CMTimeRange; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeRangeMake(start: CMTime; duration: CMTime): CMTimeRange; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeGetUnion(range1: CMTimeRange; range2: CMTimeRange): CMTimeRange; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeGetIntersection(range1: CMTimeRange; range2: CMTimeRange): CMTimeRange; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeEqual(range1: CMTimeRange; range2: CMTimeRange): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeContainsTime(range: CMTimeRange; time: CMTime): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeContainsTimeRange(range1: CMTimeRange; range2: CMTimeRange): Boolean; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeGetEnd(range: CMTimeRange): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMapTimeFromRangeToRange(t: CMTime; fromRange: CMTimeRange; toRange: CMTimeRange): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeClampToRange(time: CMTime; range: CMTimeRange): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeMapDurationFromRangeToRange(dur: CMTime; fromRange: CMTimeRange; toRange: CMTimeRange): CMTime; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeFromTimeToTime(start: CMTime; end_: CMTime): CMTimeRange; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeCopyAsDictionary(range: CMTimeRange; allocator: CFAllocatorRef): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
function CMTimeRangeMakeFromDictionary(dict: CFDictionaryRef): CMTimeRange; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMTimeRangeStartKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCMTimeRangeDurationKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMTimeRangeCopyDescription(allocator: CFAllocatorRef; range: CMTimeRange): CFStringRef; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
procedure CMTimeRangeShow(range: CMTimeRange); cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$endif}
