{ Parsed from CoreMedia.framework CMBase.h }

{$ifndef CMBASE_H}

{$ifndef __AVAILABILITY_INTERNAL__IPHONE_3_0_DEP__IPHONE_7_0}
{$ifdef TYPES}
{$endif}

{$endif}

{$if defined(TARGET_OS_WINDOWS) and defined(TARGET_CPU_X86_64)}
{$ifdef TYPES}
type
  CMItemCount = cint64;
  CMItemCountPtr = ^CMItemCount;
  CMItemIndex = cint64;
  CMItemIndexPtr = ^CMItemIndex;
{$endif}

{$else}
{$ifdef TYPES}
type
  CMItemCount = clong;
  CMItemCountPtr = ^CMItemCount;
  CMItemIndex = clong;
  CMItemIndexPtr = ^CMItemIndex;
{$endif}

{$endif}
{$ifdef TYPES}
type
  CMPersistentTrackID = cint32;
  CMPersistentTrackIDPtr = ^CMPersistentTrackID;

const
  kCMPersistentTrackID_Invalid = 0;
{$endif}

{$endif}
