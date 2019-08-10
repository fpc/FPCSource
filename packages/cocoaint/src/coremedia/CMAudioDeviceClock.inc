{ Parsed from CoreMedia.framework CMAudioDeviceClock.h }

{$ifndef __CMAUDIODEVICECLOCK__}

{$ifdef FUNCTIONS}
function CMAudioDeviceClockCreate(allocator: CFAllocatorRef; deviceUID: CFStringRef; clockOut: CMClockRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_NA }
{$endif}

{$if  not defined(TARGET_OS_IPHONE)}
{$ifdef FUNCTIONS}
function CMAudioDeviceClockCreateFromAudioDeviceID(allocator: CFAllocatorRef; deviceID: AudioDeviceID; clockOut: CMClockRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_NA }
function CMAudioDeviceClockSetAudioDeviceUID(clock: CMClockRef; deviceUID: CFStringRef): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_NA }
function CMAudioDeviceClockSetAudioDeviceID(clock: CMClockRef; deviceID: AudioDeviceID): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_NA }
function CMAudioDeviceClockGetAudioDevice(clock: CMClockRef; deviceUIDOut: CFStringRefPtr; deviceIDOut: AudioDeviceIDPtr; trackingDefaultDeviceOut: pObjCBOOL): OSStatus; cdecl; external; { available starting in __MAC_10_8, __IPHONE_NA }
{$endif}

{$endif}

{$endif}
