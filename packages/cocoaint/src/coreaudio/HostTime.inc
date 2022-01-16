{ Parsed from CoreAudio.framework HostTime.h }

{$if  not (defined(__HostTime_h__))}

{$ifdef FUNCTIONS}
function AudioConvertHostTimeToNanos(inHostTime: UInt64): UInt64; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
function AudioConvertNanosToHostTime(inNanos: UInt64): UInt64; cdecl; external; { available starting in __MAC_10_0, __IPHONE_2_0 }
{$endif}

{$endif}
