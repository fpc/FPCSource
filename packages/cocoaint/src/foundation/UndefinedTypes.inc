{$if MAC_OS_X_VERSION_10_7 <= MAC_OS_X_VERSION_MAX_ALLOWED}
const
	kCFCalendarUnitWeekOfMonth = 1 shl 12;
	kCFCalendarUnitWeekOfYear = 1 shl 13;
	kCFCalendarUnitYearForWeekOfYear = 1 shl 14;
{$endif}

// Added in 10.8
type
	uuid_t = pointer;
	au_asid_t = pid_t;
