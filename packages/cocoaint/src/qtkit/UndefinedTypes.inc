
{ Movies.h }
const
	movieProgressOpen             = 0;
	movieProgressUpdatePercent    = 1;
	movieProgressClose            = 2;

{ Movies.h 
type
	Movie = OpaqueType;
	Track = OpaqueType;
	Media = OpaqueType;
	MovieController = pointer; // ComponentInstance
	MovieControllerPtr = ^MovieController;
}
	
{type
	SMPTETime = record
		mCounter: UInt64;
		mType: UInt32;
		mFlags: UInt32;
		mHours: SInt16;
		mMinutes: SInt16;
		mSeconds: SInt16;
		mFrames: SInt16;
	end;}

type
	QTVisualContextRef = OpaqueType;
