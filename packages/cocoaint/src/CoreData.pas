{$mode objfpc}
{$modeswitch objectivec1}
unit CoreData;
interface
uses
	ctypes, CocoaAll;
{$linkframework Cocoa}	{ Cocoa is "umbrella" fremework, includes CoreData }
{$define INTERFACE}

{$define HEADER}
{$include coredata/CoreData.inc}
{$undef HEADER}

{$define TYPES}
{$include coredata/CoreData.inc}
{$undef TYPES}

{$define RECORDS}
{$include coredata/CoreData.inc}
{$undef RECORDS}

type
	{GK: ctypes don't define it... }
	uintptr_t	= ptruint;
{$define FORWARD}
{$include coredata/CoreData.inc}
{$undef FORWARD}

{$define CLASSES}
	{ Internal class in NSMigrationManager.h }
	NSMigrationContext = id;
{$include coredata/CoreData.inc}
{$undef CLASSES}

{$define PROTOCOLS}
{$include coredata/CoreData.inc}
{$undef PROTOCOLS}

{$define FUNCTIONS}
{$include coredata/CoreData.inc}
{$undef FUNCTIONS}

{$define EXTERNAL_SYMBOLS}
{$include coredata/CoreData.inc}
{$undef EXTERNAL_SYMBOLS}

{$define USER_PATCHES}
{$include coredata/CoreData.inc}
{$undef USER_PATCHES}


{$undef INTERFACE}

implementation

{$define IMPLEMENTATION}

{$undef IMPLEMENTATION}

end.
