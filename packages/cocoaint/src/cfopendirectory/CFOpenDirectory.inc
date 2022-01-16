{ Parsed from CFOpenDirectory.framework CFOpenDirectory.h }

{$if  not (defined(__OPENDIRECTORY_CFOPENDIRECTORY__))}
{$ifdef TYPES}
const
  __OPENDIRECTORY_CFOPENDIRECTORY__ = 1;

type
  ODContextRef = OpaqueType;
  ODNodeRef = OpaqueType;
  ODQueryRef = OpaqueType;
  ODRecordRef = OpaqueType;
  ODSessionRef = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kODErrorDomainFramework: CFStringRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
{$endif}

{$endif}
