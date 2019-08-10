{ Parsed from CFOpenDirectory.framework CFODContext.h }

{$if  not (defined(__OPENDIRECTORY_CFODCONTEXT__))}
{$ifdef TYPES}
const
  __OPENDIRECTORY_CFODCONTEXT__ = 1;
{$endif}

{$ifdef FUNCTIONS}
function ODContextGetTypeID: CFTypeID; cdecl; external;
{$endif}

{$endif}
