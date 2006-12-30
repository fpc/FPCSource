{$mode delphi}
unit tw7242;

interface

function _tl_get_str( input: PChar ): PChar; CDecl;

implementation

uses
  Classes, SysUtils;

//=============================================================================
// forward declarations of internal routines
//-----------------------------------------------------------------------------
function __GetStr( const input: string; var error: integer ): string; local; forward;

//=============================================================================
function _tl_get_str( input: PChar ): PChar; CDecl;
//-----------------------------------------------------------------------------
// Called by : -
//   Purpose : -
// Arguments : -
//   Returns : -
//      ToDo : -
//   Remarks : -
//-----------------------------------------------------------------------------
var
  retval: string;
  error : integer;
begin
  result := nil;
  error  := 0;

  retval := __GetStr( input, error );
  if (error = 0) and (retval <> '') then try
    GetMem( result, Length( retval ) + 1 );
    StrPCopy( result, retval );
  except
    error := 1;
  end;
end;
//-----------------------------------------------------------------------------

//=============================================================================
// INTERNAL ROUTINES ( without usage of PChar to avoid memory leaks! )
//=============================================================================

//=============================================================================
function __GetStr( const input: string; var error: integer ): string; local;
//-----------------------------------------------------------------------------
// Called by : -
//   Purpose : -
// Arguments : -
//   Returns : -
//      ToDo : -
//   Remarks : -
//----------------------------------------------------------------------------
begin
  error  := 0;
  result := input;
end;
//-----------------------------------------------------------------------------

end.

//= END OF FILE ===============================================================

