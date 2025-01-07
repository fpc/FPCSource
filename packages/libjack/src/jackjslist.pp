{$IFNDEF FPC_DOTTEDUNITS}
unit jackjslist;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$packrecords C}

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{$I jslist.inc}

end.

