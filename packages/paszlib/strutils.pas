{------------------------------------------------------------------------}
{
  STRUTILS.PAS - string utilities

  Copyright (C) 1998 by Jacques Nomssi Nzali
  For conditions of distribution and use, see copyright notice in readme.txt
}
{------------------------------------------------------------------------}

{$IFNDEF MSDOS}
  {$DEFINE SYSUTILS}
{$ENDIF}

Unit StrUtils;

interface

type
  str11 = string[11];
  Function IntToStr(value : LongInt) : str11;
  { Convert any integer type to a string }


implementation

{------------------------------------------------------------------------}
Function IntToStr(value : LongInt) : str11;
{ Convert any integer type to a string }
var
  s : str11;
begin
  Str(value:0, s);
  IntToStr := S;
end;


end.