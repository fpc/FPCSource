{
    $Id$

    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$define PALMOS}
Unit SysPalm;

{$I os.inc}

  Interface

    Type
       { type and constant declartions doesn't hurt }
       LongInt  = $80000000..$7fffffff;
       Integer  = -32768..32767;
       ShortInt = -128..127;
       Byte     = 0..255;
       Word     = 0..65535;

       DWord    = Cardinal;
       LongWord = Cardinal;

       { Zero - terminated strings }
       PChar  = ^Char;
       PPChar = ^PChar;

       { procedure type }
       TProcedure = Procedure;

       const
          { max. values for longint and int }
          MaxLongint = High(LongInt);
          MaxInt = High(Integer);

          { Must be determined at startup for both }
          Test68000 : byte = 0; 
          Test68881 : byte = 0;

  implementation

begin
   // here should be some startup code inserted
end.

{
  $Log$
  Revision 1.1  1998-08-05 17:19:07  florian
    + first few things for PalmOS support

}
