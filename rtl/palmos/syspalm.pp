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

       { !!!!
       DWord    = Cardinal;
       LongWord = Cardinal;
       }

       { The Cardinal data type isn't currently implemented for the m68k }
       DWord    = LongInt;
       LongWord = LongInt;

       { Zero - terminated strings }
       PChar    = ^Char;
       PPChar   = ^PChar;

       { procedure type }
       TProcedure = Procedure;

    const
       { max. values for longint and int }
       MaxLongint = High(LongInt);
       MaxInt = High(Integer);

       { Must be determined at startup for both }
       Test68000 : byte = 0;
       Test68881 : byte = 0;

    { Palm specific data types }
    type
       Ptr    = ^Char;

    var
       ExitCode : DWord;
       { this variables are passed to PilotMain }
       cmd : Word;
       cmdPBP : Ptr;
       launchFlags : Word;

  implementation

    { mimic the C start code }
    function _PilotMain(_cmd : Word;_cmdPBP : Ptr;_launchFlags : Word) : DWord;cdecl;export;

      begin
         cmd:=_cmd;
         cmdPBP:=_cmdPBP;
         launchFlags:=_launchFlags;
         asm
            bsr PASCALMAIN
         end;
         _PilotMain:=ExitCode;
      end;

begin
   ExitCode:=0;
end.

{
  $Log$
  Revision 1.2  1998-08-22 10:23:59  florian
    + PilotMain implemented

  Revision 1.1  1998/08/05 17:19:07  florian
    + first few things for PalmOS support

}
