{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by B‚rczi G bor

    Strings for common utilities

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}

{$mode objfpc}
unit wconsts;

  interface

  uses Dos;

const

     { History ID }
     FileId        = 101;
     TextFindId    = 105;
     TextReplaceID = 106;
     GotoID        = 107;
     TextGrepId    = 108;
     GrepArgsId    = 109;


{$ifdef LANG_HUN}
{$i wconstsh.inc}    { Hungarian language file }
{$else}
 {$ifdef LANG_GER}
 {$i wconstsg.inc}    { German language file }
 {$else}
   {$i wconstse.inc}  { English language file }
 {$endif}
{$endif}

  implementation

end.
