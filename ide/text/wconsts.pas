{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by B‚rczi G bor

    Strings for common utilities

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit wconsts;

  interface

  uses Dos;

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
{
  $Log$
  Revision 1.1  2000-07-13 09:48:37  michael
  + Initial import

  Revision 1.1  2000/06/16 09:30:01  pierre
   * new files from Gabor

}
