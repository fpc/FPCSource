{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2000 by Florian Klaempfl

    Strings for menus, dialogs etc

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
{$ifdef USERESSTRINGS}{$mode objfpc}{$endif}
unit fpstring;

  interface

    uses
       fpconst;

{$ifdef LANG_HUN}
{$i fpstrh.inc}    { Hungarian language file }
{$else}
 {$ifdef LANG_GER}
 {$i fpstrg.inc}    { German language file }
 {$else}
   {$i fpstre.inc}  { English language file }
 {$endif}
{$endif}

  implementation

end.
{
  $Log$
  Revision 1.4  2000-05-02 08:42:28  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.3  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.2  2000/02/07 08:29:13  michael
  [*] the fake (!) TOKENS.PAS still contained the typo bug
       FSplit(,n,d,e) (correctly FSplit(,d,n,e))
  [*] CodeComplete had a very ugly bug - coordinates were document-relative
      (instead of being screen-relative)
  [*] TResourceStream didn't count the size of the resource names when
      determining the file size and this could lead to the last resources not
      loaded correctly


  [+] Ctrl-Enter in editor now tries to open the file at cursor
  [+] CodeComplete option added to Options|Environment|Editor
  [+] user interface for managing CodeComplete implemented
  [+] user interface for CodeTemplates implemented
  [+] CodeComplete wordlist and CodeTemplates stored in desktop file
  [+] help topic size no longer limited to 64KB when compiled with FPC

  Revision 1.1  2000/01/23 21:25:17  florian
    + start of internationalization support

}
