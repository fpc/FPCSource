{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit generates i386 (or better) assembler from the parse tree

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$ifdef tp}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
unit cgi3862;

  interface

    uses
       objects,verbose,cobjects,systems,globals,tree,
       symtable,types,strings,pass_1,hcodegen,
       aasm,i386,tgeni386,files,cgai386;

    procedure secondadd(var p : ptree);
    procedure secondaddstring(var p : ptree);
    procedure secondas(var p : ptree);
    procedure secondis(var p : ptree);
    procedure secondloadvmt(var p : ptree);

  implementation

    uses
       cgi386;

{$I cgi386ad.inc}

end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:12  root
  Initial revision

  Revision 1.9  1998/03/10 01:17:18  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

}