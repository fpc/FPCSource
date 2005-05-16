{
    $Id: aoptcpud.pas,v 1.2 2005/02/20 19:36:03 florian Exp $
    Copyright (c) 1998-2002 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit contains the processor specific implementation of the
    assembler optimizer data flow analyzer.

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
Unit aoptcpud;

{$i fpcdefs.inc}

Interface

uses
  AOptDA;

Type
  TAOptDFACpu = class(TAOptDFA)
  End;

Implementation


End.

{
  $Log: aoptcpud.pas,v $
  Revision 1.2  2005/02/20 19:36:03  florian
    * optimizer files fixed

  Revision 1.1  2005/02/20 19:11:04  florian
    * initial commit

  Revision 1.6  2005/02/14 17:13:10  peter
    * truncate log

}
