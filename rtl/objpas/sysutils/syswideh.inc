{
    *********************************************************************
    $Id$
    Copyright (C) 2002 by Florian Klaempfl

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
    *********************************************************************
}

function WideUpperCase(const s : WideString) : WideString;
function WideLowerCase(const s : WideString) : WideString;
function WideCompareStr(const s1, s2 : WideString) : PtrInt;
function WideSameStr(const s1, s2 : WideString) : Boolean;
function WideCompareText(const s1, s2 : WideString) : PtrInt;
function WideSameText(const s1, s2 : WideString) : Boolean;

Function WideFormat (Const Fmt : WideString; const Args : Array of const) : WideString;

{
  $Log$
  Revision 1.4  2005-02-26 15:00:14  florian
    + WideSameStr

  Revision 1.3  2005/02/26 10:21:17  florian
    + implemented WideFormat
    + some Widestring stuff implemented
    * some Widestring stuff fixed

  Revision 1.2  2005/02/03 18:40:02  florian
    * compilation with 1.0.x fixed
    + infrastructure for WideCompareText implemented

  Revision 1.1  2005/02/01 20:22:50  florian
    * improved widestring infrastructure manager
}
