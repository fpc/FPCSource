{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

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

    System Utilities For Free Pascal
}

{ String functions }

function  setLength(var s: string; newLength: longint): longint;
function  UpperCase(s: string): string;
function  LowerCase(s: string): string;
function  AnsiUpperCase(s: string): string;
function  AnsiLowerCase(s: string): string;
function  Left(s: string; i: longint): string;
function  Right(s: string; i: longint): string;
function  Trim(s: string): string;
function  TrimLeft(s: string): string;
function  TrimRight(s: string): string;

{ Conversion Functions }

function  IntToStr(l:longint):string;
function  StrToInt(s:string):longint;
function  IntToHex(Value: longint; Digits: longint): string;

{
  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}
