{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

Type
  TPTCBaseSurface=Class(TObject)
  Public
{    Constructor Create;}
{    Destructor Destroy; Override;}
    Procedure copy(Var surface : TPTCBaseSurface); Virtual; Abstract;
    Procedure copy(Var surface : TPTCBaseSurface;
                   Const source, destination : TPTCArea); Virtual; Abstract;
    Function lock : Pointer; Virtual; Abstract;
    Procedure unlock; Virtual; Abstract;
    Procedure load(Const pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette); Virtual; Abstract;
    Procedure load(Const pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette;
                   Const source, destination : TPTCArea); Virtual; Abstract;
    Procedure save(pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette); Virtual; Abstract;
    Procedure save(pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette;
                   Const source, destination : TPTCArea); Virtual; Abstract;
    Procedure clear; Virtual; Abstract;
    Procedure clear(Const color : TPTCColor); Virtual; Abstract;
    Procedure clear(Const color : TPTCColor;
                    Const _area : TPTCArea); Virtual; Abstract;
    Procedure palette(Const _palette : TPTCPalette); Virtual; Abstract;
    Function palette : TPTCPalette; Virtual; Abstract;
    Procedure clip(Const _area : TPTCArea); Virtual; Abstract;
    Function width : Integer; Virtual; Abstract;
    Function height : Integer; Virtual; Abstract;
    Function pitch : Integer; Virtual; Abstract;
    Function area : TPTCArea; Virtual; Abstract;
    Function clip : TPTCArea; Virtual; Abstract;
    Function format : TPTCFormat; Virtual; Abstract;
    Function option(Const _option : String) : Boolean; Virtual; Abstract;
  End;
