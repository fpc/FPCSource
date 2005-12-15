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
  TPTCConsole=Class(TPTCBaseConsole)
  Private
    Function ConsoleCreate(index : Integer) : TPTCBaseConsole;
    Function ConsoleCreate(Const _name : String) : TPTCBaseConsole;
    Procedure check;
    console : TPTCBaseConsole;
    m_modes : Array[0..1023] Of TPTCMode;
    hacky_option_console_flag : Boolean;
  Protected
    Procedure internal_ReadKey(k : TPTCKey); Override;
    Function internal_PeekKey(k : TPTCKey) : Boolean; Override;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure configure(Const _file : String); Override;
    Function option(Const _option : String) : Boolean; Override;
    Function modes : PPTCMode; Override;
    Procedure open(Const _title : String; _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; Const _format : TPTCFormat;
                   _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; _width, _height : Integer;
                   Const _format : TPTCFormat; _pages : Integer); Overload; Override;
    Procedure open(Const _title : String; Const _mode : TPTCMode;
                   _pages : Integer); Overload; Override;

    {$WARNING this should be removed for fpc 1.1}
    {pages=0}
    Procedure open(Const _title : String); Overload; Override;
    Procedure open(Const _title : String; Const _format : TPTCFormat); Overload; Override;
    Procedure open(Const _title : String; _width, _height : Integer;
                   Const _format : TPTCFormat); Overload; Override;
    Procedure open(Const _title : String; Const _mode : TPTCMode); Overload; Override;
    {/pages=0}

    Procedure close; Override;
    Procedure flush; Override;
    Procedure finish; Override;
    Procedure update; Override;
    Procedure update(Const _area : TPTCArea); Override;
    Procedure copy(Var surface : TPTCBaseSurface); Override;
    Procedure copy(Var surface : TPTCBaseSurface;
                   Const source, destination : TPTCArea); Override;
    Function lock : Pointer; Override;
    Procedure unlock; Override;
    Procedure load(Const pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette); Override;
    Procedure load(Const pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette;
                   Const source, destination : TPTCArea); Override;
    Procedure save(pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette); Override;
    Procedure save(pixels : Pointer;
                   _width, _height, _pitch : Integer;
                   Const _format : TPTCFormat;
                   Const _palette : TPTCPalette;
                   Const source, destination : TPTCArea); Override;
    Procedure clear; Override;
    Procedure clear(Const color : TPTCColor); Override;
    Procedure clear(Const color : TPTCColor;
                    Const _area : TPTCArea); Override;
    Procedure palette(Const _palette : TPTCPalette); Override;
    Function palette : TPTCPalette; Override;
    Procedure clip(Const _area : TPTCArea); Override;
    Function width : Integer; Override;
    Function height : Integer; Override;
    Function pitch : Integer; Override;
    Function pages : Integer; Override;
    Function area : TPTCArea; Override;
    Function clip : TPTCArea; Override;
    Function format : TPTCFormat; Override;
    Function name : String; Override;
    Function title : String; Override;
    Function information : String; Override;
  End;
