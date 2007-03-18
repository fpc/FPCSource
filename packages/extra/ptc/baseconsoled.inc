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
  TPTCBaseConsole=Class(TPTCBaseSurface)
  Private
    FReleaseEnabled : Boolean;
  Public
    Constructor Create;
    Procedure configure(Const _file : String); Virtual; Abstract;
    Function modes : PPTCMode; Virtual; Abstract;
    Procedure open(Const _title : String; _pages : Integer = 0); Overload; Virtual; Abstract;
    Procedure open(Const _title : String; Const _format : TPTCFormat;
                   _pages : Integer = 0); Overload; Virtual; Abstract;
    Procedure open(Const _title : String; _width, _height : Integer;
                   Const _format : TPTCFormat; _pages : Integer = 0); Overload; Virtual; Abstract;
    Procedure open(Const _title : String; Const _mode : TPTCMode;
                   _pages : Integer = 0); Overload; Virtual; Abstract;
    Procedure close; Virtual; Abstract;
    Procedure flush; Virtual; Abstract;
    Procedure finish; Virtual; Abstract;
    Procedure update; Virtual; Abstract;
    Procedure update(Const _area : TPTCArea); Virtual; Abstract;
    
    { event handling }
    Function NextEvent(Var event : TPTCEvent; wait : Boolean; Const EventMask : TPTCEventMask) : Boolean; Virtual; Abstract;
    Function PeekEvent(wait : Boolean; Const EventMask : TPTCEventMask) : TPTCEvent; Virtual; Abstract;
    
    { key handling }
    Function KeyPressed : Boolean;
    Function PeekKey(Var k : TPTCKeyEvent) : Boolean;
    Procedure ReadKey(Var k : TPTCKeyEvent);
    Procedure ReadKey;
    Property KeyReleaseEnabled : Boolean Read FReleaseEnabled Write FReleaseEnabled;
    
    Function pages : Integer; Virtual; Abstract;
    Function name : String; Virtual; Abstract;
    Function title : String; Virtual; Abstract;
    Function information : String; Virtual; Abstract;
  End;
