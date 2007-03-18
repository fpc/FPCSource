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
  TGDIConsole = Class(TPTCBaseConsole)
  Private
    FWindow : TWin32Window;
    FWin32DIB : TWin32DIB;
    FKeyboard : TWin32Keyboard;
    FMouse : TWin32Mouse;

    FCopy : TPTCCopy;
    FClear : TPTCClear;
    FEventQueue : TEventQueue;
    FArea : TPTCArea;
    FClip : TPTCArea;
    FPalette : TPTCPalette;

    FOpen : Boolean;
    FLocked : Boolean;

    FTitle : String;

    FDefaultWidth : Integer;
    FDefaultHeight : Integer;
    FDefaultFormat : TPTCFormat;

    Function GetWidth : Integer; Override;
    Function GetHeight : Integer; Override;
    Function GetPitch : Integer; Override;
    Function GetArea : TPTCArea; Override;
    Function GetFormat : TPTCFormat; Override;
    Function GetPages : Integer; Override;
    Function GetName : String; Override;
    Function GetTitle : String; Override;
    Function GetInformation : String; Override;

    Procedure CheckOpen(    AMessage : String);
    Procedure CheckUnlocked(AMessage : String);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure Open(Const ATitle : String; APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; Const AFormat : TPTCFormat;
                   APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; AWidth, AHeight : Integer;
                   Const AFormat : TPTCFormat; APages : Integer = 0); Overload; Override;
    Procedure Open(Const ATitle : String; Const AMode : TPTCMode;
                   APages : Integer = 0); Overload; Override;
    Procedure Close; Override;

    Procedure Copy(Var ASurface : TPTCBaseSurface); Override;
    Procedure Copy(Var ASurface : TPTCBaseSurface;
                   Const ASource, ADestination : TPTCArea); Override;

    Procedure Load(Const APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette); Override;
    Procedure Load(Const APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Override;
    Procedure Save(APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette); Override;
    Procedure Save(APixels : Pointer;
                   AWidth, AHeight, APitch : Integer;
                   Const AFormat : TPTCFormat;
                   Const APalette : TPTCPalette;
                   Const ASource, ADestination : TPTCArea); Override;

    Function Lock : Pointer; Override;
    Procedure Unlock; Override;

    Procedure Clear; Override;
    Procedure Clear(Const AColor : TPTCColor); Override;
    Procedure Clear(Const AColor : TPTCColor;
                    Const AArea : TPTCArea); Override;

    Procedure Configure(Const AFileName : String); Override;
    Function Option(Const AOption : String) : Boolean; Override;

    Procedure Palette(Const APalette : TPTCPalette); Override;
    Procedure Clip(Const AArea : TPTCArea); Override;
    Function Clip : TPTCArea; Override;
    Function Palette : TPTCPalette; Override;
    Function Modes : PPTCMode; Override;

    Procedure Flush; Override;
    Procedure Finish; Override;
    Procedure Update; Override;
    Procedure Update(Const AArea : TPTCArea); Override;

    Function NextEvent(Var AEvent : TPTCEvent; AWait : Boolean; Const AEventMask : TPTCEventMask) : Boolean; Override;
    Function PeekEvent(AWait : Boolean; Const AEventMask : TPTCEventMask) : TPTCEvent; Override;
  End;
