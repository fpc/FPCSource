{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2015 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
}

type
  TCocoaConsole = class;

  { NSPTCWindowDelegate }

  NSPTCWindowDelegate = objcclass(NSObject, NSWindowDelegateProtocol)
  private
    FConsole: TCocoaConsole;
  public
    function windowShouldClose(sender: id): Boolean;
  end;

  { NSPTCWindow }

  NSPTCWindow = objcclass(NSWindow, NSWindowDelegateProtocol)
  private
    FConsole: TCocoaConsole;
  public
    procedure keyDown(theEvent: NSEvent); override;
    procedure keyUp(theEvent: NSEvent); override;
    procedure flagsChanged(theEvent: NSEvent); override;

    procedure mouseDown(theEvent: NSEvent); override;
    procedure mouseDragged(theEvent: NSEvent); override;
    procedure mouseEntered(theEvent: NSEvent); override;
    procedure mouseExited(theEvent: NSEvent); override;
    procedure mouseMoved(theEvent: NSEvent); override;
    procedure mouseUp(theEvent: NSEvent); override;
    procedure rightMouseDown(theEvent: NSEvent); override;
    procedure rightMouseDragged(theEvent: NSEvent); override;
    procedure rightMouseUp(theEvent: NSEvent); override;
    procedure otherMouseDown(theEvent: NSEvent); override;
    procedure otherMouseDragged(theEvent: NSEvent); override;
    procedure otherMouseUp(theEvent: NSEvent); override;
  end;

  { TCocoaConsole }

  TCocoaConsole = class(TPTCOpenGLLessConsole)
  private
    FTitle: string;
    FWidth, FHeight, FPitch: Integer;
    FFormat: IPTCFormat;
    FCopy: TPTCCopy;
    FClear: TPTCClear;
    FPalette: IPTCPalette;

    FArea: IPTCArea;
    FClip: IPTCArea;

    FEventQueue: TEventQueue;
    FInterceptClose: Boolean;

    FWindowDelegate: NSPTCWindowDelegate;
    FWindow: NSPTCWindow;
    FImageRep: NSBitmapImageRep;
    FImage: NSImage;
    FView: NSView;

    class procedure MaybeCreateAutoreleasePool;

    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetPitch: Integer; override;
    function GetArea: IPTCArea; override;
    function GetFormat: IPTCFormat; override;

    function GetPages: Integer; override;
    function GetName: string; override;
    function GetTitle: string; override;
    function GetInformation: string; override;

    function TranslateKeyCode(kcode: cushort): Integer;

    function HandleCocoaKeyEvent(theEvent: NSEvent; const Method: string): Boolean;
    function HandleCocoaMouseEvent(theEvent: NSEvent; const Method: string): Boolean;
    function HandleWindowShouldClose(sender: id): Boolean;
    procedure HandleEvents;

    property InterceptClose: Boolean read FInterceptClose write FInterceptClose;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Copy(ASurface: IPTCSurface); override;
    procedure Copy(ASurface: IPTCSurface;
                   ASource, ADestination: IPTCArea); override;
    function Lock: Pointer; override;
    procedure Unlock; override;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Load(const APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette); override;
    procedure Save(APixels: Pointer;
                   AWidth, AHeight, APitch: Integer;
                   AFormat: IPTCFormat;
                   APalette: IPTCPalette;
                   ASource, ADestination: IPTCArea); override;
    procedure Clear; override;
    procedure Clear(AColor: IPTCColor); override;
    procedure Clear(AColor: IPTCColor;
                    AArea: IPTCArea); override;
    procedure Palette(APalette: IPTCPalette); override;
    procedure Clip(AArea: IPTCArea); override;
    function Option(const AOption: String): Boolean; override;
    function Clip: IPTCArea; override;
    function Palette: IPTCPalette; override;

    procedure Configure(const AFileName: String); override;
    function Modes: TPTCModeList; override;
    procedure Open(const ATitle: string; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AFormat: IPTCFormat;
                   APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AWidth, AHeight: Integer;
                   AFormat: IPTCFormat; APages: Integer = 0); overload; override;
    procedure Open(const ATitle: string; AMode: IPTCMode;
                   APages: Integer = 0); overload; override;
    procedure Close; override;

    procedure Flush; override;
    procedure Finish; override;
    procedure Update; override;
    procedure Update(AArea: IPTCArea); override;

    { event handling }
    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean; override;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent; override;
  end;
