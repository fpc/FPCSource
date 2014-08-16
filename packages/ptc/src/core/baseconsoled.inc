{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2013  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  IPTCConsole = interface(IPTCSurface)
    function GetPages: Integer;
    function GetName: string;
    function GetTitle: string;
    function GetInformation: string;

    function GetOpenGL_Enabled: Boolean;
    procedure SetOpenGL_Enabled(AValue: Boolean);
    function GetOpenGL_Attributes: IPTCOpenGLAttributes;

    procedure Configure(const AFileName: string);
    function Modes: TPTCModeList;
    procedure Open(const ATitle: string; APages: Integer = 0); overload;
    procedure Open(const ATitle: string; AFormat: IPTCFormat;
                   APages: Integer = 0); overload;
    procedure Open(const ATitle: string; AWidth, AHeight: Integer;
                   AFormat: IPTCFormat; APages: Integer = 0); overload;
    procedure Open(const ATitle: string; AMode: IPTCMode;
                   APages: Integer = 0); overload;
    procedure Close;
    procedure InternalResize(AWidth, AHeight: Integer);
    procedure Flush;
    procedure Finish;
    procedure Update;
    procedure Update(AArea: IPTCArea);

    { event handling }
    function NextEvent(out AEvent: IPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
    function PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): IPTCEvent;

    { key handling }
    function KeyPressed: Boolean;
    function PeekKey(out AKey: IPTCKeyEvent): Boolean;
    procedure ReadKey(out AKey: IPTCKeyEvent);
    procedure ReadKey;
    procedure SetKeyReleaseEnabled(AValue: Boolean);
    function GetKeyReleaseEnabled: Boolean;
    property KeyReleaseEnabled: Boolean read GetKeyReleaseEnabled write SetKeyReleaseEnabled;

    property Pages: Integer read GetPages;
    property Name: string read GetName;
    property Title: string read GetTitle;
    property Information: string read GetInformation;

    { OpenGL support }
    property OpenGL_Enabled: Boolean read GetOpenGL_Enabled write SetOpenGL_Enabled;
    property OpenGL_Attributes: IPTCOpenGLAttributes read GetOpenGL_Attributes;
    procedure OpenGL_SwapBuffers;
    procedure OpenGL_SetSwapInterval(AInterval: Integer);
    function OpenGL_GetSwapInterval: Integer;
  end;

