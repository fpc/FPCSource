{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009, 2010, 2012  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TWin32Window = class
  private
    FWindow: HWND;
    FEvent: THANDLE;
    FThread: THANDLE;
    FID: DWord;
    FName: AnsiString;
    FTitle: AnsiString;
    FExtra: DWord;
    FStyle: DWord;
    FShow: Integer;
    FX, FY: Integer;
    FWidth, FHeight: Integer;
    FManaged: Boolean;
    FMultithreaded: Boolean;
    FCursorConfineInEffect: Boolean;
    FInterceptClose: Boolean;

{    class function WndProcSingleThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; StdCall;
    class function WndProcMultiThreaded(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; StdCall;}

    procedure Defaults;
    procedure Close;
    function GetThread: DWord;
    class procedure ThreadFunction(AOwner: TWin32Window);
  protected
    function WMCreate(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; virtual;
    function WMDestroy(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; virtual;
  public
    constructor Create(window: HWND);
    constructor Create(const AWndClass, ATitle: string; AExtra, AStyle, AClassStyle: DWord;
                       AShow, AX, AY, AWidth, AHeight: Integer; ACenter, AMultithreaded, ACursor, AInterceptClose: Boolean);
    destructor Destroy; override;
    procedure Cursor(AFlag: Boolean);
    procedure ConfineCursor(AFlag: Boolean);
    procedure Resize(AWidth, AHeight: Integer);
    procedure Update(AForce: Boolean = False; AWaitForMessage: Boolean = False);
    property Handle: HWND read FWindow;
    property Thread: DWord read GetThread;
    property Managed: Boolean read FManaged;
    property Multithreaded: Boolean read FMultithreaded;
    property InterceptClose: Boolean read FInterceptClose write FInterceptClose;
  end;
