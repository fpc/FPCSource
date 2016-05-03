{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2007, 2009-2011  Nikolay Nikolov (nickysn@users.sourceforge.net)
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
  TPTCKeyEvent = class(TPTCEvent, IPTCKeyEvent)
  private
    FCode: Integer;
    FUnicode: Integer;
    FAlt: Boolean;
    FShift: Boolean;
    FControl: Boolean;
    FPress: Boolean;

    function GetCode: Integer;
    function GetUnicode: Integer;
    function GetAlt: Boolean;
    function GetShift: Boolean;
    function GetControl: Boolean;
    function GetPress: Boolean;
    function GetRelease: Boolean;
  protected
    function GetEventType: TPTCEventType; override;
  public
    constructor Create;
    constructor Create(ACode: Integer);
    constructor Create(ACode, AUnicode: Integer);
    constructor Create(ACode, AUnicode: Integer; APress: Boolean);
    constructor Create(ACode: Integer; AAlt, AShift, AControl: Boolean);
    constructor Create(ACode: Integer; AAlt, AShift, AControl, APress: Boolean);
    constructor Create(ACode, AUnicode: Integer;
                       AAlt, AShift, AControl: Boolean);
    constructor Create(ACode, AUnicode: Integer;
                       AAlt, AShift, AControl, APress: Boolean);
    constructor Create(AKey: IPTCKeyEvent);
{    procedure Assign(const AKey: TPTCKeyEvent);
    function Equals(const AKey: TPTCKeyEvent): Boolean;
    property Code: Integer read GetCode;
    property Unicode: Integer read GetUnicode;
    property Alt: Boolean read GetAlt;
    property Shift: Boolean read GetShift;
    property Control: Boolean read GetControl;
    property Press: Boolean read GetPress;
    property Release: Boolean read GetRelease;}
  end;

class function TPTCKeyEventFactory.CreateNew: IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create;
end;

class function TPTCKeyEventFactory.CreateNew(ACode: Integer): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode);
end;

class function TPTCKeyEventFactory.CreateNew(ACode, AUnicode: Integer): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AUnicode);
end;

class function TPTCKeyEventFactory.CreateNew(ACode, AUnicode: Integer; APress: Boolean): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AUnicode, APress);
end;

class function TPTCKeyEventFactory.CreateNew(ACode: Integer; AAlt, AShift, AControl: Boolean): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AAlt, AShift, AControl);
end;

class function TPTCKeyEventFactory.CreateNew(ACode: Integer; AAlt, AShift, AControl, APress: Boolean): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AAlt, AShift, AControl, APress);
end;

class function TPTCKeyEventFactory.CreateNew(ACode, AUnicode: Integer;
                                             AAlt, AShift, AControl: Boolean): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AUnicode, AAlt, AShift, AControl);
end;

class function TPTCKeyEventFactory.CreateNew(ACode, AUnicode: Integer;
                                             AAlt, AShift, AControl, APress: Boolean): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(ACode, AUnicode, AAlt, AShift, AControl, APress);
end;

class function TPTCKeyEventFactory.CreateNew(AKey: IPTCKeyEvent): IPTCKeyEvent;
begin
  Result := TPTCKeyEvent.Create(AKey);
end;

function TPTCKeyEvent.GetEventType: TPTCEventType;
begin
  Result := PTCKeyEvent;
end;

constructor TPTCKeyEvent.Create;
begin
  FCode    := Integer(PTCKEY_UNDEFINED);
  FUnicode := -1;
  FAlt     := False;
  FShift   := False;
  FControl := False;
  FPress   := True;
end;

constructor TPTCKeyEvent.Create(ACode: Integer);
begin
  FCode    := ACode;
  FUnicode := -1;
  FAlt     := False;
  FShift   := False;
  FControl := False;
  FPress   := True;
end;

constructor TPTCKeyEvent.Create(ACode, AUnicode: Integer);
begin
  FCode    := ACode;
  FUnicode := AUnicode;
  FAlt     := False;
  FShift   := False;
  FControl := False;
  FPress   := True;
end;

constructor TPTCKeyEvent.Create(ACode, AUnicode: Integer; APress: Boolean);
begin
  FCode    := ACode;
  FUnicode := AUnicode;
  FAlt     := False;
  FShift   := False;
  FControl := False;
  FPress   := APress;
end;

constructor TPTCKeyEvent.Create(ACode: Integer; AAlt, AShift, AControl: Boolean);
begin
  FCode    := ACode;
  FUnicode := -1;
  FAlt     := AAlt;
  FShift   := AShift;
  FControl := AControl;
  FPress   := True;
end;

constructor TPTCKeyEvent.Create(ACode: Integer; AAlt, AShift, AControl, APress: Boolean);
begin
  FCode    := ACode;
  FUnicode := -1;
  FAlt     := AAlt;
  FShift   := AShift;
  FControl := AControl;
  FPress   := APress;
end;

constructor TPTCKeyEvent.Create(ACode, AUnicode: Integer; AAlt, AShift, AControl: Boolean);
begin
  FCode    := ACode;
  FUnicode := AUnicode;
  FAlt     := AAlt;
  FShift   := AShift;
  FControl := AControl;
  FPress   := True;
end;

constructor TPTCKeyEvent.Create(ACode, AUnicode: Integer;
                                AAlt, AShift, AControl, APress: Boolean);
begin
  FCode    := ACode;
  FUnicode := AUnicode;
  FAlt     := AAlt;
  FShift   := AShift;
  FControl := AControl;
  FPress   := APress;
end;

constructor TPTCKeyEvent.Create(AKey: IPTCKeyEvent);
begin
  FCode    := AKey.Code;
  FUnicode := AKey.Unicode;
  FAlt     := AKey.Alt;
  FShift   := AKey.Shift;
  FControl := AKey.Control;
  FPress   := AKey.Press;
end;

{procedure TPTCKeyEvent.Assign(const AKey: TPTCKeyEvent);
begin
  FCode    := AKey.Code;
  FUnicode := AKey.Unicode;
  FAlt     := AKey.Alt;
  FShift   := AKey.Shift;
  FControl := AKey.Control;
  FPress   := AKey.Press;
end;

function TPTCKeyEvent.Equals(const AKey: TPTCKeyEvent): Boolean;
begin
  Result := (FCode    = AKey.FCode) and
            (FUnicode = AKey.FUnicode) and
            (FAlt     = AKey.FAlt) and
            (FShift   = AKey.FShift) and
            (FControl = AKey.FControl) and
            (FPress   = AKey.FPress);
end;}

function TPTCKeyEvent.GetCode: Integer;
begin
  Result := FCode;
end;

function TPTCKeyEvent.GetUnicode: Integer;
begin
  Result := FUnicode;
end;

function TPTCKeyEvent.GetAlt: Boolean;
begin
  Result := FAlt;
end;

function TPTCKeyEvent.GetShift: Boolean;
begin
  Result := FShift;
end;

function TPTCKeyEvent.GetControl: Boolean;
begin
  Result := FControl;
end;

function TPTCKeyEvent.GetPress: Boolean;
begin
  Result := FPress;
end;

function TPTCKeyEvent.GetRelease: Boolean;
begin
  Result := not FPress;
end;
