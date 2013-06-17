{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

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
  TPTCOpenGLAttributes = class(TInterfacedObject, IPTCOpenGLAttributes)
  private
    FDoubleBuffer: Boolean;
    FDoubleBufferDontCare: Boolean;
    FStereo: Boolean;
    FStereoDontCare: Boolean;
    FBufferSize: Integer;
    FDepthSize: Integer;
    FStencilSize: Integer;

  public
    constructor Create;

    function GetDoubleBuffer: Boolean;
    procedure SetDoubleBuffer(ADoubleBuffer: Boolean);

    function GetDoubleBufferDontCare: Boolean;
    procedure SetDoubleBufferDontCare(ADoubleBufferDontCare: Boolean);

    function GetStereo: Boolean;
    procedure SetStereo(AStereo: Boolean);

    function GetStereoDontCare: Boolean;
    procedure SetStereoDontCare(AStereoDontCare: Boolean);

    function GetBufferSize: Integer;
    procedure SetBufferSize(ABufferSize: Integer);

    function GetDepthSize: Integer;
    procedure SetDepthSize(ADepthSize: Integer);

    function GetStencilSize: Integer;
    procedure SetStencilSize(AStencilSize: Integer);
  end;

constructor TPTCOpenGLAttributes.Create;
begin
  inherited;

  FDoubleBuffer := True;
  FDoubleBufferDontCare := False;
  FStereo := False;
  FStereoDontCare := False;
  FBufferSize := 0;
  FDepthSize := 0;
  FStencilSize := 0;
end;

function TPTCOpenGLAttributes.GetDoubleBuffer: Boolean;
begin
  Result := FDoubleBuffer;
end;

procedure TPTCOpenGLAttributes.SetDoubleBuffer(ADoubleBuffer: Boolean);
begin
  FDoubleBuffer := ADoubleBuffer;
end;

function TPTCOpenGLAttributes.GetDoubleBufferDontCare: Boolean;
begin
  Result := FDoubleBufferDontCare;
end;

procedure TPTCOpenGLAttributes.SetDoubleBufferDontCare(ADoubleBufferDontCare: Boolean);
begin
  FDoubleBufferDontCare := ADoubleBufferDontCare;
end;

function TPTCOpenGLAttributes.GetStereo: Boolean;
begin
  Result := FStereo;
end;

procedure TPTCOpenGLAttributes.SetStereo(AStereo: Boolean);
begin
  FStereo := AStereo;
end;

function TPTCOpenGLAttributes.GetStereoDontCare: Boolean;
begin
  Result := FStereoDontCare;
end;

procedure TPTCOpenGLAttributes.SetStereoDontCare(AStereoDontCare: Boolean);
begin
  FStereoDontCare := AStereoDontCare;
end;

function TPTCOpenGLAttributes.GetBufferSize: Integer;
begin
  Result := FBufferSize;
end;

procedure TPTCOpenGLAttributes.SetBufferSize(ABufferSize: Integer);
begin
  FBufferSize := ABufferSize;
end;

function TPTCOpenGLAttributes.GetDepthSize: Integer;
begin
  Result := FDepthSize;
end;

procedure TPTCOpenGLAttributes.SetDepthSize(ADepthSize: Integer);
begin
  FDepthSize := ADepthSize;
end;

function TPTCOpenGLAttributes.GetStencilSize: Integer;
begin
  Result := FStencilSize;
end;

procedure TPTCOpenGLAttributes.SetStencilSize(AStencilSize: Integer);
begin
  FStencilSize := AStencilSize;
end;
