{ This file is part of fpterm - a terminal emulator, written in Free Pascal

  This unit defines the basic abstract input/output connection of the terminal.
  This could be a serial port, an Unix-like pseudoterminal, a telnet, an SSH
  connection, etc.

  Copyright (C) 2024 Nikolay Nikolov <nickysn@users.sourceforge.net>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

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

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit System.Terminal.InputOutputConnection;

{$mode objfpc}{$H+}

interface

type

  { ITerminalInputOutputConnection }

  ITerminalInputOutputConnection = interface
    function IsDataAvailable: Boolean;
    function IsClosed: Boolean;

    function Read(var Buffer; Bytes: SizeUInt): SizeInt;
    procedure Write(const Buffer; Bytes: SizeUInt);
    procedure Resize(NewWidth, NewHeight: Integer);
    property DataAvailable: Boolean read IsDataAvailable;
    property Closed: Boolean read IsClosed;
  end;

implementation

end.

