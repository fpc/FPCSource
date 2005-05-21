{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Debugclient/server interface definition.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit msgintf;

interface

Type
  TDebugConnection = (dcUnix,dcInet);

Const
  DebugSocket          : String = '/tmp/debugserver';
  DebugHostName        : String = 'localhost';
  DebugPort            : Word   = 4321;
  DebugConnection      : TDebugConnection = dcunix;

  lctStop        = -1;
  lctInformation = 0;
  lctWarning     = 1;
  lctError       = 2;
  lctIdentify    = 3;

Type
  TDebugMessage = Record
    MsgType : Integer;
    MsgTimeStamp : TDateTime;
    Msg : String;
  end;

implementation

end.
