{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Generic implementation of 'system log' event mechanism which maps to file log.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function TEventLog.DefaultFileName : String;

begin
  Result:=ChangeFileExt(ExtractFileName(Paramstr(0)),'.log');
end;

Procedure TEventLog.ActivateSystemLog;

begin
  CheckIdentification;
  ActivateFileLog;
end;

Procedure TEventLog.DeActivateSystemLog;

begin
  DeActivateFileLog;
end;

procedure TEventLog.WriteSystemLog(EventType : TEventType; Msg : String);

begin
  WriteFileLog(EventType,Msg);
end;

Function TEventLog.RegisterMessageFile(AFileName : String) : Boolean;

begin
  Result:=True;
end;

function TEventLog.MapTypeToCategory(EventType: TEventType): Word;
begin
  Result:=0;
end;

function TEventLog.MapTypeToEventID(EventType: TEventType): DWord;

begin
  Result:=0;
end;

function TEventLog.MapTypeToEvent(EventType: TEventType): DWord;

begin
  Result:=0;
end;

{
  $Log$
  Revision 1.1  2003-10-06 20:33:58  peter
    * classes moved to rtl for 1.1
    * classes .inc and classes.pp files moved to fcl/classes for
      backwards 1.0.x compatiblity to have it in the fcl

  Revision 1.1  2003/02/19 20:25:16  michael
  + Added event log

}
