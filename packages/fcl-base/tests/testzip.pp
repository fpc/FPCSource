{
    $Id: header,v 1.1 2000/07/13 06:33:45 michael Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
Program testzip;

uses Classes,Zipper;

Var
  S : TStringList;
  I : Integer;

begin
  S:=TStringList.Create;
  For I:=2 to ParamCount do
    S.Add(ParamStr(I));
  With TZipper.Create do
    try
      ZipFiles(ParamStr(1),S);
    Finally
      Free;
    end;  
end.