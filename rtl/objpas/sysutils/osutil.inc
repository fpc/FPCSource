{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    <What does this file>
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ ---------------------------------------------------------------------
    Application name
  ---------------------------------------------------------------------}
  
Function ApplicationName : String;

begin
  If Assigned(OnGetApplicationName) then 
    Result:=OnGetApplicationName()
  else
    Result:=ChangeFileExt(ExtractFileName(Paramstr(0)),'');
end;

{ ---------------------------------------------------------------------
    Default implementations for AppConfigDir implementation.
  ---------------------------------------------------------------------}

Function DGetAppConfigDir(Global : Boolean) : String;

begin
  Result:=ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

Function DGetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  Result:=ExtractFilePath(ParamStr(0));
  If SubDir then
    Result:=IncludeTrailingPathDelimiter(Result+ApplicationName);
  Result:=Result+ApplicationName+ConfigExtension; 
end;

Function GetAppConfigFile(Global : Boolean) : String;

begin
  Result:=GetAppConfigFile(Global,False);
end;


{ ---------------------------------------------------------------------
    Fallback implementations for AppConfigDir implementation.
  ---------------------------------------------------------------------}
{
 If a particular OS does it different:
 - set the HAVE_OSCONFIG define before including sysutils.inc.
 - implement the functions.
 Default config assumes a DOS-like configuration.
}

{$ifndef HAS_OSCONFIG}
Function GetAppConfigDir(Global : Boolean) : String;

begin
  Result:=DGetAppConfigDir(Global);
end;

Function GetAppConfigFile(Global : Boolean; SubDir : Boolean) : String;

begin
  Result:=DGetAppConfigFile(Global,Subdir);
end;
{$endif}

{  
  $Log$
  Revision 1.1  2004-08-05 07:28:01  michael
  + Added getappconfigdir calls

}
