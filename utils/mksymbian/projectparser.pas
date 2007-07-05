{
projectparser.pas

Parses the project file

Copyright (C) 2006-2007 Felipe Monteiro de Carvalho

This file is part of MkSymbian build tool.

MkSymbian is free software;
you can redistribute it and/or modify it under the
terms of the GNU General Public License version 2
as published by the Free Software Foundation.

MkSymbian is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE. See the GNU General Public License for more details.

Please note that the General Public License version 2 does not permit
incorporating MkSymbian into proprietary programs.
}
unit projectparser;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, IniFiles,
  constants;

type

  { TProject }

  TProject = class(TObject)
  public
    opts: TMkSymbianOptions;
    { Main section }
    ExeName, Language: string;
    { FPC section }
    CompilerPath, RTLUnitsDir: string;
    { UIDs section }
    UID2, UID3: string;
    { Files section }
    MainSource, MainSourceNoExt, MainResource: string;
  public
    procedure ParseFile;
  end;

var
  vProject: TProject;

implementation

{ TProject }

{*******************************************************************
*  TProject.ParseFile ()
*
*  DESCRIPTION:    Parses the project file
*
*  PARAMETERS:     None
*
*  RETURNS:        Nothing
*
*******************************************************************}
procedure TProject.ParseFile;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(opts.ProjectFile);
  try
    ExeName := IniFile.ReadString(STR_PRJ_Files, STR_PRJ_ExeName, 'default.exe');
    Language := IniFile.ReadString(STR_PRJ_Files, STR_PRJ_Language, 'Pascal');

    CompilerPath := IniFile.ReadString(STR_PRJ_FPC, STR_PRJ_CompilerPath, 'C:\Programas\fpc21\compiler\ppc386.exe');
    RTLUnitsDir := IniFile.ReadString(STR_PRJ_FPC, STR_PRJ_RTLUnitsDir, 'C:\Programas\fpc21\rtl\units\i386-symbian\');

    UID2 := IniFile.ReadString(STR_PRJ_UIDs, STR_PRJ_UID2, '0x100039CE');
    UID3 := IniFile.ReadString(STR_PRJ_UIDs, STR_PRJ_UID3, '0xE1000002');

    MainSource := IniFile.ReadString(STR_PRJ_Files, STR_PRJ_MainSource, 'default.pas');
    MainSourceNoExt := ExtractFileExt(MainSource);
    MainResource := IniFile.ReadString(STR_PRJ_Files, STR_PRJ_MainResource, 'default.rss');
  finally
    IniFile.Free;
  end;
end;

end.

