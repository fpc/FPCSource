{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Skeleton XML description file generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


program MakeSkel;

uses
  SysUtils, Classes, Gettext,
  dGlobals, PasTree, PParser;

resourcestring
  STitle = 'MakeSkel - FPDoc skeleton XML description file generator';
  SCopyright = '(c) 2000 - 2003 Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org';
  SCmdLineHelp = 'See documentation for usage.';
  SCmdLineInvalidOption = 'Ignoring unknown option "%s"';
  SNoPackageNameProvided = 'Please specify a package name with --package=<name>';
  SDone = 'Done.';

type
  TCmdLineAction = (actionHelp, actionConvert);

  TSkelEngine = class(TFPDocEngine)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility :TPasMemberVisibility): TPasElement; override;
  end;

const
  CmdLineAction: TCmdLineAction = actionConvert;
  OSTarget: String = {$I %FPCTARGETOS%};
  CPUTarget: String = {$I %FPCTARGETCPU%};

var
  InputFiles, DescrFiles: TStringList;
  DocLang: String;
  Engine: TSkelEngine;
  DisableErrors,
  DisableSeealso,
  DisableArguments,
  DisableProtected,
  DisablePrivate,
  DisableFunctionResults: Boolean;
  
  EmitClassSeparator: Boolean;
  PackageName, OutputName: String;
  f: Text;


function TSkelEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility : TPasMemberVisibility): TPasElement;
begin
  Result := AClass.Create(AName, AParent);

  if AClass.InheritsFrom(TPasModule) then
    CurModule := TPasModule(Result);

  if Result.ClassType = TPasModule then
  begin
    WriteLn(f);
    WriteLn(f, '<!--');
    WriteLn(f, '  ====================================================================');
    WriteLn(f, '    ', Result.Name);
    WriteLn(f, '  ====================================================================');
    WriteLn(f, '-->');
    WriteLn(f);
    WriteLn(f, '<module name="', Result.Name, '">');
    WriteLn(f, '<short></short>');
    WriteLn(f, '<descr>');
    WriteLn(f, '</descr>');
  end else if Assigned(AParent) and (Length(AName) > 0) and
    (not DisableArguments or (Result.ClassType <> TPasArgument)) and
    (not DisableFunctionResults or (Result.ClassType <> TPasResultElement)) and
    (not DisablePrivate or (AVisibility<>visPrivate)) and
    (not DisableProtected or (AVisibility<>visProtected)) then
  begin
    WriteLn(f);
    if EmitClassSeparator and (Result.ClassType = TPasClassType) then
    begin
      WriteLn(f, '<!--');
      WriteLn(f, '  ********************************************************************');
      WriteLn(f, '    ', Result.PathName);
      WriteLn(f, '  ********************************************************************');
      WriteLn(f, '-->');
      WriteLn(f);
    end;
    Writeln(F,'<!-- ', Result.ElementTypeName,' Visibility: ',VisibilityNames[AVisibility], ' -->');
    WriteLn(f,'<element name="', Result.FullName, '">');
    WriteLn(f, '<short></short>');
    WriteLn(f, '<descr>');
    WriteLn(f, '</descr>');
    if not DisableErrors then
    begin
      WriteLn(f, '<errors>');
      WriteLn(f, '</errors>');
    end;
    if not DisableSeealso then
    begin
      WriteLn(f, '<seealso>');
      WriteLn(f, '</seealso>');
    end;
    WriteLn(f, '</element>');
  end;
end;


procedure InitOptions;
begin
  InputFiles := TStringList.Create;
  DescrFiles := TStringList.Create;
  Engine := TSkelEngine.Create;
end;

procedure FreeOptions;
begin
  Engine.Free;
  DescrFiles.Free;
  InputFiles.Free;
end;

procedure ParseOption(const s: String);

  procedure AddToFileList(List: TStringList; const FileName: String);
  var
    f: Text;
    s: String;
  begin
    if Copy(FileName, 1, 1) = '@' then
    begin
      Assign(f, Copy(FileName, 2, Length(FileName)));
      Reset(f);
      while not EOF(f) do
      begin
        ReadLn(f, s);
	List.Add(s);
      end;
      Close(f);
    end else
      List.Add(FileName);
  end;

var
  i: Integer;
  Cmd, Arg: String;
begin
  if (s = '-h') or (s = '--help') then
    CmdLineAction := actionHelp
  else if s = '--disable-arguments' then
    DisableArguments := True
  else if s = '--disable-errors' then
    DisableErrors := True
  else if s = '--disable-function-results' then
    DisableFunctionResults := True
  else if s = '--disable-seealso' then
    DisableSeealso := True
  else if s = '--disable-private' then
    DisablePrivate := True
  else if s = '--disable-protected' then
    begin
    DisableProtected := True;
    DisablePrivate :=True;
    end
  else if s = '--emitclassseparator' then
    EmitClassSeparator := True
  else
  begin
    i := Pos('=', s);
    if i > 0 then
    begin
      Cmd := Copy(s, 1, i - 1);
      Arg := Copy(s, i + 1, Length(s));
    end else
    begin
      Cmd := s;
      SetLength(Arg, 0);
    end;
    if (Cmd = '-i') or (Cmd = '--input') then
      AddToFileList(InputFiles, Arg)
    else if (Cmd = '-l') or (Cmd = '--lang') then
      DocLang := Arg
    else if (Cmd = '-o') or (Cmd = '--output') then
      OutputName := Arg
    else if Cmd = '--package' then
      PackageName := Arg
    else
      WriteLn(StdErr, Format(SCmdLineInvalidOption, [s]));
  end;
end;

procedure ParseCommandLine;
var
  i: Integer;
begin
  for i := 1 to ParamCount do
    ParseOption(ParamStr(i));
end;


var
  i: Integer;
  Module: TPasModule;
begin
{$IFDEF Unix}
  gettext.TranslateResourceStrings('/usr/local/share/locale/%s/LC_MESSAGES/makeskel.mo');
{$ELSE}
  gettext.TranslateResourceStrings('intl/makeskel.%s.mo');
{$ENDIF}

  WriteLn(STitle);
  WriteLn(SCopyright);
  WriteLn;

  InitOptions;
  ParseCommandLine;

  if CmdLineAction = actionHelp then
    WriteLn(SCmdLineHelp)
  else
  begin
    // Action is to create the XML skeleton

    if Length(PackageName) = 0 then
    begin
      WriteLn(SNoPackageNameProvided);
      Halt(2);
    end;

    Engine.SetPackageName(PackageName);

    // Translate internal documentation strings
    if Length(DocLang) > 0 then
      TranslateDocStrings(DocLang);

    Assign(f, OutputName);
    Rewrite(f);

    WriteLn(f, '<?xml version="1.0" encoding="ISO8859-1"?>');
    WriteLn(f, '<fpdoc-descriptions>');
    WriteLn(f, '<package name="', PackageName, '">');

    // Process all source files
    for i := 0 to InputFiles.Count - 1 do
    begin
      Module := ParseSource(Engine, InputFiles[i], OSTarget, CPUTarget);
      try
	WriteLn(f, '</module> <!-- ', Module.Name, ' -->');
      finally
        Module.Free;
      end;
    end;

    WriteLn(f, '</package>');
    WriteLn(f, '</fpdoc-descriptions>');

    Close(f);
  end;

  FreeOptions;

  WriteLn(SDone);
end.


{
  $Log$
  Revision 1.2  2003-03-28 13:01:36  michael
  + Patch from Charlie/iNQ to work with new scanner/parser

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS

  Revision 1.7  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.6  2002/10/12 17:00:46  michael
  + Changes to be able to disable private/protected nodes in skeleton

  Revision 1.5  2002/10/11 18:41:50  sg
  * Now requires a package name on the command line via "--package=<name>",
    to match the recent changes in the engine
  * translation files are now searched at the usual location on Linux

  Revision 1.4  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.3  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.2  2001/07/27 10:21:42  sg
  * Just a new, improved version ;)
    (detailed changelogs will be provided again with the next commits)

  Revision 1.1  2000/10/28 20:15:26  sg
  * Many internal architectural improvements (especially linking)
  * Improved writers

}
