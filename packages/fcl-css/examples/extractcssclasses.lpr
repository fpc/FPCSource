{ Demo for CSS engine : extract class names from a list of CSS files

  Copyright (C) 2022- michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can
  also obtain it by writing to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

program extractcssclasses;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fpcssScanner, fpcssutils, fpcsstree;

type

  { TCSSClassNamesApplication }

  TCSSClassNamesApplication = class(TCustomApplication)
  private
    procedure OutputClassNames(const aFileName, aOutput: String);
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(aError : String); virtual;
  end;

{ TCSSClassNamesApplication }

procedure TCSSClassNamesApplication.OutputClassNames(const aFileName,aOutput : String);

Var
  L : TStringList;
  Util : TCSSUtils;

begin
  Util:=Nil;
  L:=TStringList.Create;
  try
    Util:=TCSSUtils.Create(Self);
    L.Sorted:=True;
    L.Duplicates:=dupIgnore;
    L.Options:=L.Options-[soTrailingLineBreak];
    if (aOutput<>'') and FileExists(aOutput) then
      L.LoadFromFile(aOutput);
    if HasOption('x','extended') then
      Util.ExtraScannerOptions:=[csoExtendedIdentifiers];
    Util.ExtractClassNames(aFileName,L);
    if aOutput='' then
      begin
      if L.Count>0 then
        Writeln(L.Text);
      end
    else
      L.SaveToFile(aOutput);
  finally
    Util.Free;
    L.Free;
  end;
end;

procedure TCSSClassNamesApplication.DoRun;

Const
  Opts = 'ho::x';
  LongOpts : Array of string = ('help','output::','extended');

var
  FN, FNOutput : String;
  ErrorMsg: String;
  files : TStringArray;

begin
  Terminate;
  ErrorMsg:=CheckOptions(Opts,LongOpts);
  if HasOption('h','help') or (ErrorMsg<>'') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  Files:=GetNonOptions(Opts,LongOpts);
  FNOutput:=GetOptionValue('o','output');
  if (FNOutput<>'') and FileExists(FNOutput) then
    DeleteFile(FNOutput);
  For FN in Files do
    OutputClassNames(FN,FNOUtput);
end;

constructor TCSSClassNamesApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCSSClassNamesApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCSSClassNamesApplication.Usage(aError: String);
begin
  if aError<>'' then
    Writeln('Error: ',aError);
  Writeln('Usage: ', ExeName, ' [options] FILES');
  Writeln('where options is one or more of:');
  Writeln('-h --help           This help message');
  Writeln('-o --output=FILE    Write class names to this file.');
  Writeln('-x --extended       Allow non-standard identifiers.');
  exitCode:=Ord(aError<>'');
end;

var
  Application: TCSSClassNamesApplication;
begin
  Application:=TCSSClassNamesApplication.Create(nil);
  Application.Title:='Extract CSS Application';
  Application.Run;
  Application.Free;
end.

