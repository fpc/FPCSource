{ Demo for CSS engine : minimize a CSS file

  Copyright (C) 2022- michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can
  also obtain it by writing to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
program cssmin;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, fpcssutils;

type

  { TCSSMinimizerApplication }

  TCSSMinimizerApplication = class(TCustomApplication)
  protected
    FInputFile,
    FOutputFile : String;

    procedure DoRun; override;
  public
    Function ParseOptions : String;
    procedure Minimize(const aInputFile,aOutputFile : String);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Msg : string); virtual;
  end;

{ TCSSMinimizerApplication }

procedure TCSSMinimizerApplication.DoRun;

var
  ErrorMsg: String;

begin
  Terminate;
  ErrorMsg:=CheckOptions('hi:o:', ['help','input:','output:']);
  if (ErrorMsg='') and not HasOption('h','help') then
    ErrorMsg:=ParseOptions;
  if HasOption('h','help') or (ErrorMsg<>'') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  Minimize(FInputFile,FOutputFile);
end;

function TCSSMinimizerApplication.ParseOptions: String;
begin
  Result:='';
  FInputFile:=GetOptionValue('i','input');
  FOutputFile:=GetOptionValue('o','output');
  if FInputFile='' then
    Exit('Need input file');
  if FOutputFile='' then
    FOutputFile:=ChangeFileExt(FInputFile,'.min.css');
end;

procedure TCSSMinimizerApplication.Minimize(const aInputFile, aOutputFile: String);

Var
  SIn,SOut : TMemoryStream;
  Util : TCSSUtils;

begin
  SOut:=Nil;
  Util:=Nil;
  SIn:=TMemoryStream.Create;
  try
    Sin.LoadFromFile(aInputFile);
    Sout:=TmemoryStream.Create;
    Util:=TCSSUtils.Create(Self);
    Util.Minimize(Sin,Sout);
    Sout.SaveToFile(aOutputFile);
  finally
    Sin.Free;
    SOut.Free;
    Util.Free;
  end;
end;

constructor TCSSMinimizerApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCSSMinimizerApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCSSMinimizerApplication.Usage(Msg: string);
begin
  writeln('Usage: ', ExeName, ' -h');
  exitcode:=Ord(Msg<>'')
end;

var
  Application: TCSSMinimizerApplication;
begin
  Application:=TCSSMinimizerApplication.Create(nil);
  Application.Title:='CSS Minimizer Application';
  Application.Run;
  Application.Free;
end.

