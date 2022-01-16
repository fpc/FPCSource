{ JSON 2 Pascal class converter

  Copyright (C) 2016 Michael Van Canneyt (michael@freepascal.org)

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can
  also obtain it by writing to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

program json2pas;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, jsonparser, fpjsontopas;

type

  { TJSON2PasApplication }

  TJSON2PasApplication = class(TCustomApplication)
  Private
    FGen : TJSONToPascal;
    FIN : TFileStream;
    FON : String;
    procedure ProcessOptions;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Msg: String); virtual;
  end;

{ TJSON2PasApplication }

procedure TJSON2PasApplication.ProcessOptions;

var
  UN,J: String;

begin
  UN:='';
  if HasOption('d','generate-delphi') then
    FGen.Options:=FGen.Options+[jpoDelphiJSON];
  if HasOption('l','generate-load') then
    FGen.Options:=FGen.Options+[jpoGenerateLoad];
  if HasOption('s','generate-save') then
    FGen.Options:=FGen.Options+[jpoGenerateSave];
  if HasOption('c','load-ignores-case') then
    FGen.Options:=FGen.Options+[jpoLoadCaseInsensitive];
  if HasOption('e','use-setter') then
    FGen.Options:=FGen.Options+[jpoUseSetter];
  if HasOption('r','load-with-error') then
    FGen.Options:=FGen.Options+[jpoUnknownLoadPropsError];
  if HasOption('u','unit') then
    UN:=GetOptionValue('u','unit');
  J:=GetOptionValue('i','input');
  if (J<>'') then
    begin
    FIN:=TFileStream.Create(J,fmOpenRead or fmShareDenyWrite);
    FGen.JSONStream:=FIN;
    end
  else
    FGen.JSON:=getOptionValue('j','json');
  if HasOption('o','output') then
    FON:=GetOptionValue('o','output');
  if (FON='') then
    begin
    if Assigned(FIN) then
      FON:=ChangeFileExt(FIN.FileName,'')
    else if (UN<>'') then
      FON:=UN;
    if (UN='') then
      UN:=ChangeFileExt(ExtractFileName(FON),'');
    end
  else
    UN:=ChangeFileExt(ExtractFileName(FON),'');
  if (ExtractFileExt(FON)='') then
    if (jpoDelphiJSON in Fgen.Options) then
      FON:=FON+'.pas'
    else
      FON:=FON+'.pp';
  FGen.DestUnitName:=UN;
  If HasOption('n','classname') then
    FGen.PropertyMap.AddPath('',GetOptionValue('n','classname'));
end;

procedure TJSON2PasApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:j:u:o:n:dlscer', ['help','input:','output:','unit:','json:','generate-delphi','generate-load','generate-save','load-ignores-case','use-setter','classname:','load-with-error']);
  if (ErrorMsg<>'') or HasOption('h', 'help') then
    Usage(ErrorMsg);
  ProcessOptions;
  FGen.Execute;
  FGen.Code.SaveToFile(FON);
  Terminate;
end;

constructor TJSON2PasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FGen:=TJSONToPascal.Create(Self);
end;

destructor TJSON2PasApplication.Destroy;
begin
  FreeAndNil(FGen);
  FreeAndNil(FIN);
  inherited Destroy;
end;

procedure TJSON2PasApplication.Usage(Msg : String);
begin
  if (Msg<>'') then
    Writeln('Error : ',Msg);
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or more of:');
// 'hi:j:u:o:n:dlscer', ['help','input:','output:','unit:','json:','generate-delphi','generate-load','generate-save','load-ignores-case','use-setter','classname:','load-with-error']);
  Writeln('-h --help              Show this help');
  Writeln('-i --input=file        Use file as JSON input. The file must contain a valid JSON object.');
  Writeln('-i --json=json         Use json as JSON input. The value must contain a valid JSON object.');
  Writeln('-u --unit=name         Use name as the name of the unit.');
  Writeln('                       If no output file is specified, this sets the name');
  Writeln('-o --output=file       Set the output filename. Sets the unit name if none is given');
  Writeln('-d --generate-delphi   Use Delphi 10 JSON routines');
  Writeln('-l --generate-load     Create LoadFromJSON routine');
  Writeln('-s --generate-save     Create SaveToJSON routine');
  Writeln('-c --load-ignores-case The LoadFromJSON routine will ignore case');
  Writeln('-e --use-setter        Property setters use a routine instead of writing to field');
  Writeln('-n --classname=name    Set the name of the top-level class (default: TMyObject)');
  Writeln('-r --load-with-error   Load will raise an exception if an unknown property is met');
  Halt(Ord(Msg<>''));
end;

var
  Application: TJSON2PasApplication;
begin
  Application:=TJSON2PasApplication.Create(nil);
  Application.Title:='JSON to Pascal code generator';
  Application.Run;
  Application.Free;
end.

