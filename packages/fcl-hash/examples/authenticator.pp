{ Demo Google-authenticator compatible authenticator app

  Copyright (C) 2022 Michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can
  also obtain it by writing to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}
{$h+}
uses sysutils, classes, onetimepass, inifiles, custapp;

Type
  TMode = (mError,mHelp,mAdd,mDelete,mPrint,mGenerate,mList,mCheck);


  { TAuthenticatorApplication }

  TAuthenticatorApplication = Class(TCustomApplication)
  Private
    FIni : TMemIniFile;
    procedure CheckKey(aName, aCode: String);
    function getMode: TMode;
    procedure ListKeys;
    procedure PrintKey(aKey: String);
    procedure Usage(const aError: String);
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure DoRun; override;
  end;

Const
  SKeys = 'Keys';
  Need : array[TMode] of Integer = (0,0,2,1,1,0,0,2);

constructor TAuthenticatorApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FIni:=TMemIniFile.Create(GetAppConfigFile(False));
end;

destructor TAuthenticatorApplication.Destroy;
begin
  FreeAndNil(FIni);
  inherited Destroy;
end;

Procedure TAuthenticatorApplication.Usage(const aError : String);

begin
  if (aError<>'') then
    Writeln('Error: ',aError);
  Writeln('Usage: ',ExtractFileName(ParamStr(0)),' [-a|-d|-h|-p|-g|-c|-l] [name [key|Value]');
  Writeln('If no options are specified, print key code');
  Writeln('-h --help      This help text');
  Writeln('-a --add       Add key with given name and key value');
  Writeln('-d --remove    Remove key with given name');
  Writeln('-g --generate  Generate and print new key');
  Writeln('-l --list      List known keys');
  Writeln('-c --check     Check code against key for given name');
  Writeln('config file: ',GetAppConfigFile(False));
  ExitCode:=Ord(AError<>'')
end;

Function TAuthenticatorApplication.getMode : TMode;

var
  aMode : TMode;

begin
  aMode:=mPrint;
  if HasOption('h','help') then
    aMode:=mHelp
  else if HasOption('a','add') then
    aMode:=mAdd
  else if HasOption('g','generate') then
    aMode:=mGenerate
  else if HasOption('c','check') then
    aMode:=mCheck
  else if HasOption('r','remove') then
    aMode:=mDelete
  else if HasOption('l','list') then
    aMode:=mList;
  result:=aMode;
end;

Procedure TAuthenticatorApplication.CheckKey(aName,aCode : String);

Var
  S : String;
  aCount :  Integer;

begin
  S:=FIni.ReadString(SKeys,aName,'');
  if S='' then
    begin
    Writeln('No such key : ',aName);
    ExitCode:=1;
    end
  else
    begin
    if TOTPValidate(S,StrToIntDef(aCode,-1),1,aCount) then
      Writeln('Code OK')
    else
      begin
      Writeln('Code wrong');
      ExitCode:=1;
      end;
    end;
end;

Procedure TAuthenticatorApplication.PrintKey(aKey : String);

Var
  S : String;

begin
  S:=FIni.ReadString(SKeys,aKey,'');
  if S='' then
    begin
    Writeln('No such key : ',S);
    ExitCode:=1;
    end
  else
    Writeln('Token: ',TOTPGenerateToken(S));
end;

Procedure TAuthenticatorApplication.ListKeys;

Var
  L : TStrings;
  I : Integer;
  N,K : String;

begin
  L:=TStringList.Create;
  try
    Fini.ReadSectionValues(SKeys,L);
    Writeln('Known keys: ');
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,K);
      Writeln(N,' : ',K);
      end;
  finally
    L.Free;
  end;
end;

Procedure TAuthenticatorApplication.DoRun;

Const
  Opts : String ='harpgcl';
  LongOpts : Array of string = ('help','add','remove','print','generate','check','list');

Var
  aErr : String;
  aMode : TMode;
  NonArgs : TStringArray;

begin
  Terminate;
  aMode:=mError;
  aErr:=CheckOptions(Opts,LongOpts);
  NonArgs:=GetNonOptions(Opts,LongOpts);
  if (aErr='') then
    begin
    aMode:=GetMode;
    if aMode in [mAdd,mDelete,mGenerate] then
      if Length(NonArgs)<>Need[aMode] then
        begin
        aErr:=Format('Need %d arguments, got %d',[Need[aMode],Length(NonArgs)]);
        aMode:=mError;
        end;
    end;
  Case aMode of
    mError,mHelp:
      Usage(aErr);
    mAdd:
      begin
      FIni.WriteString(SKeys,NonArgs[0],NonArgs[1]);
      Fini.UpdateFile;
      end;
    mDelete:
      begin
      FIni.DeleteKey(SKeys,NonArgs[0]);
      Fini.UpdateFile;
      end;
    mPrint:
      begin
      if length(NonArgs)>0 then
        PrintKey(NonArgs[0])
      else
        Usage('');
      end;
    mGenerate:
      Writeln(TOTPSharedSecret());
    mCheck:
      begin
      CheckKey(NonArgs[0],NonArgs[1]);
      end;
    mList:
      ListKeys;
  end;
end;

begin
  CustomApplication:=TAuthenticatorApplication.Create(Nil);
  CustomApplication.Initialize;
  CustomApplication.Run;
  CustomApplication.Free;

end.
