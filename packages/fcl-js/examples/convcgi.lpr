{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team
    Original author: Michael van Canneyt

    CGI TypeScript definitelytyped to pas2js code generator app

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program convcgi;

{ $DEFINE USEHTTPAPP}

uses
  typinfo, sysutils, classes, cgutils, tstopas,
  {$IFDEF USEHTTPAPP} fphttpapp{$ELSE} fpcgi {$ENDIF},
  httpdefs, httproute;

function GetBoolVal(R : TRequest; aName : String) : boolean;

Var
  S : String;

begin
  S:=R.QueryFields.Values[aName];
  Result:=(S='1') or (S='true') or (S='t');
end;

Procedure CreateJSONFileList(aDir : String; aFileName : string; aTextFileName : String = '' );

Var
  L,O : TStrings;
  I : integer;
  S : String;


begin
  O:=Nil;
  L:=TStringList.Create;
  try
    O:=TstringList.Create;
    GetDeclarationFileNames(aDir,aDir,L);
    TstringList(l).Sort;
    if aTextFileName<>'' then
      L.SaveToFile(aTextFileName);
    O.Add('var dtsfiles = [');
    for I:=0 to L.Count-1 do
      begin
      S:=L[i];
      S:=''''+StringReplace(S,'''','''''',[rfReplaceAll])+'''';
      if I<L.Count-1 then
        S:=S+',';
      O.Add('  '+S);
      end;
    O.Add('  ];');
    O.SaveToFile(aFileName);
  finally
    O.Free;
    L.Free;
  end;
end;

Procedure ConvertFile(const aFilename : string);

Var
  S : TSettings;
  aPas : TStrings;
  FN,aLine : string;

begin
  S:=GetSettings;
  aPas:=TStringList.Create;
  try
    if FileExists(aFileName) then
      FN:=ExtractRelativePath(S.BaseDir,aFilename)
    else
      FN:=aFileName;
    cgUtils.ConvertFile(S.BaseDir,FN,'','','',False,[],aPas,Nil);
    for aLine in aPas do
      writeln(aLine);
  Finally
    aPas.Free;
  end;
end;

procedure DoList(ARequest: TRequest; AResponse: TResponse);
Var
  S : TSettings;
  aList : TStrings;
  isRaw : Boolean;

begin
  S:=GetSettings;
  aList:=TstringList.Create;
  try
    IsRaw:=GetBoolVal(aRequest,'raw');
    if Not (FileExists(S.cachefile) and FileExists(S.rawcachefile)) then
      CreateJSONFileList(S.BaseDir,S.cachefile,S.rawcachefile);
    if isRaw then
      aList.LoadFromFile(S.rawcachefile)
    else
      aList.LoadFromFile(S.cachefile);
    aResponse.Content:=aList.text;
    aResponse.ContentLength:=Length(aResponse.Content);
    if IsRaw then
      aResponse.ContentType:='text/text'
    else
      aResponse.ContentType:='application/javascript';
    aResponse.SendResponse;
  finally
    aList.Free;
  end;
end;

function GetRequestOptions(ARequest: TRequest) : TConversionOptions;

Var
  T : TConversionOption;
  N : String;

begin
  Result:=[];
  For T in TConversionOption do
    begin
    N:=GetEnumName(TypeInfo(TConversionOption),Ord(T));
    if GetBoolVal(aRequest,N) then
      Include(Result,T);
    end;
end;

procedure DoConvertFile(ARequest: TRequest; AResponse: TResponse);

Var
  S : TSettings;
  aPas,aLog : TStrings;
  aliases,aExtraUnits,aFileName,aUnitName,aOutput : string;
  Opts : TConversionOptions;
  skipweb : boolean;

begin
  S:=GetSettings;
  aLog:=Nil;
  aPas:=TStringList.Create;
  try
    Opts:=GetRequestOptions(aRequest);
    aFileName:=aRequest.QueryFields.Values['file'];
    aUnitName:=aRequest.QueryFields.Values['unit'];
    aExtraUnits:=aRequest.QueryFields.Values['extraunits'];
    aliases:=aRequest.QueryFields.Values['aliases'];
    skipweb:=GetBoolVal(aRequest,'skipweb');
    if GetBoolVal(aRequest,'prependlog') then
      aLog:=TStringList.Create;
    cgUtils.ConvertFile(S.BaseDir,aFileName,aUnitName,aliases,aExtraUnits,skipweb,Opts,aPas,aLog);
    if Assigned(aLog) then
      aOutput:='(* // Conversion log:'+sLineBreak+aLog.Text+sLineBreak+'*)'+sLineBreak
    else
      aOutput:='';
    aOutput:=aOutput+aPas.text;
    aResponse.Content:=aOutput;
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.ContentType:='text/x-pascal';
    aResponse.SendResponse;
  Finally
    aPas.Free;
    aLog.Free;
  end;
end;

begin
  if GetEnvironmentVariable('REQUEST_METHOD')='' then
    begin
    if ParamCount=2 then
      CreateJSONFileList(Paramstr(1),ParamStr(2))
    else if ParamCount=3 then
      CreateJSONFileList(Paramstr(1),ParamStr(2),ParamStr(3))
    else if ParamCount=1 then
      ConvertFile(Paramstr(1));
    end
  else
    begin
    HTTPRouter.RegisterRoute('list',rmGet,@DoList);

    HTTPRouter.RegisterRoute('convert',rmAll,@DoConvertFile);
    {$IFDEF USEHTTPAPP}
    Application.Port:=8080;
    {$ENDIF}
    Application.Title:='Typescript to pascal converter';
    Application.Initialize;
    Application.Run;
    end
end.

