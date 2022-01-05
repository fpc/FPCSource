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
  sysutils, classes, cgutils,
  {$IFDEF USEHTTPAPP} fphttpapp{$ELSE} fpcgi {$ENDIF},
  httpdefs, httproute;

Procedure CreateJSONFileList(aDir : String; aFileName : string);

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
    cgUtils.ConvertFile(S.BaseDir,FN,'',[],aPas,Nil);
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

begin
  S:=GetSettings;
  aList:=TstringList.Create;
  try
    if Not FileExists(S.cachefile) then
      CreateJSONFileList(S.BaseDir,S.cachefile);
    aList.LoadFromFile(S.cachefile);
    aResponse.Content:=aList.text;
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.ContentType:='application/javascript';
    aResponse.SendResponse;
  finally
    aList.Free;
  end;
end;

procedure DoConvertFile(ARequest: TRequest; AResponse: TResponse);

Var
  S : TSettings;
  aPas : TStrings;
  aFileName : string;

begin
  S:=GetSettings;
  aPas:=TStringList.Create;
  try
    aFileName:=aRequest.QueryFields.Values['file'];
    cgUtils.ConvertFile(S.BaseDir,aFileName,'',[],aPas,Nil);
    aResponse.Content:=aPas.text;
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.ContentType:='text/x-pascal';
    aResponse.SendResponse;
  Finally
    aPas.Free;
  end;
end;

begin
  if GetEnvironmentVariable('REQUEST_METHOD')='' then
    begin
    if ParamCount=2 then
      CreateJSONFileList(Paramstr(1),ParamStr(2))
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

