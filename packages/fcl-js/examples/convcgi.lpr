program convcgi;


uses sysutils, classes, cgutils, {fphttpapp, } fpcgi, httpdefs, httproute;

Procedure touch(fn : string);

begin
  With TStringList.Create do
    try
      Add(FN);
      SaveToFile('/tmp/touch-'+FN);
    finally
      Free;
    end;
end;

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
  Touch('i');
  S:=GetSettings;
  Touch('k');
  aList:=TstringList.Create;
  try
    aList.Add(S.BaseDir);
    aList.Add(S.cachefile);
    Alist.SaveToFile('/tmp/sett.txt');
    aList.Clear;
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
  Touch('g');
  S:=GetSettings;
  Touch('h');
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

procedure d;

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
    Writeln(aList.text);
//    aResponse.Content:=aList.text;
//    aResponse.ContentLength:=Length(aResponse.Content);
//    aResponse.ContentType:='application/javascript';
//    aResponse.SendResponse;
  finally
    aList.Free;
  end;
end;

begin
//  D;
//  exit;
  Touch('w0');
  if GetEnvironmentVariable('REQUEST_METHOD')='' then
    begin
    Touch('wa');
    if ParamCount=2 then
      CreateJSONFileList(Paramstr(1),ParamStr(2))
    else if ParamCount=1 then
      ConvertFile(Paramstr(1));
    Touch('wb');
    end
  else
    begin
    Touch('a');
    HTTPRouter.RegisterRoute('list',rmGet,@DoList);
    Touch('b');
    HTTPRouter.RegisterRoute('convert',rmAll,@DoConvertFile);
    Touch('c');
    // Application.Port:=8080;
    Application.Title:='Typescript to pascal converter';
    Touch('d');
    Application.Initialize;
    Touch('e');
    Application.Run;
    Touch('f');
    end
end.

