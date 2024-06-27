program restool;

{$R program.res}

uses
  custapp, sysutils, classes, resource, elfreader, resreader, coffreader, machoreader, dfmreader,
  bitmapresource, stringtableresource, versionresource, groupiconresource, groupcursorresource,
  acceleratorsresource;

Type
   TRunMode = (rmList,rmExtract);

   { TRestool }

   TRestool = Class(TCustomApplication)
   private
     FRunMode : TRunMode;
     FResfile : TResources;
     FinputFile : String;
     FDestFile : String;
     FResName,FResType : String;
     FResIndex : integer;
     function ProcessOptions: Boolean;
     procedure Usage(const aErr: String);
     procedure WriteResource(Res: TAbstractResource; const aDestFile: String);
   Protected
     procedure doRun; override;
     procedure dumpresourcefile(const aFileName: String; var aDest: Text);
     procedure ExtractResource(const aDestFile: String; aIdx: Integer);
     procedure ExtractResource(const aDestFile, aType, aName: String);
   public
     constructor Create(aOwner : TComponent); override;
     destructor destroy; override;
     class function restypename(aID: Integer; AddNumeric: Boolean): string;
     class function KnownTypeNameToID(aType : String) : Integer;
   end;

constructor TRestool.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FResfile:=TResources.Create;
end;

destructor TRestool.destroy;
begin
  FreeAndNil(FResfile);
  inherited destroy;
end;

class function TRestool.restypename(aID : Integer; AddNumeric : Boolean) : string;

begin
  case aID of
    RT_CURSOR       : Result:='RT_CURSOR';  //Hardware-dependent cursor resource.
    RT_BITMAP       : Result:='RT_BITMAP';  //Bitmap resource.
    RT_ICON         : Result:='RT_ICON';  //Hardware-dependent icon resource.
    RT_MENU         : Result:='RT_MENU';  //Menu resource.
    RT_DIALOG       : Result:='RT_DIALOG';  //Dialog box.
    RT_STRING       : Result:='RT_STRING';  //String-table entry.
    RT_FONTDIR      : Result:='RT_FONTDIR';  //Font directory resource.
    RT_FONT         : Result:='RT_FONT';  //Font resource.
    RT_ACCELERATOR  : Result:='RT_ACCELERATOR';  //Accelerator table.
    RT_RCDATA       : Result:='RT_RCDATA';  //Application-defined resource (raw data).
    RT_MESSAGETABLE : Result:='RT_MESSAGETABLE';  //Message-table entry.
    RT_GROUP_CURSOR : Result:='RT_GROUP_CURSOR';  //Hardware-independent cursor resource.
    RT_GROUP_ICON   : Result:='RT_GROUP_ICON';  //Hardware-independent icon resource.
    RT_VERSION      : Result:='RT_VERSION';  //Version resource.
    RT_DLGINCLUDE   : Result:='RT_DLGINCLUDE';  //Never present in compiled form
    RT_PLUGPLAY     : Result:='RT_PLUGPLAY';  //Plug and Play resource.
    RT_VXD          : Result:='RT_VXD';  //VXD.
    RT_ANICURSOR    : Result:='RT_ANICURSOR';  //Animated cursor.
    RT_ANIICON      : Result:='RT_ANIICON';  //Animated icon.
    RT_HTML         : Result:='RT_HTML';  //HTML.
    RT_MANIFEST     : Result:='RT_MANIFEST';  //Microsoft Windows XP: Side-by-Side Assembly XML Manifest.
    RT_DLGINIT      : Result:='RT_DLGINIT'; //Never present in compiled form
  else
    Result:='';
  end;
  if Result='' then
    Result:=IntToStr(aID)
  else if AddNumeric then
    Result:=Result+' ('+IntToStr(aId)+')';
end;

class function TRestool.KnownTypeNameToID(aType: String): Integer;
begin
  case aType of
    'RT_CURSOR'       : Result:=RT_CURSOR;  //Hardware-dependent cursor resource.
    'RT_BITMAP'       : Result:=RT_BITMAP;  //Bitmap resource.
    'RT_ICON'         : Result:=RT_ICON;  //Hardware-dependent icon resource.
    'RT_MENU'         : Result:=RT_MENU;  //Menu resource.
    'RT_DIALOG'       : Result:=RT_DIALOG;  //Dialog box.
    'RT_STRING'       : Result:=RT_STRING;  //String-table entry.
    'RT_FONTDIR'      : Result:=RT_FONTDIR;  //Font directory resource.
    'RT_FONT'         : Result:=RT_FONT;  //Font resource.
    'RT_ACCELERATOR'  : Result:=RT_ACCELERATOR;  //Accelerator table.
    'RT_RCDATA'       : Result:=RT_RCDATA;  //Application-defined resource (raw data).
    'RT_MESSAGETABLE' : Result:=RT_MESSAGETABLE;  //Message-table entry.
    'RT_GROUP_CURSOR' : Result:=RT_GROUP_CURSOR;  //Hardware-independent cursor resource.
    'RT_GROUP_ICON'   : Result:=RT_GROUP_ICON;  //Hardware-independent icon resource.
    'RT_VERSION'      : Result:=RT_VERSION;  //Version resource.
    'RT_DLGINCLUDE'   : Result:=RT_DLGINCLUDE;  //Never present in compiled form
    'RT_PLUGPLAY'     : Result:=RT_PLUGPLAY;  //Plug and Play resource.
    'RT_VXD'          : Result:=RT_VXD;  //VXD.
    'RT_ANICURSOR'    : Result:=RT_ANICURSOR;  //Animated cursor.
    'RT_ANIICON'      : Result:=RT_ANIICON;  //Animated icon.
    'RT_HTML'         : Result:=RT_HTML;  //HTML.
    'RT_MANIFEST'     : Result:=RT_MANIFEST;  //Microsoft Windows XP: Side-by-Side Assembly XML Manifest.
    'RT_DLGINIT'      : Result:=RT_DLGINIT; //Never present in compiled form
  else
    Result:=-1;
  end;
end;

procedure TRestool.dumpresourcefile(const aFileName: String; var aDest : Text);

Var
  res : TAbstractResource;
  aType,aName : string;
  i : Integer;

begin
  FResFile.LoadFromFile(aFileName);
  Writeln(aDest,'File ',aFileName,' contains ',FResFile.Count,' resources:');
  For I:=0 to FResFile.Count-1 do
    begin
    Res:=FResFile.Items[i];
    aName:=Res.Name.Name;
    if res.name.DescType=dtID then
      aName:='#'+aName;
    aType:=ResTypeName(res._Type.ID,True);
    Writeln(aDest,I:3,' : Type: ',aType,' name: ',aName);
    end;
end;

procedure TRestool.WriteResource(Res : TAbstractResource; const aDestFile : String);

var
  aCount : Int64;
  S : TStream;
begin
  if Res is TGroupCursorResource then
    S:=TGroupIconResource(Res).ItemData
  else if Res is TGroupIconResource then
    S:=TGroupIconResource(Res).ItemData
  else if Res is TBitmapResource then
    S:=TBitmapResource(Res).BitmapData
  else
    S:=Res.RawData;
  With TFileStream.Create(aDestFile,fmCreate) do
    try
      aCount:=CopyFrom(S,S.Size);
      Writeln(stdErr,'Write ',aCount,' bytes from resource data to file: ',aDestFile);
    finally
      Free;
    end;
end;

function TRestool.ProcessOptions: Boolean;

const
  Short = 'h:i:n:t:m:xlo:';
  Long : Array of string = ('help','index:','name:','type:','mode:','extract','output:','list');

var
  RM,Idx,Err : String;
  S : Array of string;

begin
  Result:=False;
  Err:=CheckOptions(Short,Long);
  if (Err<>'') or HasOption('h','help') then
    begin
    Usage(Err);
    Exit;
    end;
  S:=GetNonOptions(Short,long);
  FDestFile:=GetOptionValue('o','output');
  if Length(S)>0 then
    FinputFile:=S[0]
  else if Length(S)>1 then
    FDestFile:=S[1];
  if HasOption('x','extract') then
    FRunMode:=rmExtract
  else if HasOption('l','list') then
    FRunMode:=rmList;
  if HasOption('m','mode') then
    begin
    rm:=GetOptionValue('m','mode');
    case RM of
      'list' : FRunMode:=rmList;
      'extract' : FRunMode:=rmExtract;
    else
      Usage('Invalid run mode: '+RM);
      Exit;
    end;
    end;
  if FRunMode=rmExtract then
    begin
    if HasOption('i','index') then
      begin
      Idx:=GetOptionValue('i','index');
      FResIndex:=StrToIntDef(Idx,-1);
      if FResIndex=-1 then
        begin
        Usage('Invalid (not numerical) value for index: '+Idx);
        Exit;
        end;
      end
    else
      begin
      FResIndex:=-1;
      FResName:=GetOptionValue('n','name');
      FResType:=GetOptionValue('t','type');
      if (FResName='') or (FResType='') then
        begin
        Usage(Format('Need both type (got: "%s") and name (got: "%s") for extracting a resource. ',[fResType,fresName]));
        Exit;
        end;
      end;
    if FDestFile='' then
       if FResIndex>0 then
         FDestFile:=ChangeFileExt(FInputFile,Format('-resource-%d.dat',[FResIndex]))
       else
         begin
         RM:=FResName;
         if RM[1]='#' then
           RM:=Copy(RM,2);
         FDestFile:=ChangeFileExt(FInputFile,Format('-resource-%s-%s.dat',[FResType,RM]))
         end;
    end;
  Result:=True;
end;

procedure TRestool.Usage(const aErr: String);

begin
  If (aErr<>'') then
    Writeln('Error: ',aErr);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options] [inputfile [outputfile]]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help            This message');
  Writeln('-i --index=IDX       Index of resource to extract.');
  Writeln('-n --name=NAME       Name of resource to extract');
  Writeln('-t --type=TYPE       Type of resource to extract. Known type names (RT_RCDATA etc.) can be used.');
  Writeln('-l --list            List resources in file.');
  Writeln('-x --extract         Extract a resource from file. Specify -i or -n and -t options');
  Writeln('-o --output=FILE     Filename to extract a resource to (or specify the name as the second non-option argument.');
  Writeln('                     If no filename is given, a default name is constructed from either index or Filename to extract a resource to (or specify the name as the second non-option argument.');

  (*
  Short = 'h:i:n:t:m:xlo:';
  Long : Array of string = ('help','index:','name:','type:','mode:','extract','output:','list');

  *)
  ExitCode:=Ord(aErr<>'');
end;

procedure TRestool.doRun;

var
  FOut : Text;

begin
  Terminate;
  FInputFile:=ParamStr(0);
  if not ProcessOptions then
    Exit;
  Case FRunMode of
    rmList :
      begin
      if (FDestFile='') then
        dumpresourcefile(FInputFile,Output)
      else
        begin
        AssignFile(Fout,FDestFile);
        Rewrite(Fout);
        dumpresourcefile(FInputFile,Fout);
        CloseFile(Fout);
        end
      end;
    rmExtract:
      begin
      FResfile.LoadFromFile(FInputFile);
      if FResIndex>=0 then
        ExtractResource(FDestFile,FResIndex)
      else
        ExtractResource(FDestFile,FResType,FresName)
      end;
  end;
end;

procedure TRestool.ExtractResource(const aDestFile: String; aIdx: Integer);

var
  Res : TAbstractResource;

begin
  if (aIdx<0) or (aIdx>=FresFile.Count) then
    begin
    ExitCode:=2;
    Writeln(stdErr,'Resource with index ',aIdx,' not found. Max value for index: ',FresFile.Count-1);
    Exit;
    end;
  Res:=FResfile.Items[aIdx];
  WriteResource(Res,aDestFile);
end;

procedure TRestool.ExtractResource(const aDestFile, aType, aName: String);

var
  I : integer;
  aTypeId,aID : TResID;
  Res : TAbstractResource;

begin
  aTypeID:=0;
  aID:=0;
  I:=StrToIntDef(aType,-1);
  if (I<0) then
    I:=KnownTypeNameToID(aType);
  if I>=0 then
    aTypeID:=I;
  I:=StrToIntDef(aName,-1);
  if (I>=0) then
    aID:=I;
  if aTypeID>0 then
    begin
    if aID>0 then
      Res:=FResfile.Find(aTypeID,aID)
    else
      Res:=FResfile.Find(aTypeID,aName)
    end
  else
    begin
    if aID>0 then
      Res:=FResfile.Find(aType,aID)
    else
      Res:=FResfile.Find(aType,aName);
    end;
  if not Assigned(Res) then
    begin
    ExitCode:=2;
    Writeln(stdErr,'Resource with type ',aType,' and name ',aName,' not found.');
    end;
  WriteResource(Res,aDestFile);
end;

Var
  Application : TResTool;

begin
  Application:=TResTool.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

