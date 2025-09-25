program json2entities;

uses fpjson, jsonparser, sysutils, classes;

function GetJSONData(const aFileName : string) : TJSONObject;
var
  lData : TJSONData;
  lFile : TFileStream;
begin
  lFile:=TFileStream.Create(aFileName,fmOpenRead);
  try
    lData:=GetJSON(lFile);
    if not (lData is TJSONObject) then
      begin
      lData.Free;
      Raise EJSON.Create('Not a JSON object');
      end;
    Result:=TJSONObject(lData);
  finally
    lFile.Free;
  end;
end;

Procedure GetUnitEnd(aUnit : TStrings);
begin
  With aUnit do
    begin
    Add('');
    Add('implementation');
    Add('');
    Add('end.');
    end;
end;

Procedure GetUnitStart(aUnit : TStrings; const aFileName : string);

begin
  With aUnit do
    begin
    Add('unit %s;',[ChangeFileExt(ExtractFileName(aFileName),'')]);
    Add('');
    Add('// see also https://html.spec.whatwg.org/entities.json');
    Add('');
    Add('interface');
    Add('');
    Add('Type');
    Add('  THTMLEntityDef = record');
    Add('    e : AnsiString;');
    Add('    u : Unicodestring;');
    Add('  end;');
    Add('  THTMLEntityDefList = Array of THTMLEntityDef;');
    Add('');
    end;
end;

Procedure JSONToConst(lJSON : TJSONObject; aUnit : TStrings);

var
  I,L : integer;
  ln,LastN,N,VS : String;
  U : UnicodeString;
  UC : UnicodeChar;
  lList : TStringList;


begin
  lList:=TstringList.Create;
  LastN:=N;
  with aUnit do
    begin
    Add('const');
    Add('  EntityDefList : THTMLEntityDefList = (');
    For I:=0 to lJSON.Count-1 do
      begin
      N:=lJSON.Names[i];
      System.Delete(N,1,1);
      l:=Length(N);
      if N[L]=';' then
        SetLength(N,L-1);
      if N=LastN then
        continue;
      LastN:=N;
      U:=UTF8Decode(TJSONObject(lJSON.Items[i]).Get('characters',''));
      VS:='';
      For UC in U do
        VS:=VS+'#$'+HexStr(Ord(UC),4);
      ln:=Format('(e:''%s''; u: %s)',[N,VS]);
      lList.Add(ln);
      end;
    for I:=0 to lList.Count-1 do
      begin
      ln:=lList[I];
      if I<LList.Count-1 then
        ln:=ln+',';
      Add('    '+ln);
      end;
    Add(');');
    end;

end;

var
  lJSON : TJSONObject;
  lUnit : TStrings;

begin
  if ParamCount<2 then
    begin
    Writeln('Usage: ',ParamStr(0),' inputfile outputfile');
    Halt(1);
    end;
  lUnit:=Nil;
  lJSON:=GetJSONData(ParamStr(1));
  try
    lUnit:=TStringList.Create;
    GetUnitStart(LUnit,ParamStr(2));
    JSONToConst(lJSON,lUnit);
    GetUnitEnd(lUnit);
    lUnit.SaveToFile(ParamStr(2));
  finally
    lJSON.Free;
    lUnit.Free;
  end;

end.

