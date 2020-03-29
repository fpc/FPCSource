unit TCSrcMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpjson, JSSrcMap;

type

  { TCustomTestSrcMap }

  TCustomTestSrcMap = class(TTestCase)
  protected
    procedure CheckEl(aName: String; El: TJSONData; aClass: TClass);
    function GetEl(Obj: TJSONObject; aName: String; aClass: TClass): TJSONData;
  end;

  { TTestSrcMap }

  TTestSrcMap = class(TCustomTestSrcMap)
  published
    procedure Test_Base64VLQ;
    procedure TestSrcMapIgnoreDuplicate;
    procedure TestSrcMapNames;
  end;

implementation

{ TCustomTestSrcMap }

procedure TCustomTestSrcMap.CheckEl(aName: String; El: TJSONData; aClass: TClass);
begin
  AssertNotNull('json "'+aName+'" exists',El);
  AssertEquals('json "'+aName+'" class',El.ClassType,aClass);
end;

function TCustomTestSrcMap.GetEl(Obj: TJSONObject; aName: String; aClass: TClass): TJSONData;
begin
  Result:=Obj.Elements[aName];
  CheckEl(aName,Result,aClass);
end;

{ TTestSrcMap }

procedure TTestSrcMap.Test_Base64VLQ;
var
  i: Integer;
  s: String;
  p: PChar;
  j: NativeInt;
begin
  for i:=-511 to 511 do
  begin
    s:=EncodeBase64VLQ(i);
    p:=PChar(s);
    j:=DecodeBase64VLQ(p);
    if i<>j then
      Fail('Encode/DecodeBase64VLQ OrigIndex='+IntToStr(i)+' Code="'+s+'" NewIndex='+IntToStr(j));
  end;
end;

procedure TTestSrcMap.TestSrcMapIgnoreDuplicate;
var
  sm: TSourceMap;
  Obj: TJSONObject;
  El: TJSONData;
  Arr: TJSONArray;
begin
  Obj:=nil;
  sm:=TSourceMap.Create('generated.js');
  try
    sm.AddMapping(1,0,'a.js',1,0);
    sm.AddMapping(2,0);
    sm.AddMapping(2,0);
    sm.AddMapping(3,0,'a.js',2,0);

    //writeln(sm.ToString);
    {
      version: 3,
      file: 'generated.js',
      sources: ['a.js'],
      names: [],
      mappings: 'AAAA;A;AACA'
    }
    Obj:=sm.ToJSON;

    // version
    El:=GetEl(Obj,'version',TJSONIntegerNumber);
    AssertEquals('json "version" value',El.AsInt64,3);

    // file
    El:=GetEl(Obj,'file',TJSONString);
    AssertEquals('json "file" value',El.AsString,'generated.js');

    // sources
    Arr:=TJSONArray(GetEl(Obj,'sources',TJSONArray));
    AssertEquals('json "sources".count',Arr.Count,1);
    El:=Arr[0];
    CheckEl('sources[0]',El,TJSONString);
    AssertEquals('json "sources[0]" value',El.AsString,'a.js');

    // names
    Arr:=TJSONArray(GetEl(Obj,'names',TJSONArray));
    AssertEquals('json "names".count',Arr.Count,0);

    // mappings
    El:=GetEl(Obj,'mappings',TJSONString);
    AssertEquals('json "mappings" value',El.AsString,'AAAA;A;AACA');

  finally
    Obj.Free;
    sm.Free;
  end;
end;

procedure TTestSrcMap.TestSrcMapNames;
var
  sm: TSourceMap;
  Obj: TJSONObject;
  El: TJSONData;
  Arr: TJSONArray;
begin
  Obj:=nil;
  sm:=TSourceMap.Create('generated.js');
  try
    sm.AddMapping(1,1,'a.js',2,2,'foo');
    sm.AddMapping(3,3,'a.js',4,4,'foo');
    writeln(sm.ToString);
    {
      version: 3,
      file: 'generated.js',
      sources: ['a.js'],
      names: ['foo'],
      mappings: 'CACEA;;GAEEA'
    }
    Obj:=sm.ToJSON;

    // version
    El:=GetEl(Obj,'version',TJSONIntegerNumber);
    AssertEquals('json "version" value',El.AsInt64,3);

    // file
    El:=GetEl(Obj,'file',TJSONString);
    AssertEquals('json "file" value',El.AsString,'generated.js');

    // sources
    Arr:=TJSONArray(GetEl(Obj,'sources',TJSONArray));
    AssertEquals('json "sources".count',Arr.Count,1);
    El:=Arr[0];
    CheckEl('sources[0]',El,TJSONString);
    AssertEquals('json "sources[0]" value',El.AsString,'a.js');

    // names
    Arr:=TJSONArray(GetEl(Obj,'names',TJSONArray));
    AssertEquals('json "names".count',Arr.Count,1);
    El:=Arr[0];
    CheckEl('names[0]',El,TJSONString);
    AssertEquals('json "names[0]" value',El.AsString,'foo');

    // mappings
    El:=GetEl(Obj,'mappings',TJSONString);
    AssertEquals('json "mappings" value',El.AsString,'CACEA;;GAEEA');

  finally
    Obj.Free;
    sm.Free;
  end;
end;

initialization
  RegisterTests([TTestSrcMap]);
end.

