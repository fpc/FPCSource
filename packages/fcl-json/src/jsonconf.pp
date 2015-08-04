{
    This file is part of the Free Component Library

    Implementation of TJSONConfig class
    Copyright (c) 2007 Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  TJSONConfig enables applications to use JSON files for storing their
  configuration data
}

{$IFDEF FPC}
{$MODE objfpc}
{$H+}
{$ENDIF}

unit jsonConf;

interface

uses
  SysUtils, Classes, fpjson, jsonparser;

resourcestring
  SWrongRootName = 'XML file has wrong root element name';

type
  EJSONConfigError = class(Exception);
  TPathFlags = set of (pfHasValue, pfWriteAccess);

(* ********************************************************************
   "APath" is the path and name of a value: A JSON configuration file 
   is hierachical. "/" is the path delimiter, the part after the last 
   "/" is the name of the value. The path components will be mapped 
   to nested JSON objects, with the name equal to the part. In practice 
   this means that "/my/path/value" will be written as:
   { 
     "my" : {
       "path" : {
         "value" : Value
       }
     }
   }
   ******************************************************************** *)

  { TJSONConfig }

  TJSONConfig = class(TComponent)
  private
    FFilename: String;
    FFormatIndentSize: Integer;
    FFormatoptions: TFormatOptions;
    FFormatted: Boolean;
    FKey: TJSONObject;
    procedure DoSetFilename(const AFilename: String; ForceReload: Boolean);
    procedure SetFilename(const AFilename: String);
    Function StripSlash(P : WideString) : WideString;
  protected
    FJSON: TJSONObject;
    FModified: Boolean;
    procedure Loaded; override;
    function FindPath(Const APath: WideString; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: WideString; AllowCreate : Boolean) : TJSONObject;
    function FindObject(Const APath: WideString; AllowCreate : Boolean;Var ElName : WideString) : TJSONObject;
    function FindElement(Const APath: WideString; CreateParent : Boolean) : TJSONData;
    function FindElement(Const APath: WideString; CreateParent : Boolean; Var AParent : TJSONObject; Var ElName : WideString) : TJSONData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Reload;
    procedure Clear;
    procedure Flush;    // Writes the JSON file
    procedure OpenKey(const aPath: WideString; AllowCreate : Boolean);
    procedure CloseKey;
    procedure ResetKey;
    Procedure EnumSubKeys(Const APath : String; List : TStrings);
    Procedure EnumValues(Const APath : String; List : TStrings);

    function  GetValue(const APath: WideString; const ADefault: WideString): WideString; overload;
    function  GetValue(const APath: WideString; ADefault: Integer): Integer; overload;
    function  GetValue(const APath: WideString; ADefault: Int64): Int64; overload;
    function  GetValue(const APath: WideString; ADefault: Boolean): Boolean; overload;
    function  GetValue(const APath: WideString; ADefault: Double): Double; overload;
    procedure SetValue(const APath: WideString; const AValue: WideString); overload;
    procedure SetValue(const APath: WideString; AValue: Integer); overload;
    procedure SetValue(const APath: WideString; AValue: Int64); overload;
    procedure SetValue(const APath: WideString; AValue: Boolean); overload;
    procedure SetValue(const APath: WideString; AValue: Double); overload;

    procedure SetDeleteValue(const APath: WideString; const AValue, DefValue: WideString); overload;
    procedure SetDeleteValue(const APath: WideString; AValue, DefValue: Integer); overload;
    procedure SetDeleteValue(const APath: WideString; AValue, DefValue: Int64); overload;
    procedure SetDeleteValue(const APath: WideString; AValue, DefValue: Boolean); overload;

    procedure DeletePath(const APath: WideString);
    procedure DeleteValue(const APath: WideString);
    property Modified: Boolean read FModified;
  published
    Property Filename: String read FFilename write SetFilename;
    Property Formatted : Boolean Read FFormatted Write FFormatted;
    Property FormatOptions : TFormatOptions Read FFormatoptions Write FFormatOptions Default DefaultFormat;
    Property FormatIndentsize : Integer Read FFormatIndentSize Write FFormatIndentSize Default DefaultIndentSize;
  end;


// ===================================================================

implementation

Const
  SErrInvalidJSONFile = '"%s" is not a valid JSON configuration file.';
  SErrCouldNotOpenKey = 'Could not open key "%s".';

constructor TJSONConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FJSON:=TJSONObject.Create;
  FKey:=FJSON;
  FFormatOptions:=DefaultFormat;
  FFormatIndentsize:=DefaultIndentSize;
end;

destructor TJSONConfig.Destroy;
begin
  if Assigned(FJSON) then
    begin
    Flush;
    FreeANdNil(FJSON);
    end;
  inherited Destroy;
end;

procedure TJSONConfig.Clear;
begin
  FJSON.Clear;
  FKey:=FJSON;
end;

procedure TJSONConfig.Flush;

Var
  F : Text;
  S : TJSONStringType;
  
begin
  if Modified then
    begin
    AssignFile(F,FileName);
    Rewrite(F);
    Try
      if Formatted then
        S:=FJSON.FormatJSON(Formatoptions,FormatIndentSize)
      else
        S:=FJSON.AsJSON;
      Writeln(F,S);  
    Finally
      CloseFile(F);
    end;
    FModified := False;
    end;
end;


function TJSONConfig.FindObject(const APath: WideString; AllowCreate: Boolean
  ): TJSONObject;

Var
  Dummy : WideString;

begin
  Result:=FindObject(APath,AllowCreate,Dummy);
end;

function TJSONConfig.FindObject(const APath: WideString; AllowCreate: Boolean;
  var ElName: WideString): TJSONObject;

Var
  S,El : WideString;
  P,I : Integer;
  T : TJSonObject;
  
begin
//  Writeln('Looking for : ', APath);
  S:=APath;
  If Pos('/',S)=1 then
    Result:=FJSON
  else
    Result:=FKey;
  Repeat
    P:=Pos('/',S);
    If (P<>0) then
      begin
      // Only real paths, ignore double slash
      If (P<>1) then
        begin
        El:=Copy(S,1,P-1);
        If (Result.Count=0) then
          I:=-1
        else
          I:=Result.IndexOfName(El);
        If (I=-1) then
          // No element with this name.
          begin
          If AllowCreate then
            begin
            // Create new node.
            T:=Result;
            Result:=TJSonObject.Create;
            T.Add(El,Result);
            end
          else
            Result:=Nil
          end
        else
          // Node found, check if it is an object
          begin
          if (Result.Items[i].JSONtype=jtObject) then
            Result:=Result.Objects[el]
          else
            begin
//            Writeln(el,' type wrong');
            If AllowCreate then
              begin
//              Writeln('Creating ',el);
              Result.Delete(I);
              T:=Result;
              Result:=TJSonObject.Create;
              T.Add(El,Result);
              end
            else
              Result:=Nil
            end;
          end;
        end;
      Delete(S,1,P);
      end;
  Until (P=0) or (Result=Nil);
  ElName:=S;
end;

function TJSONConfig.FindElement(const APath: WideString; CreateParent: Boolean
  ): TJSONData;

Var
  O : TJSONObject;
  ElName : WideString;
  
begin
  Result:=FindElement(APath,CreateParent,O,ElName);
end;

function TJSONConfig.FindElement(const APath: WideString;
  CreateParent: Boolean; var AParent: TJSONObject; var ElName: WideString
  ): TJSONData;

Var
  I : Integer;

begin
  Result:=Nil;
  Aparent:=FindObject(APath,CreateParent,ElName);
  If Assigned(Aparent) then
    begin
//    Writeln('Found parent, looking for element:',elName);
    I:=AParent.IndexOfName(ElName);
//    Writeln('Element index is',I);
    If (I<>-1) And (AParent.items[I].JSONType<>jtObject) then
      Result:=AParent.Items[i];
    end;
end;


function TJSONConfig.GetValue(const APath: WideString; const ADefault: WideString): WideString;

var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Assigned(El) then
    Result:=El.AsString
  else
    Result:=ADefault;
end;

function TJSONConfig.GetValue(const APath: WideString; ADefault: Integer): Integer;
var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsInteger
  else
    Result:=StrToIntDef(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: WideString; ADefault: Int64): Int64;
var
  El : TJSONData;

begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsInt64
  else
    Result:=StrToInt64Def(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: WideString; ADefault: Boolean): Boolean;

var
  El : TJSONData;
  
begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONBoolean) then
    Result:=El.AsBoolean
  else
    Result:=StrToBoolDef(El.AsString,ADefault);
end;

function TJSONConfig.GetValue(const APath: WideString; ADefault: Double): Double;

var
  El : TJSONData;

begin
  El:=FindElement(StripSlash(APath),False);
  If Not Assigned(el) then
    Result:=ADefault
  else if (el is TJSONNumber) then
    Result:=El.AsFloat
  else
    Result:=StrToFloatDef(El.AsString,ADefault);
end;


procedure TJSONConfig.SetValue(const APath: WideString; const AValue: WideString);

var
  El : TJSONData;
  ElName : WideString;
  O : TJSONObject;
  I : integer;
  
begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (El.JSONType<>jtString) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONString.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsString:=AVAlue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: WideString; const AValue, DefValue: WideString);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: WideString; AValue: Integer);

var
  El : TJSONData;
  ElName : WideString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONIntegerNumber)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONIntegerNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInteger:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: WideString; AValue: Int64);

var
  El : TJSONData;
  ElName : WideString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONInt64Number)) then
    begin
    I:=O.IndexOfName(elName);
    If (I<>-1) then // Normally not needed...
      O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONInt64Number.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsInt64:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: WideString; AValue,
  DefValue: Integer);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetDeleteValue(const APath: WideString; AValue,
  DefValue: Int64);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;

procedure TJSONConfig.SetValue(const APath: WideString; AValue: Boolean);

var
  El : TJSONData;
  ElName : WideString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (el.JSONType<>jtBoolean) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONBoolean.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsBoolean:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetValue(const APath: WideString; AValue: Double);

var
  El : TJSONData;
  ElName : WideString;
  O : TJSONObject;
  I : integer;

begin
  El:=FindElement(StripSlash(APath),True,O,ElName);
  if Assigned(El) and (Not (El is TJSONFloatNumber)) then
    begin
    I:=O.IndexOfName(elName);
    O.Delete(i);
    El:=Nil;
    end;
  If Not Assigned(el) then
    begin
    El:=TJSONFloatNumber.Create(AValue);
    O.Add(ElName,El);
    end
  else
    El.AsFloat:=AValue;
  FModified:=True;
end;

procedure TJSONConfig.SetDeleteValue(const APath: WideString; AValue,
  DefValue: Boolean);
begin
  if AValue = DefValue then
    DeleteValue(APath)
  else
    SetValue(APath,AValue);
end;

procedure TJSONConfig.DeletePath(const APath: WideString);

Var
  P : String;
  L : integer;
  Node : TJSONObject;
  ElName : WideString;
  
begin
  P:=StripSlash(APath);
  L:=Length(P);
  If (L>0) then
    begin
    Node := FindObject(P,False,ElName);
    If Assigned(Node) then
      begin
      L:=Node.IndexOfName(ElName);
      If (L<>-1) then
        Node.Delete(L);
      end;
    end;
end;

procedure TJSONConfig.DeleteValue(const APath: WideString);

begin
  DeletePath(APath);
end;

procedure TJSONConfig.Reload;

begin
  if Length(Filename) > 0 then
    DoSetFilename(Filename,True);
end;
procedure TJSONConfig.Loaded;
begin
  inherited Loaded;
  Reload;
end;

function TJSONConfig.FindPath(const APath: WideString; AllowCreate: Boolean
  ): TJSONObject;
  
Var
  P : WideString;
  L : Integer;
  
begin
  P:=APath;
  L:=Length(P);
  If (L=0) or (P[L]<>'/') then
    P:=P+'/';
  Result:=FindObject(P,AllowCreate);
end;

procedure TJSONConfig.DoSetFilename(const AFilename: String; ForceReload: Boolean);

Var
  P : TJSONParser;
  J : TJSONData;
  F : TFileStream;
  
begin
  if (not ForceReload) and (FFilename = AFilename) then
    exit;
  FFilename := AFilename;

  if csLoading in ComponentState then
    exit;

  Flush;
  If Not FileExists(AFileName) then
    Clear
  else
    begin
    F:=TFileStream.Create(AFileName,fmopenRead);
    try
      P:=TJSONParser.Create(F);
      try
        J:=P.Parse;
        If (J is TJSONObject) then
          begin
          FreeAndNil(FJSON);
          FJSON:=J as TJSONObject;
          FKey:=FJSON;
          end
        else
          Raise EJSONConfigError.CreateFmt(SErrInvalidJSONFile,[AFileName]);
      finally
        P.Free;
      end;
    finally
      F.Free;
    end;
    end;
end;

procedure TJSONConfig.SetFilename(const AFilename: String);
begin
  DoSetFilename(AFilename, False);
end;

function TJSONConfig.StripSlash(P: WideString): WideString;

Var
  L : Integer;

begin
  L:=Length(P);
  If (L>0) and (P[l]='/') then
    Result:=Copy(P,1,L-1)
  else
    Result:=P;
end;


procedure TJSONConfig.CloseKey;
begin
  ResetKey;
end;

procedure TJSONConfig.OpenKey(const aPath: WideString; AllowCreate: Boolean);

Var
  ElName : WideString;
  P : String;
  L : Integer;
begin
  P:=APath;
  L:=Length(P);
  If (L=0) then
    FKey:=FJSON
  else
    begin
    if (P[L]<>'/') then
      P:=P+'/';
    FKey:=FindObject(P,AllowCreate);
    If (FKey=Nil) Then
      Raise EJSONConfigError.CreateFmt(SErrCouldNotOpenKey,[APath]);
    end;
end;

procedure TJSONConfig.ResetKey;
begin
  FKey:=FJSON;
end;

procedure TJSONConfig.EnumSubKeys(const APath: String; List: TStrings);

Var
  AKey : TJSONObject;
  I : Integer;
  
begin
  AKey:=FindPath(APath,False);
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If AKey.Items[i] is TJSONObject then
        List.Add(AKey.Names[i]);
    end;
end;

procedure TJSONConfig.EnumValues(const APath: String; List: TStrings);

Var
  AKey : TJSONObject;
  I : Integer;

begin
  AKey:=FindPath(APath,False);
  If Assigned(AKey) then
    begin
    For I:=0 to AKey.Count-1 do
      If Not (AKey.Items[i] is TJSONObject) then
        List.Add(AKey.Names[i]);
    end;
end;


end.
