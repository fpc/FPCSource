{ **********************************************************************
  This file is part of the Free Component Library (FCL)
  Copyright (c) 2015 by the Free Pascal development team
        
  JSON Web Token implementation
            
  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.
                   
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  **********************************************************************}
unit fpjwt;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  TypInfo, Classes, SysUtils, fpjson, basenenc;

Type
  EJWT = Class(EJSON);

  { TJWTKey }

  TJWTKey = Record
  private
    function GetAsPByte: PByte;
    function GetAsString: UTF8String;
    function GetLength: Integer;
    procedure SetAsString(AValue: UTF8String);
    procedure SetLength(AValue: Integer);
  public
    Bytes : TBytes;
    Class Function Create(aData : PByte; aSize : Word) : TJWTKey; static;
    Class Function Create(aBytes : TBytes) : TJWTKey; static;
    Class Function Create(aString : UTF8String) : TJWTKey; static;
    Class Function Empty : TJWTKey; static;
    Property AsPointer : PByte Read GetAsPByte;
    Property Length : Integer Read GetLength Write SetLength;
    Property AsBytes : TBytes Read Bytes Write Bytes;
    Property AsString : UTF8String Read GetAsString Write SetAsString;
  end;

  { TBaseJWT }

  TBaseJWT = Class(TPersistent)
  private
  Protected
    // Override this to disable writing a property to the JSON.
    function WriteProp(P: PPropInfo; All: Boolean): Boolean; virtual;
    function GetAsEncodedString: String; virtual;
    procedure SetAsEncodedString(AValue: String); virtual;
    function GetAsString: TJSONStringType; virtual;
    procedure SetAsString(AValue: TJSONStringType);virtual;
    Procedure DoLoadFromJSON(JSON : TJSONObject);virtual;
    Procedure DoSaveToJSON(JSON : TJSONObject; All : Boolean);virtual;
  Public
    Constructor Create; virtual;
    Procedure LoadFromJSON(JSON : TJSONObject);
    Procedure SaveToJSON(JSON : TJSONObject; All : Boolean);
    // Base64url conversion functions (RFC7515)
    class function Base64ToBase64URL(AValue: string): string; deprecated 'Use basenenc functions instead';
    class function Base64URLToBase64(AValue: string): string; deprecated 'Use basenenc functions instead';
    // Decode Base64url string.
    Class Function DecodeString(S : String) : String;
    // Decode Base64url string and return a JSON Object.
    Class Function DecodeStringToJSON(S : String) : TJSONObject;
    // Get/Set as string. This is normally the JSON form.
    Property AsString : TJSONStringType Read GetAsString Write SetAsString;
    // Set as string. This is normally the JSON form, encoded as Base64.
    Property AsEncodedString : String Read GetAsEncodedString Write SetAsEncodedString;
  end;

  { TJOSE }

  TJOSE = Class(TBaseJWT)
  private
    Falg: String;
    Fcrit: String;
    Fcty: String;
    Fjku: String;
    Fjwk: String;
    Fkid: String;
    Ftyp: String;
    Fx5c: String;
    Fx5t: String;
    Fx5ts256: String;
    Fx5u: String;
  Published
    // Registered names. Keep the case lowercase, the RTTI must match the registered name.
    Property cty : String Read Fcty Write Fcty;
    Property typ : String Read Ftyp Write Ftyp;
    Property alg : String Read Falg Write Falg;
    Property jku : String Read Fjku Write fjku;
    Property jwk : String Read Fjwk Write fjwk;
    Property kid : String Read Fkid Write fkid;
    Property x5u : String Read Fx5u Write fx5u;
    Property x5c : String Read Fx5c Write fx5c;
    Property x5t : String Read Fx5t Write fx5t;
    Property x5ts256 : String Read Fx5ts256 Write fx5ts256;
    Property crit : String Read Fcrit Write fcrit;
  end;
  TJOSEClass = Class of TJOSE;

  { TClaims }

  TClaims = Class(TBaseJWT)
  private
    FAud: String;
    FExp: Int64;
    FIat: Int64;
    FIss: String;
    FJTI: String;
    FNbf: Int64;
    FSub: String;
  Published
    // Registered Claim Names. Keep the case lowercase, the RTTI must match the registered name.
    Property iss : String Read FIss Write FIss;
    Property sub : String Read FSub Write FSub;
    Property aud : String Read FAud Write FAud;
    Property exp : Int64 Read FExp Write FExp;
    Property nbf : Int64 Read FNbf Write FNbf;
    Property iat : Int64 Read FIat Write FIat;
    Property jti : String Read FJTI Write FJTI;
  end;
  TClaimsClass = Class of TClaims;

  { TJWT }
  TJWT = Class;

  TJWTClass = Class of TJWT;

  TJWT = Class(TBaseJWT)
  private
    FClaims: TClaims;
    FJOSE: TJOSE;
    FSignature: String;
    procedure SetClaims(AValue: TClaims);
    procedure SetJOSE(AValue: TJOSE);
  Protected
    Function CreateJOSE : TJOSE; Virtual;
    Function CreateClaims : TClaims; Virtual;
    // AsString and AsEncodedString are the same in this case.
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(AValue: TJSONStringType);override;
    function GetAsEncodedString: String;override;
    Procedure SetAsEncodedString (AValue : String);override;
  Public
    Constructor Create; override;
    Destructor Destroy; override;
    Function Sign(aKey : TJWTKey) : String;
    Class Function ValidateJWT(const aJWT : String; aKey : TJWTKey; aClass : TJWTClass = Nil) : TJWT;
    // Owned by the JWT. The JSON header.
    Property JOSE : TJOSE Read FJOSE Write SetJOSE;
    // Owned by the JWT. The set of claims. The actual class will depend on the descendant.
    Property Claims : TClaims Read FClaims Write SetClaims;
    Property Signature : String Read FSignature Write FSignature;
  end;


  { TJWTSigner }
  TJWTSigner = Class;
  TJWTSignerClass = Class of TJWTSigner;

  TJWTSigner = Class
  Private
    class var FAlgorithms : TStringList;
    Class Procedure RegisterAlgorithm(const aName : String; aClass : TJWTSignerClass);
    Class Procedure UnRegisterAlgorithm(const aName : String);
  Public
    class Destructor done;
    Class function AlgorithmName : String; virtual; abstract;
    Class Function GetParts(const aJWT : String; out aJOSE,aClaims,aSign : String) : Boolean;
    Class Function CreateSigner(aAlgorithm : String): TJWTSigner;
    Constructor Create; virtual;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; virtual; abstract;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; virtual; abstract;
    Function AppendSignature(aJWT : TJWT; aKey : TJWTKey) : String;
    Function GetSignInputString(aJWT : TJWT) : UTF8String;
    Function GetSignInput(aJWT : TJWT) : TBytes;
    Class Function ParseAndVerify(const aJWT : String; aKey : TJWTKey; aClass : TJWTClass = Nil) : TJWT;
    Class Procedure Register;
    Class Procedure UnRegister;
  end;

  { TJWTSignerNone }

  TJWTSignerNone = Class(TJWTSigner)
  Public
    Class function AlgorithmName : String; override;
    Function CreateSignature(aJWT : TJWT; aKey : TJWTKey) : String; override;
    Function Verify(const aJWT : String; aKey : TJWTKey) : Boolean; override;
  end;

implementation

uses strutils;

Resourcestring
  SErrMissingAlgorithmName = 'Missing JWA algorithm name';
  SErrUnSupportedAlgorithmName = 'Unsupported JWA algorithm: "%s"';

type

  { TJWTSignerReg }

  TJWTSignerReg = Class
  private
    FName : String;
    FClass : TJWTSignerClass;
  Public
    Constructor Create(Const aName: String; aClass: TJWTSignerClass);
    Property Name : String Read FName;
    Property SignerClass : TJWTSignerClass Read FClass;
  end;

{ TJWTKey }

function TJWTKey.GetAsPByte: PByte;
begin
  Result:=PByte(Bytes);
end;

function TJWTKey.GetAsString: UTF8String;
begin
  Result:=TEncoding.UTF8.GetAnsiString(Bytes);
end;

function TJWTKey.GetLength: Integer;
begin
  Result:=System.Length(Bytes)
end;

procedure TJWTKey.SetAsString(AValue: UTF8String);
begin
  Bytes:=TEncoding.UTF8.GetAnsiBytes(aValue);
end;

procedure TJWTKey.SetLength(AValue: Integer);
begin
  System.SetLength(Bytes,aValue)
end;

class function TJWTKey.Create(aData: PByte; aSize : Word): TJWTKey;

Var
  B : TBytes;

begin
  B:=[];
  System.SetLength(B,aSize);
  Move(aData^,B[0],aSize);
  Result:=Create(B);
end;

class function TJWTKey.Create(aBytes: TBytes): TJWTKey;
begin
  Result.AsBytes:=aBytes;
end;

class function TJWTKey.Create(aString: UTF8String): TJWTKey;
begin
  Result.AsString:=aString;
end;

class function TJWTKey.Empty: TJWTKey;
begin
  Result:=Default(TJWTKey);
end;

{ TJWTSignerNone }

class function TJWTSignerNone.AlgorithmName: String;
begin
  Result:='none'
end;

function TJWTSignerNone.CreateSignature(aJWT: TJWT; aKey : TJWTKey): String;
begin
  Result:='';
end;

function TJWTSignerNone.Verify(const aJWT: String; aKey : TJWTKey): Boolean;

Var
  J,C,S : String;

begin
  Result:=GetParts(aJWT,J,C,S) and (S='');
end;

{ TJWTSignerReg }

constructor TJWTSignerReg.Create(Const aName: String; aClass: TJWTSignerClass);
begin
  FName:=aName;
  FClass:=aClass;
end;

{ TJWTSigner }

class procedure TJWTSigner.RegisterAlgorithm(const aName: String; aClass: TJWTSignerClass);

begin
  if (aName='') then
    Raise EJWT.Create(SErrMissingAlgorithmName);
  if (FAlgorithms=Nil) then
    begin
    FAlgorithms:=TStringList.Create;
    FAlgorithms.OwnsObjects:=True;
    end
  else
    UnregisterAlgorithm(aName);
  FAlgorithms.AddObject(aName,TJWTSignerReg.Create(aName,aClass));
end;

class procedure TJWTSigner.UnRegisterAlgorithm(Const aName: String);

Var
  Idx : Integer;

begin
  if (aName='') then
    Raise EJWT.Create(SErrMissingAlgorithmName);
  Idx:=FAlgorithms.indexOf(aName);
  if Idx<>-1 then
    FAlgorithms.Delete(Idx);
end;

constructor TJWTSigner.Create;
begin
  // Do nothing
end;

class destructor TJWTSigner.done;
begin
  FreeAndNil(FAlgorithms);
end;

class function TJWTSigner.GetParts(const aJWT : String; out aJOSE, aClaims, aSign: String): Boolean;

begin
  aJOSE:=ExtractWord(1,AJWT,['.']);
  aClaims:=ExtractWord(2,AJWT,['.']);
  aSign:=ExtractWord(3,AJWT,['.']);
  Result:=(aJOSE<>'') and (aClaims<>'');
end;

class function TJWTSigner.CreateSigner(aAlgorithm: String): TJWTSigner;

Var
  Idx : Integer;
  aClass : TJWTSignerClass;

begin
  if (aAlgorithm='') then
    Raise EJWT.Create(SErrMissingAlgorithmName);
  Idx:=-1;
  if Assigned(FAlgorithms) then
    Idx:=FAlgorithms.IndexOf(aAlgorithm);
  if Idx=-1 then
    Raise EJWT.CreateFmt(SErrUnSupportedAlgorithmName,[aAlgorithm]);
  aClass:=TJWTSignerReg(FAlgorithms.Objects[Idx]).SignerClass;
  Result:=aClass.Create;
end;

Function TJWTSigner.AppendSignature(aJWT: TJWT;aKey : TJWTKey) : String;

begin
  aJWT.Signature:=CreateSignature(aJWT,aKey);
  Result:=aJWT.AsEncodedString;
end;

function TJWTSigner.GetSignInputString(aJWT: TJWT): UTF8String;
begin
  Result:=aJWT.JOSE.AsEncodedString+'.'+aJWT.Claims.AsEncodedString
end;

function TJWTSigner.GetSignInput(aJWT: TJWT): TBytes;
begin
  Result:=TEncoding.UTF8.GetAnsiBytes(GetSignInputString(aJWT));
end;

Class function TJWTSigner.ParseAndVerify(const aJWT: String; aKey : TJWTKey; aClass : TJWTClass = Nil): TJWT;

Var
  S : TJWTSigner;
  Ok : Boolean;


begin
  if (aClass=Nil) then
    aClass:=TJWT;
  Ok:=False;
  S:=Nil;
  Result:=aClass.Create;
  try
    Result.AsEncodedString:=aJWT;
    S:=CreateSigner(Result.JOSE.alg);
    if not S.Verify(aJWT,aKey) then
      FreeAndNil(Result);
    OK:=true;
  finally
    S.Free;
    if not OK then
      Result.Free;
  end;
end;

class procedure TJWTSigner.Register;
begin
  RegisterAlgorithm(AlgorithmName,Self);
end;

class procedure TJWTSigner.UnRegister;
begin
  UnRegisterAlgorithm(AlgorithmName);
end;

{ TJWT }

procedure TJWT.SetClaims(AValue: TClaims);
begin
  if FClaims=AValue then Exit;
  FClaims:=AValue;
end;

procedure TJWT.SetJOSE(AValue: TJOSE);
begin
  if FJOSE=AValue then Exit;
  FJOSE:=AValue;
end;

function TJWT.CreateJOSE: TJOSE;
begin
  Result:=TJOSE.Create;
end;

function TJWT.CreateClaims: TClaims;
begin
  Result:=TClaims.Create;
end;

function TJWT.GetAsString: TJSONStringType;
begin
  Result:=Base64URL.Encode(JOSE.AsString,False);
  Result:=Result+'.'+Base64URL.Encode(Claims.AsString,False);
  // Dot must always be present, even if signature is empty.
  // https://tools.ietf.org/html/rfc7519#section-6.1
  // (See also Bug ID 37830)
  Result:=Result+'.'+Signature;
end;


function TJWT.GetAsEncodedString: String;
begin
  Result:=GetAsString;
end;

procedure TJWT.SetAsEncodedString(AValue: String);
begin
  SetAsString(AValue);
end;

constructor TJWT.Create;
begin
  Inherited;
  FJOSE:=CreateJOSE;
  FClaims:=CreateCLaims;
end;

destructor TJWT.Destroy;
begin
  FreeAndNil(FJOSE);
  FreeAndNil(FClaims);
  Inherited;
end;

function TJWT.Sign(aKey : TJWTKey): String;

Var
  S: TJWTSigner;

begin
  S:=TJWTSigner.CreateSigner(JOSE.alg);
  try
    Result:=S.AppendSignature(Self,aKey);
  finally
    S.Free;
  end;
end;

class function TJWT.ValidateJWT(const aJWT: String; aKey : TJWTKey; aClass: TJWTClass): TJWT;


begin
  if aClass=Nil then
    aClass:=Self;
  Result:=TJWTSigner.ParseAndVerify(aJWT,aKey,aClass);
end;

procedure TJWT.SetAsString(AValue: TJSONStringType);

Var
  J,C,S : String;

begin
  J:=ExtractWord(1,AValue,['.']);
  C:=ExtractWord(2,AValue,['.']);
  S:=ExtractWord(3,AValue,['.']);
  JOSE.AsEncodedString:=J;
  Claims.AsEncodedString:=C;
  Signature:=S;
end;

{ TBaseJWT }

function TBaseJWT.GetAsEncodedString: String;
begin
  Result:=Base64URL.Encode(AsString,False);
end;

procedure TBaseJWT.SetAsEncodedString(AValue: String);

begin
  AsString:=DecodeString(AValue);
end;

function TBaseJWT.GetAsString: TJSONStringType;

Var
  O : TJSONObject;

begin
  O:=TJSONObject.Create;
  try
    SaveToJSON(O,False);
    Result:=O.AsJSON;
  finally
    O.Free;
  end;
end;

procedure TBaseJWT.SetAsString(AValue: TJSONStringType);
Var
  D : TJSONData;
  O : TJSONObject absolute D;

begin
  D:=GetJSON(AValue);
  try
    if D is TJSONObject then
      LoadFromJSON(O);
  finally
    D.Free;
  end;
end;

procedure TBaseJWT.DoLoadFromJSON(JSON: TJSONObject);

Var
  D : TJSONEnum;
  P : PPropinfo;

begin
  For D in JSON Do
    begin
    P:=GetPropInfo(Self,D.Key);
    if (P<>Nil) and not D.Value.IsNull then
      Case P^.PropType^.Kind of
        tkInteger : SetOrdProp(Self,P,D.Value.AsInteger);
        tkChar :
            if D.Value.AsString<>'' then
              SetOrdProp(Self,P,Ord(D.Value.AsString[1]));
        tkEnumeration :
          if (D.Value.JSONType=jtNumber) and (TJSONNumber(D.Value).NumberType=ntInteger) then
            SetOrdProp(Self,P,D.Value.AsInteger)
          else
            SetOrdProp(Self,P,GetEnumValue(p^.PropType,D.Value.AsString));
        tkFloat :
          SetFloatProp(Self,P,D.Value.AsFloat);
        tkSString,tkLString,tkAString :
            SetStrProp(Self,P,D.Value.AsString);
        tkWChar, tkUString,tkWString,tkUChar:
            SetWideStrProp(Self,P,D.Value.AsString);
        tkBool :
          SetOrdProp(Self,P,Ord(D.Value.AsBoolean));
        tkInt64,tkQWord:
          SetInt64Prop(Self,P,Ord(D.Value.AsInt64));
        end;
   end;
end;

function TBaseJWT.WriteProp(P: PPropInfo; All: Boolean): Boolean;

begin
  Result:=True;
end;

procedure TBaseJWT.DoSaveToJSON(JSON: TJSONObject; All: Boolean);


Var
  D : TJSONEnum;
  P : PPropinfo;
  PL : PPropList;
  I,VI,Count : Integer;
  VF : Double;
  C : Char;
  CW : WideChar;
  I64 : Int64;
  W : UnicodeString;
  S : String;

begin
  Count:=GetPropList(Self,PL);
  try
    For I:=0 to Count-1 do
      begin
      P:=PL^[i];
      if WriteProp(P,All) then
        Case P^.PropType^.Kind of
          tkInteger :
            begin
            VI:=GetOrdProp(Self,P);
            if All or (VI<>0) then
              JSON.Add(P^.Name,VI);
            end;
          tkChar :
            begin
            C:=Char(GetOrdProp(Self,P));
            if All or (C<>#0) then
              if C=#0 then
                JSON.Add(p^.Name,'')
              else
                JSON.Add(p^.Name,C);
            end;
          tkEnumeration :
            begin
            vi:=GetOrdProp(Self,P);
            JSON.Add(P^.Name,GetEnumName(p^.PropType,VI));
            end;
          tkFloat :
            begin
            VF:=GetFloatProp(Self,P);
            If All or (VF<>0) then
              JSON.Add(P^.Name,VF);
            end;
          tkSString,tkLString,tkAString :
            begin
            S:=GetStrProp(Self,P);
            if All or (S<>'') then
              JSON.Add(P^.Name,S);
            end;
          tkWChar:
            begin
            CW:=WideChar(GetOrdProp(Self,P));
            if All or (CW<>#0) then
              if CW=#0 then
                JSON.Add(p^.Name,'')
              else
                JSON.Add(p^.Name,Utf8Encode(WideString(CW)));
            end;
          tkUString,tkWString,tkUChar:
             begin
              W:=GetWideStrProp(Self,P);
              if All or (W<>'') then
                JSON.Add(P^.Name,Utf8Encode(W));
              end;
          tkBool :
            JSON.Add(P^.Name,(GetOrdProp(Self,P)<>0));
          tkInt64,tkQWord:
            begin
            I64:=GetInt64Prop(Self,P);
            if All or (I64<>0) then
              JSON.Add(p^.Name,I64);
            end;
          end;
      end;
  finally
    FreeMem(PL);
  end;
end;

constructor TBaseJWT.Create;
begin
  Inherited Create;
end;

procedure TBaseJWT.LoadFromJSON(JSON: TJSONObject);
begin
  DoLoadFromJSon(JSON);
end;

procedure TBaseJWT.SaveToJSON(JSON: TJSONObject; All: Boolean);
begin
  DoSaveToJSon(JSON,All);
end;

class function TBaseJWT.Base64ToBase64URL(AValue: string): string;
begin
  Result := StringsReplace(AValue, ['+', '/'], ['-', '_'], [rfReplaceAll]);
  Result := TrimRightSet(Result, ['=']);
end;

class function TBaseJWT.Base64URLToBase64(AValue: string): string;
var
  i,l: integer;
begin
  Result := StringsReplace(AValue, ['-', '_'], ['+', '/'], [rfReplaceAll]);
  l := length(Result) mod 4;
  if l > 0 then
    Result:=Result+StringOfChar('=',4-l);
end;

class function TBaseJWT.DecodeString(S: String): String;
begin
  Result:=TEncoding.UTF8.GetAnsiString(Base64URL.Decode(S));
end;

class function TBaseJWT.DecodeStringToJSON(S: String): TJSONObject;

Var
  D : TJSONData;
begin
  D:=GetJSON(DecodeString(S));
  if not (D is TJSONData) then
    FreeAndNil(D);
  Result:=TJSONObject(D);
end;

initialization
  TJWTSignerNone.Register;
end.

