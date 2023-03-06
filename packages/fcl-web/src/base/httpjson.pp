{$IFNDEF FPC_DOTTEDUNITS}
unit httpjson;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpWeb.Http.Defs, FpJson.Data;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, httpdefs, fpjson;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { THTTPJSONResponseHelper }

  THTTPJSONResponseHelper = Class helper for TResponse
  Private
    function GetJSONArray: TJSONArray;
    function GetJSONObject: TJSONObject;
    procedure SetJSONArray(AValue: TJSONArray);
    procedure SetJSONContent(const AValue: TJSONData);
    function GetJSONContent : TJSONData;
    procedure SetJSONObject(AValue: TJSONObject);
  Protected
    Procedure ExceptionToJSON(aException: Exception; aJSON : TJSONObject);
  Public
    procedure SendExceptionJSON(aException: Exception; const aElement: String='');
    procedure SetContentFromJSON(const AValue: TJSONData; Formatted : Boolean = True);
    // These set without formatting
    Property ContentAsJSON : TJSONData Read GetJSONContent Write SetJSONContent;
    // Convenience
    Property ContentAsJSONObject : TJSONObject Read GetJSONObject Write SetJSONObject;
    Property ContentAsJSONArray : TJSONArray Read GetJSONArray Write SetJSONArray;
  end;

  { THTTPJSONRequestHelper }

  THTTPJSONRequestHelper = Class helper for TRequest
  Private
    function GetIsJSONContentType: Boolean;
    function GetJSONArrayContent: TJSONArray;
    function GetJSONContent : TJSONData;
    function GetJSONObjectContent: TJSONObject;
  Public
    Function GetJSONContent(aIgnoreContentType : Boolean) : TJSONData;
    Property ContentAsJSON : TJSONData Read GetJSONContent;
    Property IsJSONContentType : Boolean Read GetIsJSONContentType;
    Property ContentAsJSONObject: TJSONObject Read GetJSONObjectContent;
    Property ContentAsJSONArray: TJSONArray Read GetJSONArrayContent;
  end;



implementation

Resourcestring
  SErrContentIsNotAJSONObject = 'Content is valid JSON but not an object';
  SErrContentIsNotAJSONArray = 'Content is valid JSON but not an array';
  SErrContentTypeIsNotJSON = 'Content-Type is not application/json';

{ THTTPJSONRequestHelper }

function THTTPJSONRequestHelper.GetJSONContent: TJSONData;
begin
  Result:=GetJSONCOntent(False);
end;

function THTTPJSONRequestHelper.GetJSONObjectContent: TJSONObject;
Var
  D : TJSONData;

begin
  D:=ContentAsJSON;
  if D is TJSONObject then
    Result:=D as TJSONObject
  else
    begin
    D.Free;
    Raise EConvertError.Create(SErrContentIsNotAJSONObject);
    end;
end;

procedure THTTPJSONResponseHelper.SendExceptionJSON(aException: Exception; const aElement : String = '');

Var
  EH : EHTTP absolute aException;
  J,J2 : TJSONObject;

begin
  if ContentSent then // No point in continuing
    exit;
  if aException is EHTTP then
    if EH.StatusCode<>0 then
      begin
      Code:=EH.StatusCode;
      CodeText:=EH.StatusText;
      end
  else
    begin
    SetStatus(500);
    end;
  ContentType:='application/json';
  J:=TJSONObject.Create;
  try
    if aElement='' then
      J2:=J
    else
      begin
      J2:=TJSONObject.Create;
      J.Add(aElement,J2);
      end;
    ExceptionToJSON(aException,J2);
    ContentAsJSONObject:=J;
  finally
    J.Free;
  end;
end;

procedure THTTPJSONResponseHelper.SetContentFromJSON(const AValue: TJSONData; Formatted: Boolean);
begin
  ContentStream:=Nil;
  if Formatted then
    Content:=AValue.FormatJSON()
  else
    Content:=AValue.AsJSON;
  ContentType:='application/json';
end;

function THTTPJSONRequestHelper.GetIsJSONContentType : Boolean;

begin
  Result:=SameText(ContentType,'application/json');
end;

function THTTPJSONRequestHelper.GetJSONArrayContent: TJSONArray;
Var
  D : TJSONData;

begin
  D:=ContentAsJSON;
  if D is TJSONArray then
    Result:=D as TJSONArray
  else
    begin
    D.Free;
    Raise EConvertError.Create(SErrContentIsNotAJSONArray);
    end;
end;

function THTTPJSONRequestHelper.GetJSONContent(aIgnoreContentType: Boolean): TJSONData;

Var
  E : EHTTP;
begin
  if (Not aIgnoreContentType) and Not IsJSONContentType  then
    begin
    E:=EHTTP.Create(SErrContentTypeIsNotJSON);
    E.StatusCode:=400;
    E.StatusText:='BAD REQUEST';
    Raise E;
    end;
  Result:=GetJSON(Self.Content);
end;

{ THTTPJSONResponseHelper }

function THTTPJSONResponseHelper.GetJSONArray: TJSONArray;

Var
  D : TJSONData;

begin
  D:=ContentAsJSON;
  if D is TJSONArray then
    Result:=D as TJSONArray
  else
    begin
    D.Free;
    Raise EConvertError.Create(SErrContentIsNotAJSONArray);
    end;
end;

function THTTPJSONResponseHelper.GetJSONObject: TJSONObject;

Var
  D : TJSONData;

begin
  D:=ContentAsJSON;
  if D is TJSONObject then
    Result:=D as TJSONObject
  else
    begin
    D.Free;
    Raise EConvertError.Create(SErrContentIsNotAJSONObject);
    end;
end;

procedure THTTPJSONResponseHelper.SetJSONArray(AValue: TJSONArray);
begin
  SetJSONContent(aValue);
end;

procedure THTTPJSONResponseHelper.SetJSONContent(const AValue: TJSONData);
begin
  SetContentFromJSON(aValue,False);
end;

function THTTPJSONResponseHelper.GetJSONContent: TJSONData;
begin
  Result := GetJSON(Content);
end;

procedure THTTPJSONResponseHelper.SetJSONObject(AValue: TJSONObject);
begin
  SetJSONContent(aValue);
end;

procedure THTTPJSONResponseHelper.ExceptionToJSON(aException: Exception; aJSON: TJSONObject);
begin
  With aJSON do
    begin
    Add('exception',aException.ClassName);
    Add('message',aException.Message);
    if Not (aException is EHTTP) and (aException.HelpContext<>0) then
      Add('code',aException.HelpContext);
    end;
end;


end.

