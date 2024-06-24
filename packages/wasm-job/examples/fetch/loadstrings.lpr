library stringshelper;

{$mode objfpc}
{$h+}

uses nothreads, classes, sysutils, variants, job.js;

Type
  // Define the necessary interfaces from the browser APIs.
  IJSResponse = interface(IJSObject)
    ['{0B705D5E-D166-325F-8AD5-004BD5C7161F}']
    function text: IJSPromise; // Promise<USVString>
  end;

  IJSWindow = interface(IJSObject)
    function fetch(const aInput: UnicodeString): IJSPromise; // Promise<Response>
  end;

  { TJSResponse }

  TJSResponse = class(TJSObject,IJSResponse)
  Protected
    function text: IJSPromise; overload; // Promise<USVString>
    class function Cast(aObj : IJSObject): IJSResponse;
  end;

  { TJSWindow }

  TJSWindow = class(TJSObject,IJSWindow)
    function fetch(const aInput: UnicodeString): IJSPromise; // Promise<Response>
  end;

  TLoadHelper = class(TObject)
    FURL: string;
    FStrings: TStrings;
    constructor Create(aURL: string; AList: TStrings);
    procedure LoadFromURL;
    function LoadOK(const aRes: Variant): Variant;
    function LoadJsonOK(const aRes: Variant): Variant;
  end;

  
  TStringsHelper = class helper for TStrings
     procedure LoadFromURL(const aURL: string);
   end;

  TApp = Class(TObject)
  private
    sl: TStringList;
  public
    procedure Run;
  end;

var
  JSWindow : IJSWindow;

{ TJSWindow }

function TJSWindow.fetch(const aInput: UnicodeString): IJSPromise;

begin
  Result:=InvokeJSObjectResult('fetch',[aInput],TJSPromise) as IJSPromise;
end;

function TJSResponse.text: IJSPromise;

begin
  Result:=InvokeJSObjectResult('text',[],TJSPromise) as IJSPromise;
end;

class function TJSResponse.Cast(aObj: IJSObject): IJSResponse;
begin
  Result:=TJSResponse.JOBCast(aObj);
end;


{ TLoadHelper } 

constructor TLoadHelper.Create(aURL: string; aList: TStrings);
begin
  FURL := aURL;
  FStrings := AList;
end;

procedure TLoadHelper.LoadFromURL;
begin
  try
    JSWindow.fetch(FURL)._then(@LoadOK);
  except
    Writeln('Aaaarrghhhh, error fetching :( :(');
    Free; // Need to free ourselves
  end;
end;

function TLoadHelper.LoadOK(const aRes: Variant): Variant;
var
  aUnknown : IUnknown;
  aObject : IJSObject;
  res: IJSResponse;
begin
  try
    //fetch returns a Response object, need to call text()/json() first
    writeln('got response object');
    if vartype(aRes)=varUnknown then
      Writeln('Got interface valued variant');
    aUnknown:=aRes;
    Writeln('Got interface from variant');
    aObject:=aUnknown as IJSObject;
    Writeln('Got IJSObject interface from variant');
    res := TJSResponse.Cast(aObject);
    Writeln('Got TJSResponse interface from IJSObject');
    Writeln('request text');
    res.text._then(@LoadJsonOK);
    Writeln('Done requesting text');
  except
    On E : Exception do
      begin
      Writeln('Exception : ',E.ClassName,' : ',E.Message);
      //something went wrong, free
      Free;
      end;
  end;
end;

function TLoadHelper.LoadJsonOK(const aRes: Variant): Variant;

var
  S : String;

begin
  writeln('got text result. Dumping stringlist');
  if vartype(aRes) = varOleStr then
    begin
    FStrings.Text:=varToStr(aRes);
    Writeln('----');
    for S in FStrings do
      Writeln(S);
    Writeln('----');
    Writeln('Normally you would have a callback here to indicate the text is loaded');
    end;
  Free;
end;

{ TStringsHelper }

procedure TStringsHelper.LoadFromURL(const aURL: string);
begin
   with TLoadHelper.Create(aURL, Self) do
    LoadFromURL;
end;

{ TApp }

procedure TApp.Run;
begin
  try
    sl := TStringList.Create;
    sl.LoadFromURL('lorem.txt');
  except
    on E: Exception do
      Writeln(e.Message);
  end;
end;

var
  App : TApp;
begin
  JSWindow:=TJSWindow.JOBCreateGlobal('window');
  App:=TApp.Create;
  App.Run;
end.
