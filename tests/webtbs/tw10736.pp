{ %OPT=-Sew -vw }

unit tw10736;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 
  
type

  { TAbstractPage }

  TAbstractPage = class
  protected
    procedure Execute virtual; abstract;
  public
    class procedure PageExecute;
  end;

  TPageClass = class of TAbstractPage;

  { TPageUnknown }

  TPageUnknown = class(TAbstractPage)
  protected
    procedure Execute override;
  end;

procedure HandleRequest;

implementation

{ TAbstractPage }

class procedure TAbstractPage.PageExecute;
begin
(*
  with Self.Create do try
    Execute;
  finally
    Free;
  end;
*)
end;

{ TPageUnknown }

procedure TPageUnknown.Execute;
begin
  //Whatever...
end;

procedure HandleRequest;
//Zomaar een kleine besturing, iemand andere ideen?
var Page: TPageClass;
begin
  Page := TPageUnknown;
  Page.PageExecute;
end;


end.

