unit frmmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, IPPeerClient, REST.Client,
  REST.Authenticator.Basic, Data.Bind.Components, Data.Bind.ObjectScope,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls,
  Datasnap.DBClient, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    DSRest: TDataSource;
    CDSRest: TClientDataSet;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    EURL: TEdit;
    BFetch: TButton;
    Label1: TLabel;
    Label2: TLabel;
    EUserName: TEdit;
    LEPassword: TLabel;
    EPassword: TEdit;
    RestClient: TIdHTTP;
    procedure BFetchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BFetchClick(Sender: TObject);

Var
  URL : String;
  Response : TMemoryStream;

begin
  URL:=EURL.Text;
  if Pos('?',URL)=0 then
    URL:=URL+'?'
  else
    URL:=URL+'&';
  URL:=URL+'fmt=cds';
  Response:=TMemoryStream.Create;
  With RestClient.Request do
    begin
    UserName:=EUserName.Text;
    Password:=EPassword.Text;
    end;
  RestClient.Get(URL,Response);
  Response.Position:=0;
  CDSRest.LoadFromStream(Response);
end;

end.
