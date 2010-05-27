unit dmusers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, db, dbf,
  fpwebdata;

type

  { TDataModule1 }

  TDataModule1 = class(TDataModule)
    AName: TFPWebDataProvider;
    Datasource1: TDatasource;
    Dbf1: TDbf;
    procedure DataModuleCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DataModule1: TDataModule1; 

implementation

uses dbugintf;
{ TDataModule1 }

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  senddebug('Creating datamodule 1 '+InTToStr(Ord(WebDataProviderManager.Registering)));
  If not WebDataProviderManager.Registering then
    begin
    DBF1.TableName:=ExtractFilePath(ParamStr(0))+'users.dbf';
    DBF1.Open;
    With DBF1.FieldByname('ID') do
      ProviderFlags:=ProviderFlags+[pfInKey];
    DBF1.First;
    end;
end;

initialization
  {$I dmusers.lrs}
  WebDataProviderManager.RegisterDatamodule(TDataModule1)

end.

