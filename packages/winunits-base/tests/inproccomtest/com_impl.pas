unit com_impl;
// Comtest from Anton K. mantis #35013
{$WARN SYMBOL_PLATFORM OFF}

interface
{$ifdef fpc}{$mode delphi}{$endif}

uses
  ComObj, com_serv_TLB;

type
  TTestApp = class(TAutoObject, ITestApp)
  private
    stor:widestring;
  protected
    procedure test(const text: WideString); safecall;
    procedure test_ret(var res: OleVariant); safecall;
  public
    procedure Initialize;override;
  end;

implementation

uses comserv,sysutils;

procedure TTestApp.Initialize;
begin
  inherited;

end;

procedure TTestApp.test(const text: WideString);
begin
   stor:=formatdatetime('yyyy-mm-dd hh:nn:ss',now)+': '+text;
   writeln(stor);
end;

procedure TTestApp.test_ret(var res: OleVariant);
begin
   writeln('Got: '+widestring(res));
   res:=widestring('zzzz');
  // res:=formatdatetime('yyyy-mm-dd hh:nn:ss',now)+': '+widestring(res);
   writeln(res);
end;

initialization
  TAutoObjectFactory.Create(ComServer, TTestApp, Class_TestApp,
    ciMultiInstance, tmApartment);
end.
