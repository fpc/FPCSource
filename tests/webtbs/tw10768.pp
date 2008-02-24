
{$ifdef fpc}{$mode delphi}{$endif}
{$apptype console}
uses classes;
Type

  _AppDomain = interface(IDispatch)
                  end;
  IJclClrAppDomain = _AppDomain;

TJclClrAppDomain = class(TInterfacedobject, IJclClrAppDomain)
  private
    FDefaultInterface: IJclClrAppDomain;
  protected
    property DefaultInterface: IJclClrAppDomain read FDefaultInterface implements IJclClrAppDomain;
  end;

begin
end.