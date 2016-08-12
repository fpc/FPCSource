{ %opt=-vh -Seh }
{ %norun }

program NotUsedParamInAbstractMethodBug;

{$mode objfpc}{$H+}

type
  { TXMLWriter }

  TXMLWriter = class(TObject)
  private
  protected
    procedure Write(const Buffer; Count: Longint); virtual; abstract;
  public
  end;

var
  x: TXMLWriter;
begin
end.

