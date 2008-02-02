unit svrclass_XMLRPC;

interface

uses Classes, XMLRPC, svrclass;

type
  TServerClassXMLRPCServlet = class(TXMLRPCServlet)
  private
    FServerClass: TServerClass;
  protected
    procedure Dispatch(AParser: TXMLRPCParser; AWriter: TXMLRPCWriter; APath: TStrings); override;
  published
    property ServerClass: TServerClass read FServerClass write FServerClass;
  end;



implementation

procedure TServerClassXMLRPCServlet.Dispatch(AParser: TXMLRPCParser; AWriter: TXMLRPCWriter; APath: TStrings);
var
  s: String;
begin
  s := APath[0];
  if s = 'WRITESTRING' then
  begin
    AParser.ResetValueCursor;
    ServerClass.WriteString(AParser.GetPrevString);
    AWriter.WriteResponse(nil);
  end else if s = 'ADD' then
  begin
    AParser.ResetValueCursor;
    AWriter.WriteResponse(AWriter.CreateIntValue(ServerClass.Add(AParser.GetPrevInt, AParser.GetPrevInt)));
  end else
    AWriter.WriteFaultResponse(2, 'Invalid method name');
end;


end.
