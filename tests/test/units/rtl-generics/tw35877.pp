program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes
  { you can add units after this }
  , Generics.Collections;

type
  TSimpleRPCMessage2 = record
  end;
  TSimpleRPCResponse2 = record
    R:array of TSimpleRPCMessage2;
  end;
  TResponses2 = specialize THashMap<string, TSimpleRPCResponse2>;
  TSimpleRPCReqHandler2 = class(TInterfacedObject)
    strict private
      FResp:TResponses2;
      procedure ReadResp(var R: TSimpleRPCResponse2);
  end;

procedure TSimpleRPCReqHandler2.ReadResp(var R: TSimpleRPCResponse2);
begin
  R:=FResp.Items['123']; // throws project1.lpr(28,11) Error: Internal error 200510032
end;

begin
end.

