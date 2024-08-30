{
    This file is part of the Free Component Library

    Webassembly HTTP API - demo program 
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
library wasmhttpdemo;

uses basenenc, sysutils, classes, wasm.http.api, wasm.http.shared, wasm.http.objects;



Procedure HandleResponseCallback(Resp : TWasmHTTPResponse);

var
  H : String;

begin
  Writeln('Got response on request ID: ',Resp.RequestID);
  Writeln('Status: ',Resp.Status,' ',Resp.StatusText);
  Writeln('Headers (',Resp.Headers.Count,'):');
  For H in Resp.Headers do
   Writeln(H);
  if Pos('text/',Trim(Resp.Headers.Values['content-type']))=1 then
    begin
    Writeln('Body is text (Assumed UTF8):');
    Writeln(Resp.BodyAsUTF8);
    end
  else
    begin
    Writeln('Body is not text, base64 content:');
    Writeln(Base64.Encode(Resp.Body));
    end;
  Writeln('')
end;

procedure StartTest;

Var
  Req : TWasmHTTPRequest;
  ID : TWasmHTTPRequestID;

begin
  Writeln('Creating request');
  Req:=TWasmHTTPRequest.Create('index.html');
  Writeln('Executing request');
  ID:=Req.Execute(@HandleResponseCallback);
  Writeln('Got request ID :',ID);
  // Request is freed once the return was processed.
end;

var
  Buf : Array[1..64*1024] of byte;

begin
  SetTextBuf(output,buf,SizeOf(Buf));
  StartTest;
end.

