{*******************************************************************
*  Test library of the Apache Pascal Headers
*******************************************************************}
library mod_hello;

{$mode objfpc}{$H+}

uses SysUtils, httpd24, apr;

const
  MODULE_NAME = 'hello_module';

var
  test_module: module;{$ifdef unix} public name MODULE_NAME;{$endif}
  
exports
  test_module name MODULE_NAME;

{*******************************************************************
*  Handles apache requests
*******************************************************************}
function DefaultHandler(r: Prequest_rec): Integer; cdecl;
var
  RequestedHandler, onerow: string;

  
begin
  RequestedHandler := r^.handler;

  { We decline to handle a request if r->handler is not the value of MODULE_NAME}
  if not SameText(RequestedHandler, MODULE_NAME) then
  begin
    Result := DECLINED;
    Exit;
  end;

  { The following line just prints a message to the errorlog }
  ap_log_error(MODULE_NAME,                         //The file in which this function is called
               40,                                  //The line number on which this function is called
               0,                                   //The module_index of the module generating this message
               APLOG_NOERRNO or APLOG_NOTICE,       //The level of this error message
               0,                                   //The status code from the previous command
               r^.server,                           //The server on which we are logging
               'mod_hello: %s',                     //The format string
               [PChar('Before content is output')]); //The arguments to use to fill out fmt.

  ap_set_content_type(r, 'text/html');

  { If the request is for a header only, and not a request for
   the whole content, then return OK now. We don't have to do
   anything else. }
  if (r^.header_only <> 0) then
  begin
    Result := OK;
    Exit;
  end;

  { Now we just print the contents of the document using the
   ap_rputs and ap_rprintf functions. More information about
   the use of these can be found in http_protocol.inc }
  onerow := '<HTML>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '<HEAD>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '<TITLE>Hello There</TITLE>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '</HEAD>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '<BODY BGCOLOR="#FFFFFF">' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '<H1>Hello world</H1>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := 'This is an Apache Module working with the binding from Free Pascal' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);
  onerow := '</BODY></HTML>' + LineEnding;
  ap_rwrite(PChar(onerow), length(onerow), r);

  { We can either return OK or DECLINED at this point. If we return
         * OK, then no other modules will attempt to process this request }
  Result := OK;
end;

{*******************************************************************
*  Registers the hooks
*******************************************************************}
procedure RegisterHooks(p: Papr_pool_t); cdecl;
begin
  ap_hook_handler(@DefaultHandler, nil, nil, APR_HOOK_MIDDLE);
end;

{*******************************************************************
*  Library initialization code
*******************************************************************}

begin
  FillChar(test_module, SizeOf(test_module),0);

  STANDARD20_MODULE_STUFF(test_module);

  with test_module do
  begin
    name := MODULE_NAME;
    register_hooks := @RegisterHooks;
  end;
end.
