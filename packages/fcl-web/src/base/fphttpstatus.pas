unit FPHTTPStatus;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fphttpserver, HTTPDefs;

(* construct and return the default error message for a given
 * HTTP defined error code
 *)
function http_error_response(status: integer; ARequest: TFPHTTPConnectionRequest): string;

implementation

function error_string(status: integer; ARequest: TFPHTTPConnectionRequest): string;
begin
  case status of
    301: ;
    302: ;
    307: Result := '<p>The document has moved <a href=\' +
        HTTPEncode(ARequest.Location) +
        '\>here</a>.</p>';
    303: Result := '<p>The answer to your request is located ' +
        '<a href=\' +
        HTTPEncode(ARequest.Location) +
        '\>here</a>.</p>';
    305: Result := '<p>This resource is only accessible ' +
        'through the proxy' +
        HTTPEncode(ARequest.Location) +
        '<br />You will need to configure ' +
        'your client to use that proxy.</p>';
    407: ;
    401: Result := '<p>This server could not verify that you' +
        'are authorized to access the document' +
        'requested.  Either you supplied the wrong' +
        'credentials (e.g., bad password), or your' +
        'browser doesn''t understand how to supply' +
        'the credentials required.</p>';
    400: Result := '<p>Your browser sent a request that ' +
        'this server could not understand.<br />' +
        '</p>';
    403: Result := '<p>You don''t have permission to access ' +
        HTTPEncode(ARequest.URI) +
        'on this server.</p>';
    404: Result := '<p>The requested URL ' +
        HTTPEncode(ARequest.URI) +
        ' was not found on this server.</p>';
    405: Result := '<p>The requested method ' +
        HTTPEncode(ARequest.Method) +
        ' is not allowed for the URL ' +
        HTTPEncode(ARequest.URI) +
        '.</p>';
    406: Result := '<p>An appropriate representation of the ' +
        'requested resource ' +
        HTTPEncode(ARequest.URI) +
        ' could not be found on this server.</p>';
    300: ;
    411: Result := '<p>A request of the requested method ' +
        HTTPEncode(ARequest.Method) +
        ' requires a valid Content-length.<br />';
    412: Result := '<p>The precondition on the request ' +
        'for the URL ' +
        HTTPEncode(ARequest.URI) +
        ' evaluated to false.</p>';
    501: Result := '<p>' +
        HTTPEncode(ARequest.Method) + ' to ' +
        HTTPEncode(ARequest.URI) +
        ' not supported.<br />' +
        '</p>';
    502: Result := '<p>The proxy server received an invalid ' +
        'response from an upstream server.<br />' +
        '</p>';
    506: Result := '<p>A variant for the requested ' +
        'resource<pre>' +
        HTTPEncode(ARequest.URI) +
        '</pre>is itself a negotiable resource. ' +
        'This indicates a configuration error.</p>';
    408: Result := '<p>Server timeout waiting for the HTTP request from the client.</p>';
    410: Result := '<p>The requested resource<br />' +
        HTTPEncode(ARequest.URI) +
        '<br />is no longer available on this server ' +
        'and there is no forwarding address.' +
        'Please remove all references to this ' +
        'resource.</p>';
    413: Result := 'The requested resource<br />' +
        HTTPEncode(ARequest.URI) + '<br />' +
        'does not allow request data with ' +
        HTTPEncode(ARequest.Method) +
        ' requests, or the amount of data provided in' +
        'the request exceeds the capacity limit.';
    414: Result := '<p>The requested URL''s length exceeds the capacity' +
        'limit for this server.<br />' +
        '</p>';
    415: Result := '<p>The supplied request data is not in a format ' +
        'acceptable for processing by this resource.</p>';
    416: Result := '<p>None of the range-specifier values in the Range ' +
        'request-header field overlap the current extent ' +
        'of the selected resource.</p>';
    417:
    begin
      if pos('Expect', ARequest.HeaderLine) <> 0 then
        Result := '<p>The expectation given in the Expect request-header' +
          'field could not be met by this server.' +
          'The client sent<pre>   ' +
          HTTPEncode(ARequest.HeaderLine) + '</pre>'
      else
        Result := '<p>No expectation was seen, the Expect request-header ' +
          'field was not presented by the client.';
    end;
    422: Result := '<p>The server understands the media type of the' +
        'request entity, but was unable to process the' +
        'contained instructions.</p>';
    423: Result := '<p>The requested resource is currently locked.' +
        'The lock must be released or proper identification' +
        'given before the method can be applied.</p>';
    424: Result := '<p>The method could not be performed on the resource' +
        'because the requested action depended on another' +
        'action and that other action failed.</p>';
    426: Result := '<p>The requested resource can only be retrieved' +
        'using SSL.  The server is willing to upgrade the current' +
        'connection to SSL, but your client doesn''t support it.' +
        'Either upgrade your client, or try requesting the page' +
        'using https://';
    507: Result := '<p>The method could not be performed on the resource' +
        'because the server is unable to store the' +
        'representation needed to successfully complete the' +
        'request.  There is insufficient free space left in' +
        'your storage allocation.</p>';
    503: Result := '<p>The server is temporarily unable to service your' +
        'request due to maintenance downtime or capacity' +
        'problems. Please try again later.</p>';
    504: Result := '<p>The gateway did not receive a timely response' +
        'from the upstream server or application.</p>';
    510: Result := '<p>A mandatory extension policy in the request is not' +
        'accepted by the server for this resource.</p>';
    else
      //HTTP internal server error
      Result := '<p>The server encountered an internal ' +
        'error or' +
        'misconfiguration and was unable to complete ' +
        'your request.</p>' +
        '<p>Please contact the server ' +
        'administrator at ' +
        HTTPEncode(ARequest.Connection.Server.AdminMail) +
        ' to inform them of the time this ' +
        'error occurred,' +
        ' and the actions you performed just before ' +
        'this error.</p>' +
        '<p>More information about this error ' +
        'may be available' +
        'in the server error log.</p>';
  end;
end;

function signature(const prefix: string; ARequest: TFPHTTPConnectionRequest): string;
var
  name: string;
begin
  if ARequest.Connection.Server.AdminName <> '' then
    name := ARequest.Connection.Server.AdminName
  else
    name := ARequest.Connection.Server.AdminMail;

  if ARequest.Connection.Server.AdminMail <> '' then
    Result := prefix + '<address>' +
      ARequest.Connection.Server.ServerBanner +
      ' Server at <a href="' +
      'mailto:' +
      HTTPEncode(ARequest.Connection.Server.AdminMail) +
      '">' +
      HTTPEncode(name) +
      '</a> Port ' + ARequest.ServerPort +
      '</address>'
  else
    Result := prefix + '<address>' + ARequest.Connection.Server.ServerBanner +
      ' Server at ' +
      ARequest.Connection.Server.AdminMail +
      ' Port ' + ARequest.ServerPort +
      '</address>';
end;

function http_error_response(status: integer; ARequest: TFPHTTPConnectionRequest): string;
var
  title: string;
  h1: string;
begin
  title := Format('%d %s', [status, GetStatusCode(status)]);
  h1 := GetStatusCode(status);

  Result := '<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML//EN">' +
    '<html><head><title>' + title +
    '</title></head><body><h1>' + h1 + '</h1>' +
    error_string(status, ARequest) +
    signature('<hr>', ARequest) +
    '</body></html>';
end;

end.

