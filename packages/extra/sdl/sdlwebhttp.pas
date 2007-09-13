unit sdlwebhttp;
{******************************************************************************}
{
  $Id: sdlwebhttp.pas,v 1.2 2005/01/02 19:03:15 savage Exp $
  
}
{                                                                              }
{       Borland Delphi SDL_Net - A x-platform network library for use with SDL.}
{       Conversion of the Simple DirectMedia Layer Network Headers             }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_net.h                                           }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dean Ellis <technomage@delphigamer.com>                                      }
{                                                                              }
{ Portions created by Dean Ellis are                                           }
{ Copyright (C) 2000 - 2001 Dean Ellis.                                        }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   sdlweb.pas somehere in your search path                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }                                                                          
{
  $Log: sdlwebhttp.pas,v $
  Revision 1.2  2005/01/02 19:03:15  savage
  Slight Bug fix due to stray closed comment ( Thanks Michalis Kamburelis )

  Revision 1.1  2004/12/31 00:30:14  savage
  Initial release of Dean's excellent SDL Web classes. Demo coming soon.


}
{******************************************************************************}

interface

uses
  Classes,
  sdlweb;


type
  TContentEncoding = ( ceNone, cegzip, cedeflate, ceform, cexml );
  TContentEncodings = set of TContentEncoding;
  TTransferEncoding = ( teNone, teChunked );
  TResponseResult = ( rsOK, rsError, rsProxyAutheticate );

function SDL_Web_HTTP_GetPageToString( var Connection : TSDLWebConnection;
  Page : string; var Response : string; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceNone ) : Boolean;
function SDL_Web_HTTP_GetPageToStream( var Connection : TSDLWebConnection;
  Page : string; var AResponse : TStream; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceNone ) : Boolean;
function SDL_Web_HTTP_PostString( var Connection : TSDLWebConnection;
  TargetPage : string; Post : string; var Response : string; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceform ) : Boolean;
function SDL_Web_HTTP_PostStream( var Connection : TSDLWebConnection;
  TargetPage : string; APost : TStream; var AResponse : TStream; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceform ) : Boolean;

implementation

uses
  SysUtils,
  sdl_net;

const
  MAX_BUFFER_SIZE = 1024;
  HTTP_VER = '1.1';

type
  THTTPResponse = record
    Status : Integer;
    ServerType : string;
    ContentLength : Integer;
    ContentType : string;
    ContentEncoding : TContentEncoding;
    TransferEncoding : TTransferEncoding;
    Closed : Boolean;
  end;

  THTTPRequest = record
    Host : string;
    Username : string;
    Password : string;
    Length : Integer;
  end;

var
  Response : THTTPResponse;
  Request : THTTPRequest;

{------------------------------------------------------------------------------}
{Sends a HTTP Request to the Connection}
{------------------------------------------------------------------------------}

procedure SendHTTPRequest( var Connection : TSDLWebConnection; Method, Version, Page : string; Encoding : TContentEncoding = ceNone );
const
  ContentEncoding : array[ TContentEncoding ] of string = ( '', 'gzip', 'deflate', 'application/x-www-form-urlencoded', 'text/xml' );
begin
  if SDLWeb_Connected( Connection ) then
  begin
    if not Connection.IsProxy then
      SDLWeb_SendRequest( Connection, Method + ' ' + Page + ' HTTP/' + Version );
    if Version = '1.1' then
    begin
      if not Connection.IsProxy then
        SDLWeb_SendRequest( Connection, 'Host: ' + Connection.HostName )
      else
      begin
        SDLWeb_SendRequest( Connection, Method + ' http://' + Connection.HostName + Page + ' HTTP/' + Version );
        SDLWeb_SendRequest( Connection, 'Host: ' + Connection.Proxy );
      end;

      if Method = 'GET' then
      begin
        SDLWeb_SendRequest( Connection, 'Accept: text/html, text/xml, text/txt, application/zip, application/x-zip-compressed, */*' );
        SDLWeb_SendRequest( Connection, 'Accept-CharSet: *' );
        SDLWeb_SendRequest( Connection, 'Accept-Language: en' );
        SDLWeb_SendRequest( Connection, 'User-Agent: SDLWeb/1.0' );
        SDLWeb_SendRequest( Connection, 'Cache-Control: no-store, no-cache' );
        SDLWeb_SendRequest( Connection, 'Pragma: no-cache' );
      end;
      if Encoding <> ceNone then
        SDLWeb_SendRequest( Connection, 'Accept-Encoding: ' + ContentEncoding[ Encoding ] );
      if Connection.IsProxy then
      begin

        SDLWeb_SendRequest( Connection, 'Proxy-Authorization: Basic ' +
          SDLWeb_EncodeBase64( Connection.ProxyUser + ':' + Connection.ProxyPassword ) );
      end;
    end;
    if Method = 'POST' then
    begin
      SDLWeb_SendRequest( Connection, 'Content-Encoding: ' + ContentEncoding[ Encoding ] );
      SDLWeb_SendRequest( Connection, 'Content-Length:' + IntToStr( Request.Length ) );
    end;
    SDLWeb_SendRequest( Connection, EmptyStr );
  end;
end;

{------------------------------------------------------------------------------}
{Reads a HTTP Response from the Connection}
{------------------------------------------------------------------------------}

function ReadHTTPResponse( var Connection : TSDLWebConnection; var Error : string ) : TResponseResult;
var
  s, text : string;
  status : integer;
begin
  Result := rsOK;
  //status := 0;
  Response.Status := 0;
  Response.ServerType := EmptyStr;
  Response.ContentType := EmptyStr;
  Response.ContentLength := -1;
  Response.ContentEncoding := ceNone;
  Response.TransferEncoding := teNone;
  repeat
    SDLWeb_ReadResponse( Connection, S );
    if ( CompareText( copy( s, 1, 8 ), 'HTTP/1.1' ) = 0 ) or
      ( CompareText( copy( s, 1, 8 ), 'HTTP/1.0' ) = 0 ) then
    begin
      status := StrToInt( copy( s, 10, 3 ) );
      text := copy( s, 14, length( s ) );
      Response.Status := status;
      if status >= 400 then
      begin
        Result := rsError;
        if not Connection.IsProxy then
          Break;
        if CompareText( copy( s, 1, 19 ), 'Proxy-Authenticate:' ) = 0 then
        begin
            // get the proxy authenticate method BASIC and resend the request.
          Result := rsProxyAutheticate;
        end;
      end;
    end;
    if CompareText( copy( s, 1, 7 ), 'Server:' ) = 0 then
    begin
      Response.ServerType := copy( s, 8, 255 );
    end;
    if CompareText( copy( s, 1, 11 ), 'Connection:' ) = 0 then
    begin
      Response.Closed := CompareText( copy( s, 13, 255 ), 'Close' ) = 0;
    end;
    if CompareText( copy( s, 1, 15 ), 'Content-Length:' ) = 0 then
    begin
      Response.ContentLength := StrToInt( copy( s, 16, 255 ) );
    end;
    if CompareText( copy( s, 1, 13 ), 'Content-Type:' ) = 0 then
    begin
      Response.ContentType := copy( s, 14, 255 );
    end;
    if CompareText( copy( s, 1, 17 ), 'Content-Encoding:' ) = 0 then
    begin
      if Pos( 'gzip', copy( s, 18, 255 ) ) > 0 then
        Response.ContentEncoding := cegzip;
      if Pos( 'deflate', copy( s, 18, 255 ) ) > 0 then
        Response.ContentEncoding := cedeflate;
    end;
    if CompareText( copy( s, 1, 18 ), 'Transfer-Encoding:' ) = 0 then
    begin
      if Pos( 'chunked', copy( s, 19, 255 ) ) > 0 then
        Response.TransferEncoding := teChunked;
    end;
  until S = EmptyStr;
  Error := Text;
end;

{------------------------------------------------------------------------------}
{Reads a file from the HTTP Server into a string}
{------------------------------------------------------------------------------}

function SDL_Web_HTTP_GetPageToString( var Connection : TSDLWebConnection;
  Page : string; var Response : string; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceNone ) : Boolean;
var
  Stream : TStream;
begin
  Response := EmptyStr;
  Stream := TStringStream.Create( Response );
  try
    Result := SDL_Web_HTTP_GetPageToStream( Connection, Page, Stream, Progress, Encoding );
    //if Result then
    Response := TStringStream( Stream ).DataString;
  finally
    Stream.Free;
  end;
end;

{------------------------------------------------------------------------------}
{Reads a file from the HTTP Server into a Stream}
{------------------------------------------------------------------------------}

function SDL_Web_HTTP_GetPageToStream( var Connection : TSDLWebConnection;
  Page : string; var AResponse : TStream; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceNone ) : Boolean;
var
  Protocol, User, Password, Host, Port, Path, Error : string;
  Buffer : array[ 0..MAX_BUFFER_SIZE ] of Byte;
  BufferStream : TMemoryStream;

  function HexToInt( HexNum : string ) : LongInt;
  begin
    Result := StrToInt( '$' + HexNum );
  end;

  function ReadHTTPChunkData : Boolean;
  var
    i, l, error : integer;
    lp : Pointer;
    SocketSet : PSDLNet_SocketSet;
    sResponse : string;
    c : char;
  begin
    Result := False;
 { DONE 1 -oDRE -cHTTP : Implement Transfer encoding support }
    SocketSet := SDLNet_AllocSocketSet( 1 );
    try
      SDLNet_TCP_AddSocket( SocketSet, Connection.Socket );
      if ( Response.ContentLength = -1 ) or ( BufferStream.Size < Response.ContentLength ) then
      begin
        if SDLNet_CheckSockets( SocketSet, MAX_TIMEOUT ) > 0 then
        begin
          if SDLNet_SocketReady( PSDLNet_GenericSocket( Connection.Socket ) ) then
          begin
           // read the chunk data
            sResponse := EmptyStr;
            while true do
            begin
              Error := SDLNet_TCP_Recv( Connection.Socket, @C, 1 );
              if error <> 1 then
                Break;
              if C = #13 then
              else if C = #10 then
                Break
              else
              begin
                sResponse := sResponse + C;
              end;
            end;
            l := MAX_BUFFER_SIZE;
            if sResponse <> EmptyStr then
              l := HexToInt( sResponse ) + 2;
            l := SDLNet_TCP_Recv( Connection.Socket, @Buffer, l );
            Result := l > 0;
            lp := @Buffer;
            if Result then
            begin
              while l > 0 do
              begin
                i := BufferStream.Write( lp^, l );
                dec( l, i );
                lp := pointer( longint( lp ) + i );
              end;
              BufferStream.Position := BufferStream.Position - 2; // take off #13#10
              if Assigned( Progress ) then
                Progress( BufferStream.Size, Response.ContentLength )
            end;
          end;
        end;
      end;
    finally
      SDLNet_FreeSocketSet( SocketSet );
    end;
  end;

{--------------------------------------------}
{Reads Data from the Socket}
{--------------------------------------------}

  function ReadHTTPData : Boolean;
  var
    i, l : integer;
    lp : Pointer;
    SocketSet : PSDLNet_SocketSet;
  begin
    Result := False;
    if Response.TransferEncoding = teChunked then
    begin
      Result := ReadHTTPChunkData;
      Exit;
    end;
 { DONE 1 -oDRE -cHTTP : Implement Transfer encoding support }
    SocketSet := SDLNet_AllocSocketSet( 1 );
    try
      SDLNet_TCP_AddSocket( SocketSet, Connection.Socket );
      if ( Response.ContentLength = -1 ) or ( BufferStream.Size < Response.ContentLength ) then
      begin
        if SDLNet_CheckSockets( SocketSet, MAX_TIMEOUT ) > 0 then
        begin
          if SDLNet_SocketReady( PSDLNet_GenericSocket( Connection.Socket ) ) then
          begin
            l := SDLNet_TCP_Recv( Connection.Socket, @Buffer, MAX_BUFFER_SIZE );
            Result := l > 0;
            lp := @Buffer;
            if Result then
            begin
              while l > 0 do
              begin
                i := BufferStream.Write( lp^, l );
                dec( l, i );
                lp := pointer( longint( lp ) + i );
              end;
              if Assigned( Progress ) then
                Progress( BufferStream.Size, Response.ContentLength )
            end;
          end;
        end;
      end;
    finally
      SDLNet_FreeSocketSet( SocketSet );
    end;
  end;


begin
  Result := False;
  if Connection.Type_ <> wcHTTP then
    Exit;
  SDLWeb_ParseURL( Page, Protocol, User, Password, Host, Port, Path );
  if Path = EmptyStr then
    Path := Page;
  Request.Host := Host;
  if Request.Host = EmptyStr then
    Request.Host := Connection.HostName;
  Request.Host := Request.Host + ':' + Port;
  Request.Username := User;
  Request.Password := Password;
  SendHTTPRequest( Connection, 'GET', HTTP_VER, Path, Encoding );
  if ReadHTTPResponse( Connection, Error ) in [ rsError ] then
  begin
    AResponse.WriteBuffer( PChar( Error )^, Length( Error ) );
    Exit;
  end;
  // put this in another thread????
  BufferStream := TMemoryStream.Create;
  try
    while ReadHTTPData do
      ;
    BufferStream.Position := 0;
    case Response.ContentEncoding of
      ceNone : AResponse.CopyFrom( BufferStream, 0 );
    else
      Exit;
    end;
    Result := True;
  finally
    BufferStream.Free;
  end;
end;

function SDL_Web_HTTP_PostString( var Connection : TSDLWebConnection;
  TargetPage : string; Post : string; var Response : string; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceform ) : Boolean;
var
  Stream, AResponse : TStream;
begin
  Response := EmptyStr;
  Stream := TStringStream.Create( Post );
  AResponse := TStringStream.Create( Post );
  try
    Result := SDL_Web_HTTP_PostStream( Connection, TargetPage, Stream, AResponse, Progress, Encoding );
    //if Result then
    Response := TStringStream( AResponse ).DataString;
  finally
    AResponse.Free;
    Stream.Free;
  end;
end;

function SDL_Web_HTTP_PostStream( var Connection : TSDLWebConnection;
  TargetPage : string; APost : TStream; var AResponse : TStream; Progress : TWebProgressEvent = nil; Encoding : TContentEncoding = ceform ) : Boolean;
var
  Protocol, User, Password, Host, Port, Path, Error : string;
  Buffer : array[ 0..MAX_BUFFER_SIZE ] of Byte;
  BytesSent, Bytes : Integer;
  BufferStream, PostStream : TStream;

  function HexToInt( HexNum : string ) : LongInt;
  begin
    Result := StrToInt( '$' + HexNum );
  end;

  function ReadHTTPChunkData : Boolean;
  var
    i, l, error : integer;
    lp : Pointer;
    SocketSet : PSDLNet_SocketSet;
    sResponse : string;
    c : char;
  begin
    Result := False;
 { TODO 1 -oDRE -cHTTP : Implement Transfer encoding support }
    SocketSet := SDLNet_AllocSocketSet( 1 );
    SDLNet_TCP_AddSocket( SocketSet, Connection.Socket );
    if ( Response.ContentLength = -1 ) or ( BufferStream.Size < Response.ContentLength ) then
    begin
      if SDLNet_CheckSockets( SocketSet, MAX_TIMEOUT ) > 0 then
      begin
        if SDLNet_SocketReady( PSDLNet_GenericSocket( Connection.Socket ) ) then
        begin
         // read the chunk data
         //
          sResponse := EmptyStr;
          while true do
          begin
            Error := SDLNet_TCP_Recv( Connection.Socket, @C, 1 );
            if error <> 1 then
              Break;
            if C = #13 then
            else if C = #10 then
              Break
            else
            begin
              sResponse := sResponse + C;
            end;
          end;
          l := MAX_BUFFER_SIZE;
          if sResponse <> EmptyStr then
            l := HexToInt( sResponse ) + 2;
          l := SDLNet_TCP_Recv( Connection.Socket, @Buffer, l );
          Result := l > 0;
          lp := @Buffer;
          if Result then
          begin
            while l > 0 do
            begin
              i := BufferStream.Write( lp^, l );
              dec( l, i );
              lp := pointer( longint( lp ) + i );
            end;
            BufferStream.Position := BufferStream.Position - 2; // take off #13#10
            if Assigned( Progress ) then
              Progress( BufferStream.Size, Response.ContentLength )
          end;
        end;
      end;
    end;
    SDLNet_FreeSocketSet( SocketSet );
  end;

{--------------------------------------------}
{Reads Data from the Socket}
{--------------------------------------------}

  function ReadHTTPData : Boolean;
  var
    i, l : integer;
    lp : Pointer;
    SocketSet : PSDLNet_SocketSet;
  begin
    Result := False;
    if Response.TransferEncoding = teChunked then
    begin
      Result := ReadHTTPChunkData;
      Exit;
    end;
 { DONE 1 -oDRE -cHTTP : Implement Transfer encoding support }
    SocketSet := SDLNet_AllocSocketSet( 1 );
    SDLNet_TCP_AddSocket( SocketSet, Connection.Socket );
    if ( Response.ContentLength = -1 ) or ( BufferStream.Size < Response.ContentLength ) then
    begin
      if SDLNet_CheckSockets( SocketSet, MAX_TIMEOUT ) > 0 then
      begin
        if SDLNet_SocketReady( PSDLNet_GenericSocket( Connection.Socket ) ) then
        begin
          l := SDLNet_TCP_Recv( Connection.Socket, @Buffer, MAX_BUFFER_SIZE );
          Result := l > 0;
          lp := @Buffer;
          if Result then
          begin
            while l > 0 do
            begin
              i := BufferStream.Write( lp^, l );
              dec( l, i );
              lp := pointer( longint( lp ) + i );
            end;
            if Assigned( Progress ) then
              Progress( BufferStream.Size, Response.ContentLength )
          end;
        end;
      end;
    end;
    SDLNet_FreeSocketSet( SocketSet );
  end;

begin
  Result := False;
  if Connection.Type_ <> wcHTTP then
    Exit;
  SDLWeb_ParseURL( TargetPage, Protocol, User, Password, Host, Port, Path );
  if Path = EmptyStr then
    Path := TargetPage;
  Request.Host := Host;
  if Request.Host = EmptyStr then
    Request.Host := Connection.HostName;
  Request.Username := User;
  Request.Password := Password;
  PostStream := TMemoryStream.Create;
  try
    PostStream.CopyFrom( APost, APost.Size );
    PostStream.Position := 0;
    Request.Length := PostStream.Size;
    SendHTTPRequest( Connection, 'POST', HTTP_VER, Path, Encoding );
    if ReadHTTPResponse( Connection, Error ) in [ rsError ] then
    begin
      AResponse.WriteBuffer( PChar( Error )^, Length( Error ) );
      Exit;
    end;
  // send the data in the stream
    BytesSent := 0;
    while BytesSent < PostStream.Size do
    begin
      Bytes := PostStream.Read( Buffer, MAX_BUFFER_SIZE );
      if SDLNet_TCP_Send( Connection.Socket, @Buffer, Bytes ) = Bytes then
        inc( BytesSent, Bytes );
    end;
  finally
    PostStream.Free;
  end;
  if ReadHTTPResponse( Connection, Error ) in [ rsError ] then
  begin
    AResponse.WriteBuffer( PChar( Error )^, Length( Error ) );
    Exit;
  end;
  BufferStream := TMemoryStream.Create;
  try
    while ReadHTTPData do
      ;
    BufferStream.Position := 0;
    case Response.ContentEncoding of
      ceNone : AResponse.CopyFrom( BufferStream, 0 );
    else
      Exit;
    end;
    Result := True;
  finally
    BufferStream.Free;
  end;
end;

end.

