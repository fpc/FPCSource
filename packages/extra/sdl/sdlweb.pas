unit sdlweb;
{******************************************************************************}
{
  $Id: sdlweb.pas,v 1.2 2005/01/02 19:03:15 savage Exp $
  
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
{   sdl.pas and sdl_net.pas somehere in your search path                       }
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
  $Log: sdlweb.pas,v $
  Revision 1.2  2005/01/02 19:03:15  savage
  Slight Bug fix due to stray closed comment ( Thanks Michalis Kamburelis )

  Revision 1.1  2004/12/31 00:30:12  savage
  Initial release of Dean's excellent SDL Web classes. Demo coming soon.


}
{******************************************************************************}

interface

uses
  SysUtils,
  sdl,
  sdl_net;

const
  MAX_TIMEOUT = 5000;

type

  TWebConnection = ( wcDefault, wcHTTP, wcFTP );
  TWebProgressEvent = procedure( Progress, Total : UInt32 ); cdecl;

  TSDLWebConnection = record
    Host : TIPAddress;
    HostName, Proxy : string;
    Port : UInt32;
    Socket : PTCPSocket;
    Type_ : TWebConnection;
    IsProxy : Boolean;
    ProxyUser, ProxyPassword : string;
  end;

procedure SDLWeb_Init;
procedure SDLWeb_Quit;
  // Connection Functions
function SDLWeb_ConnectToSite( URL : string; AType : TWebConnection;
  var Connection : TSDLWebConnection ) : Boolean;
function SDLWeb_ConnectToSiteViaProxy( URL, Proxy, Username, Password : string; AType : TWebConnection;
  var Connection : TSDLWebConnection ) : Boolean;
function SDLWeb_Connected( var Connection : TSDLWebConnection ) : Boolean;
procedure SDLWeb_Disconnect( var Connection : TSDLWebConnection );
function SDLWeb_SendRequest( var Connection : TSDLWebConnection;
  Request : string ) : Boolean;
function SDLWeb_ReadResponse( var Connection : TSDLWebConnection;
  var Response : string ) : UInt32;

  // Utility Functions
function SDLWeb_IPToString( ip : TIPAddress ) : string;
function SDLWeb_StringToIP( ip : string ) : TIPAddress;
function SDLWeb_Pos( const SubString, Source : string; Count : SInt32 ) : SInt32;
procedure SDLWeb_ParseURL( const url : string; var proto, user, pass, host, port, path : string );

function SDLWeb_EncodeBase64( Value : string ) : string;
function SDLWeb_DecodeBase64( Value : string ) : string;

implementation

const
  DefaultPorts : array[ TWebConnection ] of UInt32 = ( 80, 80, 21 );

procedure SDLWeb_Init;
begin
  SDLNet_Init;
end;

procedure SDLWeb_Quit;
begin
  SDLNet_Quit;
end;

function SDLWeb_ConnectToSite( URL : string; AType : TWebConnection;
  var Connection : TSDLWebConnection ) : Boolean;
var
  Protocol, User, Password, Host, Port, Path : string;
begin
  Result := False;
  SDLWeb_ParseURL( URL, Protocol, User, Password, Host, Port, Path );
  Connection.IsProxy := False;
  Connection.HostName := Host;
  if SDLNet_ResolveHost( Connection.Host, PChar( Host ), DefaultPorts[ AType ] ) = 0 then
  begin
    Connection.Socket := SDLNet_TCP_Open( Connection.Host );
    Connection.Port := DefaultPorts[ AType ];
    if AType = wcDefault then
    begin
      if UpperCase( Protocol ) = 'HTTP' then
        Connection.Type_ := wcHTTP;
      if UpperCase( Protocol ) = 'FTP' then
        Connection.Type_ := wcFTP;
    end
    else
      Connection.Type_ := AType;
    Result := SDLWeb_Connected( Connection );
  end;
end;

function SDLWeb_ConnectToSiteViaProxy( URL, Proxy, Username, Password : string; AType : TWebConnection;
  var Connection : TSDLWebConnection ) : Boolean;
begin
  Result := SDLWeb_ConnectToSite( Proxy, AType, Connection );
  if Result then
  begin
    Connection.Proxy := Proxy;
    Connection.HostName := URL;
    Connection.ProxyUser := Username;
    Connection.ProxyPassword := Password;
    Connection.IsProxy := True;
  end;
end;

function SDLWeb_Connected( var Connection : TSDLWebConnection ) : Boolean;
begin
  Result := Connection.Socket <> nil;
end;

procedure SDLWeb_Disconnect( var Connection : TSDLWebConnection );
begin
  SDLNet_TCP_Close( Connection.Socket );
  Connection.Socket := nil;
end;

function SDLWeb_SendRequest( var Connection : TSDLWebConnection;
  Request : string ) : Boolean;
var
  Error, Len : UInt32;
  cdata : array[ 0..255 ] of char;
begin
  StrPCopy( cdata, Request + #13#10 );
  Len := StrLen( cdata );
  Error := SDLNet_TCP_Send( Connection.Socket, @cdata, Len );
  Result := Error = Len;
end;

function SDLWeb_ReadResponse( var Connection : TSDLWebConnection;
  var Response : string ) : UInt32;

var
  SocketSet : PSDLNet_SocketSet;

  function ReadLine : string;
  var
    Done : Boolean;
    C : Char;
    Error, SocketResult : Sint32;
  begin
    Result := EmptyStr;
    Done := False;
    SDLNet_TCP_AddSocket( SocketSet, Connection.Socket );
    while not Done do
    begin
      SocketResult := SDLNet_CheckSockets( SocketSet, MAX_TIMEOUT );
      if ( SocketResult <= 0 ) then
      begin
        Result := 'HTTP/1.1 400 Socket Timeout';
        Exit;
      end;
      if SDLNet_SocketReady( PSDLNet_GenericSocket( Connection.Socket ) ) then
      begin
        Error := SDLNet_TCP_Recv( Connection.Socket, @C, 1 );
        Done := ( Error < 1 );
        if C = #13 then
        else if C = #10 then
          Done := True
        else
          Result := Result + C;
      end;
    end;
  end;

begin
  SocketSet := SDLNet_AllocSocketSet( 1 );
  try
    Response := ReadLine;
    Result := Length( Response );
  finally
    SDLNet_FreeSocketSet( SocketSet );
  end;
end;

function SDLWeb_IPToString( ip : TIPAddress ) : string;
var
  IpAddress : UInt32;
begin
  IpAddress := SDL_Swap32( ip.host );
  // output the IP address nicely
  Result := format( '%d.%d.%d.%d', [ IpAddress shr 24, ( IpAddress shr 16 ) and $000000FF,
    ( IpAddress shr 8 ) and $000000FF, IpAddress and $000000FF ] );
end;

function SDLWeb_StringToIP( ip : string ) : TIPAddress;
begin
  SDLNet_ResolveHost( Result, PChar( ip ), 0 );
end;

function SDLWeb_Pos( const SubString, Source : string; Count : SInt32 ) : SInt32;
var
  i, h, last : integer;
  u : string;
begin
  u := Source;
  if count > 0 then
  begin
    result := length( Source );
    for i := 1 to count do
    begin
      h := pos( SubString, u );
      if h > 0 then
        u := copy( u, pos( SubString, u ) + 1, length( u ) )
      else
      begin
        u := '';
        inc( result );
      end;
    end;
    result := result - length( u );
  end
  else if count < 0 then
  begin
    last := 0;
    for i := length( Source ) downto 1 do
    begin
      u := copy( Source, i, length( Source ) );
      h := pos( SubString, u );
      if ( h <> 0 ) and ( h + i <> last ) then
      begin
        last := h + i - 1;
        inc( count );
        if count = 0 then
          BREAK;
      end;
    end;
    if count = 0 then
      result := last
    else
      result := 0;
  end
  else
    result := 0;
end;

procedure SDLWeb_ParseURL( const url : string; var proto, user, pass, host, port, path : string );
var
  p, p2 : integer;
  s : string;
begin
  host := '';
  path := '';
  proto := 'http';
  port := '80';
  p := Pos( '://', url );
  if p > 0 then
  begin
    // get protocol
    proto := Copy( url, 1, p - 1 );
    inc( p, 2 );
    s := copy( url, p + 1, length( url ) );
    // get path
    p := pos( '/', s );
    if p = 0 then
      p := length( s ) + 1;
    path := copy( s, p, length( s ) );
    s := copy( s, 1, p - 1 );
    // get host
    p := pos( ':', s );
    if p > Length( s ) then
      p := 0;
    p2 := SDLWeb_Pos( '@', s, -1 );
    if p2 > length( s ) then
      p2 := 0;
    if ( p = 0 ) and ( p2 = 0 ) then
    begin (* no user, password or port *)
      host := s;
      exit;
    end
    else if p2 < p then
    begin (* a port given *)
      port := copy( s, p + 1, length( s ) );
      host := copy( s, p2 + 1, p - p2 - 1 );
      if p2 = 0 then
        exit; (* no user, password *)
      s := copy( s, 1, p2 - 1 );
    end
    else
    begin
      host := copy( s, p2 + 1, length( s ) );
      s := copy( s, 1, p2 - 1 );
    end;
    p := pos( ':', s );
    if p = 0 then
      user := s
    else
    begin
      user := copy( s, 1, p - 1 );
      pass := copy( s, p + 1, length( s ) );
    end;
  end;
end;


function SDLWeb_EncodeBase64( Value : string ) : string;
var
  Position, Total, Remaining : Integer;
  InBlock : array[ 0..2 ] of Byte;
  OutBlock : array[ 0..3 ] of Char;
const
  Base64Chars : array[ 0..63 ] of Char = ( 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '0',
    '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/' );
begin
  Result := '';

  Total := Length( Value );

  Position := 1;

  while True do
  begin
    Remaining := Total - Position + 1;

    if Remaining <= 0 then
      Break;

    FillChar( InBlock, SizeOf( InBlock ), #0 );

    InBlock[ 0 ] := Ord( Value[ Position + 0 ] );
    if Remaining >= 2 then
      InBlock[ 1 ] := Ord( Value[ Position + 1 ] );
    if Remaining >= 3 then
      InBlock[ 2 ] := Ord( Value[ Position + 2 ] );
    Inc( Position, 3 );

    FillChar( OutBlock, SizeOf( OutBlock ), '=' );

    OutBlock[ 0 ] := Base64Chars[ ( InBlock[ 0 ] and $FC ) shr 2 ];
    OutBlock[ 1 ] := Base64Chars[ ( ( InBlock[ 0 ] and $03 ) shl 4 ) or
      ( ( InBlock[ 1 ] and $F0 ) shr 4 ) ];
    if Remaining >= 2 then
      OutBlock[ 2 ] := Base64Chars[ ( ( InBlock[ 1 ] and $0F ) shl 2 ) or
        ( ( InBlock[ 2 ] and $C0 ) shr 6 ) ];
    if Remaining >= 3 then
      OutBlock[ 3 ] := Base64Chars[ InBlock[ 2 ] and $3F ];

    Result := Result + OutBlock;
  end;
end;

function SDLWeb_DecodeBase64( Value : string ) : string;
var
  Position, Total, Remaining : Integer;
  InBlock : array[ 0..3 ] of Byte;
  OutBlock : array[ 0..2 ] of Char;

  function Base64Ord( Value : Char ) : Byte;
  const
    BASE64_FIRST_UPPER = 0;
    BASE64_FIRST_LOWER = 26;
    BASE64_FIRST_NUMBER = 52;
    BASE64_PLUS = 62;
    BASE64_SLASH = 63;
  begin
    if ( Ord( Value ) >= Ord( 'A' ) ) and ( Ord( Value ) <= Ord( 'Z' ) ) then
      Result := Ord( Value ) - Ord( 'A' ) + BASE64_FIRST_UPPER
    else if ( Ord( Value ) >= Ord( 'a' ) ) and ( Ord( Value ) <= Ord( 'z' ) ) then
      Result := Ord( Value ) - Ord( 'a' ) + BASE64_FIRST_LOWER
    else if ( Ord( Value ) >= Ord( '0' ) ) and ( Ord( Value ) <= Ord( '9' ) ) then
      Result := Ord( Value ) - Ord( '0' ) + BASE64_FIRST_NUMBER
    else if Ord( Value ) = Ord( '+' ) then
      Result := BASE64_PLUS
    else if Ord( Value ) = Ord( '/' ) then
      Result := BASE64_SLASH
    else
      Result := 0;
  end;

begin
  Result := '';

  Total := Length( Value );

  Position := 1;

  while True do
  begin
    Remaining := Total - Position + 1;

    if Remaining <= 0 then
      Break;

    FillChar( InBlock, SizeOf( InBlock ), #0 );

    InBlock[ 0 ] := Base64Ord( Value[ Position + 0 ] );
    if Remaining >= 2 then
      InBlock[ 1 ] := Base64Ord( Value[ Position + 1 ] );
    if Remaining >= 3 then
      InBlock[ 2 ] := Base64Ord( Value[ Position + 2 ] );
    if Remaining >= 4 then
      InBlock[ 3 ] := Base64Ord( Value[ Position + 3 ] );
    Inc( Position, 4 );

    OutBlock[ 0 ] := Chr( ( ( InBlock[ 0 ] and $3F ) shl 2 ) or
      ( ( InBlock[ 1 ] and $30 ) shr 4 ) );
    OutBlock[ 1 ] := Chr( ( ( InBlock[ 1 ] and $0F ) shl 4 ) or
      ( ( InBlock[ 2 ] and $3C ) shr 2 ) );
    OutBlock[ 2 ] := Chr( ( ( InBlock[ 2 ] and $03 ) shl 6 ) or
      ( InBlock[ 3 ] and $3F ) );

    Result := Result + OutBlock;
  end;
end;

end.

