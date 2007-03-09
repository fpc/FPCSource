unit sdlwebftp;
{******************************************************************************}
{
  $Id: sdlwebftp.pas,v 1.2 2005/01/02 19:03:15 savage Exp $
  
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
  $Log: sdlwebftp.pas,v $
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
  TFTPFiletype = ( ftNone, ftDir, ftFile, ftLink );
  TFTPRemoteFile = record
    Type_ : TFTPFiletype;
    Size : integer;
    Name : string;
    Datetime : TDateTime;
  end;

  TFTPRemoteDirectory = TStringList;
//  TFTPRemoteDirectory = record
//    Files:array of TFTPRemoteFile;
//    DirectoryName: string;
//  end;

  TFTPDownloadProgress = procedure( Position, Max : Integer );

function SDLWeb_FTP_Login( var Connection : TSDLWebConnection;
  Username, Password : string; DirectoryInfo : TFTPRemoteDirectory ) : Boolean;
function SDLWeb_FTP_Logout( var Connection : TSDLWebConnection ) : Boolean;
function SDLWeb_FTP_GetDirectory( var Connection : TSDLWebConnection;
  ADirectory : string; DirectoryInfo : TFTPRemoteDirectory ) : Boolean;
function SDLWeb_FTP_ChangeDirectory( var Connection : TSDLWebConnection;
  ADirectory : string ) : Boolean;
function SDLWeb_FTP_DownloadToStream( var Connection : TSDLWebConnection;
  AFilename : string; var Download : TStream; Progress : TFTPDownloadProgress = nil ) : Boolean;

implementation

uses
  SysUtils,
  sdl_net;

function ReadFTPStatus( var Connection : TSDLWebConnection; var Status : Integer; var Data : string ) : Boolean;
var
  S : string;
begin
  SDLWeb_ReadResponse( Connection, S );
  Status := StrToInt( Copy( S, 1, 3 ) );
  Data := Copy( S, 5, Length( S ) );
  Result := Status <= 400;
end;

{------------------------------------------------------------------------------}
{Extracts Port and IP data from the string provided usially in
 <text> (128,0,0,0,7,534) }
{------------------------------------------------------------------------------}

procedure ExtractIPAndPort( Data : string; var IP : string; var Port : integer );
var
  s : string;
  po : integer;
begin
  s := copy( Data, pos( '(', Data ) + 1, length( Data ) );
  s := copy( s, 1, pos( ')', s ) - 1 );
  // get the 4th , this is the end of the IP address
  po := SDLWeb_Pos( ',', s, 4 );
  IP := copy( s, 1, po - 1 );
  IP := StringReplace( IP, ',', '.', [ rfReplaceAll ] );
  s := copy( s, po + 1, length( s ) );
  Port := strtoint( copy( s, 1, pos( ',', s ) - 1 ) ) * 256
    + strtoint( copy( s, pos( ',', s ) + 1, length( s ) ) );
end;

function GetFTPDataSocket( var Connection : TSDLWebConnection ) : PTCPSocket;
var
  Status, Port : Integer;
  Data, IP : string;
  DataIP : TIPAddress;
begin
  Result := nil;
  SDLWeb_SendRequest( Connection, 'PASV' );
  if ReadFTPStatus( Connection, Status, Data ) then
  begin
    ExtractIPAndPort( Data, IP, Port );
    if SDLNet_ResolveHost( DataIP, PChar( IP ), Port ) = 0 then
      Result := SDLNet_TCP_Open( DataIP );
  end;
end;

function SDLWeb_FTP_Login( var Connection : TSDLWebConnection;
  Username, Password : string; DirectoryInfo : TFTPRemoteDirectory ) : Boolean;
var
  Status : Integer;
  Data : string;
begin
  Result := False;
  if Connection.Type_ <> wcFTP then
    Exit;
  if not ReadFTPStatus( Connection, Status, Data ) then
    Exit;
  SDLWeb_SendRequest( Connection, 'USER ftp' );
  if not ReadFTPStatus( Connection, Status, Data ) then
    Exit;
  SDLWeb_SendRequest( Connection, 'PASS ' );
  if ReadFTPStatus( Connection, Status, Data ) then
  begin
    SDLWeb_SendRequest( Connection, 'TYPE I' );
    if not ReadFTPStatus( Connection, Status, Data ) then
      Exit;
    if DirectoryInfo <> nil then
      Result := SDLWeb_FTP_GetDirectory( Connection, '.', DirectoryInfo )
    else
      Result := True;
  end;
end;

function SDLWeb_FTP_Logout( var Connection : TSDLWebConnection ) : Boolean;
var
  Status : Integer;
  Data : string;
begin
  SDLWeb_SendRequest( Connection, 'QUIT' );
  Result := ReadFTPStatus( Connection, Status, Data );
end;

function ReadFTPData( var Socket : PTCPSocket; var Dest : TStream; var Buffer : Pointer ) : Boolean;
var
  i, l : integer;
  lp : Pointer;
begin
  l := SDLNet_TCP_Recv( Socket, Buffer, 1024 );
  Result := l > 0;
  lp := Buffer;
  if Result then
  begin
    while l > 0 do
    begin
       //i := 0;
      i := Dest.Write( lp^, l );
      dec( l, i );
      lp := pointer( longint( lp ) + i );
    end;
  end;
end;

function SDLWeb_FTP_GetDirectory( var Connection : TSDLWebConnection;
  ADirectory : string; DirectoryInfo : TFTPRemoteDirectory ) : Boolean;
var
  Status : Integer;
  Data : string;
  Buffer : Pointer;
  DirectoryStream : TStream;
  DataSocket : PTCPSocket;

begin
  Result := False;
  if ADirectory = EmptyStr then
    Exit;

  DataSocket := GetFTPDataSocket( Connection );
  if DataSocket = nil then
    Exit;
  try
    SDLWeb_SendRequest( Connection, 'TYPE A' );
    if ReadFTPStatus( Connection, Status, Data ) then
    begin
      SDLWeb_SendRequest( Connection, 'LIST ' + ADirectory );
      if ReadFTPStatus( Connection, Status, Data ) then
      begin
        GetMem( Buffer, 1024 );
        DirectoryStream := TMemoryStream.Create;
        try
          while ReadFTPData( DataSocket, DirectoryStream, Buffer ) do
            ;
          if ReadFTPStatus( Connection, Status, Data ) then
          begin
            SDLWeb_SendRequest( Connection, 'TYPE I' );
            Result := ReadFTPStatus( Connection, Status, Data );
            DirectoryStream.Position := 0;
            DirectoryInfo.Clear;
            DirectoryInfo.LoadFromStream( DirectoryStream );
          end;
        finally
          DirectoryStream.Free;
          FreeMem( Buffer );
        end;
      end;
    end;
  finally
    SDLNet_TCP_Close( DataSocket );
  end;
end;

function SDLWeb_FTP_ChangeDirectory( var Connection : TSDLWebConnection;
  ADirectory : string ) : Boolean;
var
  Status : Integer;
  Data : string;
begin
  SDLWeb_SendRequest( Connection, 'CWD ' + ADirectory );
  Result := ReadFTPStatus( Connection, Status, Data );
end;

function SDLWeb_FTP_DownloadToStream( var Connection : TSDLWebConnection;
  AFilename : string; var Download : TStream; Progress : TFTPDownloadProgress = nil ) : Boolean;
var
  Status, FileSize : Integer;
  Data : string;
  DataSocket : PTCPSocket;
  Buffer : Pointer;

  procedure DoProgress;
  begin
    if Assigned( Progress ) then
      Progress( Download.Position, FileSize );
  end;

begin
// Download a file to a stream
  Result := False;
  SDLWeb_SendRequest( Connection, 'SIZE ' + AFilename );
  if ReadFTPStatus( Connection, Status, Data ) then
  begin
    FileSize := StrToInt( Data );
    DataSocket := GetFTPDataSocket( Connection );
    if DataSocket = nil then
      Exit;
    try
      SDLWeb_SendRequest( Connection, 'RETR ' + AFilename );
      if ReadFTPStatus( Connection, Status, Data ) then
      begin
        GetMem( Buffer, 1024 );
        if Download = nil then
          Download := TMemoryStream.Create;
        try
          DoProgress;
          while ReadFTPData( DataSocket, Download, Buffer ) do
            DoProgress;
          if ReadFTPStatus( Connection, Status, Data ) then
          begin
            SDLWeb_SendRequest( Connection, 'TYPE I' );
            Result := ReadFTPStatus( Connection, Status, Data );
            DoProgress;
            Download.Position := 0;
          end;
        finally
          FreeMem( Buffer );
        end;
      end;
    finally
      SDLNet_TCP_Close( DataSocket );
    end;
  end;
end;

end.
