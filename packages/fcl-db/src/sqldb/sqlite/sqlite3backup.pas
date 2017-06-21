unit sqlite3backup;

{ SQLite3 backup class.

  Copyright (C) 2012 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,sqlite3conn,sqlite3dyn;

type

  TOnBackupProgress = procedure (Sender: TObject; Remaining, PageCount: integer) of object;

  { TSQLite3Backup }

  TSQLite3Backup=class
  private
    FErrorMessage: string;
    FLockReleaseTime: integer;
    FOnBackupProgress: TOnBackupProgress;
    FPageStep: integer;
  public
    constructor Create;
    //Backup one TSQLite3Connection to another TSQLite3Connection
    //SourceDBName and DestinationDBName is "main" for the main database, "temp" for the temporary
    //database, or the name specified after the AS keyword in an ATTACH statement for an
    //attached database
    //LockUntilFinished: Set to false when simultanuous access from other processes is required.
    //The backup process will restart when another process writes to the database.
    //Pro: the backup is a correct snapshot
    //Contra: the backup can take a very long time when a lot of writes to the database are made during backup
    //Warning: don't use the destination TSQLite3Connection (the handle) for anything else while doing a backup.
    //It can corrupt the database and even cause a mutex deadlock.
    function Backup(Source,Destination:TSQLite3Connection;LockUntilFinished:boolean=true;
         SourceDBName:string='main';DestinationDBName:string='main'):boolean;
    //Backup a database to file
    function Backup(Source:TSQLite3Connection;FileName:string;LockUntilFinished:boolean=true;
         SourceDBName:string='main'):boolean;
    //Restore a database from file
    function Restore(FileName:string;Destination:TSQLite3Connection;LockUntilFinished:boolean=true;
         DestinationDBName:string='main'):boolean;
  published
    //Delay between backup steps in ms. Default:100ms. Only used when LockUntilFinished=false
    property LockReleaseTime:integer read FLockReleaseTime write FLockReleaseTime;
    //Page size between backup steps. Default:10 Only used when LockUntilFinished=false
    property PageStep:integer read FPageStep write FPageStep;
    property ErrorMessage:string read FErrorMessage;
    property OnBackupProgress:TOnBackupProgress read FOnBackupProgress write FOnBackupProgress;
  end;

implementation




{ TSQLite3Backup }

constructor TSQLite3Backup.Create;
begin
  FLockReleaseTime:=100;
  FPageStep:=10;
end;

function TSQLite3Backup.Backup(Source, Destination: TSQLite3Connection;
  LockUntilFinished: boolean; SourceDBName: string; DestinationDBName: string
  ): boolean;
var
  pBackup:psqlite3backup;
  nPage:integer;
  res: integer;
begin
   FErrorMessage:='';
   Source.Connected:=true;
   Destination.Connected:=true;
   pBackup := sqlite3_backup_init(Destination.Handle, pchar(DestinationDBName),
      Source.Handle, pchar(SourceDBName));
   if LockUntilFinished then
     nPage:=-1
   else
     nPage:=FPageStep;
   result:=false;
   if assigned(pBackup) then
     begin
{  Each iteration of this loop copies PageStep database pages from database
   pDb to the backup database. If the return value of backup_step()
   indicates that there are still further pages to copy, sleep for
   LockReleaseTime ms before repeating. }
     repeat
       res := sqlite3_backup_step(pBackup, nPage);
       if assigned(FOnBackupProgress) then
         FOnBackupProgress(self,sqlite3_backup_remaining(pBackup),
           sqlite3_backup_pagecount(pBackup));
       if not LockUntilFinished and (res in [SQLITE_OK,SQLITE_BUSY,SQLITE_LOCKED]) then
         sqlite3_sleep(LockReleaseTime);
     until LockUntilFinished or not (res in [SQLITE_OK,SQLITE_BUSY,SQLITE_LOCKED]);
     result:=sqlite3_backup_finish(pBackup)=SQLITE_OK;
     end;
   if not result then
     FErrorMessage:=strpas(sqlite3_errmsg(Destination.Handle));
end;

function TSQLite3Backup.Backup(Source: TSQLite3Connection; FileName: string;
  LockUntilFinished: boolean; SourceDBName: string): boolean;
var conn:TSQLite3Connection;
begin
  conn:=TSQLite3Connection.Create(nil);
  try
    conn.DatabaseName:=FileName;
    conn.Connected:=true;
    result:=Backup(Source,conn,LockUntilFinished,SourceDBName);
  finally
    conn.Destroy;
  end;
end;

function TSQLite3Backup.Restore(FileName: string;
  Destination: TSQLite3Connection; LockUntilFinished: boolean;
  DestinationDBName: string): boolean;
var conn:TSQLite3Connection;
begin
  conn:=TSQLite3Connection.Create(nil);
  try
    conn.DatabaseName:=FileName;
    conn.Connected:=true;
    result:=Backup(conn,Destination,LockUntilFinished,'main',DestinationDBName);
  finally
    conn.Destroy;
  end;
end;

end.

