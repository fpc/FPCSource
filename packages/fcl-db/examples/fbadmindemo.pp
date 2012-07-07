program fbadmindemo;

{
Program that tests/demonstrates Ludo Brands' FBAdmin unit
It shows getting server info, log, and backing up
It doesn't restore as that might delete data.
}
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  ibconnection { for EIBDatabaseError},
  FBAdmin;

function AskUser(const Question: string): string;
begin
  writeln(Question);
  readln(result);
end;

function ConnectToServer(TheServer: TFBAdmin): boolean;
var
  Response:string;
begin
  Response:=AskUser('Host name/IP address (empty for 127.0.0.1)?');
  if trim(Response)='' then Response:='127.0.0.1';
  TheServer.Host:=Response;

  Response:=AskUser('Services port (empty for 3050)?');
  if trim(Response)='' then
    TheServer.Port:=3050
  else
    TheServer.Port:=StrToInt(Response);

  Response:=AskUser('Username (empty for SYSDBA)?');
  if trim(Response)='' then Response:='SYSDBA';
  TheServer.User:=Response;

  Response:=AskUser('Password (empty for masterkey)?');
  if trim(Response)='' then Response:='masterkey';
  TheServer.Password:=Response;

  // Big change server supports TCP/IP
  // Change this if you use embedded.
  TheServer.Protocol:=IBSPTCPIP;

  // We'll just abort our program if there's any error.
  // Easier to use exceptions then.
  TheServer.UseExceptions:=true;
  try
    result:=TheServer.Connect;
  except
    on B: EIBDatabaseError do
    begin
      writeln('Database error: ', B.ClassName, '/', B.Message,
        '. GDS error code: ', B.GDSErrorCode);
    end;
    on E: Exception do
    begin
      writeln('Exception: ', E.ClassName, '/', E.Message);
    end;
  end;
end;

var
  Database: string;
  TheServer:TFBAdmin;
  Users: TStringList;
  // For filling user details:
  GroupName,FirstName,MiddleName,LastName:string;
  UserID, GroupID: longint;
begin
  TheServer:=TFBAdmin.Create(nil);
  try
    if ConnectToServer(TheServer)=false then
    begin
      writeln('Aborting.');
      halt(13);
    end;
    try
      writeln('Server type: '+TheServer.ServerImplementation);
      writeln('Server version: '+TheServer.ServerVersion);
      // Handy to know for backup purposes...
      writeln('Server root directory: '+TheServer.ServerRootDir);
      Users:=TStringList.Create;
      try
        if TheServer.GetUsers(Users) then
          writeln('List of users:'+Users.Text)
        else
          writeln('Sorry, could not get user list.');
      finally
        Users.Free;
      end;

      // Get details for current user:
      if TheServer.GetUser(TheServer.User,GroupName,FirstName,MiddleName,LastName,UserID, GroupID) then
      begin
        writeln('Name:      '+TheServer.User);
        writeln('Full name: '+Trim(Trim(FirstName+Trim(' '+MiddleName)+' ')+LastName));
        writeln('User ID:   '+IntToStr(UserID));
        writeln('Group:     '+GroupName);
        writeln('Group ID:  '+IntToStr(GroupID));
      end
      else
        writeln('Sorry, could not get user details for '+TheServer.User);

      writeln('If you want to try a backup, please enter the');
      writeln('path on the server where the database is.');
      writeln('(Aliases will not work)');
      Database:=Trim(AskUser('Enter nothing if you do not want a backup.'));
      if Database<>'' then
      begin
        writeln('Starting backup to '+Database+'.fbk');
        TheServer.Backup(Database, Database+'.fbk',[],'');
        writeln('Output:');
        writeln(TheServer.Output.Text);
        AskUser('Please press enter to continue...');
      end;

      writeln('Database log:');
      if TheServer.GetDatabaseLog then
        writeln (TheServer.Output.Text)
      else
        writeln('Could not get database log, sorry.');
      //We're at the end so it doesn't matter...
      //AskUser('Please press enter to continue...');
      TheServer.DisConnect;
    except
      on B: EIBDatabaseError do
      begin
        writeln('Database error: ', B.ClassName, '/', B.Message,
          '. GDS error code: ', B.GDSErrorCode);
      end;
      on E: Exception do
      begin
        writeln('Exception: ', E.ClassName, '/', E.Message);
      end;
    end;
  finally
    TheServer.Free;
  end;
  writeln('Program finished.');
end.

