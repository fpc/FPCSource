program FTPTest;

uses
 FTPApi;

const
 TstName1 = 'FTPTEST1';
 TstName2 = 'FTPTEST2';
{$IFDEF USECON}
 ListName = 'CON';
     (* Using output file name 'CON' here causes output directly to console. *)
{$ELSE USECON}
 ListName = 'FTP.LST';
{$ENDIF USECON}
 TstText = 'This is a sample test file for FTPTest'#13#10'check it for yourself.';

var
 Hst, Usr, Pwd: string;
 T: text;
 L: longint;
 S: string;

begin
 if ParamCount < 1 then
 begin
  WriteLn ('Syntax: FTPTest <host_name> [<user_name> [<password>]] ' + LineEnding + LineEnding +
           '        User "anonymous" is used if no user name is supplied on command line,' + LineEnding +
           '        password is being asked for if omitted');
  Halt (1);
 end;
 WriteLn;
 Hst := ParamStr (1);
 if ParamCount > 1 then
 begin
  Usr := ParamStr (2);
  if ParamCount > 2 then Pwd := ParamStr (3) else
  begin
   Write ('Password for user "' + Usr + '" (it _will_ be displayed on screen!): ');
   ReadLn (Pwd);
  end;
 end else
 begin
  Usr := 'anonymous';
  Pwd := 'nobody@nowhere.com';
 end;
 Write ('Preparing test file ' + TstName1 + '... ');
 Assign (T, TstName1);
 Rewrite (T);
 WriteLn (T, TstText);
 Close (T);
 WriteLn ('done');
 FtpVer (S);
 WriteLn ('FTPAPI.DLL was compiled ', S);
 FtpSetUser (Hst, Usr, Pwd, '');
 WriteLn ('Finding out the current directory on "', Hst, '"...');
 L := FtpPwd (S);
 if L = 0 then WriteLn (S);
 WriteLn ('RC = ', L);
 Write ('The remote system is ');
 L := FtpSys (S);
 if L = 0 then WriteLn (S);
 WriteLn ('RC = ', L);
 WriteLn ('Switching to binary');
 FtpSetBinary (T_BINARY);
 Write ('Uploading "' + TstName1 + '" to "' + Hst + '" as "' + TstName2 + '"... ');
 WriteLn ('RC = ', FtpPut (TstName1, TstName2));
 WriteLn ('Getting dir listing from "' + Hst + '" to "' + ListName + '"... ');
 L := FtpDir (ListName, '');
{$IFNDEF USECON}
 if L = 0 then
 begin
  Assign (T, ListName);
  Reset (T);
  while not (Eof (T)) do
  begin
   ReadLn (T, S);
   WriteLn (S);
  end;
  Close (T);
 end;
{$ENDIF USECON}
 WriteLn ('RC = ', L);
 Write ('Downloading "' + TstName2 + '"... ');
 WriteLn ('RC = ', FtpGet (TstName2, TstName2, 'w'));
 Write ('Setting time of the downloaded file to that one on server... ');
 WriteLn ('Result = ', Keep_File_Date (TstName2, TstName2));
 Write ('Removing "', TstName2, '" again from "', Hst, '"... ');
 WriteLn ('RC = ', FtpDelete (TstName2));
 Write ('Closing all connections... ');
 FtpLogoff;
 WriteLn ('done.');
 WriteLn (LineEnding + 'Finished.');
end.
