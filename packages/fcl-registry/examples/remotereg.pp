{
  This example program shows how to remotely connect to another machine.
  The machine name is given as UNC name as first argument (e.g.
  "\\SomeComputer") and the username as second argument. The password will be
  queried on the command line by Windows. If the connection is successful the
  keys of HKEY_USERS of the remote machine will be enumerated.

  Tested on a Windows 7 machine connecting to another Windows 7 machine.

  Note: The remote registry service must run on the destination machine.
}

program remotereg;

{$mode objfpc}{$H+}
{$apptype console}

uses
  Classes, sysutils, registry, JwaWinNetWk, JwaWinType, JwaWinError;

var
  reg: TRegistry;
  machine, username, s: String;
  res: NETRESOURCEA;
  err: DWORD;
  sl: TStringList;
begin
  if ParamCount > 0 then
    machine := ParamStr(1)
  else
    machine := '';
  if ParamCount > 1 then
    username := ParamStr(2)
  else
    username := '';

  { if we have a username given then we need to establish a connection first;
    if no username is given then either the current user is the correct one
    or a connection to e.g. share was established already }
  if (machine <> '') and (username <> '') then begin
    Writeln('Connecting to ', machine, ' as ', username);
    FillChar(res, SizeOf(res), 0);
    res.dwType := RESOURCETYPE_ANY;
    res.lpRemoteName := PChar(Format('%s\IPC$', [machine]));
    err := WNetAddConnection2A(res, Nil, PChar(username),
             CONNECT_TEMPORARY or CONNECT_INTERACTIVE or CONNECT_COMMANDLINE);
    { ERROR_SESSION_CREDENTIAL_CONFLICT means that we already connected to the
      host and the connection is still available }
    if (err <> NO_ERROR) and (err <> ERROR_SESSION_CREDENTIAL_CONFLICT) then begin
      Writeln('Error connecting to remote machine ''', machine, ''': ',
        SysErrorMessage(err), ' (', err, ')');
      Exit;
    end;
  end;

  { for this test we want only the right to enumerate subkeys }
  reg := TRegistry.Create(KEY_ENUMERATE_SUB_KEYS);
  try
    { use HKEY_USERS, because the rights of Administrator users on Windows Vista
      and newer are sufficient for enumerating this }
    reg.RootKey := HKEY_USERS;
    if not reg.RegistryConnect(machine) then begin
      Writeln('Error connecting to remote registry');
      Exit;
    end;
    { we need to open the key nevertheless }
    reg.OpenKeyReadOnly('\');
    { now enumerate the subkeys }
    sl := TStringList.Create;
    try
      reg.GetKeyNames(sl);
      Writeln(sl.Count, ' keys found');
      for s in sl do
        Writeln(#9, s);
    finally
      sl.Free;
    end;
    reg.CloseKey;
  finally
    reg.Free;
  end;
  Writeln('Done');
  Readln;
end.

