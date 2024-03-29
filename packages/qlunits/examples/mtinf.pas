{
    Copyright (c) 2021 Karoly Balogh and Norman Dunbar

    System info/System variables access on a Sinclair QL, QDOS naming
    Example program for Free Pascal's Sinclair QL support

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

program mtinf;

uses
  qdos;

type
  Tver = array[0..3] of AnsiChar;

var
  job_id: longint;
  ver_ascii: longint;
  system_vars: pSystemVariables;

function get_id_str(const id: dword): AnsiString;
begin
  case id of
    SYSID_QL: get_id_str:='QDOS';
    SYSID_AT: get_id_str:='Atari (SMS)';
    SYSID_SQ: get_id_str:='SMSQ';
    SYSID_TH: get_id_str:='Thor (ARGOS)';
  else
    get_id_str:='unknown ($'+hexstr(id,8)+')';
  end;
end;

begin
  job_id:=mt_inf(@system_vars,@ver_ascii);

  writeln('Job ID:',lo(job_id),' Tag:',hi(job_id));
  writeln('Identification: ',get_id_str(system_vars^.SV_IDENT));
  writeln('Version: ',Tver(ver_ascii));

  writeln('System vars are at: $',hexstr(system_vars));
  writeln('Processor type: 680',hexstr(system_vars^.SV_PTYP,2));
  writeln('Monitor mode: ',system_vars^.SV_TVMOD);
  writeln('Random number: ',system_vars^.SV_RAND);
end.
