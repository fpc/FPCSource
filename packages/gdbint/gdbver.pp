{
  $Id$

  Program to detect the version of libgdb that will be
  used for linking
}
program find_gdb_version;

{$Linklib gdb}

uses
  strings;

const
{$ifdef unix}
  ver_name = 'version';
{$else not unix}
  ver_name = '_version';
{$endif}
  { This variable should be change with
    change in GDB CVS PM }
  Current_cvs_version : longint = 502;

var
  v5_version : array[0..0] of char;external name ver_name;
  v4_version : pchar;external name ver_name;
  version : pchar;
  version_number : longint;
  only_ver : boolean;

begin
  only_ver:=(Paramcount>0) and (ParamStr(1)='-n');
  getmem(version,5);
  strlcopy(version,@v5_version,4);
  if (version[0] in ['4','5','6','7','8','9']) and (version[1]='.') then
    begin
      if not only_ver then
        Writeln('GDB version is ',pchar(@v5_version));
      version_number:=ord(version[0])-ord('0');
    end
  else if (version[0]='2') and (version[1]='0') and
          (version[2] in ['0'..'9']) and (version[3] in ['0'..'9']) then
    begin
      { CVS version from 2000 to 2099,
        assume current_cvs_version  PM }
      version_number:=Current_cvs_version;
    end
  else
    begin
      if not only_ver then
        Writeln('GDB version is ',v4_version);
      version_number:=ord(v4_version[0])-ord('0');
    end;
  freemem(version,5);
  if only_ver then
    Write(version_number);
  Halt(version_number);
end.

{
  $Log$
  Revision 1.4  2002-01-24 09:14:40  pierre
   * adapt to GDB 5.1

  Revision 1.3  2001/09/11 10:22:09  pierre
   * Hack to allow to recognize CVS version as 5.01

  Revision 1.2  2001/04/08 11:44:01  peter
    * new file

}
