{
  $Id$

  Program to detect the version of libgdb that will be
  used for linking
}
program find_gdb_version;

{$R-}

{$Linklib gdb}

uses
  strings;

const
{$ifdef OpenBSD}
  ver_name = '_version';
{$else}
{$ifdef unix}
  ver_name = 'version';
{$else not unix}
  ver_name = '_version';
{$endif}
{$endif}

  { This variable should be change with
    change in GDB CVS PM }
  Current_cvs_version : longint = 503;
  Max_version_length = 255;

var
  v5_version : array[0..0] of char;external name ver_name;
  v4_version : pchar;external name ver_name;
  version : pchar;
  subver_str : string;
  i, version_number,
  subversion_number : longint;
  subsubversion_number : longint;
  error : word;
  only_ver : boolean;

begin
  only_ver:=(Paramcount>0) and (ParamStr(1)='-n');
  getmem(version,Max_version_length+1);
  strlcopy(version,@v5_version,Max_version_length);
  version[Max_version_length]:=#0;
  if (version[0] in ['4','5','6','7','8','9']) and (version[1]='.') then
    begin
      if not only_ver then
        Writeln('GDB version is ',pchar(@v5_version));
      version_number:=ord(version[0])-ord('0');
      i:=2;
      subver_str:='';
      while version[i] in ['0'..'9'] do
        begin
          subver_str:=subver_str+version[i];
          inc(i);
        end;
      val(subver_str,subversion_number,error);
      inc(i);
      subver_str:='';
      while version[i] in ['0'..'9'] do
        begin
          subver_str:=subver_str+version[i];
          inc(i);
        end;
      if subver_str<>'' then
        val(subver_str,subsubversion_number,error);
      { 5.02.90 is a pretest of 5.03.. PM }
      if subsubversion_number>=90 then
        inc(subversion_number);
      if (error=0) and (subversion_number>=0) and
         (subversion_number<=99) then
        version_number:=version_number*100+subversion_number;
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
  freemem(version,Max_version_length+1);
  if only_ver then
    Write(version_number);
  Halt(version_number);
end.

{
  $Log$
  Revision 1.6  2003-02-09 19:14:48  marco
   * ugly openbsd fix

  Revision 1.5  2002/12/12 14:17:59  pierre
   * 5.3 was not correctly parsed

  Revision 1.4  2002/12/02 13:59:16  pierre
   convert 5.02.90 into 5.03

  Revision 1.3  2002/09/10 12:17:15  pierre
   * avoid RTE 201

  Revision 1.2  2002/09/07 15:42:52  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:54:49  peter
    * splitted to base and extra

  Revision 1.5  2002/01/25 08:56:50  pierre
   + add subversion for version output

  Revision 1.4  2002/01/24 09:14:40  pierre
   * adapt to GDB 5.1

}
