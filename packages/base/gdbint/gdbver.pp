{

  Program to detect the version of libgdb that will be
  used for linking
}
program find_gdb_version;

{$R-}

{$ifdef unix}
  {$Linklib c}
{$endif}

{$LINKLIB libgdb.a}

uses
  strings;

const
  { This variable should be change with change in GDB CVS PM }
  Current_cvs_version : longint = 503;
  Max_version_length = 255;

const
  output_file:string='';

var
  version : array[0..0] of char;cvar;external;
  gdbversion : pchar;
  subver_str : string;
  i, version_number,
  subversion_number : longint;
  subsubversion_number : longint;
  error : word;
  only_ver : boolean;
  o : text;

begin
  only_ver:=(Paramcount>0) and (ParamStr(1)='-n');
  if (paramcount>=2) and (paramstr(1)='-o') then
    begin
      only_ver:=true;
      output_file:=paramstr(2);
    end;
  getmem(gdbversion,Max_version_length+1);
  strlcopy(gdbversion,@version,Max_version_length);
  gdbversion[Max_version_length]:=#0;
  if (gdbversion[0] in ['4','5','6','7','8','9']) and (gdbversion[1]='.') then
    begin
      if not only_ver then
        Writeln('GDB version is ',pchar(@version));
      version_number:=ord(gdbversion[0])-ord('0');
      i:=2;
      subver_str:='';
      while gdbversion[i] in ['0'..'9'] do
        begin
          subver_str:=subver_str+gdbversion[i];
          inc(i);
        end;
      val(subver_str,subversion_number,error);
      inc(i);
      subver_str:='';
      while gdbversion[i] in ['0'..'9'] do
        begin
          subver_str:=subver_str+gdbversion[i];
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
  else if (gdbversion[0]='2') and (gdbversion[1]='0') and
          (gdbversion[2] in ['0'..'9']) and (gdbversion[3] in ['0'..'9']) then
    begin
      { CVS version from 2000 to 2099,
        assume current_cvs_version  PM }
      version_number:=Current_cvs_version;
    end
  else
    begin
      if not only_ver then
        Writeln('Unsupported GDB version');
      version_number:=0;
    end;
  freemem(gdbversion);
  if output_file<>'' then
    begin
      assign(o,output_file);
      rewrite(o);
      writeln(o,'{$define GDB_V',version_number,'}');
      close(o);
    end
  else
    begin
      if only_ver then
        Write(version_number);
      Halt(version_number);
    end;
end.
