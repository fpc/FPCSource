{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    A generic implementation of ttyname functionality.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function TTYName(Handle:cint):string;
{
  Return the name of the current tty described by handle f.
  returns empty string in case of an error.
}
var
  mydev     : dev_t;
  myino     : ino_t;
  st        : stat;

  function mysearch(n:string): boolean;
  {searches recursively for the device in the directory given by n,
    returns true if found and sets the name of the device in ttyname}
  var dirstream : pdir;
      d         : pdirent;
      name      : string;
      st        : stat;
  begin
    dirstream:=fpopendir(n);
    if (dirstream=nil) then
     exit(false);
    d:=fpReaddir(dirstream^);
    while (d<>nil) do
     begin
       name:=n+'/'+strpas(@(d^.d_name));
     //  fpstat(name,st);
       if fpstat(name,st)=0 then
        begin
          if (fpS_ISDIR(st.st_mode)) and  { if it is a directory }
             (strpas(@(d^.d_name))<>'.') and    { but not ., .. and fd subdirs }
             (strpas(@(d^.d_name))<>'..') and
             (strpas(@(d^.d_name))<>'') and
             (strpas(@(d^.d_name))<>'fd') then
           begin                      {we found a directory, search inside it}
             if mysearch(name) then
              begin                 {the device is here}
                fpclosedir(dirstream^);  {then don't continue searching}
                mysearch:=true;
                exit;
              end;
           end
{$ifndef beos}	 
          else if (ino_t(d^.d_fileno)=myino) and (st.st_dev=mydev) then
           begin
             fpclosedir(dirstream^);
             ttyname:=name;
             mysearch:=true;
             exit;
           end;
{$endif}
        end;
       d:=fpReaddir(dirstream^);
     end;
    fpclosedir(dirstream^);
    mysearch:=false;
  end;

begin
  TTYName:='';
  if (fpfstat(handle,st)=-1) or (isatty (handle)<>1) then
   exit;
{$ifndef beos}	 
  mydev:=st.st_dev;
  myino:=st.st_ino;
{$endif}
  mysearch('/dev');
end;


function TTYName(var F:Text):string;
{
  Idem as previous, only now for text variables;
}
begin
  TTYName:=TTYName(textrec(f).handle);
end;

