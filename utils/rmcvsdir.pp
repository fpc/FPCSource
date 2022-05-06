{
    Copyright (c) 1999-2012 by Marco van de Voort and Free Pascal Core 

    Recursively deletes .cvs and .svn directories essentially "exporting" a
    repository in place.  Typically used when cleaning out old checkouts, to
    prepare for a GNU diff session with another checkout.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

program rmcvsdir;

uses
   sysutils;

procedure deltree(const dirname : string);

  var
     rec : tsearchrec;

  begin
     writeln('Deleting ',dirname);
     if findfirst(dirname+'/*.*',faanyfile or fadirectory,rec)=0 then
       begin
          repeat
            if (rec.attr and fadirectory)<>0 then
              begin
                 if (rec.name<>'.') and (rec.name<>'..') then
                   deltree(dirname+'/'+rec.name)
              end
            else
              begin
                FileSetAttr(dirname+'/'+rec.name,faArchive);
                deletefile(dirname+'/'+rec.name);
              end;
          until findnext(rec)<>0;
          findclose(rec);
       end;
     rmdir(dirname);
  end;

procedure searchcvsdir(const dirname : string);

  var
     rec : tsearchrec;

  begin
     writeln('Searching ',dirname);
     if findfirst(dirname+'/*.*',faanyfile or fadirectory,rec)=0 then
       begin
          repeat
            if (rec.attr and fadirectory)<>0 then
              begin
                 if rec.name='CVS' then
                   deltree(dirname+'/CVS')
                 else if rec.name='.svn' then
                   deltree(dirname+'/.svn')
                 else if rec.name='.git' then
                   deltree(dirname+'/.git')
                 else
                   if (rec.name<>'.') and (rec.name<>'..') then
                     searchcvsdir(dirname+'/'+rec.name)
              end;
          until findnext(rec)<>0;
          findclose(rec);
       end;
  end;

var
  para : string;
begin
  if paramcount=0 then
    para:='.'
  else
    para:=paramstr(1);
  searchcvsdir(para);
end.
