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
