uses
   sysutils;

procedure deltree(const dirname : string);

  var
     rec : tsearchrec;

  begin
     writeln('Deleting ',dirname);
     if findfirst(dirname+'/*.*',faanyfile,rec)=0 then
       begin
          repeat
            if (rec.attr and fadirectory)<>0 then
              begin
                 if (rec.name<>'.') and (rec.name<>'..') then
                   deltree(dirname+'/'+rec.name)
              end
            else
              deletefile(dirname+'/'+rec.name);
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
     if findfirst(dirname+'/*.*',faanyfile,rec)=0 then
       begin
          repeat
            if (rec.attr and fadirectory)<>0 then
              begin
                 if rec.name='CVS' then
                   deltree(dirname+'/CVS')
                 else
                   if (rec.name<>'.') and (rec.name<>'..') then
                     searchcvsdir(dirname+'/'+rec.name)
              end;
          until findnext(rec)<>0;
          findclose(rec);
       end;
  end;

begin
   searchcvsdir('.');
end.
