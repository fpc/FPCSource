{$mode objfpc}
{$h+}
uses
  sysutils,classes,dom,xmlread;

procedure error(const s : string);
  begin
    writeln('Error: ',s);
    halt(1);
  end;

var
  doc : txmldocument;
  root : TDomNode;
  entry,
  currentpath,
  currentinfo : TDomNode;
  hs,
  dirprefix,
  newlineprefix,
  currentmsg,
  currentauthor,
  currentdate,
  pathtemp,
  currentrevision : string;
  paths : tstringlist;
  pathprefixes : tstringlist;
  i,j,maxequal : integer;
  firstpath : boolean;

begin
  paths:=tstringlist.create;
  paths.sorted:=true;
  pathprefixes:=tstringlist.create;
  pathprefixes.sorted:=true;
  ReadXMLFile(doc,paramstr(1));
  root:=doc.DocumentElement;
  if root.haschildnodes and
    (root.NodeName='log') then
    begin
      entry:=root.firstchild;
      while assigned(entry) do
        begin
          if entry.NodeName<>'logentry' then
            error('Only log entry entries supported, but '+entry.NodeName+' found.');
          currentmsg:='';
          currentauthor:='';
          currentdate:='';
          { get revision }
          with entry as tdomelement do
            currentrevision:=AttribStrings['revision'];
          if entry.haschildnodes then
            begin
              currentinfo:=entry.firstchild;
              while assigned(currentinfo) do
                begin
                  if currentinfo.NodeName='author' then
                    begin
                      if currentinfo.haschildnodes and
                        (currentinfo.firstchild is TDOMText) then
                        currentauthor:=(currentinfo.firstchild as TDOMText).Data
                      else
                        error('Malformed author node');
                    end
                  else if currentinfo.NodeName='msg' then
                    begin
                      if currentinfo.haschildnodes then
                        begin
                          if (currentinfo.firstchild is TDOMText) then
                            currentmsg:=(currentinfo.firstchild as TDOMText).Data
                          else
                            error('Malformed msg node');
                        end
                      else
                        currentmsg:='<empty log message>';
                    end
                  else if currentinfo.NodeName='date' then
                    begin
                      if currentinfo.haschildnodes and
                        (currentinfo.firstchild is TDOMText) then
                        currentdate:=(currentinfo.firstchild as TDOMText).Data
                      else
                        error('Malformed date node');
                    end
                  else if currentinfo.NodeName='paths' then
                    begin
                      currentpath:=currentinfo.firstchild;
                      paths.clear;
                      pathprefixes.clear;
                      while assigned(currentpath) do
                        begin
                          if currentpath.NodeName<>'path' then
                            error('Path node expected');
                          if currentpath.haschildnodes and
                            (currentpath.firstchild is TDOMText) then
                            begin
                              paths.add((currentpath.firstchild as TDOMText).Data);
                              hs:=ExtractFilePath((currentpath.firstchild as TDOMText).Data);
                              if not pathprefixes.Find(hs,i) then
                                pathprefixes.add(hs);
                            end
                          else
                            error('Malformed date node');

                          currentpath:=currentpath.NextSibling;
                        end;
                    end
                  else
                    error('Unknown logentry child '+currentinfo.NodeName+' found');
                  currentinfo:=currentinfo.nextsibling;
                end;
              currentdate:=copy(currentdate,1,16);
              { replaced T }
              currentdate[11]:=' ';
              write(currentdate,' ',currentauthor);
              if currentrevision<>'' then
                writeln(' r',currentrevision)
              else
                writeln;
              writeln;

              { search for common pathprefix }
              if pathprefixes.Count>1 then
                begin
                  maxequal:=65535;
                  for i:=1 to pathprefixes.Count-1 do
                    begin
                      j:=1;
                      while (pathprefixes[0][j]=pathprefixes[i][j]) and (j<=maxequal) do
                        inc(j);
                      dec(j);
                      if j<maxequal then
                        maxequal:=j;
                    end;

                  { test/p1.pas test/p2.pas should use the prefix test/ instead of test/p }
                  if maxequal<65535 then
                    while (maxequal>0) and (pathprefixes[0][maxequal]<>'/') do
                      dec(maxequal);
                  Writeln('  '+Copy(pathprefixes[0],1,maxequal)+': ');
                  dirprefix:='    ';
                  newlineprefix:='      ';
                end
              else
                begin
                  maxequal:=0;
                  dirprefix:='  ';
                  newlineprefix:='    ';
                end;

              for i:=0 to pathprefixes.Count-1 do
                begin
                  pathtemp:=dirprefix;
                  if maxequal+1<length(pathprefixes[i]) then
                    pathtemp:=pathtemp+Copy(pathprefixes[i],maxequal+1,65535)+': ';
                  firstpath:=true;
                  j:=0;
                  while (j<paths.Count) do
                    begin
                      if ExtractFilePath(paths[j])=pathprefixes[i] then
                        begin
                          hs:=copy(paths[j],length(pathprefixes[i])+1,65535);
                          if (length(pathtemp)+length(hs)>=78) and
                             (pathtemp<>newlineprefix) then
                            begin
                              writeln(pathtemp+',');
                              pathtemp:=newlineprefix;
                              firstpath:=true;
                            end;
                          { non empty path but not first? }
                          if firstpath then
                            firstpath:=false
                          else
                            pathtemp:=pathtemp+', ';
                          pathtemp:=pathtemp+hs;
                          { delete already processed paths for performance }
                          paths.delete(j);
                        end
                      else
                        inc(j);
                    end;
                  if pathtemp<>newlineprefix then
                    writeln(pathtemp);
                end;

              { truncate trailing spaces and new lines from log message }
              i:=length(currentmsg);
              while (i>0) and (currentmsg[i] in [#13,#10,#9,' ']) do
                dec(i);
              delete(currentmsg,i+1,length(currentmsg)-i);

              { Pretty print message starting with at least 2 spaces each line }
              writeln;
              i:=0;
              hs:=currentmsg;
              while (hs<>'') do
                begin
                  newlineprefix:='  ';
                  i:=0;
                  while (i<length(hs)) and not(hs[i+1] in [#13,#10]) do
                    inc(i);
                  j:=1;
                  while (j<length(hs)) and
                        (j<length(newlineprefix)) and
                        (hs[j] in [' ']) do
                    inc(j);
                  writeln(newlineprefix,copy(hs,j,i-j+1));
                  { remove eol and add additional empty lines }
                  j:=0;
                  while (i<length(hs)) and (hs[i+1] in [#13,#10]) do
                    begin
                      if hs[i+1]=#10 then
                        begin
                          inc(j);
                          if j>2 then
                            writeln;
                        end;
                      inc(i);
                    end;
                  delete(hs,1,i);
                end;
              writeln;
            end
          else
            error('Empty log entry found');
          entry:=entry.nextsibling;
        end;
    end
  else
    error('log element not found/wrong xml format');
end.
