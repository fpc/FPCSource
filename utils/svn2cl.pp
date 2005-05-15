{$mode objfpc}
{$h+}
uses
  classes,dom,xmlread;

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
  currentmsg,
  currentauthor,
  currentdate,
  pathtemp,
  currentrevision : string;
  paths : tstringlist;
  i,j,maxequal : longint;

begin
  paths:=tstringlist.create;
  paths.sorted:=true;
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
          currentrevision:='';
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
                      if currentinfo.haschildnodes and
                        (currentinfo.firstchild is TDOMText) then
                        currentmsg:=(currentinfo.firstchild as TDOMText).Data
                      else
                        error('Malformed msg node');
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
                      while assigned(currentpath) do
                        begin
                          if currentpath.NodeName<>'path' then
                            error('Path node expected');
                          if currentpath.haschildnodes and
                            (currentpath.firstchild is TDOMText) then
                            paths.add((currentpath.firstchild as TDOMText).Data)
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
              writeln(currentdate,' ',currentauthor);
              writeln;
              { search for common prefix }
              maxequal:=65535;
              for i:=1 to paths.Count-1 do
                begin
                  j:=1;
                  while (paths[0][j]=paths[i][j]) and (j<=maxequal) do
                    inc(j);
                  dec(j);
                  if j<maxequal then
                    maxequal:=j;
                end;

              { generate prefix }
              pathtemp:='  * '+copy(paths[0],1,maxequal)+': ';
              for i:=0 to paths.Count-1 do
                begin
                  hs:=copy(paths[i],maxequal+1,65535);
                  if (length(pathtemp)+length(', '+hs)>80) and
                    (pathtemp<>'  ') then
                    begin
                      writeln(pathtemp+',');
                      pathtemp:='  ';
                    end;
                  { non empty path but not first? }
                  if (pathtemp<>'  ') and (pathtemp[length(pathtemp)-1]<>':') then
                    pathtemp:=pathtemp+', ';
                  pathtemp:=pathtemp+hs;
                end;
              if pathtemp<>'  ' then
                writeln(pathtemp);
              writeln;
              writeln('  ',currentmsg);
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
