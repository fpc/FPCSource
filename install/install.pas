{$A+,B-,D+,E+,F-,G-,I-,L+,N-,O-,P-,Q+,R+,S+,T-,V-,X+,Y+}
{$M 16384,0,16384}
program install;

  uses
     app,dialogs,views,objects,menus,drivers,strings,msgbox,dos;

  var
     binpath,startpath : string;
     successfull : boolean;

  procedure uppervar(var s : string);

    var
       i : integer;

    begin
       for i:=1 to length(s) do
         s[i]:=upcase(s[i]);
    end;

  function file_exists(const f : string;const path : string) : boolean;

    begin
       file_exists:=fsearch(f,path)<>'';
    end;

  procedure do_install(const s : string);

    begin
       if not(file_exists(s+'.ZIP',startpath)) then
         begin
            messagebox('File: '+s+' missed for the selected installation. '+
                       'Installation doesn''t becomes complete',nil,mferror+mfokbutton);
            halt(1);
         end;
       swapvectors;
       exec(startpath+'\UNZIP.EXE','-qq -o '+startpath+'\'+s);
       swapvectors;
       if doserror<>0 then
         begin
            messagebox('Error when extracting. Disk full?',nil,mferror+mfokbutton);
            halt(1);
         end;
    end;

  function createdir(const s : string) : boolean;

    var
       result : longint;

    begin
       chdir(s);
       if ioresult=0 then
         begin
{$ifdef german}
            result:=messagebox('Das Installationsverzeichnis existiert schon. '+
              'Soll ein neues Installationsverzeichnis angegeben werden?',nil,
              mferror+mfyesbutton+mfnobutton);
{$else}
            result:=messagebox('The installation directory exists already. '+
              'Do want to enter a new installation directory ?',nil,
              mferror+mfyesbutton+mfnobutton);
{$endif}
            createdir:=result=cmyes;
            exit;
         end;
       mkdir(s);
       if ioresult<>0 then
         begin
{$ifdef german}
            messagebox('Das Installationsverzeichnis konnte nicht angelegt werden',
              @s,mferror+mfokbutton);
{$else}
            messagebox('The installation directory couldn''t be created',
              @s,mferror+mfokbutton);
{$endif}
            createdir:=true;
            exit;
         end;
       createdir:=false;
    end;

  procedure changedir(const s : string);

    begin
       chdir(s);
       if ioresult<>0 then
         begin
{$ifdef german}
            messagebox('Fehler beim Wechseln in das Installationsverzeichnis. '+
              'Installationsprogramm wird beendet',@s,mferror+mfokbutton);
{$else}
            messagebox('Error when changing directory ',@s,mferror+mfokbutton);
{$endif}
            halt(1);
         end;
    end;

  const
     cmstart = 1000;

  type
     pinstalldialog = ^tinstalldialog;

     tinstalldialog = object(tdialog)
        constructor init;
     end;

     tapp = object(tapplication)
         procedure initmenubar;virtual;
         procedure handleevent(var event : tevent);virtual;
     end;

  function diskspace(const zipfile : string) : string;

    var
       clustersize : longint;
       f : file;

    begin
       diskspace:='';
    end;

  var
     mask_components : longint;

  constructor tinstalldialog.init;

    var
       r : trect;
       line : integer;
       p,f : pview;
       s : string;

    const breite = 76;
          hoehe = 20;
          x1 = (80-breite) div 2;
          y1 = (23-hoehe) div 2;
          x2 = x1+breite;
          y2 = y1+hoehe;

    begin
       r.assign(x1,y1,x2,y2);
{$ifdef german}
       inherited init(r,'Installieren');
{$else}
       inherited init(r,'Install');
{$endif}
       line:=2;
       r.assign(3,line+1,28,line+2);
       p:=new(pinputline,init(r,79));
       f:=p;
       s:='C:\PP';
       p^.setdata(s);
       insert(p);
       r.assign(3,line,8,line+1);
       insert(new(plabel,init(r,'~P~ath',p)));
       insert(p);
       inc(line,3);
       r.assign(3,line+1,breite-3,line+11);
       p:=new(pcheckboxes,init(r,
         newsitem('~B~asic system (required)'+diskspace('BASEDOS.ZIP'),
         newsitem('GNU ~L~inker and GNU Assembler (required)'+diskspace('GNUASLD.ZIP'),
         newsitem('D~e~mos'+diskspace('DEMO.ZIP'),
         newsitem('GNU ~D~ebugger'+diskspace('GDB.ZIP'),
         newsitem('GNU ~U~tilities (required to recompile run time library)'+diskspace('GNUUTILS.ZIP'),
         newsitem('Documentation (~H~TML)'+diskspace('DOCS.ZIP'),
         newsitem('Documentation (~P~ostscript)'+diskspace('DOC100PS.ZIP'),
         newsitem('~R~un time library sources'+diskspace('RL09900S.ZIP'),
         newsitem('~C~ompiler sources'+diskspace('PP09900S.ZIP'),
         newsitem('Documentation sources (La~T~eX)'+diskspace('DOC100S.ZIP'),
         nil
       ))))))))))));
       pcluster(p)^.enablemask:=mask_components;
       insert(p);

       r.assign(3,line,14,line+1);
       insert(new(plabel,init(r,'~C~omponents',p)));

       inc(line,12);
       { Free Vision
       r.assign(3,line+1,breite-3,line+3);
       p:=new(pcheckboxes,init(r,
         newsitem('~B~asic system',
         newsitem('~D~ocumentation',
         newsitem('S~a~mples',
         newsitem('~S~ources',
         nil
       ))))));
       pcluster(p)^.enablemask:=mask_freevision;
       insert(p);
       r.assign(3,line,15,line+1);
       insert(new(plabel,init(r,'~F~ree Vision',p)));
       inc(line,4);
       }
       r.assign((breite div 2)-14,line,(breite div 2)-4,line+2);
       insert(new(pbutton,init(r,'~O~k',cmok,bfdefault)));
       r.assign((breite div 2)+4,line,(breite div 2)+14,line+2);
       insert(new(pbutton,init(r,'~C~ancel',cmcancel,bfnormal)));
       f^.select;
    end;

  procedure tapp.handleevent(var event : tevent);

    var
       p : pinstalldialog;
       p2 : pdialog;
       p3 : pstatictext;
       r : trect;
       c : word;
       t : text;
       installdata : record
                       path : string[79];
                       components : word;
                     end;
       f : file;

    label
       insertdisk1,insertdisk2,newpath;

    begin
       inherited handleevent(event);
       if event.what=evcommand then
         if event.command=cmstart then
           begin
              clearevent(event);
              installdata.path:='C:\PP';
              installdata.components:=0;

              mask_components:=$0;

              { searching files }
              if file_exists('BASEDOS.ZIP',startpath) then
                inc(mask_components,1);

              if file_exists('GNUASLD.ZIP',startpath) then
                inc(mask_components,2);

              if file_exists('DEMO.ZIP',startpath) then
                inc(mask_components,4);

              if file_exists('GDB.ZIP',startpath) then
                inc(mask_components,8);

              if file_exists('GNUUTILS.ZIP',startpath) then
                inc(mask_components,16);

              if file_exists('DOCS.ZIP',startpath) then
                inc(mask_components,32);

              if file_exists('DOC100PS.ZIP',startpath) then
                inc(mask_components,64);

              if file_exists('RL09900S.ZIP',startpath) then
                inc(mask_components,128);

              if file_exists('PP09900S.ZIP',startpath) then
                inc(mask_components,256);

              if file_exists('DOC100S.ZIP',startpath) then
                inc(mask_components,512);

              while true do
                begin
              newpath:
                   p:=new(pinstalldialog,init);
                   { default settings }
                   c:=executedialog(p,@installdata);
                   if c=cmok then
                     begin
                        if installdata.path[length(installdata.path)]='\' then
                          dec(byte(installdata.path[0]));
                        uppervar(installdata.path);
                        binpath:=installdata.path+'\BIN';
                        if createdir(installdata.path) then
                          goto newpath;
                        changedir(installdata.path);
{$ifdef unused_code}
                        assign(t,'SET_PP.BAT');
                        rewrite(t);
                        if ioresult<>0 then
{$ifdef german}
                          messagebox('Datei SET_PP.BAT konnte nicht erstellt werden',nil,mfokbutton+mferror)
{$else}
                          messagebox('File SET_PP.BAT can''t be created',nil,mfokbutton+mferror)
{$endif}
                        else
                          begin
                             { never used:
                             writeln(t,'SET LINUXUNITS='+installdata.path+'\LINUXUNITS');
                             writeln(t,'SET PPBIN='+installdata.path+'\BIN');
                             writeln(t,'SET PASLIB='+installdata.path+'\LIB');
                             writeln(t,'SET OS2UNITS='+installdata.path+'\OS2UNITS');
                             writeln(t,'SET DOSUNITS='+installdata.path+'\DOSUNITS;'+installdata.path+'\BIN');
                             }
                             writeln('REM This file may contain nothing else');
                             write(t,'SET GO32=');
{$ifdef german}
                             if messagebox('Wollen Sie den Coprozessoremulator verwenden?',
                               nil,mfyesbutton+mfnobutton)=cmyes then
                               write(t,'emu '+installdata.path+'\DRIVERS\EMU387');
{$else}
                             if messagebox('Install math coprocessor emulation?',
                               nil,mfyesbutton+mfnobutton)=cmyes then
                               write(t,'emu '+installdata.path+'\DRIVERS\EMU387');
{$endif}
                             writeln(t);
                             close(t);
                          end;
{$endif unused_code}
                        if getenv('UNZIP')<>'' then
                          begin
{$ifdef german}
                             messagebox('Die Umgebungsvariable UNZIP sollte'#13+
                                         'nicht gesetzt sein',nil,mfokbutton+mfinformation)
{$else}
                             messagebox('The enviroment variable UNZIP shouldn''t be set',nil,
                               mfokbutton+mfinformation)
{$endif}
                          end;
                        r.assign(20,7,60,16);
                        p2:=new(pdialog,init(r,'Information'));
                        r.assign(6,4,38,5);
{$ifdef german}
                        p3:=new(pstatictext,init(r,'Dateien werden entpackt ...'));
{$else}
                        p3:=new(pstatictext,init(r,'Extracting files ...'));
{$endif}
                        p2^.insert(p3);
                        desktop^.insert(p2);

                        if (installdata.components and 1)<>0 then
                           do_install('BASEDOS');

                        if (installdata.components and 2)<>0 then
                           do_install('GNUASLD');

                        if (installdata.components and 4)<>0 then
                           do_install('DEMO');

                        if (installdata.components and 8)<>0 then
                           do_install('GDB');

                        if (installdata.components and 16)<>0 then
                           do_install('GNUUTILS');

                        if (installdata.components and 32)<>0 then
                           do_install('DOCS');

                        if (installdata.components and 64)<>0 then
                           do_install('DOCS101PS');

                        if (installdata.components and 128)<>0 then
                           do_install('RL09905S');

                        if (installdata.components and 256)<>0 then
                           do_install('PP09905S');

                        if (installdata.components and 512)<>0 then
                           do_install('DOC101S');

                        assign(t,'BIN\PPC386.CFG');
                        rewrite(t);
                        writeln(t,'-l');
                        writeln(t,'#section GO32V1');
                        writeln(t,'-Up',installdata.path+'\RTL\DOS\GO32V1');
                        writeln(t,'#section GO32V2');
                        writeln(t,'-Up',installdata.path+'\RTL\DOS\GO32V2');
                        close(t);

                        desktop^.delete(p2);
                        dispose(p2,done);
{$ifdef german}
                        messagebox('Installation erfolgreich abgeschlossen',nil,mfinformation+mfokbutton);
{$else}
                        messagebox('Installation successfull',nil,mfinformation+mfokbutton);
{$endif}
                        event.what:=evcommand;
                        event.command:=cmquit;
                        successfull:=true;
                        handleevent(event);
                     end;
                   break;
                end;
           end;
    end;

  procedure tapp.initmenubar;

    var
       r : trect;

    begin
       getextent(r);
       r.b.y:=r.a.y+1;
{$ifdef german}
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu('~I~nstallation',hcnocontext,newmenu(
            newitem('~S~tart','',kbnokey,cmstart,hcnocontext,
            newline(
            newitem('~B~eenden','Alt+X',kbaltx,cmquit,hcnocontext,
            nil)))
          ),
       nil))));
{$else}
       menubar:=new(pmenubar,init(r,newmenu(
          newsubmenu('~I~nstallation',hcnocontext,newmenu(
            newitem('~S~tart','',kbnokey,cmstart,hcnocontext,
            newline(
            newitem('~E~xit','Alt+X',kbaltx,cmquit,hcnocontext,
            nil)))
          ),
       nil))));
{$endif}
    end;

  var
     installapp : tapp;
     oldexitproc : pointer;

  procedure myexitproc;far;

    begin
       exitproc:=oldexitproc;
    end;

  var
     b : byte;

begin
   getdir(0,startpath);
   {
   startpath:=paramstr(0);
   for b:=length(startpath) downto 1 do
     if startpath[b]='\' then
       begin
          startpath[0]:=chr(b-1);
          break;
       end;
   }
   oldexitproc:=exitproc;
   exitproc:=@myexitproc;
   successfull:=false;
   installapp.init;
   installapp.run;
   installapp.done;
   if successfull then
     begin
        writeln('Extend your PATH variable with');
        writeln(binpath);
        writeln;
        writeln('To compile files enter PPC386 [file]');
     end;
end.
