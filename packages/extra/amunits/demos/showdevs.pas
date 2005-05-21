program ShowDevs;

{

  Programm       : Devices  - listet angemeldet Devices auf
  Sprache        : PCQ-Pascal 1.2b nach einem kleinen Hack von
                   mir in MCC-Pascal V2.04
  Autor          : Andreas Neumann für Purity
  Datum          : 01.03.1992

}

{

  Translated to fpc pascal
  24 Mar 2001.

  nils.sjoholm@mailbox.swipnet.se

}


uses exec,amigados;

CONST   Device_Types : Array [0..2] OF pchar = (('DEVICE     '),
                                                 ('DIRECTORY  '),
                                                 ('VOLUME     '));

VAR
    mydosbase    : pDOSLibrary;
    myrootptr    : pRootNode;
    myinfoptr    : pDosInfo;
    mydeviceptr  : pDeviceNode;
    mystr        : pchar;
    eingabe      : CHAR;
    mystartup    : pFileSysStartupMsg;
    myenvec      : pDOSEnvec;
    i            : longint;

BEGIN
 WRITELN;
 WRITELN ('Device-Lister PD © 1992 by Andreas Neumann (NEUDELSoft) für Purity');

 mydosbase:= pDOSLibrary(_DosBase);

 { Man braucht ja die Adresse der DOSLibrary                      }

 myrootptr:=mydosbase^.dl_Root;
 myinfoptr:=BADDR(myrootptr^.rn_Info);
 mydeviceptr:=BADDR(myinfoptr^.di_DevInfo);

 { Man hangelt sich von Struktur zu Struktur                      }

 WHILE mydeviceptr<>NIL DO
 BEGIN
  WITH mydeviceptr^ DO
  BEGIN
   WRITELN;

   {mystr:=Address(Integer(BPTRtoAPTR(dn_Name))+1);}
   mystr:=pointer(longint(BADDR(dn_Name))+1);

   { Trick : dn_Name ist ein BSTR. Dies ist ein BPTR auf ein Feld, das }
   {         mit der Anzahl der Stringzeichen beginnt (daher +1) und   }
   {         dann die Zeichen enthält.                                 }

   WRITELN ('Name        : ',mystr,':');
   WRITELN ('Type        : ',Device_Types[dn_Type]);
   IF NOT (dn_Lock=0) THEN
    WRITELN ('there is a lock on this Device')
   ELSE
    WRITELN;
   WRITELN;

   mystartup:=BADDR(dn_Startup);
   myenvec:=BADDR(mystartup^.fssm_Environ);

   IF (NOT(dn_Startup=0)) AND (dn_Type=DLT_DEVICE) AND (myenvec^.de_SizeBlock>0) THEN
   BEGIN

    {          es ist ein dateiorientiertes Device !!!             }
    {  im Gegensatz hierzu : ein logisches Device wie L: oder S:   }

    WRITELN ('More information regarding the Organisation of Devices: ');
    WITH myenvec^ DO
    BEGIN
     WRITELN;
     WRITELN ('Size of the sectors        : ',de_SizeBlock*4,' Bytes');
     WRITELN ('Number of sectors per Block: ',de_SectorPerBlock);
     WRITELN ('Blocks per Track           : ',de_BlocksPerTrack);
     WRITELN ('Startcylinder              : ',de_LowCyl);
     WRITELN ('Endcylinder                : ',de_HighCyl);
     WRITELN ('Surfaces                   : ',de_Surfaces);

     i:=(de_HighCyl+1-de_LowCyl)*(de_Surfaces)*
         (de_BlocksPerTrack)*(de_SectorPerBlock)*(de_SizeBlock*4);

     { Anzahl der Zylinder * Anzahl der Oberflächen * Anzahl der Blöcke
        pro Spur * Anzahl der Sektoren pro Block * Größe eines
        Blockes * 4                                                     }

     WRITELN ('Storage capacity  : ',i,' Bytes    = ',i DIV 1024,' KBytes');
    END;
    WRITELN;
    writeln('The exec unit number is ',mystartup^.fssm_Unit);
   END;
  END;

  WRITELN ('(M)ore oder (S)top ?');
  READLN (eingabe);

  mydeviceptr:=BADDR(mydeviceptr^.dn_Next);
  IF (UpCase(eingabe)='S') THEN mydeviceptr:=NIL;
 END;

 WRITELN ('Good Bye. NEUDELSoft wünscht noch viel Spaß mit Amiga und Pascal.');

END.
