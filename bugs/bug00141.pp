
    type
       ttreetype = (addn,muln,subn,divn,
                   modn,assignn,loadn,rangen,
                   ltn,lten,gtn,gten,
                   equaln,unequaln,inn,orn,
                   xorn,shrn,shln,slashn,
                   andn,subscriptn,derefn,addrn,
                   ordconstn,typeconvn,calln,callparan,
                   realconstn,fixconstn,umminusn,asmn,vecn,
                   stringconstn,funcretn,selfn,
                   notn,inlinen,niln,errorn,
                   typen,hnewn,hdisposen,newn,
                   simpledisposen,setelen,setconstrn,blockn,
                   anwein,loopn,ifn,breakn,
                   continuen,repeatn,whilen,forn,
                   exitn,withn,casen,labeln,
                   goton,simplenewn,tryexceptn,raisen,
                   switchesn,tryfinallyn,isn,asn);


       { gibt an, welche Nachfolger eines Knotens }
       ptree = ^ttree;

       ttree = record
          left,right : ptree;
          treetype : ttreetype;
       end;
   function equal_trees(t1,t2 : ptree) : boolean;

     begin
             case t1^.treetype of
                   realconstn,fixconstn,umminusn,asmn,vecn,
                   stringconstn,funcretn,selfn,
                equaln,
                unequaln:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left) and
                                    equal_trees(t1^.right,t2^.right)) or
                                   (equal_trees(t1^.right,t2^.left) and
                                    equal_trees(t1^.left,t2^.right));
                   end;
             end;
     end;
