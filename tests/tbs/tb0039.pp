{ Old file: tbs0044.pp }
{  shows $ifdef and comment nesting/directive problem  OK 0.99.1 (PFV) }

 { Problem with nested comments -- as you can probably see }
 { but it does give out kind of a funny error output :)    }


 {$UNDEF VP}
 {$UNDEF WINDOWS}

 {$IFDEF Windows} ssss {$ENDIF}      {No Syntax Error}

 {$IFDEF VP}
      {$D+}{$R+}
 {$ELSE}
   {$IFDEF Windows} ssss {$ENDIF}    {Syntax Error at: Col 25 }
 {$ENDIF}

 BEGIN
 END.
