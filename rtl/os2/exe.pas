unit exe;

  interface

    type
       exe = record
          eid : WORD;
          elast : WORD;
          epagsiz : WORD;
          erelcnt : WORD;
          ehdrsiz : WORD;
          eminfre : WORD;
          emaxfre : WORD;
          eiSS : WORD;
          eiSP : WORD;
          enegsum : WORD;
          eiIP : WORD;
          eiCS : WORD;
          ereloff : WORD;
          eovlnum : WORD;
          ever : WORD;
          dumy : WORD;
          ebb : WORD;
          dumy2 : array[0..7-1] of WORD;
       end;

    const
       EXEID = $5a4d;

  implementation

end.
