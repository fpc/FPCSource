{ %norun }

program tw23819;

  type
    fixstring = string [ 255 ] ;
    t9496 = ( t94, t96 ) ;
    tSD = ( sdSingle94, sdSingle96, sdDOuble94, sdDouble96 ) ;
    tg = ( G0, G1, G2, G3 ) ;
    tG13 = G1..G3 ;
    tl = #$40..#$7f ;
    ESCstring = string [ 7 ] ;
    tgl9496 = {packed} object
                sd : tSD ;
                g : tg ;
                l : tl ;
                n : t9496 ;
                procedure Put ( const pESCseq : ESCstring ) ;
               end ;

  procedure tgl9496.Put ( const pESCseq : ESCstring ) ;

    var
      yp : tgl9496 ;
      locals : record
                 Lst : FixString ;
                 gc : Char ;
                 gp,
                 letp : LongInt ;
                 xp : tgl9496 ;
                end ;

    begin
    end ;


begin
end.
