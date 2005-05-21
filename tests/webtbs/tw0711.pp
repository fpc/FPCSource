{ %GRAPH }
{ %TARGET=go32v2,win32,linux }

program TestGetPutim; {Compiled with the 0.99.13 version under GO32V2!}

uses
 graph;

var graphdriver,graphmode :integer;

    imsize:longint;

    im:pointer;
begin
{$ifdef win32}
 graphdriver:=VGA;
 graphmode:=detect;
{$else not  win32}
 graphdriver:=VESA;
 graphmode:=$103;
{$endif}
 Initgraph(graphdriver,graphmode,'');

 {************}
(*
 setcolor(6);

 moveto(0,0);   {Some drawing}

 lineto(500,500);

 circle(95,95,80);

{************}



{!!!!!!!!!!!!}

 imsize:= imagesize(0,0,300,300); {This is the part we have problem with.}

 getmem(im,imsize);         {The result we get after PutImage is}

 getimage(0,0,300,300,im^);       {chaotic independently from the graphmode!}

  putimage(50,50,im^,0);           {We tested this on a S3Trio 3D videcard,}

                                  {which is VESA compatible.}

{!!!!!!!!!!!!}



readln;
 {repeat until keypressed;}
   *)
 closegraph;

end.
{
                                                        I. Groma
                                                        groma@metal.elte.hu
                                                        Budapest 11/24/1999
}
