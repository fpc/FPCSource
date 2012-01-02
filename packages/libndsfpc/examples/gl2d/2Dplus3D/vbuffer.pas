(*
	Space Impakto DS
	relminator
	Http://Rel.Phatcode.Net
	
  TVertexbuffer class
*)

unit VBuffer;

{$mode objfpc}
{$H+}

interface

uses
  ctypes, nds9, math;


const
  PI = 3.141593;
  TWOPI: cfloat = PI * 2;

type
  TVector3f32 = packed record
    x: cint32;
    y: cint32;
    z: cint32;
  end;
  PVector3f32 = ^TVector3f32;
  

  TRGBf32 = packed record
    r: cint32;
    g: cint32;
    b: cint32;
  end;
  PRGBf32 = ^TRGBf32;

  TTexcoordf32 = packed record
    u: cint32;
    v: cint32;
  end;
  PTexcoordf32 =^TTexcoordf32;

  TPolygon = packed record
    v1: cuint32;
    v2: cuint32;
    v3: cuint32;
  end;
  PPolygon = ^TPolygon;

// vertex buffer object. LOL
  TVertexbuffer = class
  public
    i_max_poly: cint;
    i_max_vertex: cint;
    i_primitive_type: cint;
    i_texture_ID: cint;
    ips_vertex: array of TVector3f32;
    ips_texture: array of TTexcoordf32;
    ips_color: array of TRGBf32;
    ps_poly: array of TPolygon;            
    constructor Create;
    destructor Destroy;
    procedure render(text_off_u,  text_off_v: cint32; colorize: boolean);
    //procedure render_lines(r, g, b: cuint8);
    function load_texture(texture_gfx: pcuint8): cint;
  end;
  PVertexBuffer = ^TVertexBuffer;


function init_grid( rings, bands: cint; 
                    width, height: cfloat; 
                    uscale, vscale: cint): PVertexBuffer; 
function init_super_shape(rings, bands: cint; 
                          radius: cfloat; 
                          uscale, vscale: cint;
                          a, b, m, n1, n2, n3: cfloat): PVertexBuffer;
function init_ascaris(rings, bands: cint; 
                      radius, center_offset: cfloat; 
                      uscale, vscale: cint): PVertexBuffer;
                

implementation

constructor TVertexBuffer.Create();
begin
  SetLength(ips_vertex, 0);
  SetLength(ips_texture, 0);
  SetLength(ips_color, 0);
  SetLength(ps_poly, 0);
  
  i_max_poly := 0;
  i_primitive_type := 0;
  i_texture_ID := 0;
end;

destructor TVertexBuffer.Destroy();
begin

	if Length(ips_vertex) > 0 then 
    SetLength(ips_vertex, 0);
    
  if Length(ps_poly) > 0 then
    SetLength(ps_poly, 0);

  i_max_poly := 0;
  i_primitive_type := 0;
  i_texture_ID := 0;
  
	if Length(ips_texture) > 0 then 
    SetLength(ips_texture, 0);
	
	if Length(ips_color) > 0 then 
    SetLength(ips_color, 0);

end;

procedure TVertexbuffer.render(text_off_u, text_off_v: cint32; colorize: boolean);
var
  i: integer;
  i1,i2,i3: integer;
begin
  glEnable(GL_TEXTURE_2D);
  glBindTexture(0, i_texture_ID);
  if (colorize) then
  begin
    glBegin (GL_TRIANGLES);
      //glNormal(NORMAL_PACK(0,inttov10(-1),0));
      for i := 0 to i_max_poly - 1 do
      begin
        i1 := ps_poly[i].v1;
        i2 := ps_poly[i].v2;
        i3 := ps_poly[i].v3;
        
        glColor3b(ips_color[i1].r, ips_color[i1].g, ips_color[i1].b);
        glTexCoord2f32(ips_texture[i1].u + text_off_u, ips_texture[i1].v + text_off_v);
        glVertex3v16(ips_vertex[i1].x, ips_vertex[i1].y, ips_vertex[i1].z);
        
        glColor3b(ips_color[i2].r, ips_color[i2].g, ips_color[i2].b);
        glTexCoord2f32(ips_texture[i2].u + text_off_u, ips_texture[i2].v + text_off_v);
        glVertex3v16(ips_vertex[i2].x, ips_vertex[i2].y, ips_vertex[i2].z);

        glColor3b(ips_color[i3].r, ips_color[i3].g, ips_color[i3].b);
        glTexCoord2f32(ips_texture[i3].u + text_off_u, ips_texture[i3].v + text_off_v);
        glVertex3v16(ips_vertex[i3].x, ips_vertex[i3].y, ips_vertex[i3].z);
        
      end;
    glEnd(); 
  end else
  begin
    glBegin (GL_TRIANGLES);
      //glNormal(NORMAL_PACK(0,inttov10(-1),0));
      for i := 0 to i_max_poly - 1 do
      begin
        i1 := ps_poly[i].v1;
        i2 := ps_poly[i].v2;
        i3 := ps_poly[i].v3;
        
        glTexCoord2f32(ips_texture[i1].u +text_off_u,ips_texture[i1].v +text_off_v);
        glVertex3v16 (ips_vertex[i1].x,ips_vertex[i1].y,ips_vertex[i1].z);
        
        glTexCoord2f32 (ips_texture[i2].u + text_off_u,ips_texture[i2].v +text_off_v);
        glVertex3v16 (ips_vertex[i2].x,ips_vertex[i2].y,ips_vertex[i2].z);

        glTexCoord2f32 (ips_texture[i3].u + text_off_u,ips_texture[i3].v +text_off_v);
        glVertex3v16 (ips_vertex[i3].x,ips_vertex[i3].y,ips_vertex[i3].z);
        
      end;
    glEnd(); 
  end;
end;


function Tvertexbuffer.load_texture(texture_gfx: pcuint8): cint;
begin
  if (texture_gfx^) = 0 then 
    result := 1
  else
  begin
    glGenTextures(1, @i_texture_ID);
    glBindTexture(0, i_texture_ID);
    glTexImage2D( 0, 0, GL_RGB, TEXTURE_SIZE_128 , TEXTURE_SIZE_128, 0, GL_TEXTURE_WRAP_S or GL_TEXTURE_WRAP_T or TEXGEN_TEXCOORD, texture_gfx);
    result := 0;
  end;
end;

// special functions
function init_grid(rings, bands: cint; width, height: cfloat; uscale, vscale: cint): PVertexBuffer; 
var
  vb: PVertexBuffer;
  max_point: cint;
  ivertex: array of TVector3f32;
  itexture: array of TTexcoordf32;
  icolor: array of TRGBf32;
  poly: array of TPolygon;
  s, u, slice, maxvert: cint;
  i: cint;
  ii, jj: integer;
  half_width, half_height, a1, a2: cfloat;
  k: cint;
  x, y, z: cfloat;
  _u, _v: cfloat;
begin
  new(vb);
  vb^ := TVertexBuffer.Create();
  
  max_point := (rings) * (bands);      //+1 for last band duplicate

  SetLength(ivertex, max_point); 
  SetLength(itexture, max_point);  //
  SetLength(icolor, max_point); 
  SetLength(poly, max_point * 2);  //
    
  vb^.ips_vertex := ivertex;
  vb^.ips_texture :=  itexture;
  vb^.ips_color := icolor;
  vb^.ps_poly := poly;
    
  vb^.i_max_vertex := max_point;
  vb^.i_max_poly := max_point * 2;
  vb^.i_primitive_type := GL_TRIANGLES;
    
  maxvert := max_point;
  i := 0;
  for s := 0 to rings - 1 do
  begin
    slice := s * bands;
    for u := 0 to bands - 1 do   //duplicate texture ( not bands - 1)
    begin
      poly[i].v1 := (u + bands + 1 + (slice)) mod maxvert;
      poly[i].v2 := (u + bands + (slice)) mod maxvert;
      poly[i].v3 := (u + (slice)) mod maxvert;
      poly[i+1].v1 := (u + (slice)) mod maxvert;
      poly[i+1].v2 := (u + 1 + (slice)) mod maxvert;
      poly[i+1].v3 := (u + bands + 1 + (slice)) mod maxvert;
      inc(i, 2);
    end;
  end;
        

  half_width := width / 2; 
  half_height := height / 2;
  
  
  a1 := 2 * width / rings;
  a2 := 2 * height / bands;
  k := 0;
  
      
  for ii := 0 to rings - 1 do
  begin
    for jj := 0 to bands - 1 do
    begin
      x := -half_width + (ii * a1);
      y := 0;
      z := -half_height + (jj * a2);;
            
      ivertex[k].x := floattov16(x);
      ivertex[k].y := floattov16(y);
      ivertex[k].z := floattov16(z);
      
      icolor[k].r := (1 - (jj * 4 div bands)) * 255;
      icolor[k].g := (1 - (jj * 4 div bands)) * 255;
      icolor[k].b := (1 - (jj * 4 div bands)) * 255;

      _u := (ii / rings ) * uscale;
      _v := (jj / bands ) * vscale;
      
      
      itexture[k].u := floattof32(_u);
      itexture[k].v := floattof32(_v);
      
      inc(k);
      
    end;
  end;
  
  result := vb;

end;

function init_super_shape (rings, bands: cint; radius: cfloat; uscale, vscale: cint;
                  a, b, m, n1, n2, n3: cfloat): PVertexBuffer;
var
  vb: PVertexBuffer;  
  max_point: cint;
  ivertex: array of TVector3f32; 
  itexture: array of TTexcoordf32;
  icolor: array of Trgbf32; 
  poly: array of Tpolygon;
  s, u, slice, maxvert: cint;
  i: cint;  
  
  phi, theta: cfloat;
  r1, r2, a1, a2: cfloat;
  
  Tpi_d: cfloat;
  Ppi_d: cfloat;
  k: cint;
  
  ii, jj: integer;
  x, y, z: cfloat;
  _u, _v: cint;
  
begin
  new(vb);
  vb^ := TVertexBuffer.Create();

  max_point := rings * (bands + 1);      //+1 for last band duplicate
  SetLength(ivertex, max_point); 
  SetLength(itexture, max_point); 
  SetLength(icolor, max_point);  
  SetLength(poly, max_point * 2); 
    
  vb^.ips_vertex := ivertex;
  vb^.ips_texture :=  itexture;
  vb^.ips_color :=  icolor;
  vb^.ps_poly := poly;
    
  vb^.i_max_vertex := max_point;
  vb^.i_max_poly := max_point * 2;
  vb^.i_primitive_type := GL_TRIANGLES;

  //lathing
  maxvert := max_point;
  i := 0;
  for s := 0 to rings - 1 do
  begin
     slice := s * bands;
     for u := 0 to bands - 1 do
     begin
       poly[i].v1:=(u+bands+1+(slice)) mod maxvert;
       poly[i].v2:=(u+bands+(slice)) mod maxvert;
       poly[i].v3:=(u+(slice)) mod maxvert;
       poly[i+1].v1:=(u+(slice)) mod maxvert;
       poly[i+1].v2:=(u+1+(slice)) mod maxvert;
       poly[i+1].v3:=(u+bands+1+(slice)) mod maxvert;
       inc(i, 2);
     end;
  end;
        
  
  
  Tpi_d := TWOPI / bands;
  Ppi_d := PI / rings;
  
  phi := -PI / 2;
  
  k := 0;
  
  for ii := 0 to rings - 1 do
  begin
    
    a1 := power(abs(cos(m * phi / 4) / a), n2);
    a2 := power(abs(sin(m * phi / 4) / b), n3);
    r2 := power(a1 + a2, -1 / n1) ;
    r2 := r2 * radius;
        
    phi := phi + Ppi_d;
        
    theta := -PI;
    
    for jj := 0 to bands do
    begin

      a1 := power(abs(cos(m * theta / 4) / a), n2);
      a2 := power(abs(sin(m * theta / 4) / b), n3);
      r1 := power(a1 + a2, -1 / n1) ;
      r1 := r1 * radius;
      
      
      x := r1 * cos(theta) * r2 * cos(phi);
      y := r1 * sin(theta) * r2 * cos(phi);
      z := r2 * sin(phi);
      
      theta := theta + Tpi_d; 

      ivertex[k].x := floattov16 (x);
      ivertex[k].y := floattov16 (y);
      ivertex[k].z := floattov16 (z);

      icolor[k].r := (1 - (ii * 4 div rings)) * 255;
      icolor[k].g := (1 - (ii * 4 div rings)) * 255;
      icolor[k].b := (1 - (ii * 4 div rings)) * 255;

      _u := (ii div rings ) * uscale;
      _v := (jj div bands ) * vscale;
      
      
      itexture[k].u := floattof32(_u);
      itexture[k].v := floattof32(_v);
      
      inc(k);
      
    end;
  end;
  
  result := vb;
end;

// initialize out tunnel and
// store float values to f32 classes
function init_ascaris(rings, bands: cint; radius, center_offset: cfloat; uscale, vscale: cint): PVertexBuffer;
var
  vb: PVertexBuffer;
  max_point: cint;
  ivertex: array of TVector3f32;
  itexture: array of Ttexcoordf32;
  icolor: array of Trgbf32;
  poly: array of TPolygon;
  s, u, slice, maxvert: cint;
  i: cint;
  ii, jj: integer;
  half_width, half_height, a1, a2: cfloat;
  k: cint;
  x, y, z: cfloat;
  _u, _v: cfloat;  
  xc, yc, zc: cfloat;
  
begin
  new(vb);
  vb^ := TVertexBuffer.Create();

  max_point := (rings) * (bands + 1);      //+1 for last band duplicate
  setlength(ivertex, max_point); 
  setlength(itexture, max_point);
  setlength(icolor, max_point); 
  setlength(poly, max_point * 2);
  
  vb^.ips_vertex := ivertex;
  vb^.ips_texture :=  itexture;
  vb^.ips_color :=  icolor;
  vb^.ps_poly := poly;
  
  vb^.i_max_vertex := max_point;
  vb^.i_max_poly := max_point * 2;
  vb^.i_primitive_type := GL_TRIANGLES;
    
  
  //lathing
  maxvert := max_point;
  i := 0;
  for s := 0 to (rings - 1) do
  begin
    slice := s * bands;
    for u := 0 to bands do   //duplicate texture ( not bands - 1)
    begin
      poly[i].v1:=(u+bands+1+(slice)) mod maxvert;
      poly[i].v2:=(u+bands+(slice)) mod maxvert;
      poly[i].v3:=(u+(slice)) mod maxvert;
      poly[i+1].v1:=(u+(slice)) mod maxvert;
      poly[i+1].v2:=(u+1+(slice)) mod maxvert;
      poly[i+1].v3:=(u+bands+1+(slice)) mod maxvert;
      inc(i, 2);
    end;
  end;


  k := 0;
  for ii := 0 to rings - 1 do
  begin
    zc := ii - (ii/20);
    xc := cos(TWOPI * zc / rings)* center_offset;
    yc := sin(TWOPI * zc / rings)* center_offset;
    for jj := 0 to bands do
    begin

      x := xc + cos(jj * TWOPI / bands ) * radius;
      y := yc + sin(jj * TWOPI / bands ) * radius;
      z := 0-(ii*2);
            
      ivertex[k].x := floattov16 (x);
      ivertex[k].y := floattov16 (y);
      ivertex[k].z := floattov16 (z);
           
      icolor[k].r := (1 - (ii*4 div rings)) * 255;
      icolor[k].g := (1 - (ii*4 div rings)) * 255;
      icolor[k].b := (1 - (ii*4 div rings)) * 255;

      _u := (ii / rings ) * uscale;
      _v := (jj / bands ) * vscale;
      
      itexture[k].u := floattof32(_u);
      itexture[k].v := floattof32(_v);
      
      inc(k);
    end;
  end;
  
  result := vb;
end;

end.
