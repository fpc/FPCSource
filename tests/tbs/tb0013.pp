{ Old file: tbs0016.pp }
{  }

{ %skiptarget=wince }

  uses
     crt;

  const
     { ... parameters }
     w = 10;    { max. 10 }
     h = 10;   { max. 10 }

  type
     tp = array[0..w,0..h] of double;

  var
     temp : tp;
     phi : tp;
     Bi : tp;

     boundary : array[0..w,0..h] of double;

  function start_temp(i,j : longint) : double;

    begin
       start_temp:=(boundary[i,0]*(h-j)+boundary[i,h]*j+boundary[0,j]*(w-i)+boundary[w,j]*i)/(w+h);
    end;

  procedure init;

    var
       i,j : longint;

    begin
       for i:=0 to w do
         for j:=0 to h do
           temp[i,j]:=start_temp(i,j);
    end;

  procedure draw;

    var
       i,j : longint;

    begin
       for i:=0 to w do
         for j:=0 to h do
           begin
              textcolor(white);
              gotoxy(i*7+1,j*2+1);
              writeln(temp[i,j]:6:0);
              textcolor(darkgray);
              gotoxy(i*7+1,j*2+2);
              writeln(phi[i,j]:6:3);
           end;
    end;

  procedure calc_phi;

    var
       i,j : longint;

    begin
       for i:=0 to w do
         for j:=0 to h do
           begin
              if (i=0) and (j=0) then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+0.5*temp[i,j+1]+0.5*temp[i+1,j]-(1+Bi[i,j])*temp[i,j];
                end
              else if (i=0) and (j=h) then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+0.5*temp[i,j-1]+0.5*temp[i+1,j]-(1+Bi[i,j])*temp[i,j];
                end
              else if (i=w) and (j=0) then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+0.5*temp[i,j+1]+0.5*temp[i-1,j]-(1+Bi[i,j])*temp[i,j];
                end
              else if (i=w) and (j=h) then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+0.5*temp[i,j-1]+0.5*temp[i-1,j]-(1+Bi[i,j])*temp[i,j];
                end
              else if i=0 then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+temp[i+1,j]+0.5*temp[i,j-1]+0.5*temp[i,j+1]-(2+Bi[i,j])*temp[i,j];
                end
              else if i=w then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+temp[i-1,j]+0.5*temp[i,j-1]+0.5*temp[i,j+1]-(2+Bi[i,j])*temp[i,j];
                end
              else if j=0 then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+temp[i,j+1]+0.5*temp[i-1,j]+0.5*temp[i+1,j]-(2+Bi[i,j])*temp[i,j];
                end
              else if j=h then
                begin
                   phi[i,j]:=Bi[i,j]*boundary[i,j]+temp[i,j-1]+0.5*temp[i-1,j]+0.5*temp[i+1,j]-(2+Bi[i,j])*temp[i,j];
                end
              else
                phi[i,j]:=temp[i,j-1]+temp[i-1,j]-4*temp[i,j]+temp[i+1,j]+temp[i,j+1];
           end;
    end;

  procedure adapt(i,j : longint);

    begin
       if (i=0) and (j=0) then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+0.5*temp[i,j+1]+0.5*temp[i+1,j])/(1+Bi[i,j]);
         end
       else if (i=0) and (j=h) then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+0.5*temp[i,j-1]+0.5*temp[i+1,j])/(1+Bi[i,j]);
         end
       else if (i=w) and (j=0) then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+0.5*temp[i,j+1]+0.5*temp[i-1,j])/(1+Bi[i,j]);
         end
       else if (i=w) and (j=h) then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+0.5*temp[i,j-1]+0.5*temp[i-1,j])/(1+Bi[i,j]);
         end
       else if i=0 then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+temp[i+1,j]+0.5*temp[i,j-1]+0.5*temp[i,j+1])/(2+Bi[i,j]);
         end
       else if i=w then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+temp[i-1,j]+0.5*temp[i,j-1]+0.5*temp[i,j+1])/(2+Bi[i,j]);
         end
       else if j=0 then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+temp[i,j+1]+0.5*temp[i-1,j]+0.5*temp[i+1,j])/(2+Bi[i,j]);
         end
       else if j=h then
         begin
            temp[i,j]:=(Bi[i,j]*boundary[i,j]+temp[i,j-1]+0.5*temp[i-1,j]+0.5*temp[i+1,j])/(2+Bi[i,j]);
         end
       else
         temp[i,j]:=(temp[i,j-1]+temp[i-1,j]+temp[i+1,j]+temp[i,j+1])/4;
       end;

  var
     iter,i,j,mi,mj : longint;
     habs,sigma_phi : double;

  begin
     clrscr;
     iter:=0;
     { setup boundary conditions }
     for i:=0 to w do
       for j:=0 to h do
         begin
            if (i=0) or (i=w) then
              bi[i,j]:=100
            else
              bi[i,j]:=100;

            if (j=0) then
              boundary[i,j]:=1000
            else
              boundary[i,j]:=300;
         end;
     init;
     draw;
     repeat
       calc_phi;
       mi:=0;
       mj:=0;
       sigma_phi:=0;
       inc(iter);
       habs:=abs(phi[mi,mj]);
       for i:=0 to w do
         for j:=0 to h do
           begin
              if abs(phi[i,j])>habs then
                begin
                   mi:=i;
                   mj:=j;
                   habs:=abs(phi[mi,mj]);
                end;
              { calculate error }
              sigma_phi:=sigma_phi+abs(phi[i,j]);
           end;
       adapt(mi,mj);
       gotoxy(1,23);
       textcolor(white);
       writeln(iter,' iterations, sigma_phi=',sigma_phi);
     until {keypressed or }(sigma_phi<0.5);
     draw;
     gotoxy(1,23);
     textcolor(white);
     writeln(iter,' iterations, sigma_phi=',sigma_phi);
     {writeln('press a key');
     if readkey=#0 then
       readkey;}
  end.
