{ %fail }

{ Source provided for Free Pascal Bug Report 2070 }
{ Submitted by "MrNoop" on  2002-08-09 }
{ e-mail: mrnoop@the-asw.com }

{$mode objfpc}

type
TPersistent = class (TObject)
public
constructor Create(Proprio:TPersistent);virtual;
end;

TServeur = class (TPersistent)
public
constructor Create(Proprio:TPersistent);override;
end;

constructor TServeur.Create(Proprio:TPersistent);
begin
inherited Create; {<- lethal error (see above)}
