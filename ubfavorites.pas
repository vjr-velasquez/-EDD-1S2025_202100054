unit uBFavorites;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TFavKey = record
    ID     : Integer;
    Asunto : string;
  end;

  { TBFavorites }
  TBFavorites = class
  private
    FItems: array of TFavKey;
    FGrado: Integer;
  public
    constructor Create(AGrado: Integer);
    procedure Clear;
    procedure Insert(const K: TFavKey);
    procedure SaveDOT(const APath: string);
  end;

function RenderizarPNGConDot(const DotPath, PngPath: string): Boolean;

var
  Favorites: TBFavorites = nil;

implementation

constructor TBFavorites.Create(AGrado: Integer);
begin
  inherited Create;
  FGrado := AGrado;
  SetLength(FItems, 0);
end;

procedure TBFavorites.Clear;
begin
  SetLength(FItems, 0);
end;

procedure TBFavorites.Insert(const K: TFavKey);
var
  n: Integer;
begin
  n := Length(FItems);
  SetLength(FItems, n+1);
  FItems[n] := K;
end;

procedure TBFavorites.SaveDOT(const APath: string);
var
  sl: TStringList;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Favoritos {');
    sl.Add('  node [shape=box, fontname="Arial"];');
    for i := 0 to High(FItems) do
      sl.Add(Format('  "fav%d" [label="#%d\n%s"];', [FItems[i].ID, FItems[i].ID,
        StringReplace(FItems[i].Asunto, '"', '\"', [rfReplaceAll])]));
    sl.Add('}');
    sl.SaveToFile(APath);
  finally
    sl.Free;
  end;
end;

function RenderizarPNGConDot(const DotPath, PngPath: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  if not FileExists(DotPath) then Exit;
  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');
    P.Parameters.Add(PngPath);
    P.Options := [poWaitOnExit];
    try
      P.Execute;
      Result := FileExists(PngPath);
    except
      Result := False;
    end;
  finally
    P.Free;
  end;
end;

end.

