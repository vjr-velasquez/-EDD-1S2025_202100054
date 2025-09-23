unit uBFavorites;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  // Clave del B-Tree para “Favoritos”
  TFavKey = record
    ID: LongInt;
    Asunto: AnsiString;
  end;

  PBFNode = ^TBFNode;
  TBFNode = record
    Leaf : Boolean;
    Keys : array of TFavKey;   // tamaño: 2*T-1
    Child: array of PBFNode;   // tamaño: 2*T
    NKeys: Integer;            // número de claves usadas en 'Keys'
  end;

  TVisitKey = procedure(const K: TFavKey);

  // Árbol B “Favoritos”
  TBFavorites = class
  private
    FRoot: PBFNode;
    FT   : Integer;            // grado mínimo (T>=2)

    function  NewNode(ALeaf: Boolean): PBFNode;
    procedure FreeNode(N: PBFNode);
    procedure SplitChild(Parent: PBFNode; I: Integer; Child: PBFNode);
    procedure InsertNonFull(N: PBFNode; const K: TFavKey);
    procedure TraverseIn(N: PBFNode; Proc: TVisitKey);
    function  SearchNode(N: PBFNode; ID: LongInt): PBFNode;
  public
    constructor Create(ADegree: Integer = 3);
    destructor Destroy; override;

    procedure Clear;

    procedure Insert(const K: TFavKey);
    function  Search(const ID: LongInt): PBFNode;

    procedure TraverseInOrder(Proc: TVisitKey);

    // Exportar el árbol a Graphviz DOT
    procedure SaveDOT(const FilePath: string);
  end;

var
  // Instancia global (opcional, útil para llamar desde forms)
  Favorites: TBFavorites = nil;

implementation

{================== Utilitarios internos ==================}

function TBFavorites.NewNode(ALeaf: Boolean): PBFNode;
begin
  New(Result);
  Result^.Leaf  := ALeaf;
  Result^.NKeys := 0;
  SetLength(Result^.Keys,  2*FT - 1);
  SetLength(Result^.Child, 2*FT);
end;

procedure TBFavorites.FreeNode(N: PBFNode);
var
  i: Integer;
begin
  if N = nil then Exit;
  if not N^.Leaf then
    for i := 0 to High(N^.Child) do
      if N^.Child[i] <> nil then
        FreeNode(N^.Child[i]);
  Dispose(N);
end;

procedure TBFavorites.SplitChild(Parent: PBFNode; I: Integer; Child: PBFNode);
var
  Z: PBFNode;
  j: Integer;
begin
  // Child está lleno (2T-1). Lo partimos en Child (izq) y Z (der); promovemos mediana.
  Z := NewNode(Child^.Leaf);
  Z^.NKeys := FT - 1;

  // Copiar las últimas T-1 claves de Child a Z
  for j := 0 to FT - 2 do
    Z^.Keys[j] := Child^.Keys[j + FT];

  // Si no es hoja, mover T hijos
  if not Child^.Leaf then
    for j := 0 to FT - 1 do
      Z^.Child[j] := Child^.Child[j + FT];

  // Reducir Child a T-1 claves
  Child^.NKeys := FT - 1;

  // Desplazar hijos en Parent a la derecha
  for j := Parent^.NKeys downto I + 1 do
    Parent^.Child[j + 1] := Parent^.Child[j];

  Parent^.Child[I + 1] := Z;

  // Desplazar claves en Parent para insertar la mediana
  for j := Parent^.NKeys - 1 downto I do
    Parent^.Keys[j + 1] := Parent^.Keys[j];

  // Insertar mediana en Parent
  Parent^.Keys[I] := Child^.Keys[FT - 1];
  Inc(Parent^.NKeys);
end;

procedure TBFavorites.InsertNonFull(N: PBFNode; const K: TFavKey);
var
  i: Integer;
begin
  i := N^.NKeys - 1;

  if N^.Leaf then
  begin
    // Insertar en hoja: desplazar a la derecha hasta encontrar hueco
    while (i >= 0) and (K.ID < N^.Keys[i].ID) do
    begin
      N^.Keys[i+1] := N^.Keys[i];
      Dec(i);
    end;
    N^.Keys[i+1] := K;
    Inc(N^.NKeys);
  end
  else
  begin
    // Buscar hijo donde insertar
    while (i >= 0) and (K.ID < N^.Keys[i].ID) do Dec(i);
    Inc(i);
    // Si el hijo está lleno, partirlo
    if (N^.Child[i] <> nil) and (N^.Child[i]^.NKeys = 2*FT - 1) then
    begin
      SplitChild(N, i, N^.Child[i]);
      if K.ID > N^.Keys[i].ID then Inc(i);
    end;
    // Bajar recursivamente
    if N^.Child[i] = nil then
      N^.Child[i] := NewNode(True); // seguridad, no debería pasar en inserción estándar
    InsertNonFull(N^.Child[i], K);
  end;
end;

procedure TBFavorites.TraverseIn(N: PBFNode; Proc: TVisitKey);
var
  i: Integer;
begin
  if N = nil then Exit;
  for i := 0 to N^.NKeys-1 do
  begin
    if not N^.Leaf then TraverseIn(N^.Child[i], Proc);
    if Assigned(Proc) then Proc(N^.Keys[i]);
  end;
  if not N^.Leaf then TraverseIn(N^.Child[N^.NKeys], Proc);
end;

function TBFavorites.SearchNode(N: PBFNode; ID: LongInt): PBFNode;
var
  i: Integer;
begin
  Result := nil;
  if N = nil then Exit;

  i := 0;
  while (i < N^.NKeys) and (ID > N^.Keys[i].ID) do Inc(i);

  if (i < N^.NKeys) and (ID = N^.Keys[i].ID) then Exit(N);

  if N^.Leaf then Exit(nil)
  else
    Exit(SearchNode(N^.Child[i], ID));
end;

{================== API pública ==================}

constructor TBFavorites.Create(ADegree: Integer);
begin
  inherited Create;
  if ADegree < 2 then
    FT := 2
  else
    FT := ADegree;
  FRoot := NewNode(True);
end;

destructor TBFavorites.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TBFavorites.Clear;
begin
  FreeNode(FRoot);
  FRoot := NewNode(True);
end;

procedure TBFavorites.Insert(const K: TFavKey);
var
  R, S: PBFNode;
  i: Integer; // <--- declarar aquí, no dentro del bloque
begin
  // si ya existe, actualizar (reemplazar Asunto)
  R := Search(K.ID);
  if R <> nil then
  begin
    // buscar posición exacta dentro del nodo R
    for i := 0 to R^.NKeys-1 do
      if R^.Keys[i].ID = K.ID then
      begin
        R^.Keys[i] := K;
        Exit;
      end;
  end;

  if FRoot^.NKeys = 2*FT - 1 then
  begin
    // raíz llena: crecer altura
    S := NewNode(False);
    S^.Child[0] := FRoot;
    FRoot := S;
    SplitChild(S, 0, S^.Child[0]);
    InsertNonFull(S, K);
  end
  else
    InsertNonFull(FRoot, K);
end;


function TBFavorites.Search(const ID: LongInt): PBFNode;
begin
  Result := SearchNode(FRoot, ID);
end;

procedure TBFavorites.TraverseInOrder(Proc: TVisitKey);
begin
  TraverseIn(FRoot, Proc);
end;

procedure TBFavorites.SaveDOT(const FilePath: string);
var
  F: Text;

  function Esc(const S: AnsiString): AnsiString;
  begin
    Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  end;

  procedure WriteNode(N: PBFNode; const Name: string);
  var
    i: Integer;
    lbl, childName: AnsiString;
  begin
    if N = nil then Exit;

    // Etiqueta con todas las claves del nodo
    lbl := '';
    for i := 0 to N^.NKeys-1 do
    begin
      if i > 0 then lbl += '|';
      lbl += Format('%d: %s', [N^.Keys[i].ID, Esc(N^.Keys[i].Asunto)]);
    end;
    if lbl = '' then lbl := '(vacío)';

    WriteLn(F, Format('  %s [shape=record,label="%s"];', [Name, lbl]));

    // Hijos
    if not N^.Leaf then
    begin
      for i := 0 to N^.NKeys do
      begin
        childName := Format('%s_%d', [Name, i]);
        if N^.Child[i] <> nil then
        begin
          WriteNode(N^.Child[i], childName);
          WriteLn(F, Format('  %s -> %s;', [Name, childName]));
        end
        else
        begin
          // opcional: no dibujar hijos nulos
        end;
      end;
    end;
  end;

begin
  Assign(F, FilePath);
  Rewrite(F);
  try
    WriteLn(F, 'digraph BTreeFavorites {');
    WriteLn(F, '  rankdir=TB;');
    WriteLn(F, '  node [fontname="monospace"];');
    if FRoot <> nil then
      WriteNode(FRoot, 'root')
    else
      WriteLn(F, '  empty [label="(árbol vacío)"];');
    WriteLn(F, '}');
  finally
    Close(F);
  end;
end;

initialization
  Favorites := TBFavorites.Create(3); // grado 3 por defecto

finalization
  FreeAndNil(Favorites);

end.

