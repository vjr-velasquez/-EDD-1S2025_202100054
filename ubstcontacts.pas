unit uBSTContacts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  { Contacto almacenado en BST }
  TBSTContact = record
    Email : string;  // clave (case-insensitive)
    Nombre: string;
  end;

  PBSTNode = ^TBSTNode;
  TBSTNode = record
    Data : TBSTContact;
    Left : PBSTNode;
    Right: PBSTNode;
  end;

  { Árbol BST de contactos }
  TContactBST = class
  private
    FRoot: PBSTNode;

    function  CompareEmailCI(const A, B: string): Integer;
    procedure DisposeTree(N: PBSTNode);
    function  InsertNode(var N: PBSTNode; const C: TBSTContact): Boolean;
    function  FindNode(N: PBSTNode; const Email: string; out Nombre: string): Boolean;
    function  DeleteNode(var N: PBSTNode; const Email: string): Boolean;
    procedure InOrderToStrings(N: PBSTNode; L: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function  IsEmpty: Boolean;

    function  Insert(const Email, Nombre: string): Boolean;       // False si duplica email
    function  Find(const Email: string; out Nombre: string): Boolean;
    function  Delete(const Email: string): Boolean;

    procedure ToStringsInOrder(L: TStrings);                       // "Nombre;Email" por línea

    procedure SaveDOT(const Path: string);                         // Exporta DOT
    function  RenderPNGFromDOT(const DotPath, PngPath: string): Boolean; // Graphviz
    property  Root: PBSTNode read FRoot;
  end;

{ Helpers libres (opcionales) }
procedure BSTSaveDOT(Root: PBSTNode; const Path: string);
function  BSTRenderPNGFromDOT(const DotPath, PngPath: string): Boolean;

implementation

{-------------------- Utilitarios internos --------------------}

function EscapeDOT(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
end;

{-------------------- TContactBST --------------------}

constructor TContactBST.Create;
begin
  inherited Create;
  FRoot := nil;
end;

destructor TContactBST.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TContactBST.Clear;
begin
  DisposeTree(FRoot);
  FRoot := nil;
end;

function TContactBST.IsEmpty: Boolean;
begin
  Result := FRoot = nil;
end;

function TContactBST.CompareEmailCI(const A, B: string): Integer;
begin
  // Comparación case-insensitive y orden total
  Result := CompareText(A, B);
end;

procedure TContactBST.DisposeTree(N: PBSTNode);
begin
  if N = nil then Exit;
  DisposeTree(N^.Left);
  DisposeTree(N^.Right);
  Dispose(N);
end;

function TContactBST.Insert(const Email, Nombre: string): Boolean;
var
  C: TBSTContact;
begin
  C.Email  := Trim(Email);
  C.Nombre := Nombre;
  if C.Email = '' then Exit(False);
  Result := InsertNode(FRoot, C);
end;

function TContactBST.InsertNode(var N: PBSTNode; const C: TBSTContact): Boolean;
var
  cmp: Integer;
begin
  if N = nil then
  begin
    New(N);
    N^.Data  := C;
    N^.Left  := nil;
    N^.Right := nil;
    Exit(True);
  end;

  cmp := CompareEmailCI(C.Email, N^.Data.Email);
  if cmp = 0 then
    Exit(False) // duplicado
  else if cmp < 0 then
    Result := InsertNode(N^.Left, C)
  else
    Result := InsertNode(N^.Right, C);
end;

function TContactBST.Find(const Email: string; out Nombre: string): Boolean;
begin
  Result := FindNode(FRoot, Trim(Email), Nombre);
end;

function TContactBST.FindNode(N: PBSTNode; const Email: string; out Nombre: string): Boolean;
var
  cmp: Integer;
begin
  if N = nil then Exit(False);
  cmp := CompareEmailCI(Email, N^.Data.Email);
  if cmp = 0 then
  begin
    Nombre := N^.Data.Nombre;
    Exit(True);
  end
  else if cmp < 0 then
    Result := FindNode(N^.Left, Email, Nombre)
  else
    Result := FindNode(N^.Right, Email, Nombre);
end;

function TContactBST.Delete(const Email: string): Boolean;
begin
  Result := DeleteNode(FRoot, Trim(Email));
end;

function TContactBST.DeleteNode(var N: PBSTNode; const Email: string): Boolean;

  // Extrae el mínimo del subárbol N y devuelve su Data en MinData
  function ExtractMin(var N: PBSTNode; out MinData: TBSTContact): PBSTNode;
  var
    tmp: PBSTNode;
  begin
    if (N = nil) then Exit(nil);
    if N^.Left <> nil then
      Exit(ExtractMin(N^.Left, MinData))
    else
    begin
      // N es el mínimo
      MinData := N^.Data;
      tmp := N;
      N := N^.Right; // conectar al padre con el hijo derecho
      Dispose(tmp);
      Result := N;
    end;
  end;

var
  cmp: Integer;
  minData: TBSTContact;
  tmp: PBSTNode;
begin
  if N = nil then Exit(False);

  cmp := CompareEmailCI(Email, N^.Data.Email);
  if cmp < 0 then
    Result := DeleteNode(N^.Left, Email)
  else if cmp > 0 then
    Result := DeleteNode(N^.Right, Email)
  else
  begin
    // Encontrado
    if (N^.Left = nil) and (N^.Right = nil) then
    begin
      Dispose(N);
      N := nil;
    end
    else if (N^.Left = nil) then
    begin
      tmp := N;
      N := N^.Right;
      Dispose(tmp);
    end
    else if (N^.Right = nil) then
    begin
      tmp := N;
      N := N^.Left;
      Dispose(tmp);
    end
    else
    begin
      // Reemplazar por sucesor (mínimo del subárbol derecho)
      ExtractMin(N^.Right, minData);
      N^.Data := minData;
    end;
    Exit(True);
  end;
end;

procedure TContactBST.InOrderToStrings(N: PBSTNode; L: TStrings);
begin
  if (N = nil) or (L = nil) then Exit;
  InOrderToStrings(N^.Left, L);
  L.Add(Format('%s;%s', [N^.Data.Nombre, N^.Data.Email]));
  InOrderToStrings(N^.Right, L);
end;

procedure TContactBST.ToStringsInOrder(L: TStrings);
begin
  InOrderToStrings(FRoot, L);
end;

procedure TContactBST.SaveDOT(const Path: string);
begin
  BSTSaveDOT(FRoot, Path);
end;

function TContactBST.RenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  if not FileExists(DotPath) then Exit;

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Clear;
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');
    P.Parameters.Add(PngPath);
    P.Options := [poWaitOnExit];
    try
      P.Execute;
      Result := FileExists(PngPath);
    except
      // Si dot no está instalado, simplemente devolvemos False
      Result := False;
    end;
  finally
    P.Free;
  end;
end;

{-------------------- Helpers libres --------------------}

procedure BSTSaveDOT(Root: PBSTNode; const Path: string);

  procedure SaveDOTNode(N: PBSTNode; L: TStrings);
  var
    selfId, childId: string;
  begin
    if (N = nil) or (L = nil) then Exit;

    selfId := EscapeDOT(N^.Data.Email);
    L.Add(Format('  "%s" [label="%s\n%s"];', [selfId, selfId, EscapeDOT(N^.Data.Nombre)]));

    if N^.Left <> nil then
    begin
      childId := EscapeDOT(N^.Left^.Data.Email);
      L.Add(Format('  "%s" -> "%s";', [selfId, childId]));
    end;

    if N^.Right <> nil then
    begin
      childId := EscapeDOT(N^.Right^.Data.Email);
      L.Add(Format('  "%s" -> "%s";', [selfId, childId]));
    end;

    SaveDOTNode(N^.Left, L);
    SaveDOTNode(N^.Right, L);
  end;

var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.Add('digraph BSTContacts {');
    S.Add('  rankdir=TB;');
    S.Add('  node [shape=ellipse, fontname="Arial"];');

    if Root = nil then
      S.Add('  empty [label="(árbol vacío)"];')
    else
      SaveDOTNode(Root, S);

    S.Add('}');
    S.SaveToFile(Path);
  finally
    S.Free;
  end;
end;

function BSTRenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
var
  P: TProcess;
begin
  Result := False;
  if not FileExists(DotPath) then Exit;

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Clear;
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

