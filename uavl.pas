unit uAVL;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TDraft = record
    ID: Integer;
    Remitente: string;
    Destinatario: string;
    Asunto: string;
    Mensaje: string;
    Estado: string;        // NUEVO
    Fecha: TDateTime;      // NUEVO (opcional)
  end;

  PDraft = ^TDraft;

  { --- AVL interno --- }
  PNode = ^TNode;
  TNode = record
    Data: TDraft;
    H: Integer;
    Left, Right: PNode;
  end;

  { --- API del AVL --- }
  TDraftAVL = class
  private
    Root: PNode;
    function Height(N: PNode): Integer; inline;
    procedure UpdateHeight(N: PNode); inline;
    function BalanceFactor(N: PNode): Integer; inline;

    function RotateRight(Y: PNode): PNode;
    function RotateLeft(X: PNode): PNode;

    function _Insert(N: PNode; const D: TDraft; out Added: Boolean): PNode;
    function _Delete(N: PNode; const Key: Integer; out Deleted: Boolean): PNode;
    function _Search(N: PNode; const Key: Integer; out D: TDraft): Boolean;

    procedure ClearNode(N: PNode);

    procedure ToStringsOrder(N: PNode; L: TStrings; const Order: Integer);
    // Order: 0=Pre, 1=In, 2=Post

    function EscapeDOT(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Insert(const D: TDraft): Boolean;   // False si ya existía
    function Update(const D: TDraft): Boolean;   // True si actualizado (existía)
    function Delete(const Key: Integer): Boolean;
    function Search(const Key: Integer; out D: TDraft): Boolean;

    procedure ToStringsPreOrder(L: TStrings);
    procedure ToStringsInOrder(L: TStrings);
    procedure ToStringsPostOrder(L: TStrings);

    procedure SaveDOT(const Path: string);
    function RenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
  end;

var
  Drafts: TDraftAVL = nil; // instancia global opcional

implementation

{================ AVL helpers ================}

constructor TDraftAVL.Create;
begin
  inherited Create;
  Root := nil;
end;

destructor TDraftAVL.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDraftAVL.ClearNode(N: PNode);
begin
  if N = nil then Exit;
  ClearNode(N^.Left);
  ClearNode(N^.Right);
  Dispose(N);
end;

procedure TDraftAVL.Clear;
begin
  ClearNode(Root);
  Root := nil;
end;

function TDraftAVL.Height(N: PNode): Integer; inline;
begin
  if N = nil then Exit(0) else Exit(N^.H);
end;

procedure TDraftAVL.UpdateHeight(N: PNode); inline;
var hl, hr: Integer;
begin
  if N = nil then Exit;
  hl := Height(N^.Left);
  hr := Height(N^.Right);
  if hl > hr then N^.H := hl + 1 else N^.H := hr + 1;
end;

function TDraftAVL.BalanceFactor(N: PNode): Integer; inline;
begin
  if N = nil then Exit(0);
  Result := Height(N^.Left) - Height(N^.Right);
end;

function TDraftAVL.RotateRight(Y: PNode): PNode;
var X, T2: PNode;
begin
  X := Y^.Left;
  T2 := X^.Right;

  X^.Right := Y;
  Y^.Left := T2;

  UpdateHeight(Y);
  UpdateHeight(X);
  Result := X;
end;

function TDraftAVL.RotateLeft(X: PNode): PNode;
var Y, T2: PNode;
begin
  Y := X^.Right;
  T2 := Y^.Left;

  Y^.Left := X;
  X^.Right := T2;

  UpdateHeight(X);
  UpdateHeight(Y);
  Result := Y;
end;

function TDraftAVL._Insert(N: PNode; const D: TDraft; out Added: Boolean): PNode;
var bf: Integer;
begin
  if N = nil then
  begin
    New(N);
    N^.Data := D;  // copia COMPLETA (incluye Estado/Fecha)
    N^.H := 1;
    N^.Left := nil; N^.Right := nil;
    Added := True;
    Exit(N);
  end;

  if D.ID < N^.Data.ID then
    N^.Left  := _Insert(N^.Left, D, Added)
  else if D.ID > N^.Data.ID then
    N^.Right := _Insert(N^.Right, D, Added)
  else
  begin
    // Ya existe
    Added := False;
    Exit(N);
  end;

  UpdateHeight(N);
  bf := BalanceFactor(N);

  // Rebalanceos
  // Left Left
  if (bf > 1) and (D.ID < N^.Left^.Data.ID) then Exit(RotateRight(N));
  // Right Right
  if (bf < -1) and (D.ID > N^.Right^.Data.ID) then Exit(RotateLeft(N));
  // Left Right
  if (bf > 1) and (D.ID > N^.Left^.Data.ID) then
  begin
    N^.Left := RotateLeft(N^.Left);
    Exit(RotateRight(N));
  end;
  // Right Left
  if (bf < -1) and (D.ID < N^.Right^.Data.ID) then
  begin
    N^.Right := RotateRight(N^.Right);
    Exit(RotateLeft(N));
  end;

  Result := N;
end;

function TDraftAVL.Insert(const D: TDraft): Boolean;
begin
  Root := _Insert(Root, D, Result);
end;

function TDraftAVL.Update(const D: TDraft): Boolean;
var cur: PNode;
begin
  cur := Root; Result := False;
  while cur <> nil do
  begin
    if D.ID < cur^.Data.ID then cur := cur^.Left
    else if D.ID > cur^.Data.ID then cur := cur^.Right
    else
    begin
      // Reemplaza TODOS los campos
      cur^.Data := D;
      Exit(True);
    end;
  end;
end;

function TDraftAVL._Search(N: PNode; const Key: Integer; out D: TDraft): Boolean;
begin
  if N = nil then Exit(False);
  if Key < N^.Data.ID then Exit(_Search(N^.Left, Key, D))
  else if Key > N^.Data.ID then Exit(_Search(N^.Right, Key, D))
  else begin D := N^.Data; Exit(True); end;
end;

function TDraftAVL.Search(const Key: Integer; out D: TDraft): Boolean;
begin
  Result := _Search(Root, Key, D);
end;

function TDraftAVL._Delete(N: PNode; const Key: Integer; out Deleted: Boolean): PNode;
var bf: Integer; tmp, minR: PNode;
begin
  if N = nil then begin Deleted := False; Exit(nil); end;

  if Key < N^.Data.ID then
    N^.Left := _Delete(N^.Left, Key, Deleted)
  else if Key > N^.Data.ID then
    N^.Right := _Delete(N^.Right, Key, Deleted)
  else
  begin
    // Encontrado
    Deleted := True;
    if (N^.Left = nil) or (N^.Right = nil) then
    begin
      tmp := N^.Left;
      if tmp = nil then tmp := N^.Right;
      if tmp = nil then
      begin
        Dispose(N);
        Exit(nil);
      end
      else
      begin
        N^.Data := tmp^.Data;
        N^.Left := tmp^.Left;
        N^.Right := tmp^.Right;
        N^.H := tmp^.H;
        Dispose(tmp);
      end;
    end
    else
    begin
      // Sucesor in-order
      minR := N^.Right;
      while (minR^.Left <> nil) do minR := minR^.Left;
      N^.Data := minR^.Data;
      N^.Right := _Delete(N^.Right, minR^.Data.ID, Deleted);
      // Deleted sigue True
    end;
  end;

  UpdateHeight(N);
  bf := BalanceFactor(N);

  // Rebalanceos
  // Left Left
  if (bf > 1) and (BalanceFactor(N^.Left) >= 0) then Exit(RotateRight(N));
  // Left Right
  if (bf > 1) and (BalanceFactor(N^.Left) < 0) then
  begin
    N^.Left := RotateLeft(N^.Left);
    Exit(RotateRight(N));
  end;
  // Right Right
  if (bf < -1) and (BalanceFactor(N^.Right) <= 0) then Exit(RotateLeft(N));
  // Right Left
  if (bf < -1) and (BalanceFactor(N^.Right) > 0) then
  begin
    N^.Right := RotateRight(N^.Right);
    Exit(RotateLeft(N));
  end;

  Result := N;
end;

function TDraftAVL.Delete(const Key: Integer): Boolean;
begin
  Root := _Delete(Root, Key, Result);
end;

procedure TDraftAVL.ToStringsOrder(N: PNode; L: TStrings; const Order: Integer);
var sFecha: string;
    line: string;
begin
  if N = nil then Exit;

  case Order of
    0: begin // Pre
         // salida compacta para listas: ID;Remitente;Destinatario;Asunto
         if N^.Data.Fecha > 0 then sFecha := DateToStr(N^.Data.Fecha) else sFecha := '';
         line := IntToStr(N^.Data.ID) + ';' + N^.Data.Remitente + ';' +
                 N^.Data.Destinatario + ';' + N^.Data.Asunto;
         L.Add(line);
         ToStringsOrder(N^.Left, L, 0);
         ToStringsOrder(N^.Right, L, 0);
       end;
    1: begin // In
         ToStringsOrder(N^.Left, L, 1);
         if N^.Data.Fecha > 0 then sFecha := DateToStr(N^.Data.Fecha) else sFecha := '';
         line := IntToStr(N^.Data.ID) + ';' + N^.Data.Remitente + ';' +
                 N^.Data.Destinatario + ';' + N^.Data.Asunto;
         L.Add(line);
         ToStringsOrder(N^.Right, L, 1);
       end;
    2: begin // Post
         ToStringsOrder(N^.Left, L, 2);
         ToStringsOrder(N^.Right, L, 2);
         if N^.Data.Fecha > 0 then sFecha := DateToStr(N^.Data.Fecha) else sFecha := '';
         line := IntToStr(N^.Data.ID) + ';' + N^.Data.Remitente + ';' +
                 N^.Data.Destinatario + ';' + N^.Data.Asunto;
         L.Add(line);
       end;
  end;
end;

procedure TDraftAVL.ToStringsPreOrder(L: TStrings);
begin
  L.Clear;
  ToStringsOrder(Root, L, 0);
end;

procedure TDraftAVL.ToStringsInOrder(L: TStrings);
begin
  L.Clear;
  ToStringsOrder(Root, L, 1);
end;

procedure TDraftAVL.ToStringsPostOrder(L: TStrings);
begin
  L.Clear;
  ToStringsOrder(Root, L, 2);
end;

function TDraftAVL.EscapeDOT(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

procedure TDraftAVL.SaveDOT(const Path: string);
  procedure EmitNode(sl: TStrings; const D: TDraft);
  var f: string;
  begin
    if D.Fecha > 0 then f := DateToStr(D.Fecha) else f := '';
    sl.Add(Format(
      '  "n%d" [shape=box, style="rounded,filled", fillcolor="#FFF9D7", fontname="Arial", '+
      'label="ID: %d\nRemitente: %s\nDestinatario: %s\nEstado: %s\nAsunto: %s\nFecha: %s\nMensaje: %s"];',
      [ D.ID, D.ID,
        EscapeDOT(D.Remitente),
        EscapeDOT(D.Destinatario),
        EscapeDOT(D.Estado),
        EscapeDOT(D.Asunto),
        EscapeDOT(f),
        EscapeDOT(D.Mensaje) ]));
  end;

  procedure Rec(n: PNode; sl: TStrings);
  begin
    if n = nil then Exit;
    EmitNode(sl, n^.Data);
    if n^.Left  <> nil then sl.Add(Format('  "n%d" -> "n%d";', [n^.Data.ID, n^.Left^.Data.ID]));
    if n^.Right <> nil then sl.Add(Format('  "n%d" -> "n%d";', [n^.Data.ID, n^.Right^.Data.ID]));
    Rec(n^.Left, sl);
    Rec(n^.Right, sl);
  end;

var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph AVL {');
    sl.Add('  rankdir=TB;');
    sl.Add('  labelloc="t"; fontsize=18; fontname="Arial";');
    sl.Add('  label="Árbol AVL - Correos";');
    Rec(Root, sl);
    if Root = nil then sl.Add('  empty [label="(vacío)"];');
    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

function TDraftAVL.RenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
var P: TProcess;
begin
  Result := False;
  if not FileExists(DotPath) then Exit;
  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Clear;
    P.Parameters.Add('-Tpng');  P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');     P.Parameters.Add(PngPath);
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

