unit uMerkle;

{$mode ObjFPC}{$H+}

// === Merkle Tree utilitario, sin dependencias externas ===
// - Por defecto usa FNV-1a 64-bit para el hash (rápido, 16 hex).
// - Si tienes la unit 'sha256' de FPC, puedes activar SHA-256:
//     {$DEFINE USE_SHA256}

{.$DEFINE USE_SHA256}

interface

uses
  SysUtils, Classes, uTypes;

type
  PMerkleNode = ^TMerkleNode;
  TMerkleNode = record
    Hash : string;     // hash en hex (minúsculas)
    Left : PMerkleNode;
    Right: PMerkleNode;
  end;

  // Construcción / liberación
  function MerkleRoot_FromHashes(const Hashes: array of string): string;
  function MerkleRoot_FromStrings(const Items: array of string): string;
  function MerkleRoot_FromInbox(U: PUsuario): string;                    // hojas = hash(mensaje completo)
  function MerkleRoot_FromMailBatch(const Mails: array of TMail): string;

  // Helpers de hashing
  function Merkle_HashString(const S: string): string;                   // hash de un string
  function Merkle_HashMail(const M: TMail): string;                      // hash de un correo (campos relevantes)

  // Export DOT para visualizar el árbol
  procedure Merkle_SaveDOT_FromStrings(const Items: array of string; const Path: string);
  procedure Merkle_SaveDOT_FromInbox(U: PUsuario; const Path: string);

implementation

{$IFDEF USE_SHA256}
uses sha256;
{$ENDIF}

function LowerHex(const B: TBytes): string;
const
  Hex: array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f');
var
  i: Integer;
begin
  SetLength(Result, Length(B)*2);
  for i := 0 to High(B) do
  begin
    Result[i*2+1] := Hex[(B[i] shr 4) and $0F];
    Result[i*2+2] := Hex[B[i] and $0F];
  end;
end;

function FNV1a64(const S: RawByteString): string;
var
  h: QWord;
  i: SizeInt;
  b: Byte;
  outb: TBytes;
begin
  // Semilla y multiplicador tipados como QWord para evitar warnings de rango
  h := QWord($CBF29CE484222325);
  for i := 1 to Length(S) do
  begin
    b := Byte(S[i]);
    h := h xor b;
    h := h * QWord(1099511628211);
  end;
  // devolver 8 bytes (64-bit) como 16 hex
  SetLength(outb, 8);
  outb[0] := Byte(h and $FF);
  outb[1] := Byte((h shr 8) and $FF);
  outb[2] := Byte((h shr 16) and $FF);
  outb[3] := Byte((h shr 24) and $FF);
  outb[4] := Byte((h shr 32) and $FF);
  outb[5] := Byte((h shr 40) and $FF);
  outb[6] := Byte((h shr 48) and $FF);
  outb[7] := Byte((h shr 56) and $FF);
  Result := LowerHex(outb);
end;

function HashConcatHex(const A, B: string): string;
{$IFDEF USE_SHA256}
var
  ctx : TSHA256Context;
  dig : TSHA256Digest;
  raw : TBytes;
{$ENDIF}
begin
  {$IFDEF USE_SHA256}
    // Hash de la concatenación A||B (en ASCII/UTF-8 según convenga)
    raw := TEncoding.ANSI.GetBytes(A + B);
    SHA256Init(ctx);
    if Length(raw) > 0 then
      SHA256Update(ctx, raw[0], Length(raw));
    SHA256Final(ctx, dig);
    SetLength(raw, SizeOf(dig));
    Move(dig, raw[0], SizeOf(dig));
    Result := LowerHex(raw);
  {$ELSE}
    // FNV-1a sobre la concatenación de los hex
    Result := FNV1a64(AnsiString(A + B));
  {$ENDIF}
end;

function HashStringHex(const S: string): string;
{$IFDEF USE_SHA256}
var
  ctx: TSHA256Context;
  dig: TSHA256Digest;
  raw: TBytes;
{$ENDIF}
begin
  {$IFDEF USE_SHA256}
    raw := TEncoding.UTF8.GetBytes(S);
    SHA256Init(ctx);
    if Length(raw) > 0 then
      SHA256Update(ctx, raw[0], Length(raw));
    SHA256Final(ctx, dig);
    SetLength(raw, SizeOf(dig));
    Move(dig, raw[0], SizeOf(dig));
    Result := LowerHex(raw);
  {$ELSE}
    Result := FNV1a64(AnsiString(S));
  {$ENDIF}
end;

function CanonMailText(const M: TMail): string;
var
  progFlag: string;
begin
  // Evita IfThen genérico (causa error de generics). Usamos condicional simple.
  if M.Programado then progFlag := '1' else progFlag := '0';

  // Canonicalizamos: remitente|asunto|mensaje|fecha|programado|estado
  // Usamos separador inusual para minimizar colisiones de texto.
  Result :=
    Trim(M.Remitente) + '␟' +
    Trim(M.Asunto)    + '␟' +
    Trim(M.Mensaje)   + '␟' +
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', M.Fecha) + '␟' +
    progFlag + '␟' +
    Trim(M.Estado);
end;

function NewNode(const H: string; L, R: PMerkleNode): PMerkleNode;
begin
  New(Result);
  Result^.Hash := H;
  Result^.Left := L;
  Result^.Right := R;
end;

procedure FreeNode(var N: PMerkleNode);
begin
  if N = nil then Exit;
  FreeNode(N^.Left);
  FreeNode(N^.Right);
  Dispose(N);
  N := nil;
end;

function BuildMerkleTree(const LeafHashes: array of string): PMerkleNode;
var
  cur, nxt: array of PMerkleNode;
  i, n: Integer;
  h: string;
begin
  Result := nil;
  n := Length(LeafHashes);
  if n = 0 then Exit;

  SetLength(cur, n);
  for i := 0 to n-1 do
    cur[i] := NewNode(LeafHashes[i], nil, nil);

  while Length(cur) > 1 do
  begin
    if (Length(cur) mod 2) <> 0 then
    begin
      // Duplicar el último si hay impar (regla típica de Merkle)
      SetLength(cur, Length(cur)+1);
      cur[High(cur)] := NewNode(cur[High(cur)-1]^.Hash, nil, nil);
    end;

    SetLength(nxt, Length(cur) div 2);
    for i := 0 to High(nxt) do
    begin
      h := HashConcatHex(cur[2*i]^.Hash, cur[2*i+1]^.Hash);
      nxt[i] := NewNode(h, cur[2*i], cur[2*i+1]);
    end;

    // pasar a la siguiente capa
    cur := nxt;
    SetLength(nxt, 0);
  end;

  // raíz
  Result := cur[0];
end;

function CollectHashes_FromStrings(const Items: array of string): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Capacity := Length(Items);
  for i := 0 to High(Items) do
    Result.Add(HashStringHex(Items[i]));
end;

function CollectHashes_FromInbox(U: PUsuario): TStringList;
var
  M: PMail;
begin
  Result := TStringList.Create;
  if (U = nil) then Exit;
  M := U^.InboxHead;
  while M <> nil do
  begin
    Result.Add(Merkle_HashMail(M^));
    M := M^.Next;
  end;
end;

function CollectHashes_FromMailBatch(const Mails: array of TMail): TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  Result.Capacity := Length(Mails);
  for i := 0 to High(Mails) do
    Result.Add(Merkle_HashMail(Mails[i]));
end;

function MerkleRoot_FromHashes(const Hashes: array of string): string;
var
  root: PMerkleNode;
begin
  Result := '';
  if Length(Hashes) = 0 then Exit;
  root := BuildMerkleTree(Hashes);
  if root <> nil then
  begin
    Result := root^.Hash;
    FreeNode(root); // libera todo el árbol
  end;
end;

function MerkleRoot_FromStrings(const Items: array of string): string;
var
  L: TStringList;
  arr: array of string;
  i: Integer;
begin
  Result := '';
  if Length(Items) = 0 then Exit;

  L := CollectHashes_FromStrings(Items);
  try
    SetLength(arr, L.Count);
    for i := 0 to L.Count-1 do arr[i] := L[i];
    Result := MerkleRoot_FromHashes(arr);
  finally
    L.Free;
  end;
end;

function MerkleRoot_FromInbox(U: PUsuario): string;
var
  L: TStringList;
  arr: array of string;
  i: Integer;
begin
  Result := '';
  L := CollectHashes_FromInbox(U);
  try
    if L.Count = 0 then Exit('');
    SetLength(arr, L.Count);
    for i := 0 to L.Count-1 do arr[i] := L[i];
    Result := MerkleRoot_FromHashes(arr);
  finally
    L.Free;
  end;
end;

function MerkleRoot_FromMailBatch(const Mails: array of TMail): string;
var
  L: TStringList;
  arr: array of string;
  i: Integer;
begin
  Result := '';
  if Length(Mails) = 0 then Exit;
  L := CollectHashes_FromMailBatch(Mails);
  try
    SetLength(arr, L.Count);
    for i := 0 to L.Count-1 do arr[i] := L[i];
    Result := MerkleRoot_FromHashes(arr);
  finally
    L.Free;
  end;
end;

function Merkle_HashString(const S: string): string;
begin
  Result := HashStringHex(S);
end;

function Merkle_HashMail(const M: TMail): string;
begin
  Result := HashStringHex(CanonMailText(M));
end;

procedure SaveDOT_Node(sl: TStrings; N: PMerkleNode; const parentId: string; var idSeq: QWord);
var
  myId: string;
begin
  if N = nil then Exit;
  Inc(idSeq);
  myId := 'N' + IntToStr(idSeq);
  sl.Add(Format('  %s [label="%s", shape=box, style=filled, fillcolor="#e8f5e9"];',
        [myId, N^.Hash]));
  if parentId <> '' then
    sl.Add(Format('  %s -> %s;', [parentId, myId]));
  if (N^.Left = nil) and (N^.Right = nil) then Exit;
  SaveDOT_Node(sl, N^.Left,  myId, idSeq);
  SaveDOT_Node(sl, N^.Right, myId, idSeq);
end;

procedure Merkle_SaveDOT_FromStrings(const Items: array of string; const Path: string);
var
  L: TStringList;
  arrH: array of string;
  i: Integer;
  root: PMerkleNode;
  idSeq: QWord;
begin
  L := CollectHashes_FromStrings(Items);
  try
    if L.Count = 0 then
    begin
      L.Clear;
      L.Add('digraph Merkle {');
      L.Add('  // sin elementos');
      L.Add('}');
      L.SaveToFile(Path);
      Exit;
    end;

    SetLength(arrH, L.Count);
    for i := 0 to L.Count-1 do arrH[i] := L[i];
  finally
    L.Free;
  end;

  root := BuildMerkleTree(arrH);
  L := TStringList.Create;
  try
    L.Add('digraph Merkle {');
    L.Add('  rankdir=TB;');
    L.Add('  node [fontname="Arial"];');
    idSeq := 0;
    SaveDOT_Node(L, root, '', idSeq);
    L.Add('}');
    L.SaveToFile(Path);
  finally
    L.Free;
    FreeNode(root);
  end;
end;

procedure Merkle_SaveDOT_FromInbox(U: PUsuario; const Path: string);
var
  L: TStringList;
  arrH: array of string;
  i: Integer;
  root: PMerkleNode;
  idSeq: QWord;
begin
  L := CollectHashes_FromInbox(U);
  try
    if L.Count = 0 then
    begin
      L.Clear;
      L.Add('digraph Merkle {');
      L.Add('  // inbox vacío');
      L.Add('}');
      L.SaveToFile(Path);
      Exit;
    end;

    SetLength(arrH, L.Count);
    for i := 0 to L.Count-1 do arrH[i] := L[i];
  finally
    L.Free;
  end;

  root := BuildMerkleTree(arrH);
  L := TStringList.Create;
  try
    L.Add('digraph Merkle {');
    L.Add('  rankdir=TB;');
    L.Add('  node [fontname="Arial"];');
    idSeq := 0;
    SaveDOT_Node(L, root, '', idSeq);
    L.Add('}');
    L.SaveToFile(Path);
  finally
    L.Free;
    FreeNode(root);
  end;
end;

end.

