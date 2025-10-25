unit uHashUsers;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes, uTypes;

type
  PUserNode = ^TUserNode;
  TUserNode = record
    Key  : string;    // email en minúsculas (clave canonical)
    User : PUsuario;  // puntero al usuario real
    Next : PUserNode; // siguiente en la cubeta
  end;

  TUsersHash = class
  private
    FBuckets: array of PUserNode;
    FSize   : Integer;
    function HashOf(const S: string): SizeInt;
    procedure FreeBucket(var N: PUserNode);
  public
    constructor Create(ASize: Integer = 1024);
    destructor Destroy; override;

    procedure Clear;
    procedure Insert(U: PUsuario);
    function  Find(const Email: string): PUsuario;
    function  Remove(const Email: string): Boolean;

    // Reconstruye desde la lista global de uTypes (UsuariosHead)
    procedure RebuildFromList;

    // Exporta un DOT para visualizar la tabla
    procedure SaveDOT(const Path: string);
  end;

var
  UsersHash: TUsersHash = nil;

procedure UsersHash_Init(ASize: Integer = 1024);
procedure UsersHash_Rebuild;
procedure UsersHash_Free;

implementation

function LowerKey(const S: string): string; inline;
begin
  Result := LowerCase(Trim(S));
end;

function TUsersHash.HashOf(const S: string): SizeInt;
var
  i: Integer;
  h: QWord;
begin
  // FNV-1a simple (64 bits) mod tamaño
  h := $CBF29CE484222325;
  for i := 1 to Length(S) do
  begin
    h := h xor Ord(S[i]);
    h := h * 1099511628211;
  end;
  if FSize <= 0 then Exit(0);
  Result := SizeInt(h mod QWord(FSize));
end;

constructor TUsersHash.Create(ASize: Integer);
var
  i: Integer;
begin
  inherited Create;
  if ASize < 8 then ASize := 8;
  FSize := ASize;
  SetLength(FBuckets, FSize);
  for i := 0 to FSize-1 do FBuckets[i] := nil;
end;

destructor TUsersHash.Destroy;
begin
  Clear;
  SetLength(FBuckets, 0);
  inherited Destroy;
end;

procedure TUsersHash.FreeBucket(var N: PUserNode);
var
  C, NX: PUserNode;
begin
  C := N;
  while C <> nil do
  begin
    NX := C^.Next;
    Dispose(C);
    C := NX;
  end;
  N := nil;
end;

procedure TUsersHash.Clear;
var
  i: Integer;
begin
  for i := 0 to FSize-1 do
    FreeBucket(FBuckets[i]);
end;

procedure TUsersHash.Insert(U: PUsuario);
var
  key: string;
  idx: SizeInt;
  N: PUserNode;
begin
  if (U = nil) or (Trim(U^.Email) = '') then Exit;
  key := LowerKey(U^.Email);
  idx := HashOf(key);

  // Si ya existe, actualiza puntero
  N := FBuckets[idx];
  while N <> nil do
  begin
    if N^.Key = key then
    begin
      N^.User := U;
      Exit;
    end;
    N := N^.Next;
  end;

  New(N);
  N^.Key := key;
  N^.User := U;
  N^.Next := FBuckets[idx];
  FBuckets[idx] := N;
end;

function TUsersHash.Find(const Email: string): PUsuario;
var
  key: string;
  idx: SizeInt;
  N: PUserNode;
begin
  Result := nil;
  if Trim(Email) = '' then Exit;
  key := LowerKey(Email);
  idx := HashOf(key);
  N := FBuckets[idx];
  while N <> nil do
  begin
    if N^.Key = key then Exit(N^.User);
    N := N^.Next;
  end;
end;

function TUsersHash.Remove(const Email: string): Boolean;
var
  key: string;
  idx: SizeInt;
  N, P: PUserNode;
begin
  Result := False;
  if Trim(Email) = '' then Exit;
  key := LowerKey(Email);
  idx := HashOf(key);
  N := FBuckets[idx]; P := nil;
  while N <> nil do
  begin
    if N^.Key = key then
    begin
      if P = nil then FBuckets[idx] := N^.Next
                 else P^.Next := N^.Next;
      Dispose(N);
      Exit(True);
    end;
    P := N; N := N^.Next;
  end;
end;

procedure TUsersHash.RebuildFromList;
var
  U: PUsuario;
begin
  Clear;
  U := UsuariosHead; // de uTypes
  while U <> nil do
  begin
    Insert(U);
    U := U^.Next;
  end;
end;

procedure TUsersHash.SaveDOT(const Path: string);
var
  sl: TStringList;
  i: Integer;
  N: PUserNode;
  headId, nodeId, lastId: string;
  idx: SizeInt;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph UsersHash {');
    sl.Add('  rankdir=LR;');
    sl.Add('  node [fontname="Arial"];');
    sl.Add('');

    for i := 0 to FSize-1 do
    begin
      headId := Format('B_%d', [i]);
      sl.Add(Format('  %s [label="[%d]", shape=box, style=filled, fillcolor="#e6f7ff"];', [headId, i]));
      lastId := headId;

      idx := i;
      N := FBuckets[idx];
      while N <> nil do
      begin
        nodeId := 'N_' + IntToStr(i) + '_' + StringReplace(N^.Key, '@','_',[rfReplaceAll]);
        sl.Add(Format('  %s [label="%s", shape=box, style=filled, fillcolor="#fff9c4"];',
              [nodeId, StringReplace(N^.Key, '"','\"',[rfReplaceAll])]));
        sl.Add(Format('  %s -> %s;', [lastId, nodeId]));
        lastId := nodeId;
        N := N^.Next;
      end;

      sl.Add('');
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure UsersHash_Init(ASize: Integer);
begin
  if UsersHash <> nil then Exit;
  UsersHash := TUsersHash.Create(ASize);
end;

procedure UsersHash_Rebuild;
begin
  if UsersHash <> nil then
    UsersHash.RebuildFromList;
end;

procedure UsersHash_Free;
begin
  FreeAndNil(UsersHash);
end;

end.

