unit blockchain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson;

type
  PBlock = ^TBlock;
  TBlock = record
    Index: Integer;
    Timestamp: string;   // DD-MM-YY::HH:MM:SS
    Data: string;        // ID, Remitente, Asunto, Mensaje (texto plano)
    Nonce: QWord;        // prueba de trabajo
    PrevHash: string;    // hash del bloque anterior (génesis: "0000")
    Hash: string;        // SHA256(INDEX+TIMESTAMP+DATA+NONCE+PREVHASH)
    Next: PBlock;        // siguiente en la lista (el que estaba como cabeza)
  end;

var
  BC_Head: PBlock = nil;
  BC_Count: Integer = 0; // cantidad de bloques; el índice del último será BC_Count-1

// Inicializa (si no existe) creando el bloque génesis.
procedure Blockchain_Init;

// Agrega bloque con DATA arbitrario (se inserta al inicio).
procedure Blockchain_Add(const AData: string);

// Helper para correos (arma DATA con los campos pedidos).
procedure Blockchain_AddMail(const MailID: Integer; const Remitente, Asunto, Mensaje: string);

// Devuelve JSON con toda la cadena (array de objetos).
function Blockchain_ToJSON: TJSONArray;

// Guarda JSON a archivo. Devuelve True si OK.
function Blockchain_SaveToJSONFile(const FilePath: string): Boolean;

// Libera toda la cadena.
procedure Blockchain_Clear;

implementation

const
  // Dificultad de PoW (hash debe iniciar con este prefijo). Ajusta según enunciado/profesor.
  DIFFICULTY_PREFIX = '0000';

{ ============ Utilidades ============ }



function NowTimestamp: string;
begin
  // Formato exacto: DD-MM-YY::HH:MM:SS
  Result := FormatDateTime('dd"-"mm"-"yy"::"hh":"nn":"ss', Now);
end;

function StartsWith(const S, Prefix: string): Boolean; inline;
begin
  Result := Copy(S, 1, Length(Prefix)) = Prefix;
end;



procedure PushFront(NewBlock: PBlock);
begin
  NewBlock^.Next := BC_Head;
  BC_Head := NewBlock;
  Inc(BC_Count);
end;

procedure MakeGenesis;
var
  G: PBlock;
begin
  New(G);
  G^.Index     := 0;
  G^.Timestamp := NowTimestamp;
  G^.Data      := 'GENESIS';
  G^.PrevHash  := '0000';
  G^.Nonce     := 0;
  G^.Hash      := '';
  G^.Next      := nil;
  //MineBlock(G^);
  PushFront(G);
end;

{ ============ API pública ============ }

procedure Blockchain_Init;
begin
  if BC_Head = nil then
    MakeGenesis;
end;

procedure Blockchain_Add(const AData: string);
var
  B: PBlock;
  prevHash: string;
begin
  Blockchain_Init;

  New(B);
  B^.Index     := BC_Count;      // siguiente índice (génesis fue 0)
  B^.Timestamp := NowTimestamp;
  B^.Data      := AData;
  B^.Nonce     := 0;
  if BC_Head <> nil then
    prevHash := BC_Head^.Hash
  else
    prevHash := '0000';
  B^.PrevHash := prevHash;
  B^.Hash     := '';
  B^.Next     := nil;

  //MineBlock(B^);
  PushFront(B);
end;

procedure Blockchain_AddMail(const MailID: Integer; const Remitente, Asunto, Mensaje: string);
var
  data: string;
begin
  // Puedes cambiar el formato de DATA si tu enunciado exige otro exacto.
  data := Format('ID=%d|Remitente=%s|Asunto=%s|Mensaje=%s', [MailID, Remitente, Asunto, Mensaje]);
  Blockchain_Add(data);
end;

function Blockchain_ToJSON: TJSONArray;
var
  arr: TJSONArray;
  obj: TJSONObject;
  p: PBlock;
begin
  arr := TJSONArray.Create;
  p := BC_Head;
  while p <> nil do
  begin
    obj := TJSONObject.Create;
    obj.Add('INDEX', p^.Index);
    obj.Add('TIMESTAMP', p^.Timestamp);
    obj.Add('DATA', p^.Data);
    obj.Add('NONCE', p^.Nonce);
    obj.Add('PREVIOUS_HASH', p^.PrevHash);
    obj.Add('HASH', p^.Hash);
    arr.Add(obj);
    p := p^.Next;
  end;
  Result := arr;
end;

function Blockchain_SaveToJSONFile(const FilePath: string): Boolean;
var
  arr: TJSONArray;
  sl: TStringList;
begin
  arr := Blockchain_ToJSON;
  sl := TStringList.Create;
  try
    sl.Text := arr.FormatJSON([foSingleLineArray, foSingleLineObject]);
    sl.SaveToFile(FilePath);
    Result := True;
  finally
    sl.Free;
    arr.Free;
  end;
end;

procedure Blockchain_Clear;
var
  cur, nxt: PBlock;
begin
  cur := BC_Head;
  while cur <> nil do
  begin
    nxt := cur^.Next;
    Dispose(cur);
    cur := nxt;
  end;
  BC_Head := nil;
  BC_Count := 0;
end;

end.

