unit uBlockchain;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, md5, Process, uTypes; // uTypes por PUsuario/PMail

type
  PBlock = ^TBlock;
  TBlock = record
    Index: Integer;
    Timestamp: TDateTime;
    Data: string;
    Nonce: QWord;
    PrevHash: string;
    Hash: string;
    Next: PBlock;
  end;
var
  BlockchainHead: PBlock;

procedure Blockchain_Free(var Head: PBlock);
function  Blockchain_FromInbox(U: PUsuario): PBlock; // crea génesis + 1 bloque por correo
procedure Blockchain_SaveDOT(Head: PBlock; const Path: string);
function  Blockchain_RenderDOTToPNG(const DotPath, PngPath: string): Boolean;

implementation

function HashText(const S: RawByteString): string;
begin
  // MD5 en hex (rápido y suficiente para reporte)
  Result := MD5Print(MD5String(S));
end;

function Block_CalcHash(const B: TBlock): string;
var
  payload: RawByteString;
begin
  payload :=
    IntToStr(B.Index) + '|' +
    DateTimeToStr(B.Timestamp) + '|' +
    B.Data + '|' +
    IntToStr(B.Nonce) + '|' +
    B.PrevHash;
  Result := HashText(payload);
end;

procedure Blockchain_Append(var Head, Tail: PBlock; const Data, PrevHash: string;
                            Index: Integer; Ts: TDateTime);
var
  N: PBlock;
begin
  New(N);
  N^.Index := Index;
  N^.Timestamp := Ts;
  N^.Data := Data;
  N^.PrevHash := PrevHash;
  // “minado” simple: Nonce = index * 12345 (suficiente para demo)
  N^.Nonce := QWord(Index) * 12345;
  N^.Next := nil;
  N^.Hash := Block_CalcHash(N^);

  if Tail <> nil then Tail^.Next := N else Head := N;
  Tail := N;
end;

function Blockchain_FromInbox(U: PUsuario): PBlock;
var
  head, tail: PBlock;
  M: PMail;
  prev: string;
  idx: Integer;
  data: string;
begin
  head := nil; tail := nil; prev := '0'; idx := 0;

  // Bloque génesis
  Blockchain_Append(head, tail, 'Genesis Block', prev, idx, EncodeDateTime(2025,9,30,0,0,0,0));
  prev := tail^.Hash;
  Inc(idx);

  // Un bloque por correo (puedes filtrar si quieres)
  if (U <> nil) then
  begin
    M := U^.InboxHead;
    while M <> nil do
    begin
      data := Format('ID: %d, Remitente: %s, Asunto: %s, Mensaje: %s',
               [M^.Id, M^.Remitente, M^.Asunto, M^.Mensaje]);
      Blockchain_Append(head, tail, data, prev, idx, // timestamp: fecha del correo
                        M^.Fecha);
      prev := tail^.Hash;
      Inc(idx);
      M := M^.Next;
    end;
  end;

  Result := head;
end;

procedure Blockchain_Free(var Head: PBlock);
var
  C: PBlock;
begin
  while Head <> nil do
  begin
    C := Head; Head := Head^.Next; Dispose(C);
  end;
end;

procedure AddRow(sl: TStrings; const LeftText: string);
begin
  sl.Add(Format('    <tr><td align="left" cellpadding="4">%s</td></tr>', [LeftText]));
end;

procedure Blockchain_SaveDOT(Head: PBlock; const Path: string);
var
  sl: TStringList;
  B: PBlock;
  nodeName: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Blockchain {');
    sl.Add('  rankdir=TB;');
    sl.Add('  node [shape=plaintext, fontname="Arial"];');

    B := Head;
    while B <> nil do
    begin
      nodeName := Format('blk_%d', [B^.Index]);
      sl.Add(Format('  %s [label=<', [nodeName]));
      sl.Add('  <table border="1" cellborder="1" cellspacing="0">');
      if B^.Index = 0 then
        AddRow(sl, Format('<b>Block %d (Genesis)</b>', [B^.Index]))
      else
        AddRow(sl, Format('<b>Block %d</b>', [B^.Index]));
      AddRow(sl, Format('Index: %d', [B^.Index]));
      AddRow(sl, 'Timestamp: ' + DateTimeToStr(B^.Timestamp));
      AddRow(sl, 'Data: ' + StringReplace(B^.Data, '&', '&amp;', [rfReplaceAll]));
      AddRow(sl, 'Nonce: ' + IntToStr(B^.Nonce));
      AddRow(sl, 'Prev Hash: ' + B^.PrevHash);
      AddRow(sl, 'Hash: ' + B^.Hash);
      sl.Add('  </table>');
      sl.Add('  >];');

      if (B^.Next <> nil) then
        sl.Add(Format('  %s -> blk_%d;', [nodeName, B^.Next^.Index]));

      B := B^.Next;
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

function Blockchain_RenderDOTToPNG(const DotPath, PngPath: string): Boolean;
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

