unit uAVLDrafts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Process;

type
  TDraft = record
    ID           : Integer;
    Remitente    : string;
    Destinatario : string;
    Asunto       : string;
    Mensaje      : string;
    Estado       : string;
    Fecha        : TDateTime;
  end;

  { TDraftAVL }
  TDraftAVL = class
  private
    FList: TList; // guardamos punteros a TDraft
    function FindIndexByID(AID: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Insert(const D: TDraft): Boolean;     // False si ya existe ID
    function Update(const D: TDraft): Boolean;     // True si existía y se actualizó
    function Delete(AID: Integer): Boolean;        // True si eliminó
    function Search(AID: Integer; out D: TDraft): Boolean;

    procedure ToStringsPreOrder(L: TStrings);
    procedure ToStringsInOrder(L: TStrings);
    procedure ToStringsPostOrder(L: TStrings);

    procedure SaveDOT(const APath: string);
    function RenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
  end;

var
  Drafts: TDraftAVL = nil;

implementation

type
  PDraft = ^TDraft;

{ TDraftAVL }

constructor TDraftAVL.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TDraftAVL.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    Dispose(PDraft(FList[i]));
  FList.Free;
  inherited Destroy;
end;

function TDraftAVL.FindIndexByID(AID: Integer): Integer;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if PDraft(FList[i])^.ID = AID then Exit(i);
  Result := -1;
end;

function TDraftAVL.Insert(const D: TDraft): Boolean;
var
  p: PDraft;
begin
  if FindIndexByID(D.ID) >= 0 then Exit(False);
  New(p);
  p^ := D;
  FList.Add(p);
  Result := True;
end;

function TDraftAVL.Update(const D: TDraft): Boolean;
var
  idx: Integer;
begin
  idx := FindIndexByID(D.ID);
  if idx < 0 then Exit(False);
  PDraft(FList[idx])^ := D;
  Result := True;
end;

function TDraftAVL.Delete(AID: Integer): Boolean;
var
  idx: Integer;
  p: PDraft;
begin
  idx := FindIndexByID(AID);
  if idx < 0 then Exit(False);
  p := FList[idx];
  FList.Delete(idx);
  Dispose(p);
  Result := True;
end;

function TDraftAVL.Search(AID: Integer; out D: TDraft): Boolean;
var
  idx: Integer;
begin
  idx := FindIndexByID(AID);
  Result := idx >= 0;
  if Result then D := PDraft(FList[idx])^;
end;

procedure TDraftAVL.ToStringsPreOrder(L: TStrings);
var
  i: Integer; p: PDraft;
begin
  L.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    p := PDraft(FList[i]);
    L.Add(Format('%d;%s;%s;%s', [p^.ID, p^.Remitente, p^.Destinatario, p^.Asunto]));
  end;
end;

procedure TDraftAVL.ToStringsInOrder(L: TStrings);
begin
  // Para el stub, usamos el mismo recorrido
  ToStringsPreOrder(L);
end;

procedure TDraftAVL.ToStringsPostOrder(L: TStrings);
begin
  // Para el stub, usamos el mismo recorrido
  ToStringsPreOrder(L);
end;

procedure TDraftAVL.SaveDOT(const APath: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Borradores {');
    sl.Add('  node [shape=box, fontname="Arial"];');
    sl.Add('  // Stub de AVL: representación lineal');
    sl.Add('}');
    sl.SaveToFile(APath);
  finally
    sl.Free;
  end;
end;

function TDraftAVL.RenderPNGFromDOT(const DotPath, PngPath: string): Boolean;
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

