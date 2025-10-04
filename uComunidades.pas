unit uComunidades;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils, Process;

type
  // ===== lista simple de miembros (emails) =====
  PMemberNode = ^TMemberNode;
  TMemberNode = record
    email: string;
    next : PMemberNode;
  end;

  // ===== lista simple de mensajes =====
  PMsgNode = ^TMsgNode;
  TMsgNode = record
    emailAutor: string;
    texto     : string;
    fecha     : TDateTime;
    next      : PMsgNode;
  end;

  // ===== nodo del BST (comunidad) =====
  PComNode = ^TComNode;
  TComNode = record
    nombre        : string;     // clave (se guarda en minúsculas/trim)
    fechaCreacion : TDateTime;
    msgCount      : Integer;    // número de mensajes publicados
    miembros      : PMemberNode;
    mensajes      : PMsgNode;
    left, right   : PComNode;
  end;

var
  ComRoot: PComNode = nil;      // raíz del BST

// -------- API que ya usabas --------
function Comunidades_Crear(const Nombre: string): Boolean;
function Comunidades_AgregarMiembro(const Nombre, Email: string): Boolean;

// -------- API nueva para publicar y ver mensajes --------
function Comunidades_PublicarMensaje(const Nombre, EmailAutor, Mensaje: string): Boolean;

// Mensajes de UNA comunidad en Dest (uno por línea)
function Comunidades_VerMensajes(const Nombre: string; Dest: TStrings): Boolean;
// Mensajes de TODAS las comunidades (in-order) en Dest
procedure Comunidades_VerMensajesTodas(Dest: TStrings);

// Utilitario opcional
function Comunidades_Existe(const Nombre: string): Boolean;

// --------- Reporte (Graphviz) ---------
// 1) Emite el DOT del BST en AOut (contenido completo: digraph {...})
procedure EmitirDOT_ComunidadesBST(AOut: TStrings);
// 2) Genera archivos .dot y .png en OutDir. Devuelve la ruta del PNG.
function GenerarReporteComunidades(const OutDir: string): string;

implementation

uses uUsuariosAPI;

{========================== Helpers ==========================}

function NormName(const S: string): string; inline;
begin
  Result := Trim(LowerCase(S));
end;

function CmpName(const A, B: string): Integer; inline;
begin
  Result := CompareText(NormName(A), NormName(B));
end;

function NewComNode(const Nombre: string): PComNode;
begin
  New(Result);
  Result^.nombre        := NormName(Nombre);
  Result^.fechaCreacion := Now;
  Result^.msgCount      := 0;
  Result^.miembros      := nil;
  Result^.mensajes      := nil;
  Result^.left          := nil;
  Result^.right         := nil;
end;

function FindComunidad(const Nombre: string): PComNode;
var cur: PComNode; key: string; c: Integer;
begin
  Result := nil; cur := ComRoot; key := NormName(Nombre);
  while cur <> nil do
  begin
    c := CompareText(cur^.nombre, key);
    if c = 0 then exit(cur)
    else if c > 0 then cur := cur^.left
    else cur := cur^.right;
  end;
end;

procedure BSTInsert(var Root: PComNode; Node: PComNode);
var cur, parent: PComNode; c: Integer;
begin
  if Root = nil then begin Root := Node; exit; end;
  cur := Root; parent := nil;
  while cur <> nil do
  begin
    parent := cur;
    c := CompareText(Node^.nombre, cur^.nombre);
    if c = 0 then exit;               // ya existe (no insertamos)
    if c < 0 then cur := cur^.left
             else cur := cur^.right;
  end;
  if CompareText(Node^.nombre, parent^.nombre) < 0 then
    parent^.left  := Node
  else
    parent^.right := Node;
end;

function MemberExists(L: PMemberNode; const Email: string): Boolean;
var e: string;
begin
  e := NormName(Email);
  while L <> nil do
  begin
    if SameText(L^.email, e) then exit(True);
    L := L^.next;
  end;
  Result := False;
end;

procedure MemberAdd(var L: PMemberNode; const Email: string);
var p: PMemberNode;
begin
  New(p);
  p^.email := NormName(Email);
  p^.next  := L;
  L := p;
end;

procedure MsgAdd(var L: PMsgNode; const Email, Texto: string; const F: TDateTime);
var
  p, tail: PMsgNode;
begin
  New(p);
  p^.emailAutor := NormName(Email);
  p^.texto      := Texto;
  p^.fecha      := F;
  p^.next       := nil;

  // insertar al final para orden cronológico
  if L = nil then
    L := p
  else
  begin
    tail := L;
    while tail^.next <> nil do
      tail := tail^.next;
    tail^.next := p;
  end;
end;

// --------- pequeños sanitizadores (NO EscapeDOT) ----------

// Id de nodo seguro para DOT (solo letras, números, guiones bajos)
function IdSafe(const S: string): string;
var i: Integer; c: Char;
begin
  Result := 'com_';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    if (c in ['A'..'Z','a'..'z','0'..'9']) then
      Result += c
    else
      Result += '_';
  end;
end;

// Para labels: cambiamos comillas dobles por simples y CR/LF a \n
function LabelSafe(const S: string): string;
var R: string;
begin
  R := StringReplace(S, '"', '''', [rfReplaceAll]);
  R := StringReplace(R, #13#10, '\n', [rfReplaceAll]);
  R := StringReplace(R, #10, '\n',   [rfReplaceAll]);
  R := StringReplace(R, #13, '\n',   [rfReplaceAll]);
  Result := R;
end;

{========================== API ==========================}

function Comunidades_Crear(const Nombre: string): Boolean;
var n: string; node: PComNode;
begin
  Result := False;
  n := NormName(Nombre);
  if n = '' then exit;
  if FindComunidad(n) <> nil then exit;     // ya existe
  node := NewComNode(n);
  BSTInsert(ComRoot, node);
  Result := True;
end;

function Comunidades_AgregarMiembro(const Nombre, Email: string): Boolean;
var C: PComNode; e: string;
begin
  Result := False;
  C := FindComunidad(Nombre);
  if C = nil then exit;                     // no existe comunidad
  e := NormName(Email);
  if e = '' then exit;
  // validar contra el padrón de usuarios (vía API de cada integrante)
  if not UsersAPI_UserExists(e) then exit;
  if MemberExists(C^.miembros, e) then exit;      // duplicado
  MemberAdd(C^.miembros, e);
  Result := True;
end;

function Comunidades_PublicarMensaje(const Nombre, EmailAutor, Mensaje: string): Boolean;
var C: PComNode; e, m: string;
begin
  Result := False;
  C := FindComunidad(Nombre);
  if C = nil then exit;                           // no existe
  e := NormName(EmailAutor);
  m := Trim(Mensaje);
  if (e = '') or (m = '') then exit;
  // debe ser miembro de la comunidad
  if not MemberExists(C^.miembros, e) then exit;
  MsgAdd(C^.mensajes, e, m, Now);
  Inc(C^.msgCount);
  Result := True;
end;

function Comunidades_VerMensajes(const Nombre: string; Dest: TStrings): Boolean;
var C: PComNode; p: PMsgNode;
begin
  Result := False;
  if (Dest = nil) then exit;
  Dest.BeginUpdate;
  try
    Dest.Clear;
    C := FindComunidad(Nombre);
    if C = nil then exit;
    p := C^.mensajes;
    while p <> nil do
    begin
      Dest.Add(Format('[%s] %s: %s',
        [FormatDateTime('yyyy-mm-dd hh:nn', p^.fecha), p^.emailAutor, p^.texto]));
      p := p^.next;
    end;
    Result := True;
  finally
    Dest.EndUpdate;
  end;
end;

procedure InOrderMensajes(N: PComNode; Dest: TStrings);
var p: PMsgNode;
begin
  if (N = nil) or (Dest = nil) then exit;
  InOrderMensajes(N^.left, Dest);
  Dest.Add(Format('--- Comunidad: %s (creada %s, mensajes %d) ---',
    [N^.nombre, FormatDateTime('yyyy-mm-dd', N^.fechaCreacion), N^.msgCount]));
  p := N^.mensajes;
  if p = nil then
    Dest.Add('  (sin mensajes)')
  else
    while p <> nil do
    begin
      Dest.Add(Format('  [%s] %s: %s',
        [FormatDateTime('yyyy-mm-dd hh:nn', p^.fecha), p^.emailAutor, p^.texto]));
      p := p^.next;
    end;
  InOrderMensajes(N^.right, Dest);
end;

procedure Comunidades_VerMensajesTodas(Dest: TStrings);
begin
  if Dest = nil then exit;
  Dest.BeginUpdate;
  try
    Dest.Clear;
    InOrderMensajes(ComRoot, Dest);
  finally
    Dest.EndUpdate;
  end;
end;

function Comunidades_Existe(const Nombre: string): Boolean;
begin
  Result := FindComunidad(Nombre) <> nil;
end;

{======================= Reporte Graphviz =======================}

procedure EmitirDOT_ComunidadesBST(AOut: TStrings);

  procedure Rec(N: PComNode);
  var
    myId, leftId, rightId, etq: string;
  begin
    if N = nil then Exit;

    Rec(N^.left);

    // nodo actual
    myId := IdSafe('n_'+IntToHex(PtrUInt(N), SizeOf(Pointer)*2));
    etq  :=
      'Nombre: '  + LabelSafe(N^.nombre)                + '\n' +
      'Creación: ' + FormatDateTime('yyyy-mm-dd', N^.fechaCreacion) + '\n' +
      'Mensajes: ' + IntToStr(N^.msgCount);

    AOut.Add(Format('    %s [label="%s", shape=box, style="rounded,filled", fillcolor="#EAF7FF"];',
                    [myId, etq]));

    if N^.left <> nil then
    begin
      leftId := IdSafe('n_'+IntToHex(PtrUInt(N^.left), SizeOf(Pointer)*2));
      AOut.Add(Format('    %s -> %s;', [myId, leftId]));
    end;
    if N^.right <> nil then
    begin
      rightId := IdSafe('n_'+IntToHex(PtrUInt(N^.right), SizeOf(Pointer)*2));
      AOut.Add(Format('    %s -> %s;', [myId, rightId]));
    end;

    Rec(N^.right);
  end;

begin
  if AOut = nil then Exit;
  AOut.BeginUpdate;
  try
    AOut.Clear;
    AOut.Add('digraph G {');
    AOut.Add('  rankdir=TB; fontname="Helvetica";');
    AOut.Add('  labelloc="t"; label="Comunidades (Árbol BST)";');
    AOut.Add('  subgraph cluster_0 { label="BST de Comunidades"; style="rounded"; color="#777777";');

    if ComRoot = nil then
      AOut.Add('    vacio [label="(sin comunidades)", shape=box, style="rounded,filled", fillcolor="#FBFBFB"];')
    else
      Rec(ComRoot);

    AOut.Add('  }');
    AOut.Add('}');
  finally
    AOut.EndUpdate;
  end;
end;

function GenerarReporteComunidades(const OutDir: string): string;

  procedure GuardarTexto(const Ruta, Contenido: string);
  var fs: TFileStream; S: RawByteString;
  begin
    fs := TFileStream.Create(Ruta, fmCreate);
    try
      S := UTF8Encode(Contenido);
      if Length(S) > 0 then
        fs.WriteBuffer(S[1], Length(S));
    finally
      fs.Free;
    end;
  end;

  procedure EjecutarDOT(const DotFile, PngFile: string);
  var P: TProcess;
  begin
    P := TProcess.Create(nil);
    try
      P.Executable := 'dot';
      P.Parameters.Add('-Tpng');
      P.Parameters.Add(DotFile);
      P.Parameters.Add('-o');
      P.Parameters.Add(PngFile);
      P.Options := [poUsePipes, poNoConsole, poWaitOnExit];
      P.Execute;
    finally
      P.Free;
    end;
  end;

var
  sb      : TStringList;
  dotPath : string;
  pngPath : string;
  dir     : string;
begin
  dir := IncludeTrailingPathDelimiter(OutDir);
  if (dir = '') then dir := './';

  sb := TStringList.Create;
  try
    EmitirDOT_ComunidadesBST(sb);
    dotPath := dir + 'comunidades.dot';
    pngPath := dir + 'comunidades.png';
    GuardarTexto(dotPath, sb.Text);
    EjecutarDOT(dotPath, pngPath);
    Result := pngPath;
  finally
    sb.Free;
  end;
end;

end.
