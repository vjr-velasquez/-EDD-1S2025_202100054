unit logeo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Process;

type
  { ====== ESTRUCTURAS BASE ====== }
  PMail = ^TMail;
  TMail = record
    Id: Integer;
    Remitente: string;
    Asunto: string;
    Mensaje: string;
    Fecha: TDateTime;
    Programado: Boolean;
    Estado: string;     // 'nuevo' | 'leido' | etc.
    Prev, Next: PMail;  // lista doble
  end;

  PProg = ^TProg; // cola de programados
  TProg = record
    Id: Integer;
    Remitente: string;
    Destinatario: string;
    Asunto: string;
    Mensaje: string;
    FechaProg: TDateTime;
    Next: PProg;
  end;

  PTrash = ^TTrash; // pila (papelera)
  TTrash = record
    Mail: TMail;
    Next: PTrash;
  end;

  PContacto = ^TContacto; // lista circular
  TContacto = record
    Email: string;
    Nombre: string;
    Next: PContacto;
  end;

  PRel = ^TRel; // relaciones emisor->receptor (para matriz)
  TRel = record
    FromEmail: string;
    ToEmail: string;
    Count: Integer;
    Next: PRel;
  end;

  PUsuario = ^TUsuario; // lista simple (usuarios)
  TUsuario = record
    Id: Integer;
    Nombre: string;
    Usuario: string;
    Email: string;
    Telefono: string;
    Password: string;
    // por usuario:
    InboxHead, InboxTail: PMail; // lista doble
    ProgHead, ProgTail: PProg;   // cola
    TrashTop: PTrash;            // pila
    ContactTail: PContacto;      // circular
    Next: PUsuario;              // lista simple
  end;

  { ====== COMUNIDADES (lista de listas) ====== }
  PMember = ^TMember;
  TMember = record
    Email: string;
    Next: PMember;
  end;

  PComunidad = ^TComunidad;
  TComunidad = record
    Id: Integer;
    Nombre: string;
    Miembros: PMember;   // lista simple de miembros
    Next: PComunidad;    // siguiente comunidad
  end;

  { ====== FORM LOGIN ====== }
  TForm1 = class(TForm)
    Button1: TButton; // Iniciar sesión
    Button2: TButton; // Crear cuenta
    Edit1: TEdit;     // Email
    Edit2: TEdit;     // Password
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function EmailValido(const AEmail: string): Boolean;
  public
  end;

var
  Form1: TForm1;

  // === Estado global accesible desde otros units (root/user) ===
  UsuariosHead: PUsuario = nil;
  CurrentUser: PUsuario = nil;
  RelHead: PRel = nil;

  ComunidadesHead: PComunidad = nil;

  NextId: Integer = 1;
  NextMailId: Integer = 1;
  NextComunidadId: Integer = 1;

// === API que usan otros units ===
function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
procedure AgregarUsuario(const ANombre, AUsuario, AEmail, ATelefono, APassword: string);


// —— Reportes Graphviz ——
function ExportarUsuariosDOT(const Path: string): Boolean;            // lista simple de usuarios
function ExportarInboxDOT(U: PUsuario; const Path: string): Boolean; overload; // lista doble (inbox)
function ExportarInboxDOT(const Email, Path: string): Boolean; overload;
function ExportarRelacionesDOT(const Path: string): Boolean;          // matriz dispersa (relaciones)
function RenderizarPNGConDot(const DotPath, PngPath: string): Boolean;

implementation


// para evitar ciclos en la interfaz y no provocar "Duplicate identifier LOGEO".
uses
  root, user;

{$R *.lfm}

{====================== HELPERS ======================}
function EqualCI(const A, B: string): Boolean; inline;
begin
  Result := LowerCase(A) = LowerCase(B);
end;

function _Esc(const S: string): string; inline;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
end;

function EscForLabel(const S: string): string; inline;
var R: string;
begin
  R := StringReplace(S, '\', '\\', [rfReplaceAll]);
  R := StringReplace(R, '"', '\"', [rfReplaceAll]);
  R := StringReplace(R, #13#10, '\n', [rfReplaceAll]);
  R := StringReplace(R, #10, '\n', [rfReplaceAll]);
  R := StringReplace(R, #13, '\n', [rfReplaceAll]);
  Result := R;
end;

function _IdSafe(const S: string): string;
var i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] in ['A'..'Z','a'..'z','0'..'9','_'] then
      Result += S[i]
    else
      Result += '_';
end;

function Shorten(const S: string; MaxLen: Integer): string; inline;
begin
  if Length(S) <= MaxLen then Exit(S);
  Result := Copy(S, 1, MaxLen-1) + '…';
end;

{====================== USUARIOS ======================}
function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
var Cur: PUsuario;
begin
  Result := nil;
  Cur := UsuariosHead;
  while Cur <> nil do
  begin
    if EqualCI(Cur^.Email, AEmail) then Exit(Cur);
    Cur := Cur^.Next;
  end;
end;

procedure AgregarUsuario(const ANombre, AUsuario, AEmail, ATelefono, APassword: string);
var Nuevo: PUsuario;
begin
  New(Nuevo);
  Nuevo^.Id := NextId; Inc(NextId);
  Nuevo^.Nombre   := ANombre;
  Nuevo^.Usuario  := AUsuario;
  Nuevo^.Email    := AEmail;
  Nuevo^.Telefono := ATelefono;
  Nuevo^.Password := APassword;
  // inicializar estructuras
  Nuevo^.InboxHead := nil; Nuevo^.InboxTail := nil;
  Nuevo^.ProgHead := nil;  Nuevo^.ProgTail := nil;
  Nuevo^.TrashTop := nil;
  Nuevo^.ContactTail := nil;
  // insertar al inicio (LIFO)
  Nuevo^.Next := UsuariosHead;
  UsuariosHead := Nuevo;
end;

procedure AsegurarRoot;
begin
  if BuscarUsuarioPorEmail('root@edd.com') = nil then
    AgregarUsuario('root', 'root', 'root@edd.com', '', 'root123');
end;

{====================== LOGIN ======================}
procedure TForm1.FormCreate(Sender: TObject);
begin
  AsegurarRoot; // también se llama en initialization
end;

function TForm1.EmailValido(const AEmail: string): Boolean;
var pArroba, pPunto: SizeInt;
begin
  pArroba := Pos('@', AEmail);
  pPunto  := LastDelimiter('.', AEmail);
  Result := (pArroba > 1) and (pPunto > pArroba + 1) and (pPunto < Length(AEmail));
end;

procedure TForm1.Button1Click(Sender: TObject);
var U: PUsuario;
begin
  if (Trim(Edit1.Text) = '') or (Edit2.Text = '') then
  begin
    ShowMessage('Por favor, ingrese su email y contraseña.');
    Exit;
  end;

  U := BuscarUsuarioPorEmail(Edit1.Text);
  if (U <> nil) and (U^.Password = Edit2.Text) then
  begin
    CurrentUser := U;
    if EqualCI(U^.Email, 'root@edd.com') then
    begin
      ShowMessage('¡Bienvenido, Administrador!');
      Hide;
      // OJO: calificado con la unidad para evitar "Identifier not found"
      root.Form2 := root.TForm2.Create(Application);
      root.Form2.Show;
    end
    else
    begin
      ShowMessage('¡Bienvenido, ' + U^.Email + '!');
      Hide;
      user.Form3 := user.TForm3.Create(Application);
      user.Form3.Show;
    end;
  end
  else
    ShowMessage('Credenciales incorrectas. Intente de nuevo.');
end;

procedure TForm1.Button2Click(Sender: TObject);
var email, password: string;
begin
  email := Trim(Edit1.Text);
  password := Edit2.Text;

  if (email = '') or (password = '') then
  begin
    ShowMessage('Ingrese email y contraseña para registrarse.');
    Exit;
  end;

  if not EmailValido(email) then
  begin
    ShowMessage('El formato de email no es válido.');
    Exit;
  end;

  if EqualCI(email, 'root@edd.com') then
  begin
    ShowMessage('El usuario root ya existe y no puede registrarse nuevamente.');
    Exit;
  end;

  if BuscarUsuarioPorEmail(email) <> nil then
  begin
    ShowMessage('El usuario ya existe dentro del sistema.');
    Exit;
  end;

  AgregarUsuario('', '', email, '', password);
  ShowMessage('Usuario registrado con éxito. Ahora puede iniciar sesión.');
end;
{====================== REPORTES GRAPHVIZ ======================}

function ExportarComunidadesDOT(const Path: string): Boolean;
var
  sl: TStringList;
  C: PComunidad;
  M: PMember;
  cid, prevCID, uid, firstUID, lastUID: string;
  idx: Integer;
  rankSame: string;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Add('digraph Comunidades {');
    sl.Add('  rankdir=LR;');
    sl.Add('  graph [nodesep=0.6, ranksep=1.0, splines=ortho];');
    sl.Add('  node  [shape=box, style=filled, fontname="Arial"];');
    sl.Add('');

    prevCID := '';
    rankSame := '  { rank=same; ';
    C := ComunidadesHead;
    while C <> nil do
    begin
      cid := 'C' + IntToStr(C^.Id);
      sl.Add(Format('  %s [label="%s", fillcolor="#b3d9f2"];', [cid, _Esc(C^.Nombre)]));
      if prevCID <> '' then
        sl.Add(Format('  %s -> %s;', [prevCID, cid]));
      prevCID := cid;
      rankSame += cid + '; ';
      C := C^.Next;
    end;
    rankSame += '}';
    sl.Add(rankSame);
    sl.Add('');

    sl.Add('  node [fillcolor="#fff9c4"];');
    C := ComunidadesHead;
    while C <> nil do
    begin
      cid := 'C' + IntToStr(C^.Id);
      firstUID := ''; lastUID := '';
      M := C^.Miembros; idx := 0;

      if M = nil then
      begin
        uid := Format('U_%s_vacio', [cid]);
        sl.Add(Format('  %s [label="(sin miembros)"];', [uid]));
        sl.Add(Format('  %s -> %s;', [cid, uid]));
      end
      else
      begin
        while M <> nil do
        begin
          Inc(idx);
          uid := Format('U_%s_%d_%s', [cid, idx, _IdSafe(M^.Email)]);
          sl.Add(Format('  %s [label="%s"];', [uid, _Esc(M^.Email)]));
          if firstUID = '' then firstUID := uid;
          if lastUID <> '' then sl.Add(Format('  %s -> %s;', [lastUID, uid]));
          lastUID := uid;
          M := M^.Next;
        end;
        sl.Add(Format('  %s -> %s;', [cid, firstUID]));
      end;

      C := C^.Next;
      sl.Add('');
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
    Result := True;
  finally
    sl.Free;
  end;
end;

function ExportarComunidadesMatrizDOT(const Path: string): Boolean;
var
  sl: TStringList;
  C: PComunidad;
  M: PMember;
  prevCID, firstUID, lastUID, cid, uid: string;
  idx: Integer;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Add('digraph ComunidadesMatriz {');
    sl.Add('  rankdir=LR;');
    sl.Add('  graph [splines=ortho, nodesep=0.6, ranksep=0.9];');
    sl.Add('  node  [fontname="Arial"];');
    sl.Add('');

    prevCID := '';
    C := ComunidadesHead;
    while C <> nil do
    begin
      cid := 'C' + IntToStr(C^.Id);
      sl.Add(Format('  %s [label="%s", shape=box, style=filled, fillcolor="#b3d9f2", width=2.2, height=0.8];',
            [cid, _Esc(C^.Nombre)]));

      if prevCID <> '' then
        sl.Add(Format('  %s -> %s;', [prevCID, cid]));
      prevCID := cid;

      firstUID := ''; lastUID := '';
      M := C^.Miembros; idx := 0;
      if M = nil then
      begin
        uid := Format('E_vacia_%s', [cid]);
        sl.Add(Format('  %s [label="(sin miembros)", shape=box, style=filled, fillcolor="#fff9c4"];', [uid]));
        sl.Add(Format('  %s -> %s;', [cid, uid]));
      end
      else
      begin
        while M <> nil do
        begin
          Inc(idx);
          uid := Format('U_%s_%s_%d', [_IdSafe(M^.Email), cid, idx]);
          sl.Add(Format('  %s [label="%s", shape=box, style=filled, fillcolor="#fff9c4"];',
                [uid, _Esc(M^.Email)]));
          if firstUID = '' then firstUID := uid;
          if lastUID <> '' then
            sl.Add(Format('  %s -> %s;', [lastUID, uid]));
          lastUID := uid;
          M := M^.Next;
        end;
        sl.Add(Format('  %s -> %s;', [cid, firstUID]));
      end;

      C := C^.Next;
      sl.Add('');
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
    Result := True;
  finally
    sl.Free;
  end;
end;

function ExportarUsuariosDOT(const Path: string): Boolean;
var
  sl: TStringList;
  U, Prev: PUsuario;
  nid, pid: string;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Add('digraph Usuarios {');
    sl.Add('  rankdir=LR;');
    sl.Add('  graph [nodesep=0.6, ranksep=1.0, splines=ortho];');
    sl.Add('  node  [shape=record, style=filled, fillcolor="#e6f7ff", fontname="Arial"];');
    sl.Add('');

    if UsuariosHead = nil then
    begin
      sl.Add('  vacio [label="(sin usuarios)", shape=box, fillcolor="#fff0f0"];');
      sl.Add('}');
      sl.SaveToFile(Path);
      Exit(True);
    end;

    Prev := nil;
    U := UsuariosHead;
    while U <> nil do
    begin
      nid := 'U' + IntToStr(U^.Id);
      sl.Add(Format('  %s [label="{Id: %d|Nombre: %s|Usuario: %s|Email: %s|Tel: %s}"];',
        [nid, U^.Id, _Esc(U^.Nombre), _Esc(U^.Usuario), _Esc(U^.Email), _Esc(U^.Telefono)]));
      if Prev <> nil then
      begin
        pid := 'U' + IntToStr(Prev^.Id);
        sl.Add(Format('  %s -> %s;', [pid, nid]));
      end;
      Prev := U;
      U := U^.Next;
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
    Result := True;
  finally
    sl.Free;
  end;
end;

function ExportarInboxDOT(U: PUsuario; const Path: string): Boolean;
var
  sl: TStringList;
  M, Prev: PMail;
  nid, pid: string;
  titulo, lbl, progStr, fechaStr: string;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Add('digraph Inbox {');
    sl.Add('  rankdir=LR;');
    sl.Add('  graph [nodesep=0.6, ranksep=1.0, splines=ortho];');
    sl.Add('  node  [shape=box, style=filled, fillcolor="#e8f5e9", fontname="Arial"];');
    sl.Add('');

    if (U = nil) then
    begin
      sl.Add('  vacio [label="(usuario nulo)"];');
      sl.Add('}');
      sl.SaveToFile(Path);
      Exit(True);
    end;

    titulo := 'Inbox de ' + U^.Email;
    sl.Add(Format('  title [label="%s", fillcolor="#bbdefb"];', [EscForLabel(titulo)]));
    sl.Add('');

    if U^.InboxHead = nil then
    begin
      sl.Add('  vacio [label="(inbox vacío)"];');
      sl.Add('  title -> vacio;');
      sl.Add('}');
      sl.SaveToFile(Path);
      Exit(True);
    end;

    Prev := nil;
    M := U^.InboxHead;
    while M <> nil do
    begin
      nid := 'M' + IntToStr(M^.Id);
      if M^.Programado then progStr := 'Sí' else progStr := 'No';
      fechaStr := FormatDateTime('yyyy-mm-dd hh:nn', M^.Fecha);

      lbl :=
        'Id: '        + IntToStr(M^.Id) + '\n' +
        'Remitente: ' + EscForLabel(M^.Remitente) + '\n' +
        'Estado: '    + EscForLabel(M^.Estado) + '\n' +
        'Programado: '+ progStr + '\n' +
        'Asunto: '    + EscForLabel(Shorten(M^.Asunto, 80)) + '\n' +
        'Fecha: '     + fechaStr + '\n' +
        'Mensaje: '   + EscForLabel(Shorten(M^.Mensaje, 120));

      sl.Add(Format('  %s [label="%s"];', [nid, lbl]));

      if Prev = nil then
        sl.Add(Format('  title -> %s;', [nid]))
      else
      begin
        pid := 'M' + IntToStr(Prev^.Id);
        sl.Add(Format('  %s -> %s [dir=both];', [pid, nid]));
      end;

      Prev := M;
      M := M^.Next;
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
    Result := True;
  finally
    sl.Free;
  end;
end;

function ExportarInboxDOT(const Email, Path: string): Boolean;
begin
  Result := ExportarInboxDOT(BuscarUsuarioPorEmail(Email), Path);
end;

function ExportarRelacionesDOT(const Path: string): Boolean;
var
  sl: TStringList;
  U: PUsuario;
  R: PRel;
  fromId, toId, nodeId: string;
  pen: Double;
  penStr: string;
begin
  Result := False;
  sl := TStringList.Create;
  try
    sl.Add('digraph Relaciones {');
    sl.Add('  rankdir=LR;');
    sl.Add('  graph [nodesep=0.6, ranksep=1.0, splines=ortho];');
    sl.Add('  node  [shape=box, style=filled, fillcolor="#f0f9ff", fontname="Arial"];');
    sl.Add('');

    // Nodos = todos los usuarios
    U := UsuariosHead;
    while U <> nil do
    begin
      nodeId := 'N_' + _IdSafe(U^.Email);
      sl.Add(Format('  %s [label="%s"];', [nodeId, _Esc(U^.Email)]));
      U := U^.Next;
    end;

    // Aristas = relaciones emisor -> receptor con conteo
    if RelHead = nil then
      sl.Add('  // (sin relaciones registradas)')
    else
    begin
      R := RelHead;
      while R <> nil do
      begin
        fromId := 'N_' + _IdSafe(R^.FromEmail);
        toId   := 'N_' + _IdSafe(R^.ToEmail);
        pen := 1.0 + 0.25 * R^.Count; // grosor relativo
        penStr := FloatToStrF(pen, ffFixed, 8, 2);
        sl.Add(Format('  %s -> %s [label="%d", penwidth="%s"];',
          [fromId, toId, R^.Count, penStr]));
        R := R^.Next;
      end;
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
    Result := True;
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
    P.Parameters.Clear;
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');
    P.Parameters.Add(PngPath);
    P.Options := [poWaitOnExit];
    try
      P.Execute;
      Result := True;
    except
      Result := False;
    end;
  finally
    P.Free;
  end;
end;

{====================== INITIALIZATION ======================}
initialization
  // Inicializa todo y ASEGURA root
  UsuariosHead := nil; CurrentUser := nil;
  RelHead := nil;
  ComunidadesHead := nil;
  NextId := 1; NextMailId := 1; NextComunidadId := 1;

  AsegurarRoot; // garantiza root@edd.com / root123

end.

