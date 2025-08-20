unit logeo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  // ===== Estructuras por usuario =====
  PMail = ^TMail;       // Lista doble: bandeja de entrada
  TMail = record
    Id: Integer;
    Remitente: string;
    Estado: string;      // 'nuevo' | 'leido' (puedes ampliar)
    Programado: Boolean;
    Asunto: string;
    Fecha: TDateTime;
    Mensaje: string;
    Prev, Next: PMail;
  end;

  PProg = ^TProg;       // Cola: correos programados
  TProg = record
    Id: Integer;
    Remitente: string;
    Destinatario: string;
    Asunto: string;
    Mensaje: string;
    FechaProg: TDateTime;
    Next: PProg;
  end;

  PTrash = ^TTrash;     // Pila: papelera
  TTrash = record
    Mail: TMail;        // se guarda una copia del mail
    Next: PTrash;
  end;

  PContacto = ^TContacto; // Lista circular: contactos
  TContacto = record
    Email: string;
    Nombre: string;
    Next: PContacto;    // tail^.Next apunta al head
  end;

  PRel = ^TRel;         // Matriz dispersa: relaciones (from -> to)
  TRel = record
    FromEmail: string;
    ToEmail: string;
    Count: Integer;
    Next: PRel;
  end;

  // ===== Usuarios (lista simple ya existente) =====
  PUsuario = ^TUsuario;
  TUsuario = record
    Id: Integer;
    Nombre: string;
    Usuario: string;
    Email: string;
    Telefono: string;
    Password: string;
    Next: PUsuario;

    // Estructuras propias del usuario:
    InboxHead, InboxTail: PMail;   // lista doble
    TrashTop: PTrash;              // pila
    ProgHead, ProgTail: PProg;     // cola
    ContactTail: PContacto;        // lista circular (guardamos tail)
  end;

  { TForm1 }
  TForm1 = class(TForm)
    Button1: TButton; // Inicio de Sesión
    Button2: TButton; // Registrar
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

// === Exponer funciones para otros forms (root/user) ===
function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
procedure AgregarUsuario(const ANombre, AUsuario, AEmail, ATelefono, APassword: string);

// === Estado global ===
var
  UsuariosHead: PUsuario = nil;
  NextId: Integer = 1;
  CurrentUser: PUsuario = nil;  // usuario autenticado (no root)
  RelHead: PRel = nil;          // relaciones globales (matriz dispersa)
  NextMailId: Integer = 1;      // id autoincremental para correos

implementation

uses
  root, user; // Form2 (admin) y Form3 (estándar)

{$R *.lfm}

{ ========================== Utilidades de la lista de usuarios ========================== }

function ToLower(const S: string): string;
begin
  Result := LowerCase(S); // sin LazUTF8
end;

function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
var
  Cur: PUsuario;
  E: string;
begin
  Result := nil;
  if AEmail = '' then Exit;
  E := ToLower(AEmail);
  Cur := UsuariosHead;
  while Cur <> nil do
  begin
    if ToLower(Cur^.Email) = E then
      Exit(Cur);
    Cur := Cur^.Next;
  end;
end;

procedure AgregarUsuario(const ANombre, AUsuario, AEmail, ATelefono, APassword: string);
var
  Nuevo: PUsuario;
begin
  New(Nuevo);
  Nuevo^.Id := NextId; Inc(NextId);
  Nuevo^.Nombre := ANombre;
  Nuevo^.Usuario := AUsuario;
  Nuevo^.Email := AEmail;
  Nuevo^.Telefono := ATelefono;
  Nuevo^.Password := APassword;

  // Inicializar estructuras propias
  Nuevo^.InboxHead := nil;
  Nuevo^.InboxTail := nil;
  Nuevo^.TrashTop  := nil;
  Nuevo^.ProgHead  := nil;
  Nuevo^.ProgTail  := nil;
  Nuevo^.ContactTail := nil;

  // Insertar al inicio de la lista simple
  Nuevo^.Next := UsuariosHead;
  UsuariosHead := Nuevo;
end;

procedure AsegurarRoot();
begin
  if BuscarUsuarioPorEmail('root@edd.com') = nil then
    AgregarUsuario('root', 'root', 'root@edd.com', '', 'root123');
end;

procedure FreeUsuarios;
var
  cur, nxt: PUsuario;
begin
  // (Liberación simple; no recorremos sub-estructuras para mantenerlo corto)
  cur := UsuariosHead;
  while cur <> nil do
  begin
    nxt := cur^.Next;
    Dispose(cur);
    cur := nxt;
  end;
  UsuariosHead := nil;
end;

{ ============================== TForm1 UI ============================== }

procedure TForm1.FormCreate(Sender: TObject);
begin
  AsegurarRoot(); // por si acaso
end;

function TForm1.EmailValido(const AEmail: string): Boolean;
var
  pArroba, pPunto: SizeInt;
begin
  pArroba := Pos('@', AEmail);
  pPunto := LastDelimiter('.', AEmail);
  Result := (pArroba > 1) and (pPunto > pArroba + 1) and (pPunto < Length(AEmail));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  U: PUsuario;
  email, password: string;
begin
  // Garantizar root
  AsegurarRoot;

  email := Trim(Edit1.Text);
  password := Trim(Edit2.Text);

  if (email = '') or (password = '') then
  begin
    ShowMessage('Por favor, ingrese su email y contraseña.');
    Exit;
  end;

  U := BuscarUsuarioPorEmail(email);
  if (U <> nil) and (U^.Password = password) then
  begin
    Form1.Hide;
    if ToLower(U^.Email) = 'root@edd.com' then
    begin
      ShowMessage('¡Bienvenido, user Administrador!');
      Form2 := TForm2.Create(Application);  // Form de Root (admin)
      Form2.Show;
    end
    else
    begin
      CurrentUser := U;                      // guardamos quién inició sesión
      ShowMessage('¡Bienvenido, ' + U^.Email + '!');
      Form3 := TForm3.Create(Application);   // Form Estándar (unit "user")
      Form3.Show;
    end;
  end
  else
    ShowMessage('Credenciales incorrectas. Intente de nuevo.');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  email, password: string;
begin
  // Registrar (crear usuario si no existe)
  email := Trim(Edit1.Text);
  password := Trim(Edit2.Text);

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

  if ToLower(email) = 'root@edd.com' then
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
  // Opcional: Edit1.Clear; Edit2.Clear;
end;

initialization
  // Crear root automáticamente al cargar la unidad
  AsegurarRoot;

finalization
  // Liberar memoria (buena práctica)
  FreeUsuarios;

end.

