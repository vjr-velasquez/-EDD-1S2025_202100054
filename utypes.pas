unit uTypes;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  PMail = ^TMail;
  TMail = record
    Id         : Integer;
    Remitente  : string;
    Asunto     : string;
    Mensaje    : string;
    Fecha      : TDateTime;
    Programado : Boolean;
    Estado     : string;
    Prev       : PMail;   // doblemente enlazado
    Next       : PMail;
  end;

  PTrash = ^TTrash;
  TTrash = record
    Mail : TMail;   // se guarda copia del correo eliminado
    Next : PTrash;  // pila (LIFO)
  end;

  PContacto = ^TContacto;
  TContacto = record
    Email : string;
    Nombre: string;
    Next  : PContacto; // lista circular (guardamos el tail)
  end;

  PProg = ^TProg;
  TProg = record
    Id           : Integer;
    Remitente    : string;
    Destinatario : string;
    Asunto       : string;
    Mensaje      : string;
    FechaProg    : TDateTime; // fecha programada (puede ser solo fecha)
    Next         : PProg;     // cola (FIFO)
  end;

  PRel = ^TRel;
  TRel = record
    FromEmail : string;
    ToEmail   : string;
    Count     : Integer;
    Next      : PRel;
  end;

  PUsuario = ^TUsuario;
  TUsuario = record
    // Perfil
    Email    : string;
    Nombre   : string;
    Usuario  : string;
    Telefono : string;
    Password : string;

    // Estructuras
    InboxHead  : PMail;
    InboxTail  : PMail;     // lista doble
    TrashTop   : PTrash;    // pila
    ContactTail: PContacto; // lista circular (tail)
    ProgHead   : PProg;     // cola
    ProgTail   : PProg;

    // Enlace a siguiente usuario (si llevas lista de usuarios)
    Next       : PUsuario;
  end;

var
  // Globales
  CurrentUser  : PUsuario = nil;
  UsuariosHead : PUsuario = nil;
  RelHead      : PRel     = nil;
  NextMailId   : Integer  = 1;

function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;

// Exponemos estas utilidades aquí para evitar forward-decls en otras units
procedure User_AppendInbox(Dest: PUsuario; const Rem, Asunto, Mensaje: string;
  Fecha: TDateTime; Programado: Boolean; const Estado: string);
procedure User_PushTrash(U: PUsuario; const Mail: TMail);
procedure User_IncRel(const FromEmail, ToEmail: string);

implementation

function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
var
  U: PUsuario;
begin
  Result := nil;
  U := UsuariosHead;
  while U <> nil do
  begin
    if SameText(U^.Email, AEmail) then
      Exit(U);
    U := U^.Next;
  end;
end;

procedure User_AppendInbox(Dest: PUsuario; const Rem, Asunto, Mensaje: string;
  Fecha: TDateTime; Programado: Boolean; const Estado: string);
var
  M: PMail;
  est: string;
begin
  if Dest = nil then Exit;

  est := LowerCase(Trim(Estado));
  if (est <> 'nuevo') and (est <> 'leido') then est := 'nuevo';

  New(M);
  M^.Id         := NextMailId; Inc(NextMailId);
  M^.Remitente  := Rem;
  M^.Asunto     := Asunto;
  M^.Mensaje    := Mensaje;
  M^.Fecha      := Fecha;
  M^.Programado := Programado;
  M^.Estado     := est;

  // insertar al final de la lista doble
  M^.Prev := Dest^.InboxTail;
  M^.Next := nil;
  if Dest^.InboxTail <> nil then
    Dest^.InboxTail^.Next := M
  else
    Dest^.InboxHead := M;
  Dest^.InboxTail := M;
end;

procedure User_PushTrash(U: PUsuario; const Mail: TMail);
var
  T: PTrash;
begin
  if U = nil then Exit;
  New(T);
  T^.Mail := Mail;         // copia del correo
  T^.Next := U^.TrashTop;  // push en pila
  U^.TrashTop := T;
end;

procedure User_IncRel(const FromEmail, ToEmail: string);
var
  R, Last: PRel;
  f, t: string;
begin
  f := LowerCase(Trim(FromEmail));
  t := LowerCase(Trim(ToEmail));
  R := RelHead; Last := nil;

  while R <> nil do
  begin
    if (LowerCase(R^.FromEmail) = f) and (LowerCase(R^.ToEmail) = t) then
    begin
      Inc(R^.Count);
      Exit;
    end;
    Last := R;
    R := R^.Next;
  end;

  New(R);
  R^.FromEmail := FromEmail;
  R^.ToEmail   := ToEmail;
  R^.Count     := 1;
  R^.Next      := nil;

  if Last = nil then
    RelHead := R
  else
    Last^.Next := R;
end;

end.

