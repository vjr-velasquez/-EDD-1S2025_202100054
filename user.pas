unit user;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils, logeo;

type
  { TForm3 }
  TForm3 = class(TForm)
    Button1: TButton; // Inbox
    Button2: TButton; // Enviar
    Button3: TButton; // Papelera
    Button4: TButton; // Programar
    Button5: TButton; // Programados
    Button6: TButton; // Agregar Contacto
    Button7: TButton; // Listar Contactos
    Button8: TButton; // Perfil
    Button9: TButton; // Reportes
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    // Helpers de estructuras
    procedure AppendInbox(Dest: PUsuario; const ARemitente, AAsunto, AMensaje: string;
                          const AFecha: TDateTime; AProgramado: Boolean; const AEstado: string);
    function InboxToText(U: PUsuario): string;
    function QueueToText(U: PUsuario): string;
    function ContactsToText(U: PUsuario): string;
    function DeleteFromInboxById(U: PUsuario; AId: Integer; out MailCopy: TMail): Boolean;
    procedure PushTrash(U: PUsuario; const M: TMail);
    function PopTrash(U: PUsuario; out M: TMail): Boolean;
    procedure EnqueueProgramado(U: PUsuario; const DestEmail, Asunto, Mensaje: string; const FechaProg: TDateTime);
    function ProcessProgramadosDue(U: PUsuario; const Ref: TDateTime): Integer;
    function AddContact(U: PUsuario; const CEmail, CNombre: string): Boolean;
    procedure IncRel(const FromEmail, ToEmail: string);
  public
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{======================== Helpers de estructura ========================}

procedure TForm3.AppendInbox(Dest: PUsuario; const ARemitente, AAsunto, AMensaje: string;
                             const AFecha: TDateTime; AProgramado: Boolean; const AEstado: string);
var
  N: PMail;
begin
  New(N);
  N^.Id := NextMailId; Inc(NextMailId);
  N^.Remitente := ARemitente;
  N^.Asunto := AAsunto;
  N^.Mensaje := AMensaje;
  N^.Fecha := AFecha;
  N^.Programado := AProgramado;
  N^.Estado := AEstado;
  N^.Prev := Dest^.InboxTail;
  N^.Next := nil;
  if Dest^.InboxTail <> nil then
    Dest^.InboxTail^.Next := N
  else
    Dest^.InboxHead := N;
  Dest^.InboxTail := N;
end;

function TForm3.InboxToText(U: PUsuario): string;
var
  C: PMail;
begin
  Result := '';
  C := U^.InboxHead;
  while C <> nil do
  begin
    Result += Format('Id=%d | %s -> %s | %s | %s'#10,
      [C^.Id, C^.Remitente, U^.Email, DateTimeToStr(C^.Fecha), C^.Asunto]);
    C := C^.Next;
  end;
  if Result = '' then
    Result := '(Bandeja vacía)';
end;

function TForm3.QueueToText(U: PUsuario): string;
var
  C: PProg;
begin
  Result := '';
  C := U^.ProgHead;
  while C <> nil do
  begin
    Result += Format('Id=%d | %s -> %s | Prog=%s | %s'#10,
      [C^.Id, C^.Remitente, C^.Destinatario, DateTimeToStr(C^.FechaProg), C^.Asunto]);
    C := C^.Next;
  end;
  if Result = '' then
    Result := '(Sin correos programados)';
end;

function TForm3.ContactsToText(U: PUsuario): string;
var
  T, H, C: PContacto;
begin
  Result := '';
  T := U^.ContactTail;
  if T = nil then Exit('(Sin contactos)');
  H := T^.Next;
  C := H;
  repeat
    Result += Format('%s (%s)'#10, [C^.Nombre, C^.Email]);
    C := C^.Next;
  until C = H;
end;

function TForm3.DeleteFromInboxById(U: PUsuario; AId: Integer; out MailCopy: TMail): Boolean;
var
  C: PMail;
begin
  Result := False;
  FillChar(MailCopy, SizeOf(MailCopy), 0);
  C := U^.InboxHead;
  while C <> nil do
  begin
    if C^.Id = AId then
    begin
      if C^.Prev <> nil then C^.Prev^.Next := C^.Next else U^.InboxHead := C^.Next;
      if C^.Next <> nil then C^.Next^.Prev := C^.Prev else U^.InboxTail := C^.Prev;
      MailCopy := C^;
      Dispose(C);
      Exit(True);
    end;
    C := C^.Next;
  end;
end;

procedure TForm3.PushTrash(U: PUsuario; const M: TMail);
var
  N: PTrash;
begin
  New(N);
  N^.Mail := M;
  N^.Next := U^.TrashTop;
  U^.TrashTop := N;
end;

function TForm3.PopTrash(U: PUsuario; out M: TMail): Boolean;
var
  N: PTrash;
begin
  Result := False;
  if U^.TrashTop = nil then Exit;
  N := U^.TrashTop;
  U^.TrashTop := N^.Next;
  M := N^.Mail;
  Dispose(N);
  Result := True;
end;

procedure TForm3.EnqueueProgramado(U: PUsuario; const DestEmail, Asunto, Mensaje: string; const FechaProg: TDateTime);
var
  N: PProg;
begin
  New(N);
  N^.Id := NextMailId; Inc(NextMailId);
  N^.Remitente := U^.Email;
  N^.Destinatario := DestEmail;
  N^.Asunto := Asunto;
  N^.Mensaje := Mensaje;
  N^.FechaProg := FechaProg;
  N^.Next := nil;
  if U^.ProgTail <> nil then
    U^.ProgTail^.Next := N
  else
    U^.ProgHead := N;
  U^.ProgTail := N;
end;

function TForm3.ProcessProgramadosDue(U: PUsuario; const Ref: TDateTime): Integer;
var
  C: PProg;
  Dest: PUsuario;
begin
  Result := 0;
  while (U^.ProgHead <> nil) and (U^.ProgHead^.FechaProg <= Ref) do
  begin
    C := U^.ProgHead;
    U^.ProgHead := C^.Next;
    if U^.ProgHead = nil then U^.ProgTail := nil;

    Dest := BuscarUsuarioPorEmail(C^.Destinatario);
    if Dest <> nil then
    begin
      AppendInbox(Dest, C^.Remitente, C^.Asunto, C^.Mensaje, C^.FechaProg, True, 'nuevo');
      IncRel(C^.Remitente, C^.Destinatario);
      Inc(Result);
    end;

    Dispose(C);
  end;
end;

function TForm3.AddContact(U: PUsuario; const CEmail, CNombre: string): Boolean;
var
  T, H, C, N: PContacto;
begin
  Result := False;
  if Trim(CEmail) = '' then Exit;

  T := U^.ContactTail;
  if T <> nil then
  begin
    H := T^.Next;
    C := H;
    repeat
      if SameText(C^.Email, CEmail) then Exit(False);
      C := C^.Next;
    until C = H;
  end;

  New(N);
  N^.Email := CEmail;
  N^.Nombre := CNombre;
  if U^.ContactTail = nil then
  begin
    N^.Next := N;       // primer nodo
    U^.ContactTail := N;
  end
  else
  begin
    N^.Next := U^.ContactTail^.Next;
    U^.ContactTail^.Next := N;
    U^.ContactTail := N;
  end;
  Result := True;
end;

procedure TForm3.IncRel(const FromEmail, ToEmail: string);
var
  C: PRel;
begin
  C := RelHead;
  while C <> nil do
  begin
    if SameText(C^.FromEmail, FromEmail) and SameText(C^.ToEmail, ToEmail) then
    begin
      Inc(C^.Count);
      Exit;
    end;
    C := C^.Next;
  end;
  New(C);
  C^.FromEmail := FromEmail;
  C^.ToEmail := ToEmail;
  C^.Count := 1;
  C^.Next := RelHead;
  RelHead := C;
end;

{======================== Botones del Form ========================}

// Button1: Bandeja
procedure TForm3.Button1Click(Sender: TObject);
begin
  if (CurrentUser = nil) then Exit;
  ShowMessage(InboxToText(CurrentUser));
end;

// Button2: Enviar
procedure TForm3.Button2Click(Sender: TObject);
var
  para, asunto, mensaje: string;
  Dest: PUsuario;
begin
  if (CurrentUser = nil) then Exit;
  if not InputQuery('Enviar', 'Para (email):', para) then Exit;
  para := Trim(para);
  Dest := BuscarUsuarioPorEmail(para);
  if Dest = nil then
  begin
    ShowMessage('El destinatario no existe.');
    Exit;
  end;
  if not InputQuery('Enviar', 'Asunto:', asunto) then Exit;
  if not InputQuery('Enviar', 'Mensaje:', mensaje) then Exit;

  AppendInbox(Dest, CurrentUser^.Email, asunto, mensaje, Now, False, 'nuevo');
  IncRel(CurrentUser^.Email, Dest^.Email);
  ShowMessage('Correo enviado.');
end;

// Button3: Papelera
procedure TForm3.Button3Click(Sender: TObject);
var
  s, sid: string;
  id: Integer;
  M: TMail;
begin
  if (CurrentUser = nil) then Exit;

  if not InputQuery('Papelera', '1) Eliminar de inbox por Id'+LineEnding+
                               '2) Restaurar último de Papelera'+LineEnding+
                               'Elige 1 o 2:', s) then Exit;

  if s = '1' then
  begin
    if not InputQuery('Eliminar', 'Id del correo en Inbox:', sid) then Exit;
    if not TryStrToInt(Trim(sid), id) then
    begin
      ShowMessage('Id inválido.');
      Exit;
    end;
    if DeleteFromInboxById(CurrentUser, id, M) then
    begin
      PushTrash(CurrentUser, M);
      ShowMessage('Movido a Papelera.');
    end
    else
      ShowMessage('No se encontró el Id en la bandeja.');
  end
  else if s = '2' then
  begin
    if PopTrash(CurrentUser, M) then
    begin
      AppendInbox(CurrentUser, M.Remitente, M.Asunto, M.Mensaje, Now, False, 'nuevo');
      ShowMessage('Correo restaurado.');
    end
    else
      ShowMessage('Papelera vacía.');
  end
  else
    ShowMessage('Opción inválida.');
end;

// Button4: Programar
procedure TForm3.Button4Click(Sender: TObject);
var
  para, asunto, mensaje, sfecha: string;
  dt: TDateTime;
  Dest: PUsuario;
begin
  if (CurrentUser = nil) then Exit;

  if not InputQuery('Programar', 'Para (email):', para) then Exit;
  para := Trim(para);
  Dest := BuscarUsuarioPorEmail(para);
  if Dest = nil then
  begin
    ShowMessage('El destinatario no existe.');
    Exit;
  end;
  if not InputQuery('Programar', 'Asunto:', asunto) then Exit;
  if not InputQuery('Programar', 'Mensaje:', mensaje) then Exit;
  if not InputQuery('Programar', 'Fecha y hora (ej: 2025-08-31 14:30):', sfecha) then Exit;
  sfecha := Trim(sfecha);
  if not TryStrToDateTime(sfecha, dt) then
  begin
    ShowMessage('Fecha/hora inválida.');
    Exit;
  end;

  EnqueueProgramado(CurrentUser, Dest^.Email, asunto, mensaje, dt);
  ShowMessage('Correo programado.');
end;

// Button5: Programados
procedure TForm3.Button5Click(Sender: TObject);
var
  procesados: Integer;
begin
  if (CurrentUser = nil) then Exit;
  procesados := ProcessProgramadosDue(CurrentUser, Now);
  ShowMessage('Procesados (vencidos): ' + IntToStr(procesados) + LineEnding +
              'Pendientes:' + LineEnding + QueueToText(CurrentUser));
end;

// Button6: Agregar contacto
procedure TForm3.Button6Click(Sender: TObject);
var
  em, nom: string;
begin
  if (CurrentUser = nil) then Exit;
  if not InputQuery('Agregar Contacto', 'Email:', em) then Exit;
  if not InputQuery('Agregar Contacto', 'Nombre:', nom) then Exit;
  if AddContact(CurrentUser, Trim(em), Trim(nom)) then
    ShowMessage('Contacto agregado.')
  else
    ShowMessage('Ya existía o email inválido.');
end;

// Button7: Listar contactos
procedure TForm3.Button7Click(Sender: TObject);
begin
  if (CurrentUser = nil) then Exit;
  ShowMessage(ContactsToText(CurrentUser));
end;

// Button8: Perfil
procedure TForm3.Button8Click(Sender: TObject);
var
  s: string;
begin
  if (CurrentUser = nil) then Exit;

  s := CurrentUser^.Nombre;
  if InputQuery('Perfil', 'Nombre:', s) then CurrentUser^.Nombre := s;

  s := CurrentUser^.Usuario;
  if InputQuery('Perfil', 'Usuario:', s) then CurrentUser^.Usuario := s;

  s := CurrentUser^.Telefono;
  if InputQuery('Perfil', 'Teléfono:', s) then CurrentUser^.Telefono := s;

  s := CurrentUser^.Password;
  if InputQuery('Perfil', 'Nueva contraseña:', s) then CurrentUser^.Password := s;

  ShowMessage('Perfil actualizado.');
end;

// Button9: Reportes
procedure TForm3.Button9Click(Sender: TObject);
var
  dir: string;
  sl: TStringList;
  C: PMail;
  CT, H: PContacto;
  R: PRel;
begin
  if (CurrentUser = nil) then Exit;

  dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if (not DirectoryExists(dir)) and (not CreateDir(dir)) then
  begin
    ShowMessage('No pude crear la carpeta de reportes: ' + dir);
    Exit;
  end;

  // Inbox del usuario
  sl := TStringList.Create;
  try
    C := CurrentUser^.InboxHead;
    while C <> nil do
    begin
      sl.Add(Format('%d,%s,%s,%s,%s',
        [C^.Id, DateTimeToStr(C^.Fecha), C^.Remitente, C^.Asunto, C^.Mensaje]));
      C := C^.Next;
    end;
    if sl.Count = 0 then sl.Add('(bandeja vacía)');
    sl.SaveToFile(dir + PathDelim + 'inbox_' + StringReplace(CurrentUser^.Email,'@','_',[rfReplaceAll]) + '.txt');
  finally
    sl.Free;
  end;

  // Contactos del usuario
  sl := TStringList.Create;
  try
    if CurrentUser^.ContactTail <> nil then
    begin
      H := CurrentUser^.ContactTail^.Next;
      CT := H;
      repeat
        sl.Add(CT^.Nombre + ',' + CT^.Email);
        CT := CT^.Next;
      until CT = H;
    end
    else
      sl.Add('(sin contactos)');
    sl.SaveToFile(dir + PathDelim + 'contactos_' + StringReplace(CurrentUser^.Email,'@','_',[rfReplaceAll]) + '.txt');
  finally
    sl.Free;
  end;

  // Relaciones globales (CSV)
  sl := TStringList.Create;
  try
    sl.Add('from,to,count');
    R := RelHead;
    while R <> nil do
    begin
      sl.Add(Format('%s,%s,%d', [R^.FromEmail, R^.ToEmail, R^.Count]));
      R := R^.Next;
    end;
    sl.SaveToFile(dir + PathDelim + 'relaciones.csv');
  finally
    sl.Free;
  end;

  ShowMessage('Reportes generados en: ' + dir);
end;

end.

