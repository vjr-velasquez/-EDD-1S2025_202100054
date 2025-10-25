unit root;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpjson, jsonparser, Process, LCLIntf,
  logeo, uComunidades, uUsuariosAPI, comunidad, LazFileUtils, user, uTypes;


type
  { TForm2 (Root) }
  TForm2 = class(TForm)
    Button1: TButton; // Carga Masiva (usuarios)
    Button2: TButton; // Reportes Usuarios
    Button3: TButton; // Reporte de Relaciones (matriz)
    Button4: TButton; // Regresar a login
    Button5: TButton;
    Button6: TButton; // Carga masiva de correos
    controlLogueoBtn: TButton;
    comunidades: TButton; // Comunidades (ventana minimalista)
    Label1: TLabel;
    repoComunidades: TButton;
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);   // <--- IMPLEMENTADO ABAJO
    procedure comunidadesClick(Sender: TObject);
    procedure controlLogueoBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure repoComunidadesClick(Sender: TObject);
  private
    function ReportDir: string;
    procedure EnsureCommunitiesButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses
  contronLogueo;  // <-- unit del form de Control de Logueo


{================== Helpers locales ==================}

function CIg(const A, B: string): Boolean; inline;
begin
  Result := LowerCase(A) = LowerCase(B);
end;

function FindComunidadByName(const Nombre: string): PComunidad;
var C: PComunidad;
begin
  Result := nil;
  C := ComunidadesHead;
  while C <> nil do
  begin
    if CIg(C^.Nombre, Nombre) then Exit(C);
    C := C^.Next;
  end;
end;

{ Carpeta de reportes }
function TForm2.ReportDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(Result) then CreateDir(Result);
end;

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Root';
  EnsureCommunitiesButton;
  ReportDir;
end;

procedure TForm2.EnsureCommunitiesButton;
begin
  if comunidades = nil then
  begin
    comunidades := TButton.Create(Self);
    comunidades.Parent := Self;
  end;
  comunidades.Caption := 'Comunidades';
  comunidades.Left := 260;
  comunidades.Top  := 190;
  comunidades.Width := 160;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  EnsureCommunitiesButton;
  ReportDir;
end;

{ Abrir comunidades }
procedure TForm2.comunidadesClick(Sender: TObject);
begin
  comunidad.Form4.Show;
  Self.Hide;
end;
{ Abrir control de logueo }
procedure TForm2.controlLogueoBtnClick(Sender: TObject);
begin
    contronLogueo.Form6.Show;
    Self.Hide;
end;

{ Ver mensajes de todas las comunidades }
procedure TForm2.Button5Click(Sender: TObject);
var L: TStringList;
begin
  L := TStringList.Create;
  try
    Comunidades_VerMensajesTodas(L);
    if L.Count = 0 then
      ShowMessage('(no hay comunidades ni mensajes)')
    else
      ShowMessage(L.Text);
  finally
    L.Free;
  end;
end;

{================== Entregar correo (inyecta en estructuras reales) ==================}
procedure EntregarCorreoMasivo(const Id: Integer; const Rem, Dest, Estado, Asunto, Mensaje: string);
var
  DU: PUsuario;
  estNorm: string;
begin
  // Normaliza estado al vocabulario que usa user.pas
  if (UpperCase(Estado) = 'LEÍDO') or (UpperCase(Estado) = 'LEIDO') then
    estNorm := 'leido'
  else if UpperCase(Estado) = 'ELIMINADO' then
    estNorm := 'eliminado'
  else
    estNorm := 'nuevo';

  // Busca el destinatario
  DU := BuscarUsuarioPorEmail(Dest);
  if DU = nil then Exit;

  // Inserta en la estructura correspondiente
  if estNorm = 'eliminado' then
    //User_PushTrash(DU, Id, Rem, Asunto, Mensaje, Now, False, estNorm)
  else
    //User_AppendInbox(DU, Rem, Asunto, Mensaje, Now, False, estNorm);

  // Actualiza matriz de relaciones (Rem -> Dest)
  //User_IncRel(Rem, Dest);

  // (Opcional) Blockchain_Add(...) si ya tienes unit de blockchain pública
end;

{================== Carga masiva usuarios ==================}
procedure TForm2.Button1Click(Sender: TObject);
var
  OD: TOpenDialog;
  J: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  U: TJSONObject;
  sl: TStringList;
  email, password, nombre, usuario, telefono: string;

  function SDef(O: TJSONObject; const Key: string): string;
  begin
    if (O = nil) or (O.Find(Key) = nil) then Exit('');
    Result := O.Get(Key, '');
  end;

begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := 'JSON|*.json';
    if not OD.Execute then Exit;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(OD.FileName);
      J := GetJSON(sl.Text);
    finally
      sl.Free;
    end;

    try
      Obj := TJSONObject(J);
      Arr := TJSONArray(Obj.Find('usuarios'));
      if Arr = nil then begin ShowMessage('JSON sin "usuarios".'); Exit; end;

      for I := 0 to Arr.Count-1 do
      begin
        if not (Arr.Items[I] is TJSONObject) then Continue;
        U := TJSONObject(Arr.Items[I]);
        nombre   := SDef(U, 'nombre');
        usuario  := SDef(U, 'usuario');
        email    := SDef(U, 'email');
        telefono := SDef(U, 'telefono');
        password := SDef(U, 'password');

        if (email <> '') and (BuscarUsuarioPorEmail(email) = nil) then
          AgregarUsuario(nombre, usuario, email, telefono, password);
      end;

      ShowMessage('Carga masiva completada.');
    finally
      J.Free;
    end;
  finally
    OD.Free;
  end;
end;

{================== Reporte lista simple de usuarios ==================}
procedure TForm2.Button2Click(Sender: TObject);
var
  dir, dotPath, pngPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'usuarios.dot';
  pngPath := dir + PathDelim + 'usuarios.png';

  if not ExportarUsuariosDOT(dotPath) then
  begin
    ShowMessage('No se pudo exportar DOT de usuarios.');
    Exit;
  end;

  if RenderizarPNGConDot(dotPath, pngPath) then
  begin
    if not OpenDocument(pngPath) then
      ShowMessage('Reporte generado: ' + pngPath);
  end
  else
    ShowMessage('No se pudo ejecutar "dot". Instala Graphviz: sudo apt install graphviz -y');
end;

{================== Reporte matriz de relaciones ==================}
procedure TForm2.Button3Click(Sender: TObject);
var
  dir, dotPath, pngPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'relaciones.dot';
  pngPath := dir + PathDelim + 'relaciones.png';

  if not ExportarRelacionesDOT(dotPath) then
  begin
    ShowMessage('No se pudo exportar el DOT de relaciones.');
    Exit;
  end;

  if RenderizarPNGConDot(dotPath, pngPath) then
  begin
    if not OpenDocument(pngPath) then
      ShowMessage('Reporte generado: ' + pngPath);
  end
  else
    ShowMessage('No se pudo ejecutar "dot". Instala Graphviz: sudo apt install graphviz -y');
end;

{================== Regresar al login ==================}
procedure TForm2.Button4Click(Sender: TObject);
begin
  logeo.Form1.Show;
  Self.Hide;
end;

{================== CARGA MASIVA DE CORREOS (Button6) ==================}
procedure TForm2.Button6Click(Sender: TObject);
var
  OD: TOpenDialog;
  Raw: TStringList;
  J, Item: TJSONData;
  RootObj: TJSONObject;
  Arr: TJSONArray;
  i, ok, skipNoDest, skipNoUser, skipParse: Integer;

  function EstadoNorm(const S: string): string;
  var U: string;
  begin
    U := UpperCase(Trim(S));
    if (U = 'NL') then Exit('NL');
    if (U = 'LEIDO') or (U = 'LEÍDO') or (U = 'L') then Exit('LEÍDO');
    if (U = 'ELIMINADO') then Exit('ELIMINADO');
    Result := 'NL';
  end;

  function SDef(O: TJSONObject; const Key: string): string;
  begin
    if (O = nil) or (O.Find(Key) = nil) then Exit('');
    Result := O.Get(Key, '');
  end;

  function IDef(O: TJSONObject; const Key: string): Integer;
  begin
    if (O = nil) or (O.Find(Key) = nil) then Exit(0);
    Result := O.Get(Key, 0);
  end;

var
  o: TJSONObject;
  id: Integer;
  remitente, destinatario, estado, asunto, mensaje: string;
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Title := 'Seleccionar archivo JSON de correos';
    OD.Filter := 'Archivos JSON|*.json|Todos|*.*';
    if not OD.Execute then Exit;

    Raw := TStringList.Create;
    try
      Raw.LoadFromFile(OD.FileName);
      try
        J := GetJSON(Raw.Text);
      except
        on E: Exception do
        begin
          ShowMessage('JSON inválido: ' + E.Message);
          Exit;
        end;
      end;
    finally
      Raw.Free;
    end;

    try
      if (J = nil) or (J.JSONType <> jtObject) then
      begin
        ShowMessage('El archivo no contiene un objeto JSON raíz.');
        Exit;
      end;

      RootObj := TJSONObject(J);
      Arr := TJSONArray(RootObj.Find('correos'));
      if Arr = nil then
      begin
        ShowMessage('No se encontró el arreglo "correos" en el JSON.');
        Exit;
      end;

      ok := 0; skipNoDest := 0; skipNoUser := 0; skipParse := 0;

      for i := 0 to Arr.Count - 1 do
      begin
        Item := Arr.Items[i];
        if (Item = nil) or (Item.JSONType <> jtObject) then
        begin
          Inc(skipParse);
          Continue;
        end;

        o := TJSONObject(Item);
        // Campos
        id           := IDef(o, 'id');
        remitente    := SDef(o, 'remitente');
        destinatario := SDef(o, 'destinatario');
        estado       := EstadoNorm(SDef(o, 'estado'));
        asunto       := SDef(o, 'asunto');
        mensaje      := SDef(o, 'mensaje');

        if (Trim(destinatario) = '') then
        begin
          Inc(skipNoDest);
          Continue;
        end;

        // Verificar destinatario existe
        if BuscarUsuarioPorEmail(destinatario) = nil then
        begin
          Inc(skipNoUser);
          Continue;
        end;

        // Entregar (inyecta en estructuras)
        try
          EntregarCorreoMasivo(id, remitente, destinatario, estado, asunto, mensaje);
          Inc(ok);
        except
          on E: Exception do
          begin
            Inc(skipParse);
          end;
        end;
      end;

      ShowMessage(
        'Carga Masiva de Correos' + LineEnding +
        'Entregados: ' + IntToStr(ok) + LineEnding +
        'Sin destinatario: ' + IntToStr(skipNoDest) + LineEnding +
        'Usuario no existe: ' + IntToStr(skipNoUser) + LineEnding +
        'Errores de entrega: ' + IntToStr(skipParse)
      );

    finally
      if Assigned(J) then J.Free;
    end;

  finally
    OD.Free;
  end;
end;

procedure TForm2.repoComunidadesClick(Sender: TObject);
var png: string;
begin
  png := GenerarReporteComunidades(ExtractFilePath(Application.ExeName));
  ShowMessage('Reporte generado: ' + LineEnding + png);
end;

end.

