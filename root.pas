unit root;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpjson, jsonparser, Process, LCLIntf,
  logeo,uComunidades,uUsuariosAPI,comunidad; // estructuras y APIs exportadas desde logeo.pas

type
  { TForm2 (Root) }
  TForm2 = class(TForm)
    Button1: TButton; // Carga Masiva
    Button2: TButton; // Reportes Usuarios
    Button3: TButton; // Reporte de Relaciones (matriz)
    Button4: TButton; // Regresar a login
    comunidades: TButton; // Comunidades (ventana minimalista)
    Button6: TButton; // Inbox por usuario (reporte)
    Label1: TLabel;
    procedure comunidadesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure repoComunidadesClick(Sender: TObject);
  private
    function ReportDir: string;
    procedure EnsureCommunitiesButton;
    procedure EnsureInboxButton;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

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
  EnsureInboxButton;
  ReportDir;
end;

procedure TForm2.EnsureCommunitiesButton;
begin
  // Si el .lfm no trae el comunidades, lo creamos (y lo movemos un poco a la derecha)
  if comunidades = nil then
  begin
    comunidades := TButton.Create(Self);
    comunidades.Parent := Self;
  end;
  comunidades.Caption := 'Comunidades';
  comunidades.Left := 260; // más a la derecha
  comunidades.Top  := 190;
  comunidades.Width := 160;
end;

procedure TForm2.EnsureInboxButton;
begin
  if Button6 = nil then
  begin
    Button6 := TButton.Create(Self);
    Button6.Parent := Self;
  end;
  Button6.Caption := 'Inbox por usuario';
  Button6.Left := 260;
  Button6.Top  := 230;
  Button6.Width := 160;
  Button6.OnClick := @Button6Click;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  EnsureCommunitiesButton;
  EnsureInboxButton;
  ReportDir;
end;
//boton para abrir las comunidades
procedure TForm2.comunidadesClick(Sender: TObject);
begin
  comunidad.Form4.Show;
  Self.Hide;

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
{================== Reporte inbox por usuario ==================}
procedure TForm2.Button6Click(Sender: TObject);

  function Slug(const S: string): string;
  var i: Integer; r: string;
  begin
    r := '';
    for i := 1 to Length(S) do
      if S[i] in ['A'..'Z','a'..'z','0'..'9','_'] then r += S[i] else r += '_';
    Result := r;
  end;

var
  em, dir, dotPath, pngPath: string;
begin
  em := '';
  if not InputQuery('Inbox', 'Email del usuario:', em) then Exit;
  em := Trim(em);
  if em = '' then Exit;

  dir := ReportDir;
  dotPath := dir + PathDelim + 'inbox_' + Slug(em) + '.dot';
  pngPath := dir + PathDelim + 'inbox_' + Slug(em) + '.png';

  if not ExportarInboxDOT(em, dotPath) then
  begin
    ShowMessage('No se pudo exportar el DOT (usuario inexistente o error).');
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

procedure TForm2.repoComunidadesClick(Sender: TObject);
begin
  // Genera DOT y PNG en la carpeta "Root-Reportes" junto al exe
  Comunidades_ReportePNG(ExtractFilePath(ParamStr(0)) + 'Root-Reportes');
  ShowMessage('Reporte de comunidades generado en Root-Reportes/.');
end;

end.

