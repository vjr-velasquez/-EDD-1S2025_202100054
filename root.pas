unit root;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpjson, jsonparser, Process, ComCtrls, LCLIntf,
  logeo; // usamos las APIs y reportes que están en logeo.pas

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton; // Carga Masiva (usuarios.json)
    Button2: TButton; // Reportes Usuarios (lista simple)
    Button3: TButton; // Reporte de Relaciones (matriz dispersa)
    Button4: TButton; // Regresar a Login
    Button5: TButton; // Comunidades (gestor)
    Button6: TButton; // Inbox por usuario (lista doble)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject); // Carga masiva
    procedure Button2Click(Sender: TObject); // Reporte usuarios
    procedure Button3Click(Sender: TObject); // Reporte relaciones
    procedure Button4Click(Sender: TObject); // Volver a login
    procedure Button5Click(Sender: TObject); // Comunidades
    procedure Button6Click(Sender: TObject); // Inbox por usuario
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

{================ VENTANA DE COMUNIDADES (solo root) =================}

type
  TCommunitiesWin = class(TForm)
  private
    lblC, lblM: TLabel;
    lvComs, lvMembers: TListView;
    edtNomCom, edtEmail: TEdit;
    btnAddCom, btnDelCom, btnAddM, btnDelM,
    btnListar, btnExportDot, btnRender,
    btnExportMatriz, btnRenderMatriz, btnCerrar: TButton;

    procedure LoadCommunities;
    procedure LoadMembers(Sender: TObject; Item: TListItem; Selected: Boolean);
    function SelectedCommunity: PComunidad;

    function PickCommunity(out C: PComunidad): Boolean;
    procedure AddMembersWizard(C: PComunidad);

    procedure AddCommunity(Sender: TObject);
    procedure DelCommunity(Sender: TObject);
    procedure AddMember(Sender: TObject);
    procedure DelMember(Sender: TObject);
    procedure ListarAll(Sender: TObject);
    procedure ExportDot(Sender: TObject);
    procedure RenderPng(Sender: TObject);
    procedure ExportMatriz(Sender: TObject);
    procedure RenderMatriz(Sender: TObject);
    function ReportDir: string;
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

constructor TCommunitiesWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Comunidades (root)';
  Position := poScreenCenter; Width := 900; Height := 640;

  lblC := TLabel.Create(Self); lblC.Parent := Self; lblC.Caption := 'Comunidades';
  lblC.Left := 16; lblC.Top := 12;

  lvComs := TListView.Create(Self);
  lvComs.Parent := Self; lvComs.Left := 16; lvComs.Top := 32; lvComs.Width := 360; lvComs.Height := 500;
  lvComs.ViewStyle := vsReport; lvComs.ReadOnly := True; lvComs.RowSelect := True; lvComs.GridLines := True;
  with lvComs.Columns.Add do begin Caption := 'Id'; Width := 60; end;
  with lvComs.Columns.Add do begin Caption := 'Nombre'; Width := 260; end;
  lvComs.OnSelectItem := @LoadMembers;

  lblM := TLabel.Create(Self); lblM.Parent := Self; lblM.Caption := 'Miembros';
  lblM.Left := 390; lblM.Top := 12;

  lvMembers := TListView.Create(Self);
  lvMembers.Parent := Self; lvMembers.Left := 390; lvMembers.Top := 32; lvMembers.Width := 490; lvMembers.Height := 500;
  lvMembers.ViewStyle := vsReport; lvMembers.ReadOnly := True; lvMembers.RowSelect := True; lvMembers.GridLines := True;
  with lvMembers.Columns.Add do begin Caption := 'Email'; Width := 450; end;

  // Crear / eliminar comunidad
  edtNomCom := TEdit.Create(Self); edtNomCom.Parent := Self;
  edtNomCom.Left := 16; edtNomCom.Top := 540; edtNomCom.Width := 260; edtNomCom.TextHint := 'Nombre de comunidad';

  btnAddCom := TButton.Create(Self); btnAddCom.Parent := Self;
  btnAddCom.Left := 284; btnAddCom.Top := 538; btnAddCom.Width := 92; btnAddCom.Caption := 'Crear';
  btnAddCom.OnClick := @AddCommunity;

  btnDelCom := TButton.Create(Self); btnDelCom.Parent := Self;
  btnDelCom.Left := 16; btnDelCom.Top := 572; btnDelCom.Width := 100; btnDelCom.Caption := 'Eliminar';
  btnDelCom.OnClick := @DelCommunity;

  // Agregar / eliminar miembro
  edtEmail := TEdit.Create(Self); edtEmail.Parent := Self;
  edtEmail.Left := 390; edtEmail.Top := 540; edtEmail.Width := 320; edtEmail.TextHint := 'email@edd.com';

  btnAddM := TButton.Create(Self); btnAddM.Parent := Self;
  btnAddM.Left := 714; btnAddM.Top := 538; btnAddM.Width := 80; btnAddM.Caption := 'Agregar';
  btnAddM.OnClick := @AddMember;

  btnDelM := TButton.Create(Self); btnDelM.Parent := Self;
  btnDelM.Left := 800; btnDelM.Top := 538; btnDelM.Width := 80; btnDelM.Caption := 'Eliminar';
  btnDelM.OnClick := @DelMember;

  // Reportes
  btnListar := TButton.Create(Self); btnListar.Parent := Self;
  btnListar.Left := 390; btnListar.Top := 572; btnListar.Width := 110; btnListar.Caption := 'Listar todo';
  btnListar.OnClick := @ListarAll;

  btnExportDot := TButton.Create(Self); btnExportDot.Parent := Self;
  btnExportDot.Left := 506; btnExportDot.Top := 572; btnExportDot.Width := 120; btnExportDot.Caption := 'Exportar DOT';
  btnExportDot.OnClick := @ExportDot;

  btnRender := TButton.Create(Self); btnRender.Parent := Self;
  btnRender.Left := 630; btnRender.Top := 572; btnRender.Width := 110; btnRender.Caption := 'Render PNG';
  btnRender.OnClick := @RenderPng;

  btnExportMatriz := TButton.Create(Self); btnExportMatriz.Parent := Self;
  btnExportMatriz.Left := 744; btnExportMatriz.Top := 572; btnExportMatriz.Width := 136; btnExportMatriz.Caption := 'Matriz (DOT)';
  btnExportMatriz.OnClick := @ExportMatriz;

  btnRenderMatriz := TButton.Create(Self); btnRenderMatriz.Parent := Self;
  btnRenderMatriz.Left := 744; btnRenderMatriz.Top := 604; btnRenderMatriz.Width := 136; btnRenderMatriz.Caption := 'Matriz (PNG)';
  btnRenderMatriz.OnClick := @RenderMatriz;

  btnCerrar := TButton.Create(Self); btnCerrar.Parent := Self;
  btnCerrar.Left := 16; btnCerrar.Top := 604; btnCerrar.Width := 100; btnCerrar.Caption := 'Cerrar';
  btnCerrar.ModalResult := mrClose;

  LoadCommunities;
end;

function TCommunitiesWin.ReportDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(Result) then CreateDir(Result);
end;

procedure TCommunitiesWin.LoadCommunities;
var
  C: PComunidad; it: TListItem;
begin
  lvComs.Items.BeginUpdate;
  try
    lvComs.Items.Clear;
    C := ComunidadesHead;
    while C <> nil do
    begin
      it := lvComs.Items.Add;
      it.Caption := IntToStr(C^.Id);
      it.SubItems.Add(C^.Nombre);
      it.Data := C;
      C := C^.Next;
    end;
  finally
    lvComs.Items.EndUpdate;
  end;
  LoadMembers(nil, nil, False);
end;

procedure TCommunitiesWin.LoadMembers(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  C: PComunidad; M: PMember; it: TListItem;
begin
  lvMembers.Items.BeginUpdate;
  try
    lvMembers.Items.Clear;
    C := SelectedCommunity;
    if C = nil then Exit;
    M := C^.Miembros;
    while M <> nil do
    begin
      it := lvMembers.Items.Add;
      it.Caption := M^.Email;
      it.Data := M;
      M := M^.Next;
    end;
  finally
    lvMembers.Items.EndUpdate;
  end;
end;

function TCommunitiesWin.SelectedCommunity: PComunidad;
begin
  Result := nil;
  if (lvComs.Selected <> nil) and (lvComs.Selected.Data <> nil) then
    Result := PComunidad(lvComs.Selected.Data);
end;

function TCommunitiesWin.PickCommunity(out C: PComunidad): Boolean;
var
  F: TForm; LB: TListBox; BOk, BCancel: TButton; Cur: PComunidad;
begin
  Result := False; C := nil;
  F := TForm.CreateNew(Self, 1);
  try
    F.Caption := 'Elegir comunidad';
    F.Position := poScreenCenter; F.Width := 360; F.Height := 320;

    LB := TListBox.Create(F); LB.Parent := F; LB.Align := alTop; LB.Height := 240;
    Cur := ComunidadesHead;
    while Cur <> nil do begin LB.Items.Add(Cur^.Nombre); Cur := Cur^.Next; end;

    BOk := TButton.Create(F); BOk.Parent := F; BOk.Caption := 'OK'; BOk.Left := 80; BOk.Top := 250; BOk.ModalResult := mrOk;
    BCancel := TButton.Create(F); BCancel.Parent := F; BCancel.Caption := 'Cancelar'; BCancel.Left := 180; BCancel.Top := 250; BCancel.ModalResult := mrCancel;

    if F.ShowModal = mrOk then
    begin
      if (LB.ItemIndex >= 0) then
      begin
        Cur := ComunidadesHead;
        while Cur <> nil do
        begin
          if SameText(Cur^.Nombre, LB.Items[LB.ItemIndex]) then begin C := Cur; Break; end;
          Cur := Cur^.Next;
        end;
        Result := (C <> nil);
      end;
    end;
  finally
    F.Free;
  end;
end;

procedure TCommunitiesWin.AddMembersWizard(C: PComunidad);
var em: string;
begin
  if C = nil then Exit;
  if MessageDlg('¿Agregar miembros ahora?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    repeat
      em := '';
      if not InputQuery('Agregar miembro', 'Email (vacío para terminar):', em) then Break;
      em := Trim(em);
      if em = '' then Break;
      if AgregarMiembroComunidad(C, em) then
        ShowMessage('Agregado: ' + em)
      else
        ShowMessage('No se pudo agregar (no existe o ya está): ' + em);
    until False;
    LoadMembers(nil, nil, False);
  end;
end;

procedure TCommunitiesWin.AddCommunity(Sender: TObject);
var C: PComunidad;
begin
  if Trim(edtNomCom.Text) = '' then begin ShowMessage('Ingrese nombre.'); Exit; end;
  C := CrearComunidad(edtNomCom.Text);
  if C = nil then
    ShowMessage('No se pudo crear (vacío o duplicado).')
  else
  begin
    ShowMessage('Comunidad "'+edtNomCom.Text+'" creada.');
    edtNomCom.Clear;
    LoadCommunities;
    AddMembersWizard(C);
  end;
end;

procedure TCommunitiesWin.DelCommunity(Sender: TObject);
var C: PComunidad;
begin
  C := SelectedCommunity;
  if (C = nil) and (not PickCommunity(C)) then begin ShowMessage('Seleccione una comunidad.'); Exit; end;
  if EliminarComunidad(C^.Nombre) then
  begin
    ShowMessage('Comunidad eliminada.');
    LoadCommunities;
  end
  else
    ShowMessage('No se pudo eliminar.');
end;

procedure TCommunitiesWin.AddMember(Sender: TObject);
var
  C: PComunidad; em: string;
begin
  C := SelectedCommunity;
  if C = nil then
    if not PickCommunity(C) then begin ShowMessage('Seleccione una comunidad.'); Exit; end;

  em := Trim(edtEmail.Text);
  if em = '' then
    if not InputQuery('Agregar miembro', 'Email:', em) then Exit;

  em := Trim(em);
  if em = '' then Exit;

  if AgregarMiembroComunidad(C, em) then
  begin
    ShowMessage('Miembro agregado a "' + C^.Nombre + '": ' + em);
    edtEmail.Clear; LoadMembers(nil, nil, False);
  end
  else
    ShowMessage('No se pudo agregar (usuario inexistente o duplicado).');
end;

procedure TCommunitiesWin.DelMember(Sender: TObject);
var
  C: PComunidad; em: string;
begin
  C := SelectedCommunity;
  if (C = nil) and (not PickCommunity(C)) then begin ShowMessage('Seleccione una comunidad.'); Exit; end;

  if (lvMembers.Selected <> nil) then
    em := lvMembers.Selected.Caption
  else begin
    em := '';
    if not InputQuery('Eliminar miembro', 'Email a eliminar:', em) then Exit;
  end;

  em := Trim(em);
  if em = '' then Exit;

  if EliminarMiembroComunidad(C, em) then
  begin
    ShowMessage('Miembro eliminado de "' + C^.Nombre + '": ' + em);
    LoadMembers(nil, nil, False);
  end
  else
    ShowMessage('No se pudo eliminar.');
end;

procedure TCommunitiesWin.ListarAll(Sender: TObject);
var
  s: string;
  F: TForm;
  M: TMemo;
begin
  ListarComunidades(s);

  F := TForm.CreateNew(Self, 1);
  try
    F.Caption := 'Listado de Comunidades';
    F.Position := poScreenCenter; F.Width := 600; F.Height := 420;

    M := TMemo.Create(F);
    M.Parent := F;
    M.Align := alClient;
    M.ReadOnly := True;
    M.ScrollBars := ssAutoBoth;
    M.Lines.Text := s;

    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TCommunitiesWin.ExportDot(Sender: TObject);
var
  dir, dotPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'comunidades.dot';
  if ExportarComunidadesDOT(dotPath) then
    ShowMessage('DOT exportado en: ' + dotPath)
  else
    ShowMessage('No se pudo exportar DOT.');
end;

procedure TCommunitiesWin.RenderPng(Sender: TObject);
var
  dir, dotPath, pngPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'comunidades.dot';
  pngPath := dir + PathDelim + 'comunidades.png';
  if (not FileExists(dotPath)) and (not ExportarComunidadesDOT(dotPath)) then
  begin
    ShowMessage('No hay DOT para renderizar.'); Exit;
  end;
  if RenderizarPNGConDot(dotPath, pngPath) then
    if not OpenDocument(pngPath) then ShowMessage('PNG en: ' + pngPath)
  else
    ShowMessage('Error ejecutando dot. Instala Graphviz: sudo apt install graphviz -y');
end;

procedure TCommunitiesWin.ExportMatriz(Sender: TObject);
var dir, dotPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'comunidades_matriz.dot';
  if ExportarComunidadesMatrizDOT(dotPath) then
    ShowMessage('DOT (matriz) exportado en: ' + dotPath)
  else
    ShowMessage('No se pudo exportar DOT de matriz.');
end;

procedure TCommunitiesWin.RenderMatriz(Sender: TObject);
var dir, dotPath, pngPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'comunidades_matriz.dot';
  pngPath := dir + PathDelim + 'comunidades_matriz.png';
  if (not FileExists(dotPath)) and (not ExportarComunidadesMatrizDOT(dotPath)) then
  begin
    ShowMessage('No hay DOT de matriz para renderizar.'); Exit;
  end;
  if RenderizarPNGConDot(dotPath, pngPath) then
    if not OpenDocument(pngPath) then ShowMessage('PNG en: ' + pngPath)
  else
    ShowMessage('Error ejecutando dot. Instala Graphviz: sudo apt install graphviz -y');
end;

{============================ TForm2 ============================}

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
  if Button5 = nil then
  begin
    Button5 := TButton.Create(Self);
    Button5.Parent := Self;
    Button5.Caption := 'Comunidades';
    Button5.Left := 260;  // un poco más a la derecha
    Button5.Top  := 190;
    Button5.Width := 160;
  end;
  Button5.OnClick := @Button5Click;
end;

procedure TForm2.EnsureInboxButton;
begin
  if Button6 = nil then
  begin
    Button6 := TButton.Create(Self);
    Button6.Parent := Self;
    Button6.Caption := 'Inbox por usuario';
    Button6.Left := 260;
    Button6.Top  := 230;
    Button6.Width := 160;
  end;
  Button6.OnClick := @Button6Click;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  EnsureCommunitiesButton;
  EnsureInboxButton;
  ReportDir;
end;

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
  // CARGA MASIVA de usuarios
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
    ShowMessage('No se pudo ejecutar "dot"');
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  dir, dotPath, pngPath: string;
begin
  // REPORTE DE RELACIONES (MATRIZ DISPERSA)
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

procedure TForm2.Button4Click(Sender: TObject);
begin
  // Regresar a Login
  if Assigned(Form1) then Form1.Show;
  Self.Hide;
end;

procedure TForm2.Button5Click(Sender: TObject);
var W: TCommunitiesWin;
begin
  W := TCommunitiesWin.CreateSimple(Self);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TForm2.Button6Click(Sender: TObject);
  function Slug(const S: string): string;
  var i: Integer; r: string;
  begin
    r := '';
    for i := 1 to Length(S) do
      if S[i] in ['A'..'Z','a'..'z','0'..'9','_'] then r += S[i]
      else r += '_';
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

end.

