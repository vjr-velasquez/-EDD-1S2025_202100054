unit user;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Grids, LCLIntf, Process,
  uTypes;  // Debe exportar PUsuario, TMail, PTrash, PContacto, PProg, PRel, CurrentUser, etc.

type
  { TForm3 }
  TForm3 = class(TForm)
    Button1: TButton;  // Bandeja de Entrada
    Button10: TButton; // Borradores (AVL)
    Button11: TButton; // Favoritos (Árbol B)
    Button12: TButton; // (libre) - puedes abrir carpeta reportes si quieres
    Button13: TButton; // Eliminar Contactos (lote)
    Button2: TButton;  // Enviar Correo
    Button3: TButton;  // Papelera
    Button4: TButton;  // Contactos
    Button5: TButton;  // Programar Correo
    Button6: TButton;  // Correos Programados
    Button7: TButton;  // Actualizar Perfil
    Button8: TButton;  // Generar Reportes
    Button9: TButton;  // Cerrar Sesión
    Label1: TLabel;
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  public
    procedure AfterConstruction; override;
  private
    procedure SafeMsg(const S: string);
  end;

var
  Form3: TForm3;

implementation

uses
  fgl,               // ← si usas el LZW con TFPGMap
  logeo, uAVLDrafts, uBFavorites, uAVL, uLoginAudit, uMerkle, uBlockchain;

{$R *.lfm}

// ---- LZW (forward para evitar orden)
function LZW_CompressToText(const S: string): string; forward;

// ===== LZW: comprime un string a "códigos" separados por espacio =====
type
  TStringIntMap = specialize TFPGMap<string, Integer>;

function LZW_CompressToText(const S: string): string;
var
  dict: TStringIntMap;
  i, code, idx: Integer;
  c, w, wk: string;
  outCodes: TStringList;
begin
  dict := TStringIntMap.Create;
  outCodes := TStringList.Create;
  try
    dict.Sorted := True;
    for i := 0 to 255 do
      dict.Add(Chr(i), i);

    w := '';
    for i := 1 to Length(S) do
    begin
      c := S[i];
      wk := w + c;
      if dict.Find(wk, idx) then
        w := wk
      else
      begin
        if w <> '' then
        begin
          dict.Find(w, idx);
          outCodes.Add(IntToStr(dict.Data[idx]));
        end;
        code := dict.Count;
        dict.Add(wk, code);
        w := c;
      end;
    end;

    if w <> '' then
      if dict.Find(w, idx) then
        outCodes.Add(IntToStr(dict.Data[idx]));

    Result := Trim(StringReplace(outCodes.Text, LineEnding, ' ', [rfReplaceAll]));
  finally
    outCodes.Free;
    dict.Free;
  end;
end;

{--- Ventana de texto simple ---}
procedure ShowTextWindow(const ATitle, AText: string);
var
  F: TForm;
  M: TMemo;
begin
  F := TForm.CreateNew(nil, 1);
  try
    F.Caption := ATitle;
    F.Position := poScreenCenter;
    F.Width := 720; F.Height := 520;
    M := TMemo.Create(F);
    M.Parent := F; M.Align := alClient;
    M.ReadOnly := True; M.ScrollBars := ssAutoBoth;
    M.Lines.Text := AText;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TForm3.SafeMsg(const S: string);
begin
  ShowMessage(S);
end;

{================= Subventanas (clases internas) =================}

type
  // Inbox
  TInboxWin = class(TForm)
  private
    U: PUsuario;
    pnlTop, pnlBottom: TPanel;
    lblBuscar, lblCampo, lblEstado, lblDesde, lblHasta: TLabel;
    edtBuscar, edtDesde, edtHasta: TEdit;
    cmbCampo, cmbEstado: TComboBox;
    chkProg: TCheckBox;
    btnFiltrar, btnVer, btnEliminar, btnMarcar, btnCerrar: TButton;
    LV: TListView;
    function ContainsTextCI(const Haystack, Needle: string): Boolean;
    function ParseDate(const S: string; out D: TDateTime): Boolean;
    procedure CargarLista(Sender: TObject);
    procedure VerSeleccion(Sender: TObject);
    procedure EliminarSeleccion(Sender: TObject);
    procedure ToggleLeidoSeleccion(Sender: TObject);
  public
    constructor CreateForUser(AOwner: TComponent; AUser: PUsuario);
  end;

  // Enviar
  TSendWin = class(TForm)
  private
    edtPara, edtAsunto: TEdit;
    memoMsg: TMemo;
    btnEnviar, btnCerrar: TButton;
    procedure SendDo(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Programar envío
  TProgWin = class(TForm)
  private
    edtPara, edtAsunto, edtFecha: TEdit;
    memoMsg: TMemo;
    btnProgramar, btnCerrar: TButton;
    procedure ProgEnqueue(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Lista programados
  TProgListWin = class(TForm)
  private
    LV: TListView;
    pnlBtns: TPanel;
    btnProcVencidos, btnRefrescar, btnCerrar: TButton;
    procedure LoadList;
    procedure ProcessDue(Sender: TObject);
    procedure Refresh(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Papelera
  TTrashWin = class(TForm)
  private
    LV: TListView;
    pnlBtns: TPanel;
    btnRestaurar, btnVaciar, btnCerrar: TButton;
    procedure LoadList;
    procedure RestoreSelected(Sender: TObject);
    procedure EmptyAll(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Contactos (lista circular)
  TContactsWin = class(TForm)
  private
    pnlTop: TPanel;
    lblEmail, lblNombre: TLabel;
    edtEmail, edtNombre: TEdit;
    btnAdd, btnEdit, btnDel, btnFindDup, btnCleanDup, btnRefresh, btnCerrar: TButton;
    LV: TListView;
    procedure RefreshList(Sender: TObject);
    procedure OnSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DoAdd(Sender: TObject);
    procedure DoEdit(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoFindDup(Sender: TObject);
    procedure DoCleanDup(Sender: TObject);
    function AddContact(U: PUsuario; const CEmail, CNombre: string): Boolean;
    function UpdateContact(U: PUsuario; const OldEmail, NewEmail, NewNombre: string): Boolean;
    function DeleteContact(U: PUsuario; const CEmail: string): Boolean;
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Perfil
  TProfileWin = class(TForm)
  private
    lblNom, lblUsu, lblTel, lblPass: TLabel;
    edtNom, edtUsu, edtTel, edtPass: TEdit;
    btnGuardar, btnCerrar: TButton;
    procedure SaveProfile(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Reportes (incluye matriz relaciones)
  TMatrixWin = class(TForm)
  private
    Grid: TStringGrid;
    pnlBtns: TPanel;
    btnExportCSV, btnExportDOT, btnRender, btnCerrar: TButton;
    Emails: TStringList;
    Matrix: array of array of Integer;
    function ReportDir: string;
    procedure BuildMatrix;
    procedure FillGrid;
    procedure ExportCSV(Sender: TObject);
    procedure ExportDOT(Sender: TObject);
    procedure RenderGraphviz(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
    destructor Destroy; override;
  end;

  TReportsWin = class(TForm)
  private
    btnGen, btnMatriz, btnCerrar: TButton;
    btnDotAll, btnRenderAll: TButton;
    btnMerkleRoot, btnMerkleDOT: TButton;  // <-- NUEVOS
    btnBC_DOT, btnBC_PNG: TButton; // blockchain
    lblHint: TLabel;
    function ReportDir: string;
    function SafeEmail(const S: string): string;
    function EscapeDOT(const S: string): string;
    procedure GenReports(Sender: TObject);
    procedure OpenMatrix(Sender: TObject);
    procedure ExportAllDOTs(Sender: TObject);
    procedure RenderAllPNGs(Sender: TObject);
    procedure WriteDOT_Inbox(U: PUsuario; const Path: string);
    procedure WriteDOT_Programados(U: PUsuario; const Path: string);
    procedure WriteDOT_Papelera(U: PUsuario; const Path: string);
    procedure WriteDOT_Contactos(U: PUsuario; const Path: string);
    procedure WriteDOT_Usuarios(const Path: string);
    procedure RenderOnePNG(const DotPath: string);
    procedure BtnMerkleRootClick(Sender: TObject);
    procedure BtnMerkleDOTClick(Sender: TObject);

    procedure BtnBlockchainDOTClick(Sender: TObject);
    procedure BtnBlockchainPNGClick(Sender: TObject);

  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  // Favoritos (Árbol B)
  TFavoritesWin = class(TForm)
  private
    U: PUsuario;
    LV: TListView;
    pnlBottom: TPanel;
    btnSelAll, btnClear, btnGuardar, btnVerPNG, btnCerrar: TButton;
    btnDescargar: TButton;                          // ← NUEVO
    procedure LoadInbox;
    procedure SelectAll(Sender: TObject);
    procedure ClearAll(Sender: TObject);
    procedure SaveFavorites(Sender: TObject);
    procedure OpenPNG(Sender: TObject);
    procedure DownloadSelected(Sender: TObject);    // ← NUEVO

  public
    constructor CreateForUser(AOwner: TComponent; AUser: PUsuario);
  end;

  // Eliminación MASIVA de contactos
  TContactsBulkDelWin = class(TForm)
  private
    U: PUsuario;
    LV: TListView;
    pnlBtns: TPanel;
    btnSelAll, btnClear, btnDelete, btnCerrar: TButton;
    procedure LoadList;
    procedure SelectAll(Sender: TObject);
    procedure ClearAll(Sender: TObject);
    procedure DeleteChecked(Sender: TObject);
    function DeleteByEmail(const CEmail: string): Boolean;
  public
    constructor CreateForUser(AOwner: TComponent; AUser: PUsuario);
  end;

  // Borradores (AVL)
  TDraftsWin = class(TForm)
  private
    LV: TListView;
    pnlTop, pnlBottom: TPanel;
    lblID, lblRem, lblDest, lblAsu, lblRec: TLabel;
    lblEst, lblFec: TLabel;
    edtID, edtRem, edtDest, edtAsu: TEdit;
    edtEst, edtFec: TEdit;
    memoMsg: TMemo;
    cmbRec: TComboBox;
    btnNuevo, btnGuardar, btnEliminar, btnEnviar, btnExportDOT, btnVerPNG, btnCerrar: TButton;
    btnDescargar: TButton;  // NUEVO
    procedure LoadList(Sender: TObject);
    function ReadDraftFromForm(out D: TDraft): Boolean;
    procedure Nuevo(Sender: TObject);
    procedure Guardar(Sender: TObject);
    procedure Eliminar(Sender: TObject);
    procedure Enviar(Sender: TObject);
    procedure ExportDOT(Sender: TObject);
    procedure VerPNG(Sender: TObject);
    procedure OnSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DescargarBorrador(Sender: TObject); // NUEVO
    function ReportDir: string;
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

{--- Helpers de Inbox ---}

function TInboxWin.ContainsTextCI(const Haystack, Needle: string): Boolean;
begin
  Result := Pos(LowerCase(Needle), LowerCase(Haystack)) > 0;
end;

function TInboxWin.ParseDate(const S: string; out D: TDateTime): Boolean;
begin
  Result := (Trim(S) <> '') and TryStrToDateTime(S, D);
end;

constructor TInboxWin.CreateForUser(AOwner: TComponent; AUser: PUsuario);
var
  it: TListColumn;
begin
  inherited CreateNew(AOwner, 1);
  U := AUser;
  Caption := 'Bandeja - ' + U^.Email;
  Position := poScreenCenter; Width := 920; Height := 620;

  pnlTop := TPanel.Create(Self); pnlTop.Parent := Self; pnlTop.Align := alTop; pnlTop.Height := 80;

  lblBuscar := TLabel.Create(Self); lblBuscar.Parent := pnlTop; lblBuscar.Caption := 'Buscar:'; lblBuscar.Left := 8; lblBuscar.Top := 12;
  edtBuscar := TEdit.Create(Self); edtBuscar.Parent := pnlTop; edtBuscar.Left := 65; edtBuscar.Top := 8; edtBuscar.Width := 200;

  lblCampo := TLabel.Create(Self); lblCampo.Parent := pnlTop; lblCampo.Caption := 'Campo:'; lblCampo.Left := 275; lblCampo.Top := 12;
  cmbCampo := TComboBox.Create(Self); cmbCampo.Parent := pnlTop; cmbCampo.Style := csDropDownList;
  cmbCampo.Items.Add('Todos'); cmbCampo.Items.Add('Remitente'); cmbCampo.Items.Add('Asunto'); cmbCampo.ItemIndex := 0;
  cmbCampo.Left := 330; cmbCampo.Top := 8; cmbCampo.Width := 110;

  lblEstado := TLabel.Create(Self); lblEstado.Parent := pnlTop; lblEstado.Caption := 'Estado:'; lblEstado.Left := 450; lblEstado.Top := 12;
  cmbEstado := TComboBox.Create(Self); cmbEstado.Parent := pnlTop; cmbEstado.Style := csDropDownList;
  cmbEstado.Items.Add('Todos'); cmbEstado.Items.Add('nuevo'); cmbEstado.Items.Add('leido'); cmbEstado.ItemIndex := 0;
  cmbEstado.Left := 505; cmbEstado.Top := 8; cmbEstado.Width := 100;

  chkProg := TCheckBox.Create(Self); chkProg.Parent := pnlTop; chkProg.Caption := 'Solo programados'; chkProg.Left := 615; chkProg.Top := 10;

  lblDesde := TLabel.Create(Self); lblDesde.Parent := pnlTop; lblDesde.Caption := 'Desde (YYYY-MM-DD hh:mm):'; lblDesde.Left := 8; lblDesde.Top := 46;
  edtDesde := TEdit.Create(Self); edtDesde.Parent := pnlTop; edtDesde.Left := 200; edtDesde.Top := 42; edtDesde.Width := 150;

  lblHasta := TLabel.Create(Self); lblHasta.Parent := pnlTop; lblHasta.Caption := 'Hasta (YYYY-MM-DD hh:mm):'; lblHasta.Left := 360; lblHasta.Top := 46;
  edtHasta := TEdit.Create(Self); edtHasta.Parent := pnlTop; edtHasta.Left := 560; edtHasta.Top := 42; edtHasta.Width := 150;

  btnFiltrar := TButton.Create(Self); btnFiltrar.Parent := pnlTop; btnFiltrar.Caption := 'Filtrar'; btnFiltrar.Left := 720; btnFiltrar.Top := 40; btnFiltrar.Width := 80;
  btnFiltrar.OnClick := @CargarLista;

  pnlBottom := TPanel.Create(Self); pnlBottom.Parent := Self; pnlBottom.Align := alBottom; pnlBottom.Height := 44;

  btnVer := TButton.Create(Self); btnVer.Parent := pnlBottom; btnVer.Caption := 'Ver'; btnVer.Left := 8; btnVer.Top := 8; btnVer.Width := 80; btnVer.OnClick := @VerSeleccion;
  btnMarcar := TButton.Create(Self); btnMarcar.Parent := pnlBottom; btnMarcar.Caption := 'Marcar leído/nuevo'; btnMarcar.Left := 96; btnMarcar.Top := 8; btnMarcar.Width := 140; btnMarcar.OnClick := @ToggleLeidoSeleccion;
  btnEliminar := TButton.Create(Self); btnEliminar.Parent := pnlBottom; btnEliminar.Caption := 'Eliminar'; btnEliminar.Left := 244; btnEliminar.Top := 8; btnEliminar.Width := 90; btnEliminar.OnClick := @EliminarSeleccion;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBottom; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 800; btnCerrar.Top := 8; btnCerrar.Width := 80; btnCerrar.ModalResult := mrClose;

  LV := TListView.Create(Self); LV.Parent := Self; LV.Align := alClient; LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;

  it := LV.Columns.Add; it.Caption := 'Id'; it.Width := 60;
  it := LV.Columns.Add; it.Caption := 'Remitente'; it.Width := 180;
  it := LV.Columns.Add; it.Caption := 'Fecha/Hora'; it.Width := 160;
  it := LV.Columns.Add; it.Caption := 'Asunto'; it.Width := 330;
  it := LV.Columns.Add; it.Caption := 'Estado'; it.Width := 90;
  it := LV.Columns.Add; it.Caption := 'Prog'; it.Width := 60;

  edtBuscar.OnChange := @CargarLista;
  cmbCampo.OnChange := @CargarLista;
  cmbEstado.OnChange := @CargarLista;
  chkProg.OnChange := @CargarLista;

  CargarLista(nil);
end;

procedure TInboxWin.CargarLista(Sender: TObject);
var
  M: PMail;
  q, campo, estado: string;
  dDesde, dHasta: TDateTime;
  usarDesde, usarHasta, soloProg, pasa: Boolean;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if U = nil then Exit;

    q := Trim(edtBuscar.Text);
    campo := cmbCampo.Text;
    estado := cmbEstado.Text;
    soloProg := chkProg.Checked;
    usarDesde := ParseDate(edtDesde.Text, dDesde);
    usarHasta := ParseDate(edtHasta.Text, dHasta);

    M := U^.InboxHead;
    while M <> nil do
    begin
      pasa := True;

      if (estado <> 'Todos') and (LowerCase(M^.Estado) <> LowerCase(estado)) then pasa := False;
      if soloProg and (not M^.Programado) then pasa := False;
      if usarDesde and (M^.Fecha < dDesde) then pasa := False;
      if usarHasta and (M^.Fecha > dHasta) then pasa := False;

      if (q <> '') and pasa then
      begin
        if campo = 'Todos' then
          pasa := ContainsTextCI(M^.Asunto, q) or ContainsTextCI(M^.Remitente, q)
        else if campo = 'Remitente' then
          pasa := ContainsTextCI(M^.Remitente, q)
        else
          pasa := ContainsTextCI(M^.Asunto, q);
      end;

      if pasa then
      begin
        it := LV.Items.Add;
        it.Caption := IntToStr(M^.Id);
        it.SubItems.Add(M^.Remitente);
        it.SubItems.Add(DateTimeToStr(M^.Fecha));
        it.SubItems.Add(M^.Asunto);
        it.SubItems.Add(M^.Estado);
        if M^.Programado then
    it.SubItems.Add('Sí')
    else
    it.SubItems.Add('No');
          it.Data := M;
      end;

      M := M^.Next;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TInboxWin.VerSeleccion(Sender: TObject);
var
  it: TListItem;
  M: PMail;
  cuerpo, progStr: string;
begin
  it := LV.Selected;
  if (it = nil) or (it.Data = nil) then begin ShowMessage('Selecciona un correo.'); Exit; end;
  M := PMail(it.Data);

  if M^.Programado then progStr := 'Sí' else progStr := 'No';

  cuerpo :=
    'Id: ' + IntToStr(M^.Id) + LineEnding +
    'Remitente: ' + M^.Remitente + LineEnding +
    'Fecha: ' + DateTimeToStr(M^.Fecha) + LineEnding +
    'Asunto: ' + M^.Asunto + LineEnding +
    'Estado: ' + M^.Estado + LineEnding +
    'Programado: ' + progStr + LineEnding + LineEnding +
    M^.Mensaje;

  ShowTextWindow('Correo #' + IntToStr(M^.Id), cuerpo);
end;

procedure TInboxWin.ToggleLeidoSeleccion(Sender: TObject);
var
  it: TListItem;
  M: PMail;
begin
  it := LV.Selected;
  if (it = nil) or (it.Data = nil) then begin ShowMessage('Selecciona un correo.'); Exit; end;
  M := PMail(it.Data);
  if LowerCase(M^.Estado) = 'nuevo' then M^.Estado := 'leido' else M^.Estado := 'nuevo';
  CargarLista(nil);
end;

procedure TInboxWin.EliminarSeleccion(Sender: TObject);
var
  it: TListItem;
  M: PMail;
  backup: TMail;
begin
  it := LV.Selected;
  if (it = nil) or (it.Data = nil) then begin ShowMessage('Selecciona un correo.'); Exit; end;

  M := PMail(it.Data);
  if M^.Prev <> nil then M^.Prev^.Next := M^.Next else U^.InboxHead := M^.Next;
  if M^.Next <> nil then M^.Next^.Prev := M^.Prev else U^.InboxTail := M^.Prev;

  backup := M^; Dispose(M);

  User_PushTrash(U, backup);
  CargarLista(nil);
  ShowMessage('Correo movido a Papelera.');
end;

{--- Enviar ---}

constructor TSendWin.CreateSimple(AOwner: TComponent);
var
  lbl: TLabel;
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Enviar correo';
  Position := poScreenCenter; Width := 720; Height := 520;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Para (email):'; lbl.Left := 16; lbl.Top := 20;
  edtPara := TEdit.Create(Self); edtPara.Parent := Self; edtPara.Left := 120; edtPara.Top := 16; edtPara.Width := 360;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Asunto:'; lbl.Left := 16; lbl.Top := 56;
  edtAsunto := TEdit.Create(Self); edtAsunto.Parent := Self; edtAsunto.Left := 120; edtAsunto.Top := 52; edtAsunto.Width := 560;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Mensaje:'; lbl.Left := 16; lbl.Top := 92;
  memoMsg := TMemo.Create(Self); memoMsg.Parent := Self; memoMsg.Left := 120; memoMsg.Top := 92; memoMsg.Width := 560; memoMsg.Height := 320;

  btnEnviar := TButton.Create(Self); btnEnviar.Parent := Self; btnEnviar.Caption := 'Enviar'; btnEnviar.Left := 120; btnEnviar.Top := 424; btnEnviar.Width := 120; btnEnviar.OnClick := @SendDo;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := Self; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 560; btnCerrar.Top := 424; btnCerrar.Width := 120; btnCerrar.ModalResult := mrClose;
end;

procedure TSendWin.SendDo(Sender: TObject);
var
  para, asunto, mensaje: string;
  Dest: PUsuario;
begin
  if (CurrentUser = nil) then Exit;
  para := Trim(edtPara.Text);
  asunto := edtAsunto.Text;
  mensaje := memoMsg.Text;

  if para = '' then begin ShowMessage('Ingrese destinatario.'); Exit; end;
  Dest := BuscarUsuarioPorEmail(para);
  if Dest = nil then begin ShowMessage('El destinatario no existe.'); Exit; end;

  User_AppendInbox(Dest, CurrentUser^.Email, asunto, mensaje, Now, False, 'nuevo');
  User_IncRel(CurrentUser^.Email, Dest^.Email);
  ShowMessage('Correo enviado.');
  edtPara.Clear; edtAsunto.Clear; memoMsg.Clear;
end;

{--- Programar ---}

constructor TProgWin.CreateSimple(AOwner: TComponent);
var
  lbl: TLabel;
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Programar envío';
  Position := poScreenCenter; Width := 720; Height := 520;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Para (email):'; lbl.Left := 16; lbl.Top := 20;
  edtPara := TEdit.Create(Self); edtPara.Parent := Self; edtPara.Left := 120; edtPara.Top := 16; edtPara.Width := 360;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Asunto:'; lbl.Left := 16; lbl.Top := 56;
  edtAsunto := TEdit.Create(Self); edtAsunto.Parent := Self; edtAsunto.Left := 120; edtAsunto.Top := 52; edtAsunto.Width := 560;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Mensaje:'; lbl.Left := 16; lbl.Top := 92;
  memoMsg := TMemo.Create(Self); memoMsg.Parent := Self; memoMsg.Left := 120; memoMsg.Top := 92; memoMsg.Width := 560; memoMsg.Height := 320;

  lbl := TLabel.Create(Self); lbl.Parent := Self; lbl.Caption := 'Fecha (YYYY-MM-DD o DD/MM/AAAA):'; lbl.Left := 16; lbl.Top := 424;
  edtFecha := TEdit.Create(Self); edtFecha.Parent := Self; edtFecha.Left := 260; edtFecha.Top := 420; edtFecha.Width := 180;

  btnProgramar := TButton.Create(Self); btnProgramar.Parent := Self; btnProgramar.Caption := 'Programar'; btnProgramar.Left := 460; btnProgramar.Top := 420; btnProgramar.Width := 120; btnProgramar.OnClick := @ProgEnqueue;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := Self; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 590; btnCerrar.Top := 420; btnCerrar.Width := 90; btnCerrar.ModalResult := mrClose;
end;

procedure TProgWin.ProgEnqueue(Sender: TObject);
  function TryParseDateOnly(const S: string; out D: TDateTime): Boolean;
  var
    fs: TFormatSettings;
    tmp: TDateTime;
  begin
    Result := TryStrToDate(S, D);
    if Result then Exit;
    fs := DefaultFormatSettings;
    fs.DateSeparator   := '-';
    fs.ShortDateFormat := 'yyyy-mm-dd';
    Result := TryStrToDate(S, tmp, fs);
    if Result then D := tmp;
  end;

var
  para, asunto, mensaje, sfecha: string;
  dt: TDateTime;
  Dest: PUsuario;
  N: PProg;
begin
  if (CurrentUser = nil) then Exit;

  para := Trim(edtPara.Text);
  asunto := edtAsunto.Text;
  mensaje := memoMsg.Text;
  sfecha := Trim(edtFecha.Text);

  if para = '' then begin ShowMessage('Ingrese destinatario.'); Exit; end;
  Dest := BuscarUsuarioPorEmail(para);
  if Dest = nil then begin ShowMessage('El destinatario no existe.'); Exit; end;

  if not TryParseDateOnly(sfecha, dt) then
  begin
    ShowMessage('Fecha inválida. Usa: 25/08/2025  o  2025-08-25');
    Exit;
  end;

  New(N);
  N^.Id := NextMailId; Inc(NextMailId);
  N^.Remitente := CurrentUser^.Email;
  N^.Destinatario := Dest^.Email;
  N^.Asunto := asunto;
  N^.Mensaje := mensaje;
  N^.FechaProg := dt;
  N^.Next := nil;

  if CurrentUser^.ProgTail <> nil then
    CurrentUser^.ProgTail^.Next := N
  else
    CurrentUser^.ProgHead := N;
  CurrentUser^.ProgTail := N;

  ShowMessage('Correo programado para ' + DateToStr(dt) + '.');
  edtPara.Clear; edtAsunto.Clear; memoMsg.Clear; edtFecha.Clear;
end;

{--- Lista de programados ---}

constructor TProgListWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Programados';
  Position := poScreenCenter; Width := 820; Height := 520;

  LV := TListView.Create(Self); LV.Parent := Self; LV.Align := alClient; LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.Columns.Add.Caption := 'Id';
  LV.Columns.Add.Caption := 'Remitente';
  LV.Columns.Add.Caption := 'Para';
  LV.Columns.Add.Caption := 'Prog. Fecha';
  LV.Columns.Add.Caption := 'Asunto';
  LV.Columns[0].Width := 60;
  LV.Columns[1].Width := 180;
  LV.Columns[2].Width := 200;
  LV.Columns[3].Width := 180;
  LV.Columns[4].Width := 180;

  pnlBtns := TPanel.Create(Self); pnlBtns.Parent := Self; pnlBtns.Align := alBottom; pnlBtns.Height := 44;
  btnProcVencidos := TButton.Create(Self); btnProcVencidos.Parent := pnlBtns; btnProcVencidos.Caption := 'Procesar vencidos (ahora)'; btnProcVencidos.Left := 8; btnProcVencidos.Top := 8; btnProcVencidos.Width := 200; btnProcVencidos.OnClick := @ProcessDue;
  btnRefrescar := TButton.Create(Self); btnRefrescar.Parent := pnlBtns; btnRefrescar.Caption := 'Refrescar'; btnRefrescar.Left := 212; btnRefrescar.Top := 8; btnRefrescar.Width := 100; btnRefrescar.OnClick := @Refresh;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBtns; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 720; btnCerrar.Top := 8; btnCerrar.Width := 80; btnCerrar.ModalResult := mrClose;

  LoadList;
end;

procedure TProgListWin.LoadList;
var
  C: PProg;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if CurrentUser = nil then Exit;
    C := CurrentUser^.ProgHead;
    while C <> nil do
    begin
      it := LV.Items.Add;
      it.Caption := IntToStr(C^.Id);
      it.SubItems.Add(C^.Remitente);
      it.SubItems.Add(C^.Destinatario);
      it.SubItems.Add(DateToStr(C^.FechaProg));
      it.SubItems.Add(C^.Asunto);
      C := C^.Next;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TProgListWin.ProcessDue(Sender: TObject);
var
  processed: Integer;
  C: PProg;
  Dest: PUsuario;
begin
  processed := 0;
  if (CurrentUser = nil) then Exit;

  while (CurrentUser^.ProgHead <> nil) and (CurrentUser^.ProgHead^.FechaProg <= Now) do
  begin
    C := CurrentUser^.ProgHead;
    CurrentUser^.ProgHead := C^.Next;
    if CurrentUser^.ProgHead = nil then CurrentUser^.ProgTail := nil;

    Dest := BuscarUsuarioPorEmail(C^.Destinatario);
    if Dest <> nil then
    begin
      User_AppendInbox(Dest, C^.Remitente, C^.Asunto, C^.Mensaje, C^.FechaProg, True, 'nuevo');
      User_IncRel(C^.Remitente, C^.Destinatario);
      Inc(processed);
    end;
    Dispose(C);
  end;

  ShowMessage('Procesados: ' + IntToStr(processed));
  LoadList;
end;

procedure TProgListWin.Refresh(Sender: TObject);
begin
  LoadList;
end;

{--- Papelera ---}

constructor TTrashWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Papelera';
  Position := poScreenCenter; Width := 820; Height := 520;

  LV := TListView.Create(Self); LV.Parent := Self; LV.Align := alClient; LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.Columns.Add.Caption := 'Id';
  LV.Columns.Add.Caption := 'Remitente';
  LV.Columns.Add.Caption := 'Fecha/Hora';
  LV.Columns.Add.Caption := 'Asunto';
  LV.Columns[0].Width := 60;
  LV.Columns[1].Width := 180;
  LV.Columns[2].Width := 160;
  LV.Columns[3].Width := 360;

  pnlBtns := TPanel.Create(Self); pnlBtns.Parent := Self; pnlBtns.Align := alBottom; pnlBtns.Height := 44;
  btnRestaurar := TButton.Create(Self); btnRestaurar.Parent := pnlBtns; btnRestaurar.Caption := 'Restaurar seleccionado'; btnRestaurar.Left := 8; btnRestaurar.Top := 8; btnRestaurar.Width := 180; btnRestaurar.OnClick := @RestoreSelected;
  btnVaciar := TButton.Create(Self); btnVaciar.Parent := pnlBtns; btnVaciar.Caption := 'Vaciar papelera'; btnVaciar.Left := 196; btnVaciar.Top := 8; btnVaciar.Width := 140; btnVaciar.OnClick := @EmptyAll;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBtns; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 720; btnCerrar.Top := 8; btnCerrar.Width := 80; btnCerrar.ModalResult := mrClose;

  LoadList;
end;

procedure TTrashWin.LoadList;
var
  P: PTrash;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if CurrentUser = nil then Exit;
    P := CurrentUser^.TrashTop;
    while P <> nil do
    begin
      it := LV.Items.Add;
      it.Caption := IntToStr(P^.Mail.Id);
      it.SubItems.Add(P^.Mail.Remitente);
      it.SubItems.Add(DateTimeToStr(P^.Mail.Fecha));
      it.SubItems.Add(P^.Mail.Asunto);
      it.Data := P;
      P := P^.Next;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TTrashWin.RestoreSelected(Sender: TObject);
var
  it: TListItem;
  P, Prev: PTrash;
  M: TMail;
begin
  it := LV.Selected;
  if (it = nil) or (it.Data = nil) then begin ShowMessage('Selecciona un elemento.'); Exit; end;

  Prev := nil;
  P := CurrentUser^.TrashTop;
  while (P <> nil) and (P <> PTrash(it.Data)) do
  begin
    Prev := P; P := P^.Next;
  end;
  if P = nil then Exit;

  M := P^.Mail;

  if Prev = nil then
    CurrentUser^.TrashTop := P^.Next
  else
    Prev^.Next := P^.Next;
  Dispose(P);

  User_AppendInbox(CurrentUser, M.Remitente, M.Asunto, M.Mensaje, Now, M.Programado, 'nuevo');
  ShowMessage('Restaurado a bandeja.');
  LoadList;
end;

procedure TTrashWin.EmptyAll(Sender: TObject);
var
  P: PTrash;
begin
  while CurrentUser^.TrashTop <> nil do
  begin
    P := CurrentUser^.TrashTop;
    CurrentUser^.TrashTop := P^.Next;
    Dispose(P);
  end;
  ShowMessage('Papelera vacía.');
  LoadList;
end;

{--- Contactos ---}

constructor TContactsWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Contactos';
  Position := poScreenCenter; Width := 760; Height := 520;

  pnlTop := TPanel.Create(Self); pnlTop.Parent := Self; pnlTop.Align := alTop; pnlTop.Height := 56;

  lblEmail := TLabel.Create(Self); lblEmail.Parent := pnlTop; lblEmail.Caption := 'Email:'; lblEmail.Left := 8; lblEmail.Top := 16;
  edtEmail := TEdit.Create(Self); edtEmail.Parent := pnlTop; edtEmail.Left := 60; edtEmail.Top := 12; edtEmail.Width := 220;

  lblNombre := TLabel.Create(Self); lblNombre.Parent := pnlTop; lblNombre.Caption := 'Nombre:'; lblNombre.Left := 290; lblNombre.Top := 16;
  edtNombre := TEdit.Create(Self); edtNombre.Parent := pnlTop; edtNombre.Left := 350; edtNombre.Top := 12; edtNombre.Width := 220;

  btnAdd := TButton.Create(Self); btnAdd.Parent := pnlTop; btnAdd.Caption := 'Agregar'; btnAdd.Left := 580; btnAdd.Top := 10; btnAdd.Width := 80; btnAdd.OnClick := @DoAdd;
  btnEdit := TButton.Create(Self); btnEdit.Parent := pnlTop; btnEdit.Caption := 'Editar'; btnEdit.Left := 8; btnEdit.Top := 34; btnEdit.Width := 80; btnEdit.OnClick := @DoEdit;
  btnDel := TButton.Create(Self); btnDel.Parent := pnlTop; btnDel.Caption := 'Eliminar'; btnDel.Left := 92; btnDel.Top := 34; btnDel.Width := 80; btnDel.OnClick := @DoDelete;
  btnFindDup := TButton.Create(Self); btnFindDup.Parent := pnlTop; btnFindDup.Caption := 'Duplicados'; btnFindDup.Left := 176; btnFindDup.Top := 34; btnFindDup.Width := 100; btnFindDup.OnClick := @DoFindDup;
  btnCleanDup := TButton.Create(Self); btnCleanDup.Parent := pnlTop; btnCleanDup.Caption := 'Limpiar dups'; btnCleanDup.Left := 280; btnCleanDup.Top := 34; btnCleanDup.Width := 100; btnCleanDup.OnClick := @DoCleanDup;

  btnRefresh := TButton.Create(Self); btnRefresh.Parent := pnlTop; btnRefresh.Caption := 'Refrescar'; btnRefresh.Left := 384; btnRefresh.Top := 34; btnRefresh.Width := 90; btnRefresh.OnClick := @RefreshList;

  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlTop; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 480; btnCerrar.Top := 34; btnCerrar.Width := 90; btnCerrar.ModalResult := mrClose;

  LV := TListView.Create(Self); LV.Parent := Self; LV.Align := alClient; LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.Columns.Add.Caption := 'Nombre';
  LV.Columns.Add.Caption := 'Email';
  LV.Columns[0].Width := 260;
  LV.Columns[1].Width := 360;

  LV.OnSelectItem := @OnSelect;

  RefreshList(nil);
end;

procedure TContactsWin.RefreshList(Sender: TObject);
var
  T, H, C: PContacto;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if (CurrentUser = nil) or (CurrentUser^.ContactTail = nil) then Exit;

    T := CurrentUser^.ContactTail;
    H := T^.Next;
    C := H;
    repeat
      it := LV.Items.Add;
      it.Caption := C^.Nombre;
      it.SubItems.Add(C^.Email);
      C := C^.Next;
    until C = H;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TContactsWin.OnSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (Item <> nil) and Selected then
  begin
    edtNombre.Text := Item.Caption;
    if Item.SubItems.Count > 0 then
      edtEmail.Text := Item.SubItems[0];
  end;
end;

function TContactsWin.AddContact(U: PUsuario; const CEmail, CNombre: string): Boolean;
var
  T, H, C, N: PContacto;
  function EqualCI(const A, B: string): Boolean; inline; begin Result := LowerCase(A) = LowerCase(B); end;
begin
  Result := False;
  if (U = nil) or (Trim(CEmail) = '') then Exit;

  T := U^.ContactTail;
  if T <> nil then
  begin
    H := T^.Next;
    C := H;
    repeat
      if EqualCI(C^.Email, CEmail) then Exit(False);
      C := C^.Next;
    until C = H;
  end;

  New(N);
  N^.Email := Trim(CEmail);
  N^.Nombre := CNombre;
  if U^.ContactTail = nil then
  begin
    N^.Next := N;
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

function TContactsWin.UpdateContact(U: PUsuario; const OldEmail, NewEmail, NewNombre: string): Boolean;
var
  T, H, C: PContacto;
  target: PContacto;
  function EqualCI(const A, B: string): Boolean; inline; begin Result := LowerCase(A) = LowerCase(B); end;
begin
  Result := False; target := nil;
  if (U = nil) then Exit;

  if U^.ContactTail = nil then Exit;
  T := U^.ContactTail; H := T^.Next;
  C := H;
  repeat
    if EqualCI(C^.Email, OldEmail) then begin target := C; Break; end;
    C := C^.Next;
  until C = H;

  if target = nil then Exit;

  if (Trim(NewEmail) <> '') and (not EqualCI(OldEmail, NewEmail)) then
  begin
    C := H;
    repeat
      if (C <> target) and EqualCI(C^.Email, NewEmail) then Exit(False);
      C := C^.Next;
    until C = H;
    target^.Email := Trim(NewEmail);
  end;
  target^.Nombre := NewNombre;
  Result := True;
end;

function TContactsWin.DeleteContact(U: PUsuario; const CEmail: string): Boolean;
var
  T, H, C, Prev: PContacto;
  function EqualCI(const A, B: string): Boolean; inline; begin Result := LowerCase(A) = LowerCase(B); end;
begin
  Result := False;
  if (U = nil) or (U^.ContactTail = nil) then Exit;

  T := U^.ContactTail; H := T^.Next;
  Prev := T; C := H;
  repeat
    if EqualCI(C^.Email, CEmail) then
    begin
      if C = C^.Next then
        U^.ContactTail := nil
      else
      begin
        Prev^.Next := C^.Next;
        if U^.ContactTail = C then U^.ContactTail := Prev;
      end;
      Dispose(C);
      Exit(True);
    end;
    Prev := C; C := C^.Next;
  until C = H;
end;

procedure TContactsWin.DoAdd(Sender: TObject);
begin
  if (CurrentUser = nil) then Exit;
  if AddContact(CurrentUser, edtEmail.Text, edtNombre.Text) then
  begin
    ShowMessage('Contacto agregado.'); edtEmail.Clear; edtNombre.Clear; RefreshList(nil);
  end
  else
    ShowMessage('Ya existe o email inválido.');
end;

procedure TContactsWin.DoEdit(Sender: TObject);
var
  oldEmail: string;
begin
  if (CurrentUser = nil) then Exit;
  if LV.Selected = nil then begin ShowMessage('Selecciona un contacto.'); Exit; end;
  oldEmail := LV.Selected.SubItems[0];
  if UpdateContact(CurrentUser, oldEmail, edtEmail.Text, edtNombre.Text) then
    begin ShowMessage('Contacto actualizado.'); RefreshList(nil); end
  else
    ShowMessage('No se pudo actualizar (duplicado o inexistente).');
end;

procedure TContactsWin.DoDelete(Sender: TObject);
begin
  if (CurrentUser = nil) then Exit;
  if LV.Selected = nil then begin ShowMessage('Selecciona un contacto.'); Exit; end;
  if DeleteContact(CurrentUser, LV.Selected.SubItems[0]) then
    begin ShowMessage('Contacto eliminado.'); RefreshList(nil); end
  else
    ShowMessage('No se pudo eliminar.');
end;

procedure TContactsWin.DoFindDup(Sender: TObject);
var
  T, H, C: PContacto;
  emails, dups: TStringList;
  key: string;
begin
  emails := TStringList.Create;
  dups := TStringList.Create;
  try
    emails.CaseSensitive := False; emails.Sorted := True; emails.Duplicates := dupAccept;

    if (CurrentUser <> nil) and (CurrentUser^.ContactTail <> nil) then
    begin
      T := CurrentUser^.ContactTail; H := T^.Next; C := H;
      repeat
        key := Trim(LowerCase(C^.Email));
        if emails.IndexOf(key) >= 0 then dups.Add(C^.Email) else emails.Add(key);
        C := C^.Next;
      until C = H;
    end;

    if dups.Count = 0 then ShowMessage('Sin duplicados.') else ShowTextWindow('Duplicados', dups.Text);
  finally
    emails.Free; dups.Free;
  end;
end;

procedure TContactsWin.DoCleanDup(Sender: TObject);
var
  T, H, C, Prev, NextNode: PContacto;
  seen: TStringList;
  removed: Integer;
  key: string;
begin
  removed := 0;
  if (CurrentUser = nil) or (CurrentUser^.ContactTail = nil) then begin ShowMessage('Sin contactos.'); Exit; end;

  seen := TStringList.Create;
  try
    seen.CaseSensitive := False; seen.Sorted := True; seen.Duplicates := dupIgnore;

    T := CurrentUser^.ContactTail; H := T^.Next;
    Prev := T; C := H;
    repeat
      key := Trim(LowerCase(C^.Email));
      if seen.IndexOf(key) >= 0 then
      begin
        NextNode := C^.Next;
        Prev^.Next := NextNode;
        if CurrentUser^.ContactTail = C then CurrentUser^.ContactTail := Prev;
        Dispose(C);
        C := NextNode;
        Inc(removed);
      end
      else
      begin
        seen.Add(key);
        Prev := C;
        C := C^.Next;
      end;
    until C = H;

    ShowMessage(Format('Duplicados eliminados: %d', [removed]));
    RefreshList(nil);
  finally
    seen.Free;
  end;
end;

{--- Perfil ---}

constructor TProfileWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Perfil'; Position := poScreenCenter; Width := 520; Height := 260;

  lblNom := TLabel.Create(Self); lblNom.Parent := Self; lblNom.Caption := 'Nombre:'; lblNom.Left := 24; lblNom.Top := 24;
  edtNom := TEdit.Create(Self); edtNom.Parent := Self; edtNom.Left := 120; edtNom.Top := 20; edtNom.Width := 320;

  lblUsu := TLabel.Create(Self); lblUsu.Parent := Self; lblUsu.Caption := 'Usuario:'; lblUsu.Left := 24; lblUsu.Top := 60;
  edtUsu := TEdit.Create(Self); edtUsu.Parent := Self; edtUsu.Left := 120; edtUsu.Top := 56; edtUsu.Width := 320;

  lblTel := TLabel.Create(Self); lblTel.Parent := Self; lblTel.Caption := 'Teléfono:'; lblTel.Left := 24; lblTel.Top := 96;
  edtTel := TEdit.Create(Self); edtTel.Parent := Self; edtTel.Left := 120; edtTel.Top := 92; edtTel.Width := 320;

  lblPass := TLabel.Create(Self); lblPass.Parent := Self; lblPass.Caption := 'Nueva contraseña:'; lblPass.Left := 24; lblPass.Top := 132;
  edtPass := TEdit.Create(Self); edtPass.Parent := Self; edtPass.Left := 160; edtPass.Top := 128; edtPass.Width := 280; edtPass.PasswordChar := '*';

  btnGuardar := TButton.Create(Self); btnGuardar.Parent := Self; btnGuardar.Caption := 'Guardar'; btnGuardar.Left := 120; btnGuardar.Top := 168; btnGuardar.Width := 120; btnGuardar.OnClick := @SaveProfile;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := Self; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 320; btnCerrar.Top := 168; btnCerrar.Width := 120; btnCerrar.ModalResult := mrClose;

  if CurrentUser <> nil then
  begin
    edtNom.Text := CurrentUser^.Nombre;
    edtUsu.Text := CurrentUser^.Usuario;
    edtTel.Text := CurrentUser^.Telefono;
    edtPass.Text := CurrentUser^.Password;
  end;
end;

procedure TProfileWin.SaveProfile(Sender: TObject);
begin
  if CurrentUser = nil then Exit;
  CurrentUser^.Nombre := edtNom.Text;
  CurrentUser^.Usuario := edtUsu.Text;
  CurrentUser^.Telefono := edtTel.Text;
  if Trim(edtPass.Text) <> '' then CurrentUser^.Password := edtPass.Text;
  ShowMessage('Perfil actualizado.');
end;

{--- Matriz (Relaciones) ---}

function TMatrixWin.ReportDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if (not DirectoryExists(Result)) then CreateDir(Result);
end;

procedure TMatrixWin.BuildMatrix;
var
  R: PRel;
  iFrom, iTo, i: Integer;
begin
  Emails.Clear;

  R := RelHead;
  while R <> nil do
  begin
    if Emails.IndexOf(LowerCase(R^.FromEmail)) < 0 then Emails.Add(LowerCase(R^.FromEmail));
    if Emails.IndexOf(LowerCase(R^.ToEmail)) < 0 then Emails.Add(LowerCase(R^.ToEmail));
    R := R^.Next;
  end;
  Emails.Sort;

  SetLength(Matrix, Emails.Count, Emails.Count);
  for i := 0 to High(Matrix) do
    FillChar(Matrix[i][0], SizeOf(Integer) * Emails.Count, 0);

  R := RelHead;
  while R <> nil do
  begin
    iFrom := Emails.IndexOf(LowerCase(R^.FromEmail));
    iTo   := Emails.IndexOf(LowerCase(R^.ToEmail));
    if (iFrom >= 0) and (iTo >= 0) then
      Matrix[iFrom][iTo] := Matrix[iFrom][iTo] + R^.Count;
    R := R^.Next;
  end;
end;

procedure TMatrixWin.FillGrid;
var
  N, r, c: Integer;
begin
  N := Emails.Count;
  Grid.ColCount := N + 1; Grid.RowCount := N + 1;
  Grid.FixedCols := 1; Grid.FixedRows := 1;
  Grid.Cells[0,0] := 'From \ To';
  for c := 0 to N-1 do Grid.Cells[c+1, 0] := Emails[c];
  for r := 0 to N-1 do Grid.Cells[0, r+1] := Emails[r];
  for r := 0 to N-1 do
    for c := 0 to N-1 do
      if Matrix[r][c] > 0 then Grid.Cells[c+1, r+1] := IntToStr(Matrix[r][c]) else Grid.Cells[c+1, r+1] := '';
end;

procedure TMatrixWin.ExportCSV(Sender: TObject);
var
  dir, path: string; sl: TStringList;
  N, r, c: Integer; line: string;
begin
  dir := ReportDir; path := dir + PathDelim + 'matriz_relaciones.csv';
  sl := TStringList.Create;
  try
    N := Emails.Count;
    line := 'From\To'; for c := 0 to N-1 do line := line + ',' + Emails[c]; sl.Add(line);
    for r := 0 to N-1 do
    begin
      line := Emails[r];
      for c := 0 to N-1 do
        if Matrix[r][c] > 0 then line := line + ',' + IntToStr(Matrix[r][c]) else line := line + ',';
      sl.Add(line);
    end;
    sl.SaveToFile(path);
    ShowMessage('CSV exportado en: ' + path);
  finally
    sl.Free;
  end;
end;

procedure TMatrixWin.ExportDOT(Sender: TObject);
var
  dir, path: string; sl: TStringList; r, c: Integer;
begin
  dir := ReportDir; path := dir + PathDelim + 'relaciones.dot';
  sl := TStringList.Create;
  try
    sl.Add('digraph G {'); sl.Add('  rankdir=LR;'); sl.Add('  node [shape=ellipse, fontname="Arial"];');
    for r := 0 to Emails.Count-1 do sl.Add(Format('  "%s";', [Emails[r]]));
    for r := 0 to Emails.Count-1 do
      for c := 0 to Emails.Count-1 do
        if Matrix[r][c] > 0 then
          sl.Add(Format('  "%s" -> "%s" [label="%d", penwidth=%f];',
               [Emails[r], Emails[c], Matrix[r][c], 1.0 + Matrix[r][c] * 0.2]));
    sl.Add('}'); sl.SaveToFile(path);
    ShowMessage('DOT exportado en: ' + path);
  finally
    sl.Free;
  end;
end;

procedure TMatrixWin.RenderGraphviz(Sender: TObject);
var
  dir, dotPath, pngPath: string;
  P: TProcess;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'relaciones.dot';
  pngPath := dir + PathDelim + 'relaciones.png';
  if not FileExists(dotPath) then ExportDOT(nil);

  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Clear;
    P.Parameters.Add('-Tpng'); P.Parameters.Add(dotPath);
    P.Parameters.Add('-o');    P.Parameters.Add(pngPath);
    P.Options := [poWaitOnExit];
    try P.Execute; except
      on E: Exception do begin
        ShowMessage('Error al ejecutar Graphviz (dot): ' + E.Message + LineEnding +
                    'Instala con: sudo apt install graphviz -y'); Exit;
      end;
    end;
  finally
    P.Free;
  end;

  if FileExists(pngPath) then
    if not OpenDocument(pngPath) then ShowMessage('Imagen generada en: ' + pngPath);
end;

constructor TMatrixWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Relaciones (Matriz Dispersa)';
  Position := poScreenCenter; Width := 1000; Height := 640;

  Grid := TStringGrid.Create(Self);
  Grid.Parent := Self; Grid.Align := alClient;
  Grid.Options := Grid.Options + [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect];
  Grid.DefaultColWidth := 120; Grid.DefaultRowHeight := 22;

  pnlBtns := TPanel.Create(Self); pnlBtns.Parent := Self; pnlBtns.Align := alBottom; pnlBtns.Height := 48;
  btnExportCSV := TButton.Create(Self); btnExportCSV.Parent := pnlBtns; btnExportCSV.Caption := 'Exportar CSV'; btnExportCSV.Left := 8; btnExportCSV.Top := 8; btnExportCSV.Width := 120; btnExportCSV.OnClick := @ExportCSV;
  btnExportDOT := TButton.Create(Self); btnExportDOT.Parent := pnlBtns; btnExportDOT.Caption := 'Exportar DOT (Graphviz)'; btnExportDOT.Left := 136; btnExportDOT.Top := 8; btnExportDOT.Width := 180; btnExportDOT.OnClick := @ExportDOT;
  btnRender := TButton.Create(Self); btnRender.Parent := pnlBtns; btnRender.Caption := 'Renderizar con Graphviz (PNG)'; btnRender.Left := 324; btnRender.Top := 8; btnRender.Width := 220; btnRender.OnClick := @RenderGraphviz;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBtns; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 900; btnCerrar.Top := 8; btnCerrar.Width := 80; btnCerrar.ModalResult := mrClose;

  Emails := TStringList.Create; Emails.CaseSensitive := False;

  BuildMatrix; FillGrid;
end;

destructor TMatrixWin.Destroy;
begin
  FreeAndNil(Emails);
  inherited Destroy;
end;

{--- Reportes ---}

function TReportsWin.ReportDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(Result) then CreateDir(Result);
end;

function TReportsWin.SafeEmail(const S: string): string;
begin
  Result := StringReplace(S, '@', '_', [rfReplaceAll]);
end;

function TReportsWin.EscapeDOT(const S: string): string;
begin
  Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
end;

constructor TReportsWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Reportes'; Position := poScreenCenter; Width := 640; Height := 240;

  btnGen := TButton.Create(Self); btnGen.Parent := Self; btnGen.Caption := 'Generar reportes';
  btnGen.Left := 24; btnGen.Top := 24; btnGen.Width := 180; btnGen.OnClick := @GenReports;

  btnMatriz := TButton.Create(Self); btnMatriz.Parent := Self; btnMatriz.Caption := 'Ver Matriz (Relaciones)';
  btnMatriz.Left := 216; btnMatriz.Top := 24; btnMatriz.Width := 200; btnMatriz.OnClick := @OpenMatrix;

  btnCerrar := TButton.Create(Self); btnCerrar.Parent := Self; btnCerrar.Caption := 'Cerrar';
  btnCerrar.Left := 432; btnCerrar.Top := 24; btnCerrar.Width := 96; btnCerrar.ModalResult := mrClose;

  btnDotAll := TButton.Create(Self); btnDotAll.Parent := Self; btnDotAll.Caption := 'Exportar DOT (estructuras)';
  btnDotAll.Left := 24; btnDotAll.Top := 72; btnDotAll.Width := 200; btnDotAll.OnClick := @ExportAllDOTs;

  btnRenderAll := TButton.Create(Self); btnRenderAll.Parent := Self; btnRenderAll.Caption := 'Renderizar DOTs (PNG)';
  btnRenderAll.Left := 236; btnRenderAll.Top := 72; btnRenderAll.Width := 200; btnRenderAll.OnClick := @RenderAllPNGs;

  // ---- MERKLE ----
  btnMerkleRoot := TButton.Create(Self); btnMerkleRoot.Parent := Self;
  btnMerkleRoot.Caption := 'Merkle Root (Inbox actual)';
  btnMerkleRoot.Left := 24; btnMerkleRoot.Top := 120; btnMerkleRoot.Width := 200;
  btnMerkleRoot.OnClick := @BtnMerkleRootClick;

  btnMerkleDOT := TButton.Create(Self); btnMerkleDOT.Parent := Self;
  btnMerkleDOT.Caption := 'Exportar DOT (Merkle Inbox)';
  btnMerkleDOT.Left := 236; btnMerkleDOT.Top := 120; btnMerkleDOT.Width := 200;
  btnMerkleDOT.OnClick  := @BtnMerkleDOTClick;

  // ---- BLOCKCHAIN ----
  btnBC_DOT := TButton.Create(Self);
  btnBC_DOT.Parent := Self;
  btnBC_DOT.Caption := 'Exportar DOT (Blockchain)';
  btnBC_DOT.Left := 24; btnBC_DOT.Top := 168; btnBC_DOT.Width := 200;
  btnBC_DOT.OnClick := @BtnBlockchainDOTClick;

  btnBC_PNG := TButton.Create(Self);
  btnBC_PNG.Parent := Self;
  btnBC_PNG.Caption := 'Renderizar Blockchain (PNG)';
  btnBC_PNG.Left := 236; btnBC_PNG.Top := 168; btnBC_PNG.Width := 200;
  btnBC_PNG.OnClick := @BtnBlockchainPNGClick;

  // Mover el hint para que no se solape
  lblHint := TLabel.Create(Self); lblHint.Parent := Self;
  lblHint.Caption := 'Archivos en "Reportes"';
  lblHint.Left := 24; lblHint.Top := 208; // <- antes estaba en 120
  lblHint.AutoSize := True;
end;


procedure TReportsWin.BtnMerkleRootClick(Sender: TObject);
var
  rootHex: string;
begin
  if (CurrentUser = nil) then
  begin
    ShowMessage('Inicie sesión para calcular el Merkle Root de su bandeja.');
    Exit;
  end;

  rootHex := MerkleRoot_FromInbox(CurrentUser);
  if rootHex = '' then
    ShowMessage('Inbox vacío: no hay Merkle Root.')
  else
    ShowMessage('Merkle Root (Inbox): ' + rootHex);
end;

procedure TReportsWin.BtnBlockchainPNGClick(Sender: TObject);
var
  dir, dotPath, pngPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'blockchain.dot';
  pngPath := dir + PathDelim + 'blockchain.png';

  if not FileExists(dotPath) then BtnBlockchainDOTClick(nil);

  if Blockchain_RenderDOTToPNG(dotPath, pngPath) then
    if not OpenDocument(pngPath) then
      ShowMessage('PNG generado: ' + pngPath)
  else
    ShowMessage('No se pudo renderizar. Instala graphviz (dot).');
end;



procedure TReportsWin.BtnMerkleDOTClick(Sender: TObject);
var
  dir, dotPath: string;
begin
  if (CurrentUser = nil) then
  begin
    ShowMessage('Inicie sesión para exportar el árbol Merkle.');
    Exit;
  end;

  dir := ReportDir; // ya existe en esta clase
  dotPath := dir + PathDelim + 'merkle_inbox.dot';

  Merkle_SaveDOT_FromInbox(CurrentUser, dotPath);
  ShowMessage('DOT exportado: ' + dotPath + LineEnding +
              'Para PNG: dot -Tpng merkle_inbox.dot -o merkle_inbox.png');
end;


procedure TReportsWin.GenReports(Sender: TObject);
var
  dir, Safe: string;
  sl: TStringList;
  C: PMail;
  CT, H: PContacto;
  R: PRel;
begin
  if (CurrentUser = nil) then Exit;
  dir := ReportDir;
  Safe := SafeEmail(CurrentUser^.Email);

  // Inbox TXT
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
    sl.SaveToFile(dir + PathDelim + 'inbox_' + Safe + '.txt');
  finally
    sl.Free;
  end;

  // Contactos TXT
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
    else sl.Add('(sin contactos)');
    sl.SaveToFile(dir + PathDelim + 'contactos_' + Safe + '.txt');
  finally
    sl.Free;
  end;

  // Relaciones CSV
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

  ShowMessage('TXT/CSV generados en: ' + dir);
end;

procedure TReportsWin.OpenMatrix(Sender: TObject);
var F: TMatrixWin;
begin
  F := TMatrixWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TReportsWin.WriteDOT_Inbox(U: PUsuario; const Path: string);
var
  sl: TStringList; M: PMail;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Inbox {');
    sl.Add('  rankdir=LR; node [shape=box, fontname="Arial"];');
    M := U^.InboxHead;
    if M = nil then
      sl.Add('  empty [label="(bandeja vacía)"];')
    else
    begin
      while M <> nil do
      begin
        sl.Add(Format('  "m%d" [label="#%d\n%s"];',
          [M^.Id, M^.Id, EscapeDOT(M^.Asunto)]));
        if M^.Next <> nil then
          sl.Add(Format('  "m%d" -> "m%d" [dir=both,label="next/prev"];',
            [M^.Id, M^.Next^.Id]));
        M := M^.Next;
      end;
    end;
    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure TReportsWin.WriteDOT_Programados(U: PUsuario; const Path: string);
var
  sl: TStringList; P: PProg; firstId, lastId: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Programados {');
    sl.Add('  rankdir=LR; node [shape=box, fontname="Arial"];');

    P := U^.ProgHead; firstId := -1; lastId := -1;
    if P = nil then
      sl.Add('  empty [label="(cola vacía)"];')
    else
      while P <> nil do
      begin
        sl.Add(Format('  "p%d" [label="#%d\n%s -> %s\n%s"];',
          [P^.Id, P^.Id, EscapeDOT(P^.Remitente), EscapeDOT(P^.Destinatario),
           DateToStr(P^.FechaProg)]));
        if firstId = -1 then firstId := P^.Id;
        if lastId <> -1 then sl.Add(Format('  "p%d" -> "p%d";', [lastId, P^.Id]));
        lastId := P^.Id;
        P := P^.Next;
      end;

    if firstId <> -1 then
    begin
      sl.Add(Format('  head [shape=oval,label="HEAD"]; head -> "p%d";', [firstId]));
      sl.Add(Format('  "p%d" -> tail [shape=oval,label="TAIL"];', [lastId]));
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure TReportsWin.WriteDOT_Papelera(U: PUsuario; const Path: string);
var
  sl: TStringList; T: PTrash; prevId: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Papelera {');
    sl.Add('  rankdir=TB; node [shape=box, fontname="Arial"];');

    T := U^.TrashTop; prevId := -1;
    if T = nil then
      sl.Add('  empty [label="(pila vacía)"];')
    else
      while T <> nil do
      begin
        sl.Add(Format('  "t%d" [label="#%d\n%s"];',
          [T^.Mail.Id, T^.Mail.Id, EscapeDOT(T^.Mail.Asunto)]));
        if prevId = -1 then
          sl.Add(Format('  TOP -> "t%d";', [T^.Mail.Id]))
        else
          sl.Add(Format('  "t%d" -> "t%d";', [prevId, T^.Mail.Id]));
        prevId := T^.Mail.Id;
        T := T^.Next;
      end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure TReportsWin.WriteDOT_Contactos(U: PUsuario; const Path: string);
var
  sl: TStringList; T, H, C: PContacto; firstLbl, prevLbl, lbl: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Contactos {');
    sl.Add('  rankdir=LR; node [shape=ellipse, fontname="Arial"];');

    if U^.ContactTail = nil then
      sl.Add('  empty [label="(lista circular vacía)"];')
    else
    begin
      T := U^.ContactTail; H := T^.Next; C := H;
      firstLbl := ''; prevLbl := '';
      repeat
        lbl := EscapeDOT(C^.Email);
        sl.Add(Format('  "%s" [label="%s"];', [lbl, lbl]));
        if prevLbl <> '' then sl.Add(Format('  "%s" -> "%s";', [prevLbl, lbl])) else firstLbl := lbl;
        prevLbl := lbl;
        C := C^.Next;
      until C = H;
      if (firstLbl <> '') and (prevLbl <> '') then sl.Add(Format('  "%s" -> "%s";', [prevLbl, firstLbl]));
    end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure TReportsWin.WriteDOT_Usuarios(const Path: string);
var
  sl: TStringList; U: PUsuario; prev: string;
begin
  sl := TStringList.Create;
  try
    sl.Add('digraph Usuarios {');
    sl.Add('  rankdir=LR; node [shape=box, fontname="Arial"];');

    U := UsuariosHead; prev := '';
    if U = nil then
      sl.Add('  empty [label="(lista vacía)"];')
    else
      while U <> nil do
      begin
        sl.Add(Format('  "%s" [label="%s"];',
          [EscapeDOT(U^.Email), EscapeDOT(U^.Email)]));
        if prev <> '' then sl.Add(Format('  "%s" -> "%s";', [prev, EscapeDOT(U^.Email)]));
        prev := EscapeDOT(U^.Email);
        U := U^.Next;
      end;

    sl.Add('}');
    sl.SaveToFile(Path);
  finally
    sl.Free;
  end;
end;

procedure TReportsWin.ExportAllDOTs(Sender: TObject);
var
  dir, safe, pInbox, pProg, pTrash, pCont, pUsers: string;
begin
  if CurrentUser = nil then begin ShowMessage('Inicie sesión.'); Exit; end;
  dir := ReportDir; safe := SafeEmail(CurrentUser^.Email);

  pInbox := dir + PathDelim + 'inbox_' + safe + '.dot';
  pProg  := dir + PathDelim + 'programados_' + safe + '.dot';
  pTrash := dir + PathDelim + 'papelera_' + safe + '.dot';
  pCont  := dir + PathDelim + 'contactos_' + safe + '.dot';
  pUsers := dir + PathDelim + 'usuarios.dot';

  WriteDOT_Inbox(CurrentUser, pInbox);
  WriteDOT_Programados(CurrentUser, pProg);
  WriteDOT_Papelera(CurrentUser, pTrash);
  WriteDOT_Contactos(CurrentUser, pCont);
  WriteDOT_Usuarios(pUsers);

  ShowMessage('DOTs generados en: ' + dir);
end;

procedure TReportsWin.RenderOnePNG(const DotPath: string);
var
  pngPath: string; P: TProcess;
begin
  if not FileExists(DotPath) then Exit;
  pngPath := ChangeFileExt(DotPath, '.png');
  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotPath);
    P.Parameters.Add('-o');
    P.Parameters.Add(pngPath);
    P.Options := [poWaitOnExit];
    P.Execute;
  finally
    P.Free;
  end;
end;

procedure TReportsWin.BtnBlockchainDOTClick(Sender: TObject);
var
  dir, dotPath: string;
begin
  dir := ReportDir;
  dotPath := dir + PathDelim + 'blockchain.dot';

  // Usa la variable global de uBlockchain
  Blockchain_SaveDOT(BlockchainHead, dotPath);

  ShowMessage('DOT exportado: ' + dotPath);
end;









procedure TReportsWin.RenderAllPNGs(Sender: TObject);
var
  dir, safe: string;
begin
  dir := ReportDir; safe := SafeEmail(CurrentUser^.Email);

  if not FileExists(dir + PathDelim + 'inbox_' + safe + '.dot') then ExportAllDOTs(nil);

  RenderOnePNG(dir + PathDelim + 'inbox_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'programados_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'papelera_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'contactos_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'usuarios.dot');

  ShowMessage('PNGs generados');
end;

{--- Favoritos (Árbol B) ---}

constructor TFavoritesWin.CreateForUser(AOwner: TComponent; AUser: PUsuario);
begin
  inherited CreateNew(AOwner, 1);
  U := AUser;
  Caption := 'Seleccionar favoritos (Árbol B)';
  Position := poScreenCenter; Width := 820; Height := 520;

  LV := TListView.Create(Self);
  LV.Parent := Self; LV.Align := alClient;
  LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.CheckBoxes := True;
  LV.Columns.Add.Caption := 'Id';
  LV.Columns.Add.Caption := 'Asunto';
  LV.Columns[0].Width := 60;
  LV.Columns[1].Width := 680;

  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self; pnlBottom.Align := alBottom; pnlBottom.Height := 44;

  btnSelAll := TButton.Create(Self);
  btnSelAll.Parent := pnlBottom; btnSelAll.Caption := 'Seleccionar todo';
  btnSelAll.Left := 8; btnSelAll.Top := 8; btnSelAll.Width := 130;
  btnSelAll.OnClick := @SelectAll;

  btnClear := TButton.Create(Self);
  btnClear.Parent := pnlBottom; btnClear.Caption := 'Limpiar selección';
  btnClear.Left := 146; btnClear.Top := 8; btnClear.Width := 130;
  btnClear.OnClick := @ClearAll;

  btnGuardar := TButton.Create(Self);
  btnGuardar.Parent := pnlBottom; btnGuardar.Caption := 'Guardar en Árbol B y Graficar';
  btnGuardar.Left := 284; btnGuardar.Top := 8; btnGuardar.Width := 220;
  btnGuardar.OnClick := @SaveFavorites;


  btnDescargar := TButton.Create(Self);
  btnDescargar.Parent := pnlBottom;
  btnDescargar.Caption := 'Descargar (LZW)';
  btnDescargar.Left := 620;   // ajusta si choca con otros
  btnDescargar.Top := 8;
  btnDescargar.Width := 120;
  btnDescargar.OnClick := @DownloadSelected;



  btnVerPNG := TButton.Create(Self);
  btnVerPNG.Parent := pnlBottom; btnVerPNG.Caption := 'Abrir PNG';
  btnVerPNG.Left := 512; btnVerPNG.Top := 8; btnVerPNG.Width := 100;
  btnVerPNG.OnClick := @OpenPNG;

  btnCerrar := TButton.Create(Self);
  btnCerrar.Parent := pnlBottom; btnCerrar.Caption := 'Cerrar';
  btnCerrar.Left := 720; btnCerrar.Top := 8; btnCerrar.Width := 80;
  btnCerrar.ModalResult := mrClose;

  LoadInbox;
end;

procedure TFavoritesWin.LoadInbox;
var
  M: PMail;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if (U = nil) then Exit;
    M := U^.InboxHead;
    while M <> nil do
    begin
      it := LV.Items.Add;
      it.Caption := IntToStr(M^.Id);
      it.SubItems.Add(M^.Asunto);
      it.Data := M;
      it.Checked := False;
      M := M^.Next;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TFavoritesWin.SelectAll(Sender: TObject);
var i: Integer;
begin
  for i := 0 to LV.Items.Count-1 do
    LV.Items[i].Checked := True;
end;

procedure TFavoritesWin.ClearAll(Sender: TObject);
var i: Integer;
begin
  for i := 0 to LV.Items.Count-1 do
    LV.Items[i].Checked := False;
end;

procedure TFavoritesWin.SaveFavorites(Sender: TObject);
var
  i: Integer;
  M: PMail;
  K: TFavKey;
  Dir, DotPath, PngPath: string;
begin
  if CurrentUser = nil then Exit;

  Dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(Dir) then CreateDir(Dir);
  DotPath := Dir + PathDelim + 'favoritos.dot';
  PngPath := Dir + PathDelim + 'favoritos.png';

  if Favorites = nil then
    Favorites := TBFavorites.Create(3); // grado 3

  Favorites.Clear;

  for i := 0 to LV.Items.Count-1 do
    if LV.Items[i].Checked then
    begin
      M := PMail(LV.Items[i].Data);
      if M <> nil then
      begin
        K.ID     := M^.Id;
        K.Asunto := M^.Asunto;
        Favorites.Insert(K);
      end;
    end;

  Favorites.SaveDOT(DotPath);
  if RenderizarPNGConDot(DotPath, PngPath) then
  begin
    if not OpenDocument(PngPath) then
      ShowMessage('Imagen generada: ' + PngPath);
  end
  else
    ShowMessage('DOT exportado: ' + DotPath + LineEnding +
                'Si deseas PNG instala Graphviz: sudo apt install graphviz -y');
end;

procedure TFavoritesWin.DownloadSelected(Sender: TObject);
var
  it: TListItem;
  M: uTypes.PMail;
  msg, dir, path, data: string;
begin
  it := LV.Selected;
  if (it = nil) or (it.Data = nil) then
  begin
    ShowMessage('Selecciona un correo de la lista para descargar.');
    Exit;
  end;

  M := uTypes.PMail(it.Data);
  msg := M^.Mensaje;

  if Trim(msg) = '' then
  begin
    ShowMessage('Este correo no tiene contenido en "Mensaje".');
    Exit;
  end;

  dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(dir) then CreateDir(dir);
  path := Format('%s%scorreo_%d_lzw.txt', [dir, PathDelim, M^.Id]);

  data := LZW_CompressToText(msg);
  with TStringList.Create do
  try
    Text := data;
    SaveToFile(path);
  finally
    Free;
  end;

  ShowMessage('Mensaje comprimido (LZW) guardado en: ' + path);
end;



procedure TFavoritesWin.OpenPNG(Sender: TObject);
var
  Dir, PngPath: string;
begin
  Dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  PngPath := Dir + PathDelim + 'favoritos.png';
  if FileExists(PngPath) then
    if not OpenDocument(PngPath) then
      ShowMessage('Imagen: ' + PngPath)
  else
    ShowMessage('Aún no hay PNG. Presiona "Guardar en Árbol B y Graficar" primero.');
end;

{--- BORRADORES (AVL) ---}

constructor TDraftsWin.CreateSimple(AOwner: TComponent);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Borradores (AVL)'; Position := poScreenCenter; Width := 960; Height := 700;

  pnlTop := TPanel.Create(Self); pnlTop.Parent := Self; pnlTop.Align := alTop; pnlTop.Height := 200;

  lblID := TLabel.Create(Self); lblID.Parent := pnlTop; lblID.Caption := 'ID:'; lblID.Left := 12; lblID.Top := 12;
  edtID := TEdit.Create(Self); edtID.Parent := pnlTop; edtID.Left := 60; edtID.Top := 8; edtID.Width := 80;

  lblRem := TLabel.Create(Self); lblRem.Parent := pnlTop; lblRem.Caption := 'Remitente:'; lblRem.Left := 160; lblRem.Top := 12;
  edtRem := TEdit.Create(Self); edtRem.Parent := pnlTop; edtRem.Left := 240; edtRem.Top := 8; edtRem.Width := 300;

  lblDest := TLabel.Create(Self); lblDest.Parent := pnlTop; lblDest.Caption := 'Destinatario:'; lblDest.Left := 560; lblDest.Top := 12;
  edtDest := TEdit.Create(Self); edtDest.Parent := pnlTop; edtDest.Left := 650; edtDest.Top := 8; edtDest.Width := 280;

  lblAsu := TLabel.Create(Self); lblAsu.Parent := pnlTop; lblAsu.Caption := 'Asunto:'; lblAsu.Left := 12; lblAsu.Top := 44;
  edtAsu := TEdit.Create(Self); edtAsu.Parent := pnlTop; edtAsu.Left := 70; edtAsu.Top := 40; edtAsu.Width := 860;

  lblEst := TLabel.Create(Self); lblEst.Parent := pnlTop; lblEst.Caption := 'Estado:'; lblEst.Left := 12; lblEst.Top := 72;
  edtEst := TEdit.Create(Self);  edtEst.Parent := pnlTop;  edtEst.Left := 70; edtEst.Top := 68; edtEst.Width := 180;

  lblFec := TLabel.Create(Self); lblFec.Parent := pnlTop; lblFec.Caption := 'Fecha (YYYY-MM-DD o DD/MM/AAAA):'; lblFec.Left := 270; lblFec.Top := 72;
  edtFec := TEdit.Create(Self);  edtFec.Parent := pnlTop;  edtFec.Left := 560; edtFec.Top := 68; edtFec.Width := 200;

  memoMsg := TMemo.Create(Self); memoMsg.Parent := pnlTop; memoMsg.Left := 12; memoMsg.Top := 100;
  memoMsg.Width := 918; memoMsg.Height := 92;

  lblRec := TLabel.Create(Self);
  lblRec.Parent := pnlTop; lblRec.Caption := 'Recorrido:'; lblRec.Left := 12; lblRec.Top := 196;

  cmbRec := TComboBox.Create(Self);
  cmbRec.Parent := pnlTop; cmbRec.Style := csDropDownList;
  cmbRec.Items.Add('Pre-Orden'); cmbRec.Items.Add('In-Orden'); cmbRec.Items.Add('Post-Orden');
  cmbRec.ItemIndex := 1;
  cmbRec.Left := 90; cmbRec.Top := 192; cmbRec.Width := 120;
  cmbRec.OnChange := @LoadList;

  LV := TListView.Create(Self); LV.Parent := Self; LV.Align := alClient;
  LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.Columns.Add.Caption := 'ID';
  LV.Columns.Add.Caption := 'Remitente';
  LV.Columns.Add.Caption := 'Destinatario';
  LV.Columns.Add.Caption := 'Asunto';
  LV.Columns[0].Width := 60;
  LV.Columns[1].Width := 220;
  LV.Columns[2].Width := 220;
  LV.Columns[3].Width := 360;
  LV.OnSelectItem := @OnSelect;

  pnlBottom := TPanel.Create(Self); pnlBottom.Parent := Self; pnlBottom.Align := alBottom; pnlBottom.Height := 48;

  btnNuevo := TButton.Create(Self); btnNuevo.Parent := pnlBottom; btnNuevo.Caption := 'Nuevo'; btnNuevo.Left := 8; btnNuevo.Top := 8; btnNuevo.Width := 80; btnNuevo.OnClick := @Nuevo;
  btnGuardar := TButton.Create(Self); btnGuardar.Parent := pnlBottom; btnGuardar.Caption := 'Guardar (Ins/Upd)'; btnGuardar.Left := 96; btnGuardar.Top := 8; btnGuardar.Width := 140; btnGuardar.OnClick := @Guardar;
  btnEliminar := TButton.Create(Self); btnEliminar.Parent := pnlBottom; btnEliminar.Caption := 'Eliminar'; btnEliminar.Left := 244; btnEliminar.Top := 8; btnEliminar.Width := 100; btnEliminar.OnClick := @Eliminar;
  btnEnviar := TButton.Create(Self); btnEnviar.Parent := pnlBottom; btnEnviar.Caption := 'Enviar'; btnEnviar.Left := 348; btnEnviar.Top := 8; btnEnviar.Width := 100; btnEnviar.OnClick := @Enviar;
  btnExportDOT := TButton.Create(Self); btnExportDOT.Parent := pnlBottom; btnExportDOT.Caption := 'Exportar DOT/PNG'; btnExportDOT.Left := 452; btnExportDOT.Top := 8; btnExportDOT.Width := 140; btnExportDOT.OnClick := @ExportDOT;
  btnVerPNG := TButton.Create(Self); btnVerPNG.Parent := pnlBottom; btnVerPNG.Caption := 'Abrir PNG'; btnVerPNG.Left := 596; btnVerPNG.Top := 8; btnVerPNG.Width := 100; btnVerPNG.OnClick := @VerPNG;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBottom; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 804; btnCerrar.Top := 8; btnCerrar.Width := 80; btnCerrar.ModalResult := mrClose;
  btnDescargar := TButton.Create(Self);
  btnDescargar.Parent := pnlBottom;
  btnDescargar.Caption := 'Descargar (LZW)';
  btnDescargar.Left := 700;  // ajusta si es necesario
  btnDescargar.Top := 8;
  btnDescargar.Width := 120;
  btnDescargar.OnClick := @DescargarBorrador;


  LoadList(nil);
end;

procedure TDraftsWin.LoadList(Sender: TObject);
var
  L: TStringList;
  i: Integer;
  parts: TStringArray;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if Drafts = nil then Exit;

    L := TStringList.Create;
    try
      case cmbRec.ItemIndex of
        0: Drafts.ToStringsPreOrder(L);
        1: Drafts.ToStringsInOrder(L);
        2: Drafts.ToStringsPostOrder(L);
      else
        Drafts.ToStringsInOrder(L);
      end;

      for i := 0 to L.Count-1 do
      begin
        parts := L[i].Split([';']);
        if Length(parts) >= 4 then
        begin
          it := LV.Items.Add;
          it.Caption := parts[0];
          it.SubItems.Add(parts[1]);
          it.SubItems.Add(parts[2]);
          it.SubItems.Add(parts[3]);
        end;
      end;
    finally
      L.Free;
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TDraftsWin.DescargarBorrador(Sender: TObject);
var
  id: Integer;
  D: TDraft;
  dir, path, data: string;
begin
  if (LV.Selected = nil) then
  begin
    ShowMessage('Selecciona un borrador en la lista.');
    Exit;
  end;

  if not TryStrToInt(LV.Selected.Caption, id) then
  begin
    ShowMessage('ID de borrador inválido.');
    Exit;
  end;

  if (Drafts = nil) or (not Drafts.Search(id, D)) then
  begin
    ShowMessage('No se encontró el borrador.');
    Exit;
  end;

  if Trim(D.Mensaje) = '' then
  begin
    ShowMessage('Este borrador no tiene contenido en "Mensaje".');
    Exit;
  end;

  dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(dir) then CreateDir(dir);
  path := Format('%s%sborrador_%d_lzw.txt', [dir, PathDelim, D.ID]);

  data := LZW_CompressToText(D.Mensaje);
  with TStringList.Create do
  try
    Text := data;
    SaveToFile(path);
  finally
    Free;
  end;

  ShowMessage('Borrador comprimido (LZW) guardado en: ' + path);
end;




function TDraftsWin.ReadDraftFromForm(out D: TDraft): Boolean;

  function TryParseDateLoose(const S: string; out DT: TDateTime): Boolean;
  var fs: TFormatSettings; tmp: TDateTime;
  begin
    Result := False;
    if Trim(S) = '' then Exit;
    if TryStrToDate(S, DT) then Exit(True);
    fs := DefaultFormatSettings; fs.DateSeparator := '-'; fs.ShortDateFormat := 'yyyy-mm-dd';
    if TryStrToDate(S, tmp, fs) then begin DT := tmp; Exit(True); end;
  end;

begin
  Result := False;
  if not TryStrToInt(Trim(edtID.Text), D.ID) then begin ShowMessage('ID inválido.'); Exit; end;
  D.Remitente := Trim(edtRem.Text);
  D.Destinatario := Trim(edtDest.Text);
  D.Asunto := edtAsu.Text;
  D.Mensaje := memoMsg.Text;
  D.Estado := Trim(edtEst.Text);
  if not TryParseDateLoose(edtFec.Text, D.Fecha) then D.Fecha := 0;

  if D.Remitente = '' then begin ShowMessage('Remitente requerido.'); Exit; end;
  if D.Destinatario = '' then begin ShowMessage('Destinatario requerido.'); Exit; end;
  Result := True;
end;

procedure TDraftsWin.Nuevo(Sender: TObject);
begin
  edtID.Clear; edtRem.Clear; edtDest.Clear; edtAsu.Clear; memoMsg.Clear; edtEst.Clear; edtFec.Clear;
  if (CurrentUser <> nil) and (edtRem.Text = '') then
    edtRem.Text := CurrentUser^.Email;
end;

procedure TDraftsWin.Guardar(Sender: TObject);
var D: TDraft;
begin
  if Drafts = nil then Exit;
  if not ReadDraftFromForm(D) then Exit;

  if not Drafts.Insert(D) then
  begin
    if Drafts.Update(D) then
      ShowMessage('Borrador actualizado.')
    else
    begin
      ShowMessage('No se pudo insertar/actualizar.');
      Exit;
    end;
  end
  else
    ShowMessage('Borrador guardado.');

  LoadList(nil);
end;

procedure TDraftsWin.Eliminar(Sender: TObject);
var id: Integer;
begin
  if Drafts = nil then Exit;
  if LV.Selected = nil then begin ShowMessage('Selecciona un borrador.'); Exit; end;
  if not TryStrToInt(LV.Selected.Caption, id) then Exit;
  if Drafts.Delete(id) then begin ShowMessage('Borrador eliminado.'); LoadList(nil); end
  else ShowMessage('No se pudo eliminar.');
end;

procedure TDraftsWin.Enviar(Sender: TObject);
var
  id: Integer;
  D: TDraft;
  Dest: PUsuario;
begin
  if (CurrentUser = nil) then begin ShowMessage('Inicie sesión.'); Exit; end;
  if Drafts = nil then Exit;
  if LV.Selected = nil then begin ShowMessage('Selecciona un borrador.'); Exit; end;
  if not TryStrToInt(LV.Selected.Caption, id) then Exit;

  if not Drafts.Search(id, D) then begin ShowMessage('No se encontró el borrador.'); Exit; end;

  Dest := BuscarUsuarioPorEmail(D.Destinatario);
  if Dest = nil then begin ShowMessage('El destinatario no existe.'); Exit; end;

  User_AppendInbox(Dest, D.Remitente, D.Asunto, D.Mensaje, Now, False, 'nuevo');
  User_IncRel(D.Remitente, D.Destinatario);
  Drafts.Delete(D.ID);

  ShowMessage('Correo enviado desde borrador.');
  LoadList(nil);
end;

procedure TDraftsWin.ExportDOT(Sender: TObject);
var dir, dotPath, pngPath: string;
begin
  if Drafts = nil then Exit;
  dir := ReportDir;
  dotPath := dir + PathDelim + 'borradores.dot';
  pngPath := dir + PathDelim + 'borradores.png';
  Drafts.SaveDOT(dotPath);
  if Drafts.RenderPNGFromDOT(dotPath, pngPath) then
    ShowMessage('DOT y PNG generados en: ' + dir)
  else
    ShowMessage('DOT exportado en: ' + dotPath + LineEnding +
                'Para PNG instala Graphviz (dot).');
end;

procedure TDraftsWin.VerPNG(Sender: TObject);
var dir, pngPath: string;
begin
  dir := ReportDir;
  pngPath := dir + PathDelim + 'borradores.png';
  if FileExists(pngPath) then
  begin
    if not OpenDocument(pngPath) then
      ShowMessage('Imagen: ' + pngPath);
  end
  else
    ShowMessage('No hay PNG. Usa "Exportar DOT/PNG".');
end;

procedure TDraftsWin.OnSelect(Sender: TObject; Item: TListItem; Selected: Boolean);
var id: Integer; D: TDraft;
begin
  if (Item <> nil) and Selected then
  begin
    if TryStrToInt(Item.Caption, id) and (Drafts <> nil) then
      if Drafts.Search(id, D) then
      begin
        edtID.Text := IntToStr(D.ID);
        edtRem.Text := D.Remitente;
        edtDest.Text := D.Destinatario;
        edtAsu.Text := D.Asunto;
        memoMsg.Text := D.Mensaje;
        edtEst.Text := D.Estado;
        if D.Fecha > 0 then edtFec.Text := DateToStr(D.Fecha) else edtFec.Clear;
      end;
  end;
end;

function TDraftsWin.ReportDir: string;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(Result) then CreateDir(Result);
end;

{--- Eliminar Contactos en Lote ---}

constructor TContactsBulkDelWin.CreateForUser(AOwner: TComponent; AUser: PUsuario);
begin
  inherited CreateNew(AOwner, 1);
  U := AUser;
  Caption := 'Eliminar contactos';
  Position := poScreenCenter; Width := 760; Height := 520;

  LV := TListView.Create(Self);
  LV.Parent := Self; LV.Align := alClient;
  LV.ViewStyle := vsReport; LV.ReadOnly := True; LV.RowSelect := True; LV.GridLines := True;
  LV.CheckBoxes := True;
  LV.Columns.Add.Caption := 'Nombre';
  LV.Columns.Add.Caption := 'Email';
  LV.Columns[0].Width := 260;
  LV.Columns[1].Width := 360;

  pnlBtns := TPanel.Create(Self); pnlBtns.Parent := Self; pnlBtns.Align := alBottom; pnlBtns.Height := 48;

  btnSelAll := TButton.Create(Self); btnSelAll.Parent := pnlBtns; btnSelAll.Caption := 'Seleccionar todo'; btnSelAll.Left := 8; btnSelAll.Top := 8; btnSelAll.Width := 130; btnSelAll.OnClick := @SelectAll;
  btnClear  := TButton.Create(Self); btnClear.Parent := pnlBtns; btnClear.Caption := 'Limpiar selección'; btnClear.Left := 144; btnClear.Top := 8; btnClear.Width := 130; btnClear.OnClick := @ClearAll;
  btnDelete := TButton.Create(Self); btnDelete.Parent := pnlBtns; btnDelete.Caption := 'Eliminar seleccionados'; btnDelete.Left := 280; btnDelete.Top := 8; btnDelete.Width := 170; btnDelete.OnClick := @DeleteChecked;
  btnCerrar := TButton.Create(Self); btnCerrar.Parent := pnlBtns; btnCerrar.Caption := 'Cerrar'; btnCerrar.Left := 640; btnCerrar.Top := 8; btnCerrar.Width := 96; btnCerrar.ModalResult := mrClose;

  LoadList;
end;

procedure TContactsBulkDelWin.LoadList;
var
  T, H, C: PContacto;
  it: TListItem;
begin
  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    if (U = nil) or (U^.ContactTail = nil) then Exit;

    T := U^.ContactTail;
    H := T^.Next;
    C := H;
    repeat
      it := LV.Items.Add;
      it.Caption := C^.Nombre;
      it.SubItems.Add(C^.Email);
      it.Checked := False;
      C := C^.Next;
    until C = H;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure TContactsBulkDelWin.SelectAll(Sender: TObject);
var i: Integer;
begin
  for i := 0 to LV.Items.Count-1 do LV.Items[i].Checked := True;
end;

procedure TContactsBulkDelWin.ClearAll(Sender: TObject);
var i: Integer;
begin
  for i := 0 to LV.Items.Count-1 do LV.Items[i].Checked := False;
end;

function TContactsBulkDelWin.DeleteByEmail(const CEmail: string): Boolean;
var
  T, H, C, Prev: PContacto;
  function EqualCI(const A,B:string):Boolean; inline; begin Result := LowerCase(A)=LowerCase(B); end;
begin
  Result := False;
  if (Self.U = nil) or (Self.U^.ContactTail = nil) then Exit;

  T := Self.U^.ContactTail; H := T^.Next; Prev := T; C := H;
  repeat
    if EqualCI(C^.Email, CEmail) then
    begin
      if C = C^.Next then
        Self.U^.ContactTail := nil
      else
      begin
        Prev^.Next := C^.Next;
        if Self.U^.ContactTail = C then Self.U^.ContactTail := Prev;
      end;
      Dispose(C);
      Exit(True);
    end;
    Prev := C; C := C^.Next;
  until C = H;
end;

procedure TContactsBulkDelWin.DeleteChecked(Sender: TObject);
var
  i, countSel, removed: Integer;
  emails: TStringList;
begin
  if (U = nil) then Exit;

  emails := TStringList.Create;
  try
    for i := 0 to LV.Items.Count-1 do
      if LV.Items[i].Checked and (LV.Items[i].SubItems.Count > 0) then
        emails.Add(LV.Items[i].SubItems[0]);

    countSel := emails.Count;
    if countSel = 0 then begin ShowMessage('Selecciona al menos un contacto.'); Exit; end;

    if MessageDlg('Confirmar',
                  Format('¿Eliminar %d contacto(s) seleccionados?', [countSel]),
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;

    removed := 0;
    for i := 0 to emails.Count-1 do
      if DeleteByEmail(emails[i]) then Inc(removed);

    ShowMessage(Format('Eliminados: %d', [removed]));
    LoadList;
  finally
    emails.Free;
  end;
end;

{--- Handlers de Form3 ---}

procedure TForm3.AfterConstruction;
begin
  inherited AfterConstruction;
  Button1.Caption  := 'Bandeja de Entrada';
  Button2.Caption  := 'Enviar Correo';
  Button3.Caption  := 'Papelera';
  Button4.Caption  := 'Contactos';
  Button5.Caption  := 'Programar Correo';
  Button6.Caption  := 'Correos Programados';
  Button7.Caption  := 'Actualizar Perfil';
  Button8.Caption  := 'Generar Reportes';
  Button9.Caption  := 'Cerrar Sesión';
  Button10.Caption := 'Borradores (AVL)';
  Button11.Caption := 'Favoritos (Árbol B)';
  Button13.Caption := 'Eliminar Contactos';

  Button1.OnClick  := @Button1Click;
  Button2.OnClick  := @Button2Click;
  Button3.OnClick  := @Button3Click;
  Button4.OnClick  := @Button4Click;
  Button5.OnClick  := @Button5Click;
  Button6.OnClick  := @Button6Click;
  Button7.OnClick  := @Button7Click;
  Button8.OnClick  := @Button8Click;
  Button9.OnClick  := @Button9Click;
  if Assigned(Button10) then Button10.OnClick := @Button10Click;
  if Assigned(Button11) then Button11.OnClick := @Button11Click;
  if Assigned(Button13) then Button13.OnClick := @Button13Click;
end;

procedure TForm3.Button1Click(Sender: TObject);
var F: TInboxWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TInboxWin.CreateForUser(Self, CurrentUser);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button10Click(Sender: TObject);
var W: TDraftsWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  if Drafts = nil then Drafts := TDraftAVL.Create;
  W := TDraftsWin.CreateSimple(Self);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TForm3.Button11Click(Sender: TObject);
var W: TFavoritesWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  W := TFavoritesWin.CreateForUser(Self, CurrentUser);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TForm3.Button12Click(Sender: TObject);
var dir: string;
begin
  // Abrir carpeta de Reportes (útil para DOT/PNG/CSV)
  dir := IncludeTrailingPathDelimiter(GetCurrentDir) + 'Reportes';
  if not DirectoryExists(dir) then CreateDir(dir);
  if not OpenDocument(dir) then
    ShowMessage('Carpeta: ' + dir);
end;

procedure TForm3.Button13Click(Sender: TObject);
var W: TContactsBulkDelWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  if (CurrentUser^.ContactTail = nil) then begin SafeMsg('No hay contactos.'); Exit; end;
  W := TContactsBulkDelWin.CreateForUser(Self, CurrentUser);
  try
    W.ShowModal;
  finally
    W.Free;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var F: TSendWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TSendWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button3Click(Sender: TObject);
var F: TTrashWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TTrashWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button4Click(Sender: TObject);
var F: TContactsWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TContactsWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button5Click(Sender: TObject);
var F: TProgWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TProgWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button6Click(Sender: TObject);
var F: TProgListWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TProgListWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button7Click(Sender: TObject);
var F: TProfileWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TProfileWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button8Click(Sender: TObject);
var F: TReportsWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TReportsWin.CreateSimple(Self);
  try F.ShowModal; finally F.Free; end;
end;

procedure TForm3.Button9Click(Sender: TObject);
begin
  if (CurrentUser <> nil) then
    LoginAudit_Add(CurrentUser^.Email, 'LOGOUT');

  CurrentUser := nil;
  if Assigned(Form1) then
  begin
    Form1.Edit1.Clear; Form1.Edit2.Clear; Form1.Show;
  end
  else
  begin
    Application.CreateForm(TForm1, Form1);
    Form1.Show;
  end;
  Self.Hide;
end;

end.

