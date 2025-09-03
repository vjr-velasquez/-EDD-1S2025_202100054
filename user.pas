unit user;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Grids, LCLIntf, Process;

type
  { TForm3 }
  TForm3 = class(TForm)
    Button1: TButton; // Bandeja de Entrada
    Button2: TButton; // Enviar Correo
    Button3: TButton; // Papelera
    Button4: TButton; // Contactos
    Button5: TButton; // Programar Correo
    Button6: TButton; // Correos Programados
    Button7: TButton; // Actualizar Perfil
    Button8: TButton; // Generar Reportes
    Button9: TButton; // Cerrar Sesión
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
  protected
    procedure AfterConstruction; override;
  private
    procedure SafeMsg(const S: string);
    function ContainsTextCI(const Haystack, Needle: string): Boolean;
    function ParseDate(const S: string; out D: TDateTime): Boolean;
    procedure AppendInbox(Dest: Pointer; const ARemitente, AAsunto, AMensaje: string;
                          const AFecha: TDateTime; AProgramado: Boolean; const AEstado: string);
    procedure PushTrash(U: Pointer; const M);
    procedure IncRel(const FromEmail, ToEmail: string);
  public
  end;

var
  Form3: TForm3;

implementation

uses
  logeo; // aquí están las estructuras y variables globales

{$R *.lfm}

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

function TForm3.ContainsTextCI(const Haystack, Needle: string): Boolean;
begin
  Result := Pos(LowerCase(Needle), LowerCase(Haystack)) > 0;
end;

function TForm3.ParseDate(const S: string; out D: TDateTime): Boolean;
begin
  Result := (Trim(S) <> '') and TryStrToDateTime(S, D);
end;

procedure TForm3.IncRel(const FromEmail, ToEmail: string);
var
  C, N: PRel;
  function EqualCI(const A, B: string): Boolean; inline;
  begin
    Result := LowerCase(A) = LowerCase(B);
  end;
begin
  C := RelHead;
  while C <> nil do
  begin
    if EqualCI(C^.FromEmail, FromEmail) and EqualCI(C^.ToEmail, ToEmail) then
    begin
      Inc(C^.Count);
      Exit;
    end;
    C := C^.Next;
  end;
  New(N);
  N^.FromEmail := FromEmail;
  N^.ToEmail := ToEmail;
  N^.Count := 1;
  N^.Next := RelHead;
  RelHead := N;
end;

procedure TForm3.AppendInbox(Dest: Pointer; const ARemitente, AAsunto, AMensaje: string;
  const AFecha: TDateTime; AProgramado: Boolean; const AEstado: string);
var
  DU: PUsuario;
  N: PMail;
begin
  if Dest = nil then Exit;
  DU := PUsuario(Dest);
  New(N);
  N^.Id := NextMailId; Inc(NextMailId);
  N^.Remitente := ARemitente;
  N^.Asunto := AAsunto;
  N^.Mensaje := AMensaje;
  N^.Fecha := AFecha;
  N^.Programado := AProgramado;
  N^.Estado := AEstado;
  N^.Prev := DU^.InboxTail;
  N^.Next := nil;
  if DU^.InboxTail <> nil then
    DU^.InboxTail^.Next := N
  else
    DU^.InboxHead := N;
  DU^.InboxTail := N;
end;

procedure TForm3.PushTrash(U: Pointer; const M);
var
  DU: PUsuario;
  N: PTrash;
  TM: TMail absolute M;
begin
  DU := PUsuario(U);
  if DU = nil then Exit;
  New(N);
  N^.Mail := TM;
  N^.Next := DU^.TrashTop;
  DU^.TrashTop := N;
end;

{================= Subventanas =================}

type
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

  TSendWin = class(TForm)
  private
    edtPara, edtAsunto: TEdit;
    memoMsg: TMemo;
    btnEnviar, btnCerrar: TButton;
    procedure SendDo(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

  TProgWin = class(TForm)
  private
    edtPara, edtAsunto, edtFecha: TEdit;
    memoMsg: TMemo;
    btnProgramar, btnCerrar: TButton;
    procedure ProgEnqueue(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

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

  TProfileWin = class(TForm)
  private
    lblNom, lblUsu, lblTel, lblPass: TLabel;
    edtNom, edtUsu, edtTel, edtPass: TEdit;
    btnGuardar, btnCerrar: TButton;
    procedure SaveProfile(Sender: TObject);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

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
    lblHint: TLabel;
    function ReportDir: string;
    function SafeEmail(const S: string): string;
    function EscapeDOT(const S: string): string;
    procedure GenReports(Sender: TObject);
    procedure OpenMatrix(Sender: TObject);
    // DOTs de todas las estructuras:
    procedure ExportAllDOTs(Sender: TObject);
    procedure RenderAllPNGs(Sender: TObject);
    procedure WriteDOT_Inbox(U: PUsuario; const Path: string);
    procedure WriteDOT_Programados(U: PUsuario; const Path: string);
    procedure WriteDOT_Papelera(U: PUsuario; const Path: string);
    procedure WriteDOT_Contactos(U: PUsuario; const Path: string);
    procedure WriteDOT_Usuarios(const Path: string);
    procedure RenderOnePNG(const DotPath: string);
  public
    constructor CreateSimple(AOwner: TComponent);
  end;

{--- Inbox ---}

function TInboxWin.ContainsTextCI(const Haystack, Needle: string): Boolean;
begin
  Result := Pos(LowerCase(Needle), LowerCase(Haystack)) > 0;
end;

function TInboxWin.ParseDate(const S: string; out D: TDateTime): Boolean;
begin
  Result := (Trim(S) <> '') and TryStrToDateTime(S, D);
end;

constructor TInboxWin.CreateForUser(AOwner: TComponent; AUser: PUsuario);
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
  with LV.Columns.Add do begin Caption := 'Id'; Width := 60; end;
  with LV.Columns.Add do begin Caption := 'Remitente'; Width := 180; end;
  with LV.Columns.Add do begin Caption := 'Fecha/Hora'; Width := 160; end;
  with LV.Columns.Add do begin Caption := 'Asunto'; Width := 330; end;
  with LV.Columns.Add do begin Caption := 'Estado'; Width := 90; end;
  with LV.Columns.Add do begin Caption := 'Prog'; Width := 60; end;

  edtBuscar.OnChange := @CargarLista; cmbCampo.OnChange := @CargarLista; cmbEstado.OnChange := @CargarLista; chkProg.OnChange := @CargarLista;

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
        if M^.Programado then it.SubItems.Add('Sí') else it.SubItems.Add('No');
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

  Form3.PushTrash(U, backup);
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

  Form3.AppendInbox(Dest, CurrentUser^.Email, asunto, mensaje, Now, False, 'nuevo');
  Form3.IncRel(CurrentUser^.Email, Dest^.Email);
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
  N^.FechaProg := dt;  // 00:00 de ese día
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
  with LV.Columns.Add do begin Caption := 'Id'; Width := 60; end;
  with LV.Columns.Add do begin Caption := 'Remitente'; Width := 180; end;
  with LV.Columns.Add do begin Caption := 'Para'; Width := 200; end;
  with LV.Columns.Add do begin Caption := 'Prog. Fecha'; Width := 180; end;
  with LV.Columns.Add do begin Caption := 'Asunto'; Width := 180; end;

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
      Form3.AppendInbox(Dest, C^.Remitente, C^.Asunto, C^.Mensaje, C^.FechaProg, True, 'nuevo');
      Form3.IncRel(C^.Remitente, C^.Destinatario);
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
  with LV.Columns.Add do begin Caption := 'Id'; Width := 60; end;
  with LV.Columns.Add do begin Caption := 'Remitente'; Width := 180; end;
  with LV.Columns.Add do begin Caption := 'Fecha/Hora'; Width := 160; end;
  with LV.Columns.Add do begin Caption := 'Asunto'; Width := 360; end;

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

  Form3.AppendInbox(CurrentUser, M.Remitente, M.Asunto, M.Mensaje, Now, M.Programado, 'nuevo');
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

{--- Contactos (lista circular) ---}

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
  with LV.Columns.Add do begin Caption := 'Nombre'; Width := 260; end;
  with LV.Columns.Add do begin Caption := 'Email'; Width := 360; end;

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

  lblHint := TLabel.Create(Self); lblHint.Parent := Self;
  lblHint.Caption := 'Archivos en "Reportes". Para PNGs instala Graphviz: sudo apt install graphviz -y';
  lblHint.Left := 24; lblHint.Top := 120; lblHint.AutoSize := True;
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

procedure TReportsWin.RenderAllPNGs(Sender: TObject);
var
  dir, safe: string;
begin
  dir := ReportDir; safe := SafeEmail(CurrentUser^.Email);

  // Si no existen, primero los exportamos
  if not FileExists(dir + PathDelim + 'inbox_' + safe + '.dot') then ExportAllDOTs(nil);

  RenderOnePNG(dir + PathDelim + 'inbox_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'programados_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'papelera_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'contactos_' + safe + '.dot');
  RenderOnePNG(dir + PathDelim + 'usuarios.dot');

  ShowMessage('PNGs generados');
end;

{--- Handlers de Form3 ---}

procedure TForm3.AfterConstruction;
begin
  inherited AfterConstruction;
  Button1.Caption := 'Bandeja de Entrada';
  Button2.Caption := 'Enviar Correo';
  Button3.Caption := 'Papelera';
  Button4.Caption := 'Contactos';
  Button5.Caption := 'Programar Correo';
  Button6.Caption := 'Correos Programados';
  Button7.Caption := 'Actualizar Perfil';
  Button8.Caption := 'Generar Reportes';
  Button9.Caption := 'Cerrar Sesión';

  Button1.OnClick := @Button1Click;
  Button2.OnClick := @Button2Click;
  Button3.OnClick := @Button3Click;
  Button4.OnClick := @Button4Click;
  Button5.OnClick := @Button5Click;
  Button6.OnClick := @Button6Click;
  Button7.OnClick := @Button7Click;
  Button8.OnClick := @Button8Click;
  Button9.OnClick := @Button9Click;
end;

procedure TForm3.Button1Click(Sender: TObject);
var F: TInboxWin;
begin
  if CurrentUser = nil then begin SafeMsg('Inicie sesión.'); Exit; end;
  F := TInboxWin.CreateForUser(Self, CurrentUser);
  try F.ShowModal; finally F.Free; end;
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

