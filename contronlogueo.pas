unit contronLogueo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

type

  { TForm6 }

  TForm6 = class(TForm)
    btnExport: TButton;
    btnControl: TButton;
    btnRegresar: TButton;
    logueolabel: TLabel;
    procedure btnControlClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
  private
    LV: TListView;
    procedure EnsureListView;
  public
  end;

var
  Form6: TForm6;

implementation

{$R *.lfm}

uses
  root, fpjson, uLoginAudit;

{ TForm6 }

procedure TForm6.EnsureListView;
begin
  if LV <> nil then Exit;

  LV := TListView.Create(Self);
  LV.Parent := Self;
  LV.Align := alClient;
  LV.ViewStyle := vsReport;
  LV.ReadOnly := True;
  LV.RowSelect := True;
  LV.GridLines := True;

  // Columnas: Fecha/Hora | Email | Acción
  with LV.Columns.Add do begin Caption := 'Fecha/Hora'; Width := 160; end;
  with LV.Columns.Add do begin Caption := 'Email'; Width := 260; end;
  with LV.Columns.Add do begin Caption := 'Acción'; Width := 120; end;
end;

procedure TForm6.btnControlClick(Sender: TObject);
begin
  EnsureListView;
  LoginAudit_FillListView(LV);
  if LoginAudit_Count = 0 then
    ShowMessage('Aún no hay eventos de logueo.');
end;

procedure TForm6.btnExportClick(Sender: TObject);
var
  SD: TSaveDialog;
  RootObj: TJSONObject;
  Arr: TJSONArray;
  S: TStringList;
begin
  RootObj := TJSONObject.Create;
  try
    LoginAudit_ToJSONArray(Arr);
    RootObj.Add('auditoria', Arr); // RootObj es dueño de Arr

    SD := TSaveDialog.Create(Self);
    try
      SD.Title := 'Exportar control de logueo (JSON)';
      SD.Filter := 'JSON|*.json|Todos|*.*';
      SD.DefaultExt := 'json';
      SD.FileName := 'control_logueo.json';

      if not SD.Execute then Exit;

      S := TStringList.Create;
      try
        S.Text := RootObj.FormatJSON([foSingleLineArray], 2); // bonito
        S.SaveToFile(SD.FileName);
      finally
        S.Free;
      end;

      ShowMessage('Exportado: ' + SD.FileName);
    finally
      SD.Free;
    end;
  finally
    RootObj.Free;
  end;
end;

procedure TForm6.btnRegresarClick(Sender: TObject);
begin
  root.Form2.Show;
  Self.Hide;
end;

end.

