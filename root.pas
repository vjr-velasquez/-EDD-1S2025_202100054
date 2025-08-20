unit root;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fpjson, jsonparser, logeo;

type
  { TForm2 }
  TForm2 = class(TForm)
    Button1: TButton; // Carga Masiva
    Button2: TButton; // Reportes Usuarios (pendiente)
    Button3: TButton; // Reporte de Relaciones (pendiente)
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

// CARGA MASIVA DESDE JSON
procedure TForm2.Button1Click(Sender: TObject);
var
  OpenDlg: TOpenDialog;
  SL: TStringList;
  RootData, UsuariosData: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  Itm: TJSONObject;
  i: Integer;
  nombre, usuario, email, tel: string;
  cntTotal, cntNuevos, cntDuplicados, cntInvalidos: Integer;
begin
  OpenDlg := TOpenDialog.Create(Self);
  try
    OpenDlg.Title := 'Seleccionar archivo JSON de usuarios';
    OpenDlg.Filter := 'Archivos JSON|*.json';
    OpenDlg.Options := OpenDlg.Options + [ofFileMustExist, ofEnableSizing];

    if not OpenDlg.Execute then Exit;

    SL := TStringList.Create;
    try
      try
        SL.LoadFromFile(OpenDlg.FileName);
      except
        on E: Exception do
        begin
          ShowMessage('No se pudo leer el archivo: ' + E.Message);
          Exit;
        end;
      end;

      try
        RootData := GetJSON(SL.Text);
      except
        on E: Exception do
        begin
          ShowMessage('JSON inválido: ' + E.Message);
          Exit;
        end;
      end;

      try
        if RootData.JSONType <> jtObject then
        begin
          ShowMessage('El JSON debe tener un objeto raíz.');
          Exit;
        end;

        Obj := TJSONObject(RootData);

        if not Obj.Find('usuarios', UsuariosData) then
        begin
          ShowMessage('El JSON no contiene la clave "usuarios".');
          Exit;
        end;

        if UsuariosData.JSONType <> jtArray then
        begin
          ShowMessage('"usuarios" debe ser un arreglo.');
          Exit;
        end;

        Arr := TJSONArray(UsuariosData);

        cntTotal := Arr.Count;
        cntNuevos := 0; cntDuplicados := 0; cntInvalidos := 0;

        for i := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[i].JSONType <> jtObject then
          begin
            Inc(cntInvalidos);
            Continue;
          end;

          Itm := Arr.Objects[i];
          nombre  := Itm.Get('nombre',  '');
          usuario := Itm.Get('usuario', '');
          email   := Itm.Get('email',   '');
          tel     := Itm.Get('telefono','');

          if Trim(email) = '' then
          begin
            Inc(cntInvalidos);
            Continue;
          end;

          // Duplicado por email -> omitir
          if BuscarUsuarioPorEmail(email) <> nil then
          begin
            Inc(cntDuplicados);
            Continue;
          end;

          // Insertar con password por defecto (ajústalo si quieres)
          AgregarUsuario(nombre, usuario, email, tel, 'edd2025');
          Inc(cntNuevos);
        end;

        ShowMessage(Format(
          'Carga masiva finalizada.' + LineEnding +
          'Total en archivo: %d' + LineEnding +
          'Agregados: %d' + LineEnding +
          'Duplicados: %d' + LineEnding +
          'Inválidos: %d',
          [cntTotal, cntNuevos, cntDuplicados, cntInvalidos]
        ));

      finally
        RootData.Free;
      end;

    finally
      SL.Free;
    end;

  finally
    OpenDlg.Free;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  // (pendiente) Reportes Usuarios
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  // (pendiente) Reporte de Relaciones
end;

end.

