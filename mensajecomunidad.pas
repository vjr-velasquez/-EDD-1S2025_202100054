unit mensajeComunidad;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  logeo,         // <-- aquí está CurrentUser
  uComunidades;  // API de comunidades

type
  { TForm5 }
  TForm5 = class(TForm)
    btnpublicar: TButton;
    txtmensaje: TMemo;
    txtcomunidad: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnpublicarClick(Sender: TObject);
    procedure txtcomunidadChange(Sender: TObject);
    procedure txtmensajeChange(Sender: TObject);
  private
  public
  end;

var
  Form5: TForm5;

implementation

{$R *.lfm}

{ TForm5 }

procedure TForm5.txtcomunidadChange(Sender: TObject);
begin
  // opcional
end;

procedure TForm5.btnpublicarClick(Sender: TObject);
var
  nomCom, miEmail, msg: string;
begin
  nomCom  := Trim(txtcomunidad.Text);
  msg     := Trim(txtmensaje.Lines.Text);

  // Obtener el email del usuario logueado desde el unit 'logeo'
  if (logeo.CurrentUser <> nil) then
    miEmail := Trim(logeo.CurrentUser^.Email)
  else
    miEmail := '';

  if miEmail = '' then
  begin
    ShowMessage('No hay sesión activa. Inicia sesión para publicar.');
    Exit;
  end;

  if (nomCom = '') or (msg = '') then
  begin
    ShowMessage('Escribe la comunidad y el mensaje.');
    Exit;
  end;

  if not Comunidades_Existe(nomCom) then
  begin
    ShowMessage('La comunidad "' + nomCom + '" no existe.');
    Exit;
  end;

  if Comunidades_PublicarMensaje(nomCom, miEmail, msg) then
  begin
    ShowMessage('Mensaje publicado en "' + nomCom + '".');
    txtmensaje.Clear;
  end
  else
    ShowMessage('No se pudo publicar (verifica membresía o datos).');
end;

procedure TForm5.txtmensajeChange(Sender: TObject);
begin
  // opcional
end;

end.

