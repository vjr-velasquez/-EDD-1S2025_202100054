unit login;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  // <<<<<<<<<<<<<< CORRECCIÓN 1: La declaración de variables locales debe ir aquí
  Form3: TForm3;
begin
  // Verificación de campos vacíos
  if (Edit1.Text = '') or (Edit2.Text = '') then
  begin
    ShowMessage('Por favor, ingrese su email y contraseña.');
    Exit;
  end;

  // Verificación de credenciales de Root
  if (Edit1.Text = 'root@edd.com') and (Edit2.Text = 'root123') then
  begin
    ShowMessage('¡Bienvenido, usuario Root!');

    // Ocultar el formulario actual
    Form1.Hide;

    // <<<<<<<<<<<<<< CORRECCIÓN 2: Ahora puedes usar la variable 'Form3' sin el 'var'
    Form3 := TForm3.Create(Application);
    Form3.Show;

  end
  else
  begin
    ShowMessage('Credenciales incorrectas. Intente de nuevo.');
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin


end;

procedure TForm1.Edit1Change(Sender: TObject);
begin

end;

procedure TForm1.Edit2Change(Sender: TObject);
begin

end;

end.

