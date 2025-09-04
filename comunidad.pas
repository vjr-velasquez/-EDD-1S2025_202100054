unit comunidad;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,uComunidades,uUsuariosAPI;


type

  { TForm4 }

  TForm4 = class(TForm)
    btnRegresar: TButton;
    btnCrear: TButton;
    btnAgregar: TButton;
    cajaNombre: TEdit;
    cajaComunidades: TEdit;
    cajaCorreo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnAgregarClick(Sender: TObject);
    procedure btnCrearClick(Sender: TObject);
    procedure btnRegresarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form4: TForm4;

implementation
uses root;

{$R *.lfm}

{ TForm4 }

procedure TForm4.btnCrearClick(Sender: TObject);
var
  nombre: String;
begin
  // ⬇⬇⬇ SI ALGUNO CAMBIA LOS NOMBRES, LEAN AQUI ⬇⬇⬇
  // Reemplaza EditNombre por el NOMBRE REAL de tu TEdit del campo "Nombre"
  nombre := Trim(cajaNombre.Text); // <-- TU TEdit DE "Nombre"

  if Comunidades_Crear(nombre) then
    ShowMessage('Comunidad creada.')
  else
    ShowMessage('No se pudo crear (vacía o ya existe).');
end;

procedure TForm4.btnRegresarClick(Sender: TObject);
begin
  root.Form2.Show;
  Self.Hide;

end;

procedure TForm4.FormCreate(Sender: TObject);
begin

end;

procedure TForm4.btnAgregarClick(Sender: TObject);
var
  nombreCom, email: String;
begin
  // ⬇⬇⬇ AJUSTA ESTOS NOMBRES A TUS EDITS REALES ⬇⬇⬇
  // EditComunidad = TEdit donde se escribe el nombre de la comunidad
  // EditCorreo    = TEdit donde se escribe el email del usuario
  nombreCom := Trim(cajaComunidades.Text); // <-- TU TEdit DE "Comunidad"
  email     := Trim(cajaCorreo.Text);    // <-- TU TEdit DE "Correo"

  if Comunidades_AgregarMiembro(nombreCom, email) then
    ShowMessage('Miembro agregado.')
  else
    ShowMessage('No se pudo agregar (comunidad inexistente / usuario no existe / duplicado).');
end;

end.

