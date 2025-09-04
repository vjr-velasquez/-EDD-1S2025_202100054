program fase1edd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // LCL primero
  Forms,
  logeo,  // Form1 (login) y estructuras
  root,   // Form2 (root)
  user, uUsuariosAPI, uUsuariosAPI_VictorAdapter, uComunidades,
  comunidad;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  UsersAPI_BindExists(@Victor_UserExists);
  Application.CreateForm(TForm1, Form1); // solo auto-creamos el login
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.

