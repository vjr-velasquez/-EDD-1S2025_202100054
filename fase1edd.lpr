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
  user;   // Form3 (usuario estándar)

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1); // solo auto-creamos el login
  Application.Run;
end.

