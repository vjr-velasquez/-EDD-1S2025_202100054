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
  user,
  uUsuariosAPI,
  uUsuariosAPI_VictorAdapter,
  uComunidades,
  comunidad,
  uBFavorites,
  uAVL,
  uBSTContacts,
  mensajeComunidad,
  contronLogueo,
  blockchain,
  uLoginAudit,
  uTypes,
  uAVLDrafts, uHashUsers, uMerkle, uBlockchain;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  UsersHash_Init(1024);
  UsersHash_Rebuild;

  // Enlaza la función "existe usuario" del adapter Victor
  //UsersAPI_BindExists(@Victor_UserExists);

  // Crea solo los forms que necesitas al inicio
  Application.CreateForm(TForm1, Form1); // login
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TForm5, Form5);
  Application.CreateForm(TForm6, Form6);

  Application.Run;
end.

