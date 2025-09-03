unit uUsuariosAPI_VictorAdapter;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, uUsuariosAPI, logeo; // unidad de Victor

function Victor_UserExists(const Email: String): Boolean;

implementation

function Victor_UserExists(const Email: String): Boolean;
begin
  Result := BuscarUsuarioPorEmail(Email) <> nil;
end;

end.
