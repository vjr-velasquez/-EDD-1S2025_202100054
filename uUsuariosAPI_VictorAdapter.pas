unit uUsuariosAPI_VictorAdapter;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, uTypes;  // <-- IMPORTANTE: aquí está BuscarUsuarioPorEmail oficial

// Wrapper con el mismo nombre para no tocar el resto del proyecto:
function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;

// (opcional) helper alternativo si prefieres llamarlo distinto
function GetUsuarioPorEmail(const Email: string): PUsuario;

implementation

function BuscarUsuarioPorEmail(const AEmail: string): PUsuario;
begin
  Result := uTypes.BuscarUsuarioPorEmail(AEmail);
end;

function GetUsuarioPorEmail(const Email: string): PUsuario;
begin
  Result := uTypes.BuscarUsuarioPorEmail(Email);
end;

end.

