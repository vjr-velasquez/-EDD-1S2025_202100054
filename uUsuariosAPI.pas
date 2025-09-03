unit uUsuariosAPI;

{$mode ObjFPC}{$H+}

interface

type
  TUserExistsFunc = function (const Email: String): Boolean;

procedure UsersAPI_BindExists(Func: TUserExistsFunc);
function  UsersAPI_UserExists(const Email: String): Boolean;

implementation

var
  GUserExists: TUserExistsFunc = nil;

procedure UsersAPI_BindExists(Func: TUserExistsFunc);
begin
  GUserExists := Func;
end;

function UsersAPI_UserExists(const Email: String): Boolean;
begin
  if Assigned(GUserExists) then
    Result := GUserExists(Email)
  else
    Result := False; // si nadie nos enlaza, decimos que no existe
end;

end.
