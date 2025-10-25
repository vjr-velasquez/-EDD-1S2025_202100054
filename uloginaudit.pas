unit uLoginAudit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, fpjson;

type
  TLoginEvent = record
    Email: string;
    Timestamp: TDateTime;
    Status: string; // 'LOGIN' o 'LOGOUT' (o cualquier etiqueta que uses)
  end;

procedure LoginAudit_Add(const AEmail, AStatus: string);
procedure LoginAudit_Clear;
function  LoginAudit_Count: Integer;
function  LoginAudit_Get(const Index: Integer): TLoginEvent;

// Helpers para UI/Export
procedure LoginAudit_FillListView(LV: TListView);
procedure LoginAudit_ToJSONArray(var Arr: TJSONArray);

implementation

var
  GAudit: array of TLoginEvent = nil;
  GAuditCount: Integer = 0;

procedure LoginAudit_Add(const AEmail, AStatus: string);
begin
  if GAuditCount >= Length(GAudit) then
    SetLength(GAudit, GAuditCount + 64); // crecer en bloques

  GAudit[GAuditCount].Email     := AEmail;
  GAudit[GAuditCount].Timestamp := Now;
  GAudit[GAuditCount].Status    := AStatus;
  Inc(GAuditCount);
end;

procedure LoginAudit_Clear;
begin
  GAudit := nil;
  GAuditCount := 0;
end;

function LoginAudit_Count: Integer;
begin
  Result := GAuditCount;
end;

function LoginAudit_Get(const Index: Integer): TLoginEvent;
begin
  if (Index >= 0) and (Index < GAuditCount) then
    Result := GAudit[Index]
  else
  begin
    Result.Email := '';
    Result.Timestamp := 0;
    Result.Status := '';
  end;
end;

procedure LoginAudit_FillListView(LV: TListView);
var
  i: Integer;
  it: TListItem;
  E: TLoginEvent;
begin
  if LV = nil then Exit;

  LV.Items.BeginUpdate;
  try
    LV.Items.Clear;
    for i := 0 to GAuditCount - 1 do
    begin
      E := GAudit[i];
      it := LV.Items.Add;
      it.Caption := FormatDateTime('dd-mm-yy hh:nn:ss', E.Timestamp); // Columna 1
      it.SubItems.Add(E.Email);   // Columna 2
      it.SubItems.Add(E.Status);  // Columna 3
    end;
  finally
    LV.Items.EndUpdate;
  end;
end;

procedure LoginAudit_ToJSONArray(var Arr: TJSONArray);
var
  i: Integer;
  E: TLoginEvent;
  O: TJSONObject;
begin
  Arr := TJSONArray.Create;
  for i := 0 to GAuditCount - 1 do
  begin
    E := GAudit[i];
    O := TJSONObject.Create;
    O.Add('email', E.Email);
    O.Add('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', E.Timestamp));
    O.Add('status', E.Status);
    Arr.Add(O);
  end;
end;

end.

