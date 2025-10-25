program Tarea4GrafoNoDirigido;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, fgl, Process;

function TimeStampStr: string;
begin
  Result := FormatDateTime('yyyymmdd_hhnnss', Now); // 20251015_144259
end;

function JoinName(const Base, ExtWithDot, Stamp: string): string;
var clean: string;
begin
  clean := Trim(Base);
  if clean = '' then clean := 'grafo';
  Result := clean + '_' + Stamp + ExtWithDot;
end;

type
  TEdge = record
    ToIdx   : Integer;
    HasW    : Boolean;
    W       : Integer;
  end;
  PEdge = ^TEdge;

  TStringIntMap = specialize TFPGMap<string, Integer>;
  TEdgePtrList  = specialize TFPGList<PEdge>;

  { TGraph: grafo no dirigido con lista de adyacencia mediante punteros }
  TGraph = class
  private
    FCities : TStringIntMap;
    FAdj    : array of TEdgePtrList;

    function  GetOrAddCityIndex(const NameRaw: string): Integer;
    procedure EnsureAdjSize(N: Integer);
    function  HasEdge(u, v: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function  AddCity(const NameRaw: string): Integer;
    procedure AddConnection(const CityA, CityB: string; const Weight: Integer; const WithWeight: Boolean);
    procedure PrintAdjacency;
    procedure SaveDOT(const FileName: string; const WithWeights: Boolean = True);
    function  CityName(Index: Integer): string;
    function  CityCount: Integer;
  end;

{ ==== TGraph ==== }

constructor TGraph.Create;
begin
  inherited Create;
  FCities := TStringIntMap.Create;
  FCities.Sorted := True;
  SetLength(FAdj, 0);
end;

destructor TGraph.Destroy;
var
  i, k: Integer;
  p: PEdge;
begin
  for i := 0 to High(FAdj) do
  begin
    if FAdj[i] <> nil then
    begin
      for k := 0 to FAdj[i].Count - 1 do
      begin
        p := FAdj[i][k];
        if p <> nil then Dispose(p);
      end;
      FAdj[i].Free;
    end;
  end;
  FCities.Free;
  inherited Destroy;
end;

procedure TGraph.EnsureAdjSize(N: Integer);
var
  i, oldLen: Integer;
begin
  if Length(FAdj) >= N then Exit;
  oldLen := Length(FAdj);
  SetLength(FAdj, N);
  for i := oldLen to High(FAdj) do
    FAdj[i] := TEdgePtrList.Create;
end;

function NormalizeCityName(const S: string): string;
begin
  Result := Trim(S);
end;

function TGraph.CityName(Index: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCities.Count - 1 do
    if FCities.Data[i] = Index then
    begin
      Result := FCities.Keys[i];
      Exit;
    end;
end;

function TGraph.CityCount: Integer;
begin
  Result := FCities.Count;
end;

function TGraph.GetOrAddCityIndex(const NameRaw: string): Integer;
var
  k: string;
  idx: Integer;
begin
  k := NormalizeCityName(NameRaw);
  if k = '' then
    raise Exception.Create('El nombre de ciudad no puede ser vacío.');

  if FCities.Find(k, idx) then
    Exit(FCities.Data[idx]);

  Result := FCities.Count;
  FCities.Add(k, Result);
  EnsureAdjSize(FCities.Count);
end;

function TGraph.AddCity(const NameRaw: string): Integer;
begin
  Result := GetOrAddCityIndex(NameRaw);
end;

function TGraph.HasEdge(u, v: Integer): Boolean;
var
  k: Integer;
  p: PEdge;
begin
  Result := False;
  for k := 0 to FAdj[u].Count - 1 do
  begin
    p := FAdj[u][k];
    if (p <> nil) and (p^.ToIdx = v) then
      Exit(True);
  end;
end;

procedure TGraph.AddConnection(const CityA, CityB: string; const Weight: Integer; const WithWeight: Boolean);
var
  u, v: Integer;
  p: PEdge;
begin
  u := GetOrAddCityIndex(CityA);
  v := GetOrAddCityIndex(CityB);
  if u = v then
    raise Exception.Create('No se permite conectar una ciudad consigo misma.');

  if not HasEdge(u, v) then
  begin
    New(p);
    p^.ToIdx := v; p^.HasW := WithWeight; p^.W := Weight;
    FAdj[u].Add(p);
  end;

  if not HasEdge(v, u) then
  begin
    New(p);
    p^.ToIdx := u; p^.HasW := WithWeight; p^.W := Weight;
    FAdj[v].Add(p);
  end;
end;

procedure TGraph.PrintAdjacency;
var
  i, k: Integer;
  p: PEdge;
  line, neighbor: string;
  parts: TStringList;
begin
  Writeln('=== Lista de adyacencia (no dirigido) ===');
  for i := 0 to FCities.Count - 1 do
  begin
    line := CityName(i) + ' -> ';
    if FAdj[i].Count = 0 then
      line += '(sin vecinos)'
    else
    begin
      parts := TStringList.Create;
      try
        parts.Delimiter := ',';
        parts.StrictDelimiter := True;
        for k := 0 to FAdj[i].Count - 1 do
        begin
          p := FAdj[i][k];
          if p <> nil then
          begin
            neighbor := CityName(p^.ToIdx);
            if p^.HasW then
              parts.Add(Format('%s[%d]', [neighbor, p^.W]))
            else
              parts.Add(neighbor);
          end;
        end;
        line += StringReplace(parts.CommaText, ',', ', ', [rfReplaceAll]);
      finally
        parts.Free;
      end;
    end;
    Writeln(line);
  end;
end;

procedure TGraph.SaveDOT(const FileName: string; const WithWeights: Boolean);
var
  sl: TStringList;
  i, j, k: Integer;
  p: PEdge;
  aName, bName: string;
  hasA, hasB, hasC, hasD: Boolean;
begin
  sl := TStringList.Create;
  try
    sl.Add('graph G {');
    // Top-to-Bottom (por defecto), y separaciones agradables
    sl.Add('  layout=dot;');            // usa el layout jerárquico
    sl.Add('  nodesep=0.5;');           // separación horizontal entre nodos de un mismo nivel
    sl.Add('  ranksep=0.8;');           // separación entre niveles
    sl.Add('  margin=0.1;');
    sl.Add('  node [shape=circle, fontsize=16, width=0.9, fixedsize=true, style=""];');
    sl.Add('  edge [fontsize=12];');

    // Declarar nodos (por si quedan aislados)
    for i := 0 to FCities.Count - 1 do
      sl.Add(Format('  "%s";', [CityName(i)]));

    // Escribir aristas sin duplicar
    for i := 0 to FCities.Count - 1 do
    begin
      aName := CityName(i);
      for k := 0 to FAdj[i].Count - 1 do
      begin
        p := FAdj[i][k];
        if p <> nil then
        begin
          j := p^.ToIdx;
          if i < j then
          begin
            bName := CityName(j);
            if WithWeights and p^.HasW then
              sl.Add(Format('  "%s" -- "%s" [label="%d"];', [aName, bName, p^.W]))
            else
              sl.Add(Format('  "%s" -- "%s";', [aName, bName]));
          end;
        end;
      end;
    end;

    // Si existen A,B,C,D, forzamos B y C al mismo nivel y D debajo de B naturalmente
    hasA := FCities.IndexOf('A') <> -1;
    hasB := FCities.IndexOf('B') <> -1;
    hasC := FCities.IndexOf('C') <> -1;
    hasD := FCities.IndexOf('D') <> -1;

    if hasB and hasC then
      sl.Add('  {rank=same; "B"; "C";}');    // B y C en la misma fila

    // Opcional: empujar A hacia arriba
    if hasA then
      sl.Add('  {rank=min; "A";}');          // A en el nivel superior (si existe)

    // (D caerá un nivel debajo de B por la arista B--D)

    sl.Add('}');
    sl.SaveToFile(FileName);
    Writeln('DOT guardado en: ', FileName);
  finally
    sl.Free;
  end;
end;


{ ==== Utilidades de menú ==== }

procedure PressEnter;
begin
  Writeln;
  Write('Presiona ENTER para continuar...'); ReadLn;
end;

procedure RenderWithGraphviz(const DotFile, OutPng: string);
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotFile);
    P.Parameters.Add('-o');
    P.Parameters.Add(OutPng);
    P.Options := [poUsePipes];
    try
      P.Execute;
      Writeln('Imagen generada: ', OutPng);
    except
      on E: Exception do
        Writeln('No se pudo ejecutar Graphviz (dot). Generé el .dot; compílalo manualmente con: dot -Tpng ', DotFile, ' -o ', OutPng);
    end;
  finally
    P.Free;
  end;
end;

procedure DemoSeed(g: TGraph);
begin
  g.AddConnection('A', 'B', 0, False);
  g.AddConnection('A', 'C', 0, False);
  g.AddConnection('B', 'D', 0, False);
end;

{ ==== Programa principal ==== }

var
  g: TGraph;
  opt: Integer;
  a, b: string;
  wStr: string;
  wVal: Integer;
  withW: Boolean;
  s: string;
  dotName, pngName: string;
begin
  g := TGraph.Create;
  try
    repeat
      Writeln;
      Writeln('===== Tarea #4 - Grafo No Dirigido (Lista de Adyacencia) =====');
      Writeln('1) Agregar ciudad');
      Writeln('2) Agregar conexión (arista)');
      Writeln('3) Mostrar lista de adyacencia');
      Writeln('4) Exportar Graphviz');
      Writeln('5) Cargar ejemplo (A--B, A--C, B--D)');
      Writeln('0) Salir');
      Write('Opción: ');
      ReadLn(s);
      if not TryStrToInt(Trim(s), opt) then opt := -1;

      case opt of
        1: begin
             Write('Nombre de la ciudad: ');
             ReadLn(a);
             g.AddCity(a);
             Writeln('Ciudad agregada (o ya existente). Total = ', g.CityCount);
             PressEnter;
           end;
        2: begin
             Write('Ciudad A: '); ReadLn(a);
             Write('Ciudad B: '); ReadLn(b);
             Write('¿Con peso? [s/N]: '); ReadLn(wStr);
             withW := (LowerCase(Trim(wStr)) = 's');
             if withW then
             begin
               Write('Peso (entero): ');
               ReadLn(wStr);
               if not TryStrToInt(Trim(wStr), wVal) then
               begin
                 Writeln('Peso inválido, se usará 0.');
                 wVal := 0;
               end;
               g.AddConnection(a, b, wVal, True);
             end
             else
               g.AddConnection(a, b, 0, False);
             Writeln('Conexión agregada.');
             PressEnter;
           end;
        3: begin
             g.PrintAdjacency;
             PressEnter;
           end;
        4: begin
            Write('Nombre base para exportar (vacío = "grafo"): ');
            ReadLn(wStr);
            s := TimeStampStr;  // un solo sello para DOT y PNG
            dotName := JoinName(wStr, '.dot', s);
            pngName := JoinName(wStr, '.png', s);

            g.SaveDOT(dotName, True);
            Writeln('Archivo DOT: ', dotName);

            Write('¿Generar PNG con Graphviz ahora? [s/N]: ');
            ReadLn(wStr);
            if LowerCase(Trim(wStr)) = 's' then
            begin
              RenderWithGraphviz(dotName, pngName);
              Writeln('Archivo PNG: ', pngName);
            end;
            PressEnter;
          end;

        5: begin
             DemoSeed(g);
             Writeln('Ejemplo cargado.');
             PressEnter;
           end;
        0: ; // salir
      else
        Writeln('Opción inválida.');
        PressEnter;
      end;
    until opt = 0;
  finally
    g.Free;
  end;
end.
