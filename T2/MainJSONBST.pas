program MainJSONBST;

uses
    SysUtils, Classes, Process;

type
    // Definición del nodo del árbol
    PNodo = ^Nodo;
    Nodo = record
        ID: Integer;
        first_name: String;
        last_name: String;
        email: String;
        izquierdo: PNodo;
        derecho: PNodo;
    end;
    
    // Definición del árbol BST
    ArbolBST = record
        raiz: PNodo;
    end;

    // Registro para almacenar datos del JSON
    TPersona = record
        ID: Integer;
        first_name: String;
        last_name: String;
        email: String;
    end;

var
    arbol: ArbolBST;
    nombreArchivo: String;
    Error: Integer;  // Variable para validación de conversiones

// Crear un nuevo nodo
function CrearNodo(persona: TPersona): PNodo;
var
    nuevoNodo: PNodo;
begin
    New(nuevoNodo);
    nuevoNodo^.ID := persona.ID;
    nuevoNodo^.first_name := persona.first_name;
    nuevoNodo^.last_name := persona.last_name;
    nuevoNodo^.email := persona.email;
    nuevoNodo^.izquierdo := nil;
    nuevoNodo^.derecho := nil;
    CrearNodo := nuevoNodo;
end;

// Inicializar el árbol
procedure InicializarArbol(var arbol: ArbolBST);
begin
    arbol.raiz := nil;
end;

// Insertar un nodo en el árbol BST
function InsertarNodo(raiz: PNodo; persona: TPersona): PNodo;
begin
    // Si el árbol está vacío, crear nuevo nodo
    if raiz = nil then
    begin
        InsertarNodo := CrearNodo(persona);
        WriteLn('Nodo con ID ', persona.ID, ' (', persona.first_name, ' ', persona.last_name, ') insertado correctamente.');
    end
    // Si el ID es menor, insertar en el subárbol izquierdo
    else if persona.ID < raiz^.ID then
    begin
        raiz^.izquierdo := InsertarNodo(raiz^.izquierdo, persona);
        InsertarNodo := raiz;
    end
    // Si el ID es mayor, insertar en el subárbol derecho
    else if persona.ID > raiz^.ID then
    begin
        raiz^.derecho := InsertarNodo(raiz^.derecho, persona);
        InsertarNodo := raiz;
    end
    // Si el ID ya existe, no insertar
    else
    begin
        WriteLn('Advertencia: El ID ', persona.ID, ' ya existe en el árbol. No se insertó.');
        InsertarNodo := raiz;
    end;
end;

// Función para extraer valor de un campo JSON
function ExtraerValorJSON(linea, campo: String): String;
var
    inicio, fin: Integer;
    buscar: String;
begin
    buscar := '"' + campo + '":';
    inicio := Pos(buscar, linea);
    if inicio > 0 then
    begin
        inicio := inicio + Length(buscar);
        
        // Saltar espacios en blanco
        while (inicio <= Length(linea)) and (linea[inicio] = ' ') do
            Inc(inicio);
        
        // Si es string (empieza con comillas)
        if linea[inicio] = '"' then
        begin
            Inc(inicio); // Saltar la primera comilla
            fin := inicio;
            while (fin <= Length(linea)) and (linea[fin] <> '"') do
                Inc(fin);
            ExtraerValorJSON := Copy(linea, inicio, fin - inicio);
        end
        // Si es número
        else
        begin
            fin := inicio;
            while (fin <= Length(linea)) and (linea[fin] in ['0'..'9', '-']) do
                Inc(fin);
            ExtraerValorJSON := Copy(linea, inicio, fin - inicio);
        end;
    end
    else
        ExtraerValorJSON := '';
end;

// Cargar datos desde archivo JSON (parser simple)
procedure CargarDesdeJSON(var arbol: ArbolBST; nombreArchivo: String);
var
    archivo: TextFile;
    linea: String;
    persona: TPersona;
    dentroDeObjeto: Boolean;
    contadorPersonas, contadorLineas: Integer;
begin
    if not FileExists(nombreArchivo) then
    begin
        WriteLn('Error: El archivo ', nombreArchivo, ' no existe.');
        WriteLn('Ruta actual: ', GetCurrentDir);
        Exit;
    end;
    
    Assign(archivo, nombreArchivo);
    Reset(archivo);
    
    WriteLn('Cargando datos desde: ', nombreArchivo);
    WriteLn('========================================');
    
    dentroDeObjeto := False;
    contadorPersonas := 0;
    contadorLineas := 0;
    
    // Inicializar registro
    persona.ID := 0;
    persona.first_name := '';
    persona.last_name := '';
    persona.email := '';
    
    while not EOF(archivo) do
    begin
        ReadLn(archivo, linea);
        Inc(contadorLineas);
        linea := Trim(linea);
        
        // Detectar inicio de objeto
        if Pos('{', linea) > 0 then
        begin
            dentroDeObjeto := True;
            // Reinicializar registro para nuevo objeto
            persona.ID := 0;
            persona.first_name := '';
            persona.last_name := '';
            persona.email := '';
        end
        // Detectar fin de objeto
        else if Pos('}', linea) > 0 then
        begin
            if dentroDeObjeto and (persona.ID > 0) then
            begin
                arbol.raiz := InsertarNodo(arbol.raiz, persona);
                Inc(contadorPersonas);
            end;
            dentroDeObjeto := False;
        end
        // Procesar campos dentro del objeto
        else if dentroDeObjeto then
        begin
            if Pos('"id":', linea) > 0 then
            begin
                Val(ExtraerValorJSON(linea, 'id'), persona.ID, Error);
                if Error <> 0 then
                    persona.ID := 0;
            end
            else if Pos('"first_name":', linea) > 0 then
            begin
                persona.first_name := ExtraerValorJSON(linea, 'first_name');
            end
            else if Pos('"last_name":', linea) > 0 then
            begin
                persona.last_name := ExtraerValorJSON(linea, 'last_name');
            end
            else if Pos('"email":', linea) > 0 then
            begin
                persona.email := ExtraerValorJSON(linea, 'email');
            end;
        end;
    end;
    
    Close(archivo);
    WriteLn('========================================');
    WriteLn('Total de registros cargados: ', contadorPersonas);
    WriteLn;
end;

// Recorrido en orden (InOrder)
procedure RecorridoInOrder(raiz: PNodo);
begin
    if raiz <> nil then
    begin
        RecorridoInOrder(raiz^.izquierdo);
        WriteLn('ID: ', raiz^.ID, ' | Nombre: ', raiz^.first_name, ' ', raiz^.last_name, 
                ' | Email: ', raiz^.email);
        RecorridoInOrder(raiz^.derecho);
    end;
end;

// Generar código Graphviz para visualizar el árbol - escribir a archivo
procedure GenerarGraphvizNodosArchivo(raiz: PNodo; var archivo: TextFile);
begin
    if raiz <> nil then
    begin
        // Generar el nodo actual con información completa
        WriteLn(archivo, '    ', raiz^.ID, ' [label="ID: ', raiz^.ID, 
                '\nNombre: ', raiz^.first_name, ' ', raiz^.last_name,
                '\nEmail: ', raiz^.email, '"];');
        
        // Generar conexiones y nodos hijos
        if raiz^.izquierdo <> nil then
        begin
            WriteLn(archivo, '    ', raiz^.ID, ' -> ', raiz^.izquierdo^.ID, ' [color=red, label="L"];');
            GenerarGraphvizNodosArchivo(raiz^.izquierdo, archivo);
        end;
        
        if raiz^.derecho <> nil then
        begin
            WriteLn(archivo, '    ', raiz^.ID, ' -> ', raiz^.derecho^.ID, ' [color=blue, label="R"];');
            GenerarGraphvizNodosArchivo(raiz^.derecho, archivo);
        end;
    end;
end;

// Generar código Graphviz para visualizar el árbol
procedure GenerarGraphvizNodos(raiz: PNodo);
begin
    if raiz <> nil then
    begin
        // Generar el nodo actual con información completa
        WriteLn('    ', raiz^.ID, ' [label="ID: ', raiz^.ID, 
                '\nNombre: ', raiz^.first_name, ' ', raiz^.last_name,
                '\nEmail: ', raiz^.email, '"];');
        
        // Generar conexiones y nodos hijos
        if raiz^.izquierdo <> nil then
        begin
            WriteLn('    ', raiz^.ID, ' -> ', raiz^.izquierdo^.ID, ' [color=red, label="L"];');
            GenerarGraphvizNodos(raiz^.izquierdo);
        end;
        
        if raiz^.derecho <> nil then
        begin
            WriteLn('    ', raiz^.ID, ' -> ', raiz^.derecho^.ID, ' [color=blue, label="R"];');
            GenerarGraphvizNodos(raiz^.derecho);
        end;
    end;
end;

procedure GenerarGraphviz(arbol: ArbolBST);
var
    archivoSalida: TextFile;
    nombreSalida, respuesta: String;
begin
    nombreSalida := 'arbol_bst.dot';
    
    WriteLn('========== GENERANDO CÓDIGO GRAPHVIZ ==========');
    WriteLn('Archivo de salida: ', nombreSalida);
    WriteLn;
    
    // Mostrar en pantalla
    WriteLn('digraph BST {');
    WriteLn('    node [shape=box, style=rounded, fontname="Arial"];');
    WriteLn('    edge [fontname="Arial"];');
    WriteLn('    rankdir=TB;');
    WriteLn('    bgcolor=lightgray;');
    WriteLn('    ');
    
    if arbol.raiz <> nil then
        GenerarGraphvizNodos(arbol.raiz)
    else
        WriteLn('    // Árbol vacío');
    
    WriteLn('}');
    WriteLn('===============================================');
    
    // Guardar en archivo
    {$I-}  // Desactivar verificación de E/S
    Assign(archivoSalida, nombreSalida);
    Rewrite(archivoSalida);
    
    if IOResult = 0 then
    begin
        WriteLn(archivoSalida, 'digraph BST {');
        WriteLn(archivoSalida, '    node [shape=box, style=rounded, fontname="Arial"];');
        WriteLn(archivoSalida, '    edge [fontname="Arial"];');
        WriteLn(archivoSalida, '    rankdir=TB;');
        WriteLn(archivoSalida, '    bgcolor=lightgray;');
        WriteLn(archivoSalida, '    ');
        
        // Escribir los nodos al archivo
        if arbol.raiz <> nil then
            GenerarGraphvizNodosArchivo(arbol.raiz, archivoSalida)
        else
            WriteLn(archivoSalida, '    // Árbol vacío');
        
        WriteLn(archivoSalida, '}');
        Close(archivoSalida);
        
        WriteLn('Archivo Graphviz guardado como: ', nombreSalida);
    end
    else
        WriteLn('Error al crear el archivo Graphviz.');
    
    {$I+}  // Reactivar verificación de E/S
    
    WriteLn('Para generar la imagen usar: dot -Tpng arbol_bst.dot -o arbol_bst.png');
    
    // Opción: generar imagen automáticamente
    Write('¿Desea generar y abrir la imagen automáticamente? (s/n): ');
    ReadLn(respuesta);
    if (respuesta = 's') or (respuesta = 'S') or (respuesta = 'si') or (respuesta = 'SI') then
    begin
        WriteLn('Generando imagen...');
        // Ejecutar comando para generar imagen
        if ExecuteProcess('dot', '-Tpng arbol_bst.dot -o arbol_bst.png') = 0 then
        begin
            WriteLn('Imagen generada: arbol_bst.png');
            WriteLn('Abriendo imagen...');
            ExecuteProcess('cmd', '/c start arbol_bst.png');
        end
        else
            WriteLn('Error: Asegúrate de tener Graphviz instalado');
    end;
end;

// Mostrar menú
procedure MostrarMenu;
begin
    WriteLn('========== ARBOL BST DESDE JSON ==========');
    WriteLn('1. Cargar archivo JSON');
    WriteLn('2. Mostrar arbol (InOrder)');
    WriteLn('3. Generar codigo Graphviz');
    WriteLn('4. Salir');
    WriteLn('==========================================');
    Write('Seleccione una opcion: ');
end;

// Programa principal
var
    opcion: Integer;
begin
    WriteLn('=== SISTEMA DE ARBOL BST CON CARGA JSON ===');
    WriteLn('==========================================');
    WriteLn;
    
    InicializarArbol(arbol);
    
    repeat
        MostrarMenu;
        ReadLn(opcion);
        WriteLn;
        
        case opcion of
            1: begin
                Write('Ingrese el nombre del archivo JSON: ');
                ReadLn(nombreArchivo);
                if nombreArchivo = '' then
                    nombreArchivo := 'datos.json';
                CargarDesdeJSON(arbol, nombreArchivo);
            end;
            
            2: begin
                WriteLn('========== CONTENIDO DEL ARBOL (InOrder) ==========');
                if arbol.raiz <> nil then
                    RecorridoInOrder(arbol.raiz)
                else
                    WriteLn('El arbol está vacio. Cargue primero un archivo JSON.');
                WriteLn('===================================================');
            end;
            
            3: begin
                if arbol.raiz <> nil then
                    GenerarGraphviz(arbol)
                else
                    WriteLn('El arbol está vacío. Cargue primero un archivo JSON.');
            end;
            
            4: begin
                WriteLn('¡Gracias por usar el programa!');
            end;
            
            else
                WriteLn('Opción inválida. Intente nuevamente.');
        end;
        
        if opcion <> 4 then
        begin
            WriteLn;
            Write('Presione Enter para continuar...');
            ReadLn;
            WriteLn;
        end;
        
    until opcion = 4;
end.