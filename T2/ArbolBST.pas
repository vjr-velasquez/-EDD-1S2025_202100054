program ArbolBST;

type
    // Definición del nodo del árbol
    PNodo = ^Nodo;
    Nodo = record
        ID: Integer;
        dato: String;        // Puedes agregar más campos según necesites
        izquierdo: PNodo;
        derecho: PNodo;
    end;
    
    // Definición del árbol BST
    ArbolBST = record
        raiz: PNodo;
    end;

var
    arbol: ArbolBST;
    opcion: Integer;
    id: Integer;
    dato: String;

// Crear un nuevo nodo
function CrearNodo(id: Integer; dato: String): PNodo;
var
    nuevoNodo: PNodo;
begin
    New(nuevoNodo);
    nuevoNodo^.ID := id;
    nuevoNodo^.dato := dato;
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
function InsertarNodo(raiz: PNodo; id: Integer; dato: String): PNodo;
begin
    // Si el árbol está vacío, crear nuevo nodo
    if raiz = nil then
    begin
        InsertarNodo := CrearNodo(id, dato);
        WriteLn('Nodo con ID ', id, ' insertado correctamente.');
    end
    // Si el ID es menor, insertar en el subárbol izquierdo
    else if id < raiz^.ID then
    begin
        raiz^.izquierdo := InsertarNodo(raiz^.izquierdo, id, dato);
        InsertarNodo := raiz;
    end
    // Si el ID es mayor, insertar en el subárbol derecho
    else if id > raiz^.ID then
    begin
        raiz^.derecho := InsertarNodo(raiz^.derecho, id, dato);
        InsertarNodo := raiz;
    end
    // Si el ID ya existe, no insertar
    else
    begin
        WriteLn('Error: El ID ', id, ' ya existe en el árbol.');
        InsertarNodo := raiz;
    end;
end;

// Buscar un nodo por ID
function BuscarNodo(raiz: PNodo; id: Integer): PNodo;
begin
    if (raiz = nil) or (raiz^.ID = id) then
        BuscarNodo := raiz
    else if id < raiz^.ID then
        BuscarNodo := BuscarNodo(raiz^.izquierdo, id)
    else
        BuscarNodo := BuscarNodo(raiz^.derecho, id);
end;

// Encontrar el nodo con el valor mínimo
function EncontrarMinimo(raiz: PNodo): PNodo;
begin
    while raiz^.izquierdo <> nil do
        raiz := raiz^.izquierdo;
    EncontrarMinimo := raiz;
end;

// Eliminar un nodo del árbol BST
function EliminarNodo(raiz: PNodo; id: Integer): PNodo;
var
    temp: PNodo;
begin
    if raiz = nil then
    begin
        WriteLn('Error: El ID ', id, ' no se encuentra en el árbol.');
        EliminarNodo := raiz;
    end
    else if id < raiz^.ID then
    begin
        raiz^.izquierdo := EliminarNodo(raiz^.izquierdo, id);
        EliminarNodo := raiz;
    end
    else if id > raiz^.ID then
    begin
        raiz^.derecho := EliminarNodo(raiz^.derecho, id);
        EliminarNodo := raiz;
    end
    else
    begin
        // Nodo con un hijo o sin hijos
        if raiz^.izquierdo = nil then
        begin
            temp := raiz^.derecho;
            Dispose(raiz);
            EliminarNodo := temp;
        end
        else if raiz^.derecho = nil then
        begin
            temp := raiz^.izquierdo;
            Dispose(raiz);
            EliminarNodo := temp;
        end
        else
        begin
            // Nodo con dos hijos
            temp := EncontrarMinimo(raiz^.derecho);
            raiz^.ID := temp^.ID;
            raiz^.dato := temp^.dato;
            raiz^.derecho := EliminarNodo(raiz^.derecho, temp^.ID);
            EliminarNodo := raiz;
        end;
        WriteLn('Nodo con ID ', id, ' eliminado correctamente.');
    end;
end;

// Recorrido en orden (InOrder)
procedure RecorridoInOrder(raiz: PNodo);
begin
    if raiz <> nil then
    begin
        RecorridoInOrder(raiz^.izquierdo);
        WriteLn('ID: ', raiz^.ID, ', Dato: ', raiz^.dato);
        RecorridoInOrder(raiz^.derecho);
    end;
end;

// Generar código Graphviz para visualizar el árbol
procedure GenerarGraphvizNodos(raiz: PNodo);
begin
    if raiz <> nil then
    begin
        // Generar el nodo actual
        WriteLn('    ', raiz^.ID, ' [label="ID: ', raiz^.ID, '\nDato: ', raiz^.dato, '"];');
        
        // Generar conexiones y nodos hijos
        if raiz^.izquierdo <> nil then
        begin
            WriteLn('    ', raiz^.ID, ' -> ', raiz^.izquierdo^.ID, ' [label="L"];');
            GenerarGraphvizNodos(raiz^.izquierdo);
        end;
        
        if raiz^.derecho <> nil then
        begin
            WriteLn('    ', raiz^.ID, ' -> ', raiz^.derecho^.ID, ' [label="R"];');
            GenerarGraphvizNodos(raiz^.derecho);
        end;
    end;
end;

procedure GenerarGraphviz(arbol: ArbolBST);
begin
    WriteLn('========== CÓDIGO GRAPHVIZ ==========');
    WriteLn('digraph BST {');
    WriteLn('    node [shape=box, style=rounded];');
    WriteLn('    rankdir=TB;');
    WriteLn('    ');
    
    if arbol.raiz <> nil then
        GenerarGraphvizNodos(arbol.raiz)
    else
        WriteLn('    // Árbol vacío');
    
    WriteLn('}');
    WriteLn('====================================');
    WriteLn('Copia este código en un archivo .dot y usa Graphviz para generar la imagen.');
    WriteLn('Ejemplo: dot -Tpng archivo.dot -o arbol.png');
end;

// Mostrar menú
procedure MostrarMenu;
begin
    WriteLn('========== ÁRBOL BST ==========');
    WriteLn('1. Insertar nodo');
    WriteLn('2. Buscar nodo');
    WriteLn('3. Eliminar nodo');
    WriteLn('4. Mostrar árbol (InOrder)');
    WriteLn('5. Generar código Graphviz');
    WriteLn('6. Salir');
    WriteLn('===============================');
    Write('Seleccione una opción: ');
end;

// Programa principal
begin
    InicializarArbol(arbol);
    
    repeat
        MostrarMenu;
        ReadLn(opcion);
        
        case opcion of
            1: begin
                Write('Ingrese el ID: ');
                ReadLn(id);
                Write('Ingrese el dato: ');
                ReadLn(dato);
                arbol.raiz := InsertarNodo(arbol.raiz, id, dato);
            end;
            
            2: begin
                Write('Ingrese el ID a buscar: ');
                ReadLn(id);
                if BuscarNodo(arbol.raiz, id) <> nil then
                    WriteLn('Nodo encontrado con ID: ', id)
                else
                    WriteLn('Nodo con ID ', id, ' no encontrado.');
            end;
            
            3: begin
                Write('Ingrese el ID a eliminar: ');
                ReadLn(id);
                arbol.raiz := EliminarNodo(arbol.raiz, id);
            end;
            
            4: begin
                WriteLn('Recorrido InOrder del árbol:');
                if arbol.raiz <> nil then
                    RecorridoInOrder(arbol.raiz)
                else
                    WriteLn('El árbol está vacío.');
            end;
            
            5: begin
                GenerarGraphviz(arbol);
            end;
            
            6: begin
                WriteLn('Saliendo del programa...');
            end;
            
            else
                WriteLn('Opción inválida. Intente nuevamente.');
        end;
        
        if opcion <> 6 then
        begin
            WriteLn;
            Write('Presione Enter para continuar...');
            ReadLn;
        end;
        
    until opcion = 6;
end.