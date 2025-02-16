# Inventory Management in Haskell

Este es un sencillo sistema de gestión de inventario implementado en Haskell. Permite agregar, actualizar, eliminar productos y aplicar descuentos, además de obtener un resumen del inventario.

## Estructura de Datos

- **Product**: Representa un producto como una tupla `(Nombre, Precio, Cantidad)`.
- **Inventory**: Es una lista de productos (`[Product]`).

## Funcionalidades

### 1. Agregar un Producto
```haskell
addProduct :: Inventory -> String -> Double -> Int -> Inventory
```
Agrega un nuevo producto al inventario.

### 2. Actualizar Cantidad de un Producto
```haskell
updateQuantity :: Inventory -> String -> Int -> Inventory
```
Modifica la cantidad de un producto en el inventario.

### 3. Eliminar un Producto
```haskell
removeProduct :: Inventory -> String -> Inventory
```
Elimina un producto del inventario por su nombre.

### 4. Obtener un Resumen del Inventario
```haskell
inventorySummary :: Inventory -> (Int, Double)
```
Devuelve la cantidad total de productos y el valor total del inventario.

### 5. Buscar un Producto por su Nombre
```haskell
searchProduct :: Inventory -> String -> Maybe (Double, Int)
```
Busca un producto en el inventario y devuelve su precio y cantidad si existe.

### 6. Aplicar un Descuento a Todos los Productos
```haskell
applyDiscount :: Inventory -> Double -> Inventory
```
Aplica un descuento en porcentaje a todos los productos del inventario.

## Ejecución del Programa

El código incluye una función `main` para probar las funcionalidades:
```haskell
main :: IO ()
```
Esta función:
1. Agrega productos al inventario.
2. Modifica cantidades.
3. Elimina productos.
4. Muestra un resumen del inventario.
5. Busca un producto específico.
6. Aplica un descuento a todos los productos.

## Ejemplo de Salida
```sh
Inventario Final: [("Manzanas",0.5,120)]
Total de productos en stock: 120
Valor total del inventario: 60.0
Producto no encontrado
Inventario con descuento: [("Manzanas",0.45,120)]
```

