# SchemON - Schema Object Notation

SchemON is a **Schem**a **O**bject **N**otation that is designed to be human readable and writable. It can be used to define the structure of data without explicitly defining the data itself. It was designed with messages in mind, but can be used to define any kind of data structure.

## Types

SchemON supports the following types:
* `string` - a string of characters
* `int` - an integer
* `float` - a floating point number
* `bool` - a boolean value
* `char` - a single character
* `[<type>]` - a list of the specified type
* `{(<key>: <type>)+}` - a dictionary with the specified key and value pairs

Types are limited to the basic types that are supported by most programming languages for compatibility. More complex types can be defined using dictionaries and you are able to reuse your own defined messages as types.

## Example

Here is an example of a SchemON schema that defines a base `packet` type and a `translate` packet that includes the base type in its definition.

```js
packet: {
  id: int
},

position: {
  x: float,
  y: float
},

translate: {
  packet: packet,
  entityId: int,
  dx: float,
  dy: float,
  collisions: [position]
}
```

Given the C# target, the above schema is transpiled to the following:

```cs
public class Packet {
    public int Id { get; set; }
}
public class Position {
    public float X { get; set; }
    public float Y { get; set; }
}
public class Translate {
    public Packet Packet { get; set; }
    public int EntityId { get; set; }
    public float Dx { get; set; }
    public float Dy { get; set; }
    public List<Position> Collisions { get; set; }
}
```

## Usage

Typically, you'd store your entire schema in a single file called `schema.son` and then you can use `schemon -i schema.schemon --target csharp` to generate the corresponding C# code, for example.

All currently available targets are listed below and are supplied in the `Targets.hs` module. You can also write your own target by instancing the `Encoder` class and implementing the `encode :: Program a -> String` method.

## Targets

* C#
* SchemON

## Compiling

Given you have the latest GHC compiler, simply run:
```bash
$ ghc Main.hs
```

Then you can run the newly created `Main` executable using the usage instructions listed earlier.