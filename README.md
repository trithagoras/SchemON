# SchemON - Schema Object Notation

SchemON is a schema object notation that is designed to be human readable and writable. It is a simple and easy to understand schema language that can be used to define the structure of data without explicitly defining the data itself. It was designed with messages in mind, but can be used to define any kind of data structure.

## Types

SchemON supports the following types:
* `string` - a string of characters
* `int` - an integer
* `float` - a floating point number
* `bool` - a boolean value
* `char` - a single character
* `[<type>]` - a list of the specified type
* `{(<key>: <type>)+}` - a dictionary with the specified key and value pairs

Types are limited to the basic types that are supported by most programming languages for compatibility. More complex types can be defined using dictionaries and you are able to reuse your own defined messages as types. You can only refer to messages that were defined earlier in the file to avoid circular recursion, etc.

## Example

Here is an example of a SchemON schema that defines a base `packet` type and a `translate` packet that includes the base type in its definition.

```js
packet: {
  id: int
}

translate: {
  packet: packet,
  entityId: int,
  dx: float,
  dy: float
}
```

## Usage

Typically, you'd store your entire schema in a single file called `schema.schemon` and then you can use `schemon -i schema.schemon --target python3` to generate the corresponding Python code, for example.

There are a few included targets that you can use to generate code in different languages. You can also write your own target by instancing the `Encoder` class and implementing the `encode` method.