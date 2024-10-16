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

Types are limited to the basic types that are supported by most programming languages for compatibility. More complex types can be defined using dictionaries.

You are also able to create your own types and use them in your schema. The entry point of the schema is the `root` type, which is the type that is expected to be at the top level of the data structure.

If there is only a single object in the schema, the `root` type can be omitted and the object can be defined directly.

## Example

Here is a simple example that only defines the root object, and thus the `root` type can be omitted:

```js
{
    id: int,
    message: string
}
```

Here is an example of a SchemON schema that defines a message with a header and a body and uses a custom type for the timestamp:

```js
timestamp: {
    year: int,
    month: int,
    day: int,
    hour: int,
    minute: int,
    second: int
}

root: {
  header: {
    id: int,
    timestamp: timestamp
  },
  body: {
    message: string
  }
}
```